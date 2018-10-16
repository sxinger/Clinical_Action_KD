#### predict SI: latest value abstraction, gbm ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
library("Matrix") 
require_libraries(c(
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "xgboost",
                    "pdp"
                  ))

##==============load data
data_at_enc<-readRDS("./data/data_at_enc.rda")
target<-readRDS("./data/target_sidx.rda")

##=============feature aggregation 
x_mt<-data_at_enc %>%
  group_by(ENCOUNTER_NUM,VARIABLE) %>%
  arrange(desc(START_SINCE_TRIAGE)) %>%
  dplyr::slice(1:1) %>% ## latest values
  ungroup %>%
  arrange(ENCOUNTER_NUM) %>%
  long_to_sparse_matrix(.,
                        id="ENCOUNTER_NUM",
                        variable="VARIABLE",
                        val="NVAL")

y_mt<-target %>% 
  # semi_join(data_at_enc,by="ENCOUNTER_NUM") %>%
  arrange(ENCOUNTER_NUM)

mean(y_mt$real) #12.5%

all(row.names(x_mt)==y_mt$ENCOUNTER_NUM) #alignment check

Xy_sparse<-list(x_mt=x_mt,y_mt=y_mt)
# saveRDS(Xy_sparse,"./data/Xy_sparse.rda")
rm(Xy_sparse); gc()

##================partition
dtrain<-xgb.DMatrix(data=x_mt[which(y_mt$part73=="T"),],
                    label=y_mt[which(y_mt$part73=="T"),]$real)

dtest<-xgb.DMatrix(data=x_mt[which(y_mt$part73=="V"),],
                   label=y_mt[which(y_mt$part73=="V"),]$real)

##==================tune
start_k_i<-Sys.time()

## global parameters
eval_metric<-"auc"
objective<-"binary:logistic"
grid_params_tree<-expand.grid(
  # max_depth=c(4,6,10),
  max_depth=10,
  # eta=c(0.02,0.01,0.005),
  eta=0.02,
  # min_child_weight=c(1,10),
  min_child_weight=1,
  # subsample=c(0.5,0.8,1),
  subsample=0.8,
  # colsample_bytree=c(0.5,0.8,1), 
  colsample_bytree=0.8, 
  gamma=1
)

verb<-TRUE
bst_grid<-c()
bst_grid_cv<-c()
metric_name<-paste0("test_", eval_metric,"_mean")
metric_sd_name<-paste0("test_", eval_metric,"_std")
grid_params<-grid_params_tree

for(i in seq_len(dim(grid_params)[1])){
  start_i<-Sys.time()
  param<-as.list(grid_params[i,])
  # param$scale_pos_weight=mean(as.numeric(as.character(y_mt$complt))) #inbalance sampling
  param$scale_pos_weight=1 #balance sampling
  
  bst <- xgb.cv(param,
                dtrain,
                objective = objective,
                metrics = eval_metric,
                maximize = TRUE,
                nrounds=1000,
                nfold = 5,
                early_stopping_rounds = 100,
                print_every_n = 100,
                prediction = T) #keep cv results
  
  bst_grid<-rbind(bst_grid, cbind(grid_params[i,],
                                  metric=max(bst$evaluation_log[[metric_name]]),
                                  steps=which(bst$evaluation_log[[metric_name]]==max(bst$evaluation_log[[metric_name]]))[1]))
  
  bst_grid_cv<-cbind(bst_grid_cv,bst$pred)
  
  if(verb){
    cat('finished train case:',paste0(paste0(c(colnames(grid_params),"scale_pos_weight"),"="),param,collapse="; "),
        'in',Sys.time()-start_i,units(Sys.time()-start_i),"\n")
    start_i<-Sys.time()
  }
}
hyper_param<-bst_grid[which.max(bst_grid$metric),]
valid_cv<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which(y_mt$part73=="T")],
                     valid_type = 'T',
                     pred = bst_grid_cv[,which.max(bst_grid$metric)],
                     real = getinfo(dtrain,"label"),
                     stringsAsFactors = F)


##================================ validation
watchlist<-list(test=dtest)
xgb_tune<-xgb.train(data=dtrain,
                    max_depth=hyper_param$max_depth,
                    maximize = TRUE,
                    eta=hyper_param$eta,
                    nrounds=hyper_param$steps,
                    eval_metric="auc",
                    watchlist = watchlist,
                    objective="binary:logistic",
                    print_every_n = 100)

valid<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which(y_mt$part73=="V")],
                  valid_type = 'V',
                  pred = predict(xgb_tune,newdata=dtest),
                  real = getinfo(dtest,"label"),
                  stringsAsFactors = F)

##============================= variable importance
var_imp<-xgb.importance(colnames(x_mt),model=xgb_tune) %>%
  filter(Gain > 0) %>%
  mutate(Gain_rescale = round((Gain/Gain[1])*100)) %>%
  dplyr::mutate(rank = 1:n())

######################################################################################################
##============================== iterations of testing features that may cause potential overfitting
feat_at_enc<-readRDS("./data/feat_at_enc.rda")
feat_sel<-var_imp %>%
  left_join(feat_at_enc,by=c("Feature"="VARIABLE")) %>%
  dplyr::select(Feature,rank,Gain,Gain_rescale,overall,
                pos_wi,neg_wi,pos_wo,neg_wo,empirical_odd_ratio,
                CONCEPT_CD,NAME_CHAR,CONCEPT_PATH) %>%
  group_by(Feature,rank,Gain,Gain_rescale,overall,
           pos_wi,neg_wi,pos_wo,neg_wo,empirical_odd_ratio,
           CONCEPT_CD) %>%
  dplyr::slice(1:1) %>%
  ungroup %>%
  arrange(rank)

#--round1: 
#---remove concept types: "KUMC|%","KUH|HOSP%", 
#---remove concepts: "KUH|PROC_ID:10211024","KUH|PROC_ID:277","	KUH|PROC_ID:10201618","KUH|PROC_ID:682"

#--round2:

write.csv(feat_sel,file="./output/feat_sel.csv")
######################################################################################################

##=========================== top k partial
k<-nrow(var_imp)
part_eff_topk<-c()
for(j in seq_len(k)){
  feat_j<-c(var_imp %>% 
              dplyr::select(Feature) %>%
              dplyr::slice(j) %>% unlist)
  
  #breaking points are saved in data dictionary
  pred_grid<-feat_dict %>% ungroup %>%
    filter(VARIABLE_agg == feat_j[1]) %>%
    dplyr::select(starts_with("q_")) %>%
    gather(key,val) %>%
    filter(!is.na(val))
  
  pred_grid_distinct<-pred_grid %>% dplyr::select(val) %>% unique
  names(pred_grid_distinct)<-feat_j[1]
  
  #evaluate partial effect
  part_eff<-partial(object=xgb_tune,
                    pred.var=feat_j[1],
                    train=x_mt[which(y_mt$part73=="T"),],
                    pred.grid=pred_grid_distinct,
                    prob = T,
                    progress="text")
  
  #track both predictor values and predicted probabilities at each cut
  pred_grid %<>% left_join(part_eff %>% mutate(val=get(feat_j[1])) %>%
                             dplyr::select(val,yhat),
                           by="val") %>%
    mutate(feature=feat_j[1])
  
  part_eff_topk %<>%
    bind_rows(pred_grid)
}

# save results
gbm_out<-list(data=Xy_sparse,
              valid_out=rbind(valid_cv,valid),
              model=xgb_tune,
              var_imp=feat_sel,
              # part_eff=part_eff_topk,
              hyper_param=hyper_param)

date_at_record<-Sys.Date()
saveRDS(gbm_out,file=paste0("./output/SI_gbm_recency",date_at_record,".rda"))





