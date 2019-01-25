#### predict SI: latest value abstraction, gbm ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "xgboost",
                    "pdp"
                  ))

##======================== load data =====================================
Xy_sparse<-readRDS("./data/Xy_sp_rec.rda")
x_mt<-Xy_sparse$x_mt
y_mt<-Xy_sparse$y_mt

##======================== partition ============================================
sample_idx<-readRDS("./data/sample_idx.rda")

dtrain<-xgb.DMatrix(data=x_mt[which(sample_idx$rs$rs_part73=="T"),],
                    label=y_mt[which(sample_idx$rs$rs_part73=="T"),]$CASE_CTRL)

dtest<-xgb.DMatrix(data=x_mt[which(sample_idx$rs$rs_part73=="V"),],
                   label=y_mt[which(sample_idx$rs$rs_part73=="V"),]$CASE_CTRL)

##======================== tune ============================================
start_k_i<-Sys.time()

## global parameters
eval_metric<-"auc"
objective<-"binary:logistic"
grid_params_tree<-expand.grid(
  # max_depth=c(2,6),
  max_depth=6,
  # eta=c(0.3,0.1),
  eta=0.1,
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


##================================ validation =====================================
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

##============================= variable importance =========================================
feat_dict<-readRDS("./data/feat_at_enc.rda") %>%
  dplyr::select(VARIABLE,CONCEPT_CD,NAME_CHAR,CONCEPT_PATH,enc_wi,odds_ratio_emp)

var_imp<-xgb.importance(colnames(x_mt),model=xgb_tune) %>%
  filter(Gain > 0) %>%
  mutate(Gain_rescale = round((Gain/Gain[1])*100)) %>%
  dplyr::mutate(rank = 1:n()) %>%
  left_join(feat_dict,by=c("Feature"="VARIABLE"))

##============================= top k partial ===============================================
feat_dict<-readRDS("./data/feat_at_enc.rda")
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

##========================== save results ============================
gbm_out<-list(data=Xy_sparse,
              valid_out=rbind(valid_cv,valid),
              model=xgb_tune,
              var_imp=feat_sel,
              part_eff=part_eff_topk,
              hyper_param=hyper_param)

saveRDS(gbm_out,file=paste0("./output/gbm1_rec_fs",k,".rda"))

