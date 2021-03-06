#### 3hr completion prediction ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
library("Matrix") 
require_libraries(c(
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",
                    "xgboost",
                    "h2o",
                    "pdp"
                  ))

##=====================model development=======================
## global parameters
eval_metric<-"auc"
objective<-"binary:logistic"
grid_params_tree<-expand.grid(
  max_depth=c(4,6,10),
  # max_depth=10,
  eta=c(0.3,0.1,0.02,0.01),
  eta=0.02,
  min_child_weight=1,
  subsample=0.8,
  colsample_bytree=0.8, 
  gamma=1
)

## Load in fact_stack and pat_tbl
fact_stack<-readRDS("./data/sepsis_presel_long_ohc.rda")
rand_sample<-readRDS("./data/rand_sample_idx.rda")


## partition 
x_mt<-fact_stack %>%
  filter(!grepl("^((Lactate)|(Organ Dysfunc_OD:IL))+",VARIABLE_agg)) %>% # drop lactate
  filter(!grepl("((Infection Grp POA_038 Septicemia)|(Infection POA_038))+",VARIABLE_agg)) %>% # drop septicemia dx POA
  filter(!grepl("(Scr_)+",VARIABLE_agg)) %>% # duplicates
  filter(!grepl("(Organ Dysfunc_distincts)+",VARIABLE_agg)) %>% # drop distinct OD sites (confound with hypotension)
  # filter(!grepl("^((SBP_)|(MAP_)|(DBP_)|(Organ Dysfunc_OD:Hypotension))+",VARIABLE_agg)) %>% # drop hypotension-related
  semi_join(rand_sample,by="ENCOUNTER_NUM") %>%
  arrange(ENCOUNTER_NUM) %>%
  long_to_sparse_matrix(.,
                        id="ENCOUNTER_NUM",
                        variable="VARIABLE_agg",
                        val="NVAL")

y_mt<-rand_sample %>%
  mutate(subgrp=ifelse(subgrp > 1, 2, subgrp)) %>%
  arrange(ENCOUNTER_NUM)

mean(y_mt$fast_trt3hr) #1.3%

all(row.names(x_mt)==y_mt$ENCOUNTER_NUM)


## modeling
out_grp<-list()
# sub-group analysis
for(grp in 1:2){
  if(grp==1){
    dtrain<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),],
                        label=y_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),]$fast_trt3hr_mix)
    
    dtest<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),],
                       label=y_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),]$fast_trt3hr_mix)
  }else{
    dtrain<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),],
                        label=y_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),]$fast_trt3hr)
    
    dtest<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),],
                       label=y_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),]$fast_trt3hr)
  }
  
  ## tune
  verb<-TRUE
  bst_grid<-c()
  bst_grid_cv<-c()
  metric_name<-paste0("test_", eval_metric,"_mean")
  metric_sd_name<-paste0("test_", eval_metric,"_std")
  # grid_params<-grid_params_reg
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
                  early_stopping_rounds = 50,
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
  valid_cv<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which((y_mt$part73=="T")&(y_mt$subgrp==grp))],
                       valid_type = 'T',
                       pred = bst_grid_cv[,which.max(bst_grid$metric)],
                       real = getinfo(dtrain,"label"),
                       stringsAsFactors = F)
  # validation
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
  
  valid<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which((y_mt$part73=="V")&(y_mt$subgrp==grp))],
                    valid_type = 'V',
                    pred = predict(xgb_tune,newdata=dtest),
                    real = getinfo(dtest,"label"),
                    stringsAsFactors = F)
  
  # variable importance
  var_imp<-xgb.importance(colnames(x_mt),model=xgb_tune) %>%
    filter(Gain > 0) %>%
    mutate(Gain_rescale = round((Gain/Gain[1])*100)) %>%
    dplyr::mutate(rank = 1:n())
  
  # top k partial
  feat_dict<-readRDS("./data/presel_data_dict.rda")
  
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
  gbm_out<-list(valid_out=valid,
                model=xgb_tune,
                var_imp=var_imp,
                part_eff=part_eff_topk,
                hyper_param=hyper_param)
  
  out_grp[[paste0("grp",grp)]]<-gbm_out

}
saveRDS(out_grp,file="./output/subgrp_analysis_gbm.rda")

##=====================post-validation====================
#cross validate models of two groups
out_grp<-readRDS("./output/subgrp_analysis_gbm.rda")

## Load in fact_stack and pat_tbl
fact_stack<-readRDS("./data/sepsis_presel_long_ohc.rda")
rand_sample<-readRDS("./data/rand_sample_idx.rda")

## partition 
x_mt<-fact_stack %>%
  filter(!grepl("^((Lactate)|(Organ Dysfunc_OD:IL))+",VARIABLE_agg)) %>% # drop lactate
  filter(!grepl("((Infection Grp POA_038 Septicemia)|(Infection POA_038))+",VARIABLE_agg)) %>% # drop septicemia dx POA
  filter(!grepl("(Organ Dysfunc_distincts)+",VARIABLE_agg)) %>% # drop distinct OD sites (confound with hypotension)
  # filter(!grepl("^((SBP_)|(MAP_)|(DBP_)|(Organ Dysfunc_OD:Hypotension))+",VARIABLE_agg)) %>% # drop hypotension-related
  semi_join(rand_sample,by="ENCOUNTER_NUM") %>%
  arrange(ENCOUNTER_NUM) %>%
  long_to_sparse_matrix(.,
                        id="ENCOUNTER_NUM",
                        variable="VARIABLE_agg",
                        val="NVAL")

y_mt<-rand_sample %>%
  mutate(subgrp=ifelse(subgrp > 1, 2, subgrp)) %>%
  arrange(ENCOUNTER_NUM)

cross_val<-c()
for(grp in 1:2){
  if(grp==1){
    #test set for group2
    dtest<-xgb.DMatrix(data=x_mt[which(y_mt$subgrp==grp+1),],
                       label=y_mt[which(y_mt$subgrp==grp+1),]$fast_trt3hr_mix)
    #get model from group1
    xgb_tune<-out_grp[[paste0("grp",grp)]]$model
    valid<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which(y_mt$subgrp==grp+1)],
                      valid_type = 'Mod1_CV2',
                      pred = predict(xgb_tune,newdata=dtest),
                      real = getinfo(dtest,"label"),
                      stringsAsFactors = F)
  }else{
    #test set for group1
    dtest<-xgb.DMatrix(data=x_mt[which((y_mt$subgrp==grp-1)),],
                       label=y_mt[which((y_mt$subgrp==grp-1)),]$fast_trt3hr)
    #get model from group2
    xgb_tune<-out_grp[[paste0("grp",grp)]]$model
    valid<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which(y_mt$subgrp==grp-1)],
                      valid_type = 'Mod2_CV1',
                      pred = predict(xgb_tune,newdata=dtest),
                      real = getinfo(dtest,"label"),
                      stringsAsFactors = F)
  }
  
  cross_val %<>% bind_rows(valid)
}

cross_val %<>% 
  dplyr::mutate(ENCOUNTER_NUM=as.numeric(ENCOUNTER_NUM)) %>%
  left_join(rand_sample %>% 
              dplyr::select(ENCOUNTER_NUM,
                            BLOOD_C,ABX,LAC1,BOLUS_TRIGGER,TRIGGER_TYPE,
                            BOLUS_BEGIN,BOLUS_END,liter2_in_2hr),
            by="ENCOUNTER_NUM")

saveRDS(cross_val,file="./output/subgrp_analysis_gbm_cgv.rda")


