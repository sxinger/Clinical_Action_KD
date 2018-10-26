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
                    "keras",
                    "h2o",
                    "pdp"
                  ))

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

mean(y_mt$fast_trt3hr) #1.3%

all(row.names(x_mt)==y_mt$ENCOUNTER_NUM)

#initialize h2o
h2o.init(nthreads=-1)
h2o.removeAll()

## modeling
out_grp<-list()
# sub-group analysis
for(grp in 1:2){
  if(grp==1){
    tr_mt<-cbind(data=x_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),],
                 label=y_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),]$fast_trt3hr_mix)
    
    ts_mt<-cbind(data=x_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),],
                 label=y_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),]$fast_trt3hr_mix)
  }else{
    tr_mt<-cbind(data=x_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),],
                 label=y_mt[which((y_mt$part73=="T")&(y_mt$subgrp==grp)),]$fast_trt3hr)
    
    ts_mt<-cbind(data=x_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),],
                 label=y_mt[which((y_mt$part73=="V")&(y_mt$subgrp==grp)),]$fast_trt3hr)
  }
  
  ## tune
  start_k_i<-Sys.time()
  
  train_h2o<-as.h2o(tr_mt)
  train_h2o_splits<-h2o.splitFrame(data=train_h2o,ratios=0.8)
  train_h2o_tr<-train_h2o_splits[[1]]
  train_h2o_ts<-train_h2o_splits[[2]]
  
  pred_idx<-which(colnames(tr_mt)!="label")
  target_idx<-which(colnames(tr_mt)=="label")
  train_h2o_tr[,target_idx]<-as.factor(train_h2o_tr[,target_idx])
  
  ##global values
  hyper_params<-list(
    activation=c("Rectifier","Tanh"), #default
    hidden=list(rep(50,2),rep(100,2),rep(200,2),
                rep(50,3),rep(100,3),rep(200,3),
                rep(50,4),rep(100,4),rep(200,4)),
    input_dropout_ratio=c(0.1,0.2),   #common choice: 0.1,0.2
    l1=c(1e-3,1e-5),  #default
    l2=c(1e-3,1e-5)   #default
  )
  
  search_criteria<-list(
    strategy = "RandomDiscrete",
    max_runtime_secs = 360,
    max_models = 100,
    stopping_metric = "AUC", 
    stopping_rounds=5,
    stopping_tolerance=1e-2
  )
  
  dl_random_grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id = paste0("dl_grid_random",grp),
    training_frame=train_h2o_tr,   ## make it faster
    validation_frame=train_h2o_ts, ## make it faster
    # training_frame=train_h2o,       ## cv
    # nfolds=5,                       ## cv
    x=pred_idx,
    y=target_idx,
    distribution="bernoulli",
    standardize=T,
    epochs=100,                    ## make it fast
    stopping_metric="logloss",
    stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
    stopping_rounds=10,
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    max_w2=10,                      ## can help improve stability for Rectifier
    sparse=T,
    hyper_params = hyper_params,
    search_criteria = search_criteria,
    variable_importances=T          ## track variable importance
  )
  
  ## pick out the optimal model
  dlr_grid<-h2o.getGrid(paste0("dl_grid_random",grp),sort_by="auc",decreasing=T)
  flr_opt_model<-h2o.getModel(dlr_grid@model_ids[[1]])
  flr_hyper_params<-flr_opt_model@parameters
  hyper_param_lst<-cbind(hidden=paste(flr_hyper_params$hidden,collapse = "_"),
                         input_dropout_ratio=flr_hyper_params$input_dropout_ratio,
                         l1=flr_hyper_params$l1,
                         l2=flr_hyper_params$l2)
  
  # validation
  test_h2o<-as.h2o(ts_mt)
  valid<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which((y_mt$part73=="V")&(y_mt$subgrp==grp))],
                    pred = as.data.frame(predict(flr_opt_model,test_h2o))$p1,
                    real = ts_mt[,(colnames(ts_mt)=="label")],
                    stringsAsFactors = F)
  
  # variable importance
  decode_colnames<-data.frame(Feature=colnames(tr_mt),
                              col_code=paste0("C",seq_along(colnames(tr_mt))),
                              stringsAsFactors = FALSE)
  
  feat_nnet<-h2o.varimp(flr_opt_model) %>%
    left_join(decode_colnames,by=c("variable"="col_code")) %>%
    dplyr::select(Feature,relative_importance,scaled_importance,percentage) %>% 
    unique #decode
  
  # save results
  gbm_out<-list(valid_out=valid,
                model=flr_opt_model,
                var_imp=feat_nnet,
                hyper_param=hyper_param_lst)
  
  out_grp[[paste0("grp",grp)]]<-gbm_out
}

saveRDS(out_grp,file="./output/subgrp_analysis_nnet.rda")






