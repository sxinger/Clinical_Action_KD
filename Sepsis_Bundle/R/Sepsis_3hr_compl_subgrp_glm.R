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
  
  ## global parameters
  hyper_params<-list(alpha=seq(1,0,by=-0.1))
  
  start_k<-Sys.time()
  cat("......tune alpha for elastic net model (include lasso) and train the model \n")
  predictors<-colnames(train_h2o_tr)[(colnames(tr_mt)!="label")]
  response<-colnames(train_h2o_tr)[(colnames(tr_mt)=="label")]
  alpha_grid<-h2o.grid(x=predictors,
                       y=response,  
                       training_frame=train_h2o_tr,
                       validation_frame=train_h2o_ts,
                       algorithm = "glm",
                       grid_id = paste0("elastnet_alpha_grid",grp),
                       family="binomial",
                       solver="COORDINATE_DESCENT",   #same optimization method as glmnet
                       lambda_search=TRUE,
                       early_stopping = TRUE,
                       missing_values_handling="Skip",
                       remove_collinear_columns=TRUE,
                       hyper_params = hyper_params,
                       search_criteria = list(strategy = "Cartesian")
  )
  ## alpha search for elastic net---if time allowed
  sortedGrid<-h2o.getGrid(paste0("elastnet_alpha_grid",grp),sort_by = "auc",decreasing = TRUE)
  alpha_opt_model<-h2o.getModel(sortedGrid@model_ids[[1]])
  alpha_opt<-alpha_opt_model@parameters$alpha
  lambda_opt<-alpha_opt_model@parameters$lambda[which.min(alpha_opt_model@model$scoring_history$deviance_test)]

  # validation
  test_h2o<-as.h2o(ts_mt)
  valid<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which((y_mt$part73=="V")&(y_mt$subgrp==grp))],
                    pred = as.data.frame(predict(alpha_opt_model,test_h2o))$p1,
                    real = ts_mt[,(colnames(ts_mt)=="label")],
                    stringsAsFactors = F)
  
  # variable importance
  decode_colnames<-data.frame(Feature=colnames(tr_mt),
                              col_code=paste0("C",seq_along(colnames(tr_mt))),
                              stringsAsFactors = FALSE)
  
  feat_elast<-h2o.varimp(alpha_opt_model) %>%
    left_join(decode_colnames,by=c("names"="col_code")) %>%
    dplyr::filter(coefficients != 0) %>%
    dplyr::select(Feature,coefficients,sign) %>% 
    unique #decode

  # save results
  gbm_out<-list(valid_out=valid,
                model=alpha_opt_model,
                var_imp=feat_elast,
                hyper_param=c(alpha_opt,lambda_opt))
  
  out_grp[[paste0("grp",grp)]]<-gbm_out
}

saveRDS(out_grp,file="./output/subgrp_analysis_glm.rda")





