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
                    "pdp"
                  ))

## Load in fact_stack and pat_tbl
fact_stack<-readRDS("./data/sepsis_presel_long_ohc.rda")
rand_sample<-readRDS("./data/rand_sample_idx.rda")
strat_sample<-readRDS("./data/")


## partition 
x_mt<-fact_stack %>%
  # filter(!grepl("^((Lactate)|(Organ Dysfunc_OD:IL))+",VARIABLE_agg)) %>% # drop lactate
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
  start_k_i<-Sys.time()
  
  ## global parameters
  eval_metric<-"auc"
  objective<-"binary:logistic"
  grid_params_tree<-expand.grid(
    # max_depth=c(4,6,10),
    max_depth=10,
    # eta=c(0.02,0.01,0.005),
    eta=0.02,
    min_child_weight=1,
    subsample=0.8,
    colsample_bytree=0.8, 
    gamma=1
  )
  
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
                  nrounds=2000,
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
  valid_cv<-data.frame(ENCOUNTER_NUM = row.names(x_mt)[which((y_mt$part73=="T")&(y_mt$subgrp==grp))],
                       valid_type = 'T',
                       pred = bst_grid_cv[,which.max(bst_grid$metric)],
                       real = getinfo(dtrain,"label"),
                       stringsAsFactors = F)
  
  
  # xgb_tune<-xgb.train(data=dtrain,
  #                     max_depth=hyper_param$max_depth,
  #                     maximize = TRUE,
  #                     eta=hyper_param$eta,
  #                     nrounds=hyper_param$steps,
  #                     eval_metric="auc",
  #                     objective="binary:logistic",
  #                     print_every_n = 100)
  
  # #candidate feature list
  # feat_cand<-xgb.importance(colnames(x_mt),model=xgb_tune) %>%
  #   filter(Gain > 0) %>%
  #   mutate(Gain_rescale = round((Gain/Gain[1])*100)) %>%
  #   dplyr::mutate(base_feat = ifelse(grepl("^(age)",Feature),1,0)) %>%
  #   dplyr::mutate(rank = 1:n())
  
  ######stepwise selection#####
  # #find minimal feature set
  # #threshold
  # p_thresh<-0.5
  # 
  # #holder for results
  # roc_obj<-list()
  # pred_step<-list()
  # 
  # #initialization (demo)
  # x_mt_sub<-fact_stack %>%
  #   semi_join(feat_cand %>% filter(base_feat==1),
  #             by=c("VARIABLE_agg"="Feature")) %>%
  #   semi_join(rand_sample,by="ENCOUNTER_NUM") %>%
  #   arrange(ENCOUNTER_NUM) 
  # 
  # rand_sample_sub<-rand_sample %>%
  #   semi_join(x_mt_sub, by = "ENCOUNTER_NUM")
  # 
  # x_mt_sub %<>%
  #   long_to_sparse_matrix(.,
  #                         id="ENCOUNTER_NUM",
  #                         variable="VARIABLE_agg",
  #                         val="NVAL")
  # 
  # y_mt_sub<-rand_sample_sub %>%
  #   arrange(ENCOUNTER_NUM)
  # 
  # all(row.names(x_mt)==y_mt$ENCOUNTER_NUM)
  # 
  # dtrain<-xgb.DMatrix(data=as.matrix(x_mt_sub[which(y_mt_sub$part73=="T"),]),
  #                     label=y_mt_sub[which(y_mt_sub$part73=="T"),]$liter2_in_2hr)
  # 
  # gbm_cv <- xgb.cv(hyper_param,
  #                  dtrain,
  #                  objective = objective,
  #                  metrics = eval_metric,
  #                  maximize = TRUE,
  #                  nrounds=10*ncol(x_mt_sub),
  #                  nfold = 5,
  #                  early_stopping_rounds = ncol(x_mt_sub),
  #                  print_every_n = ncol(x_mt_sub),
  #                  prediction = T) #keep cv results
  # 
  # cv_result<-data.frame(ENCOUNTER_NUM = row.names(x_mt_sub)[(y_mt_sub$part73=="T")],
  #                       pred = gbm_cv$pred,
  #                       real = getinfo(dtrain,"label"),
  #                       stringsAsFactors = F)
  # 
  # #save results
  # pred_step[["top1"]]<-cv_result
  # roc_obj[["top1"]]<-pROC::roc(cv_result$real,cv_result$pred)
  # 
  # i<-1
  # roc_opt<-roc_obj[["top1"]]
  # auc_inc<-c(-Inf)
  # auc_incp<-c(1)
  # auc_inc_step<-auc_inc
  # auc_incp_step<-auc_incp
  # update<-c(1)
  # 
  # while(i<=nrow(feat_cand) & 
  #       (auc_inc[i]>0 & auc_incp[i]<=p_thresh) | 
  #       (auc_inc[i]<=0 & auc_incp[i]>p_thresh)){
  #   
  #   x_mt_sub<-fact_stack %>%
  #     semi_join(feat_cand %>% filter(base_feat==1 | rank <= i),
  #               by=c("VARIABLE_agg"="Feature"))
  # 
  #   rand_sample_sub<-rand_sample %>%
  #     semi_join(x_mt_sub, by = "ENCOUNTER_NUM")
  #   
  #   x_mt_sub %<>%
  #     semi_join(rand_sample_sub,by="ENCOUNTER_NUM") %>%
  #     arrange(ENCOUNTER_NUM) %>%
  #     long_to_sparse_matrix(.,
  #                           id="ENCOUNTER_NUM",
  #                           variable="VARIABLE_agg",
  #                           val="NVAL")
  #   
  #   y_mt_sub<-rand_sample_sub %>%
  #     arrange(ENCOUNTER_NUM)
  #   
  #   mean(as.numeric(as.character(y_mt$liter2_in_2hr))) #6%
  #   
  #   all(row.names(x_mt)==y_mt$ENCOUNTER_NUM)
  #   
  #   dtrain<-xgb.DMatrix(data=as.matrix(x_mt_sub[which(y_mt$part73=="T"),]),
  #                       label=y_mt_sub[which(y_mt_sub$part73=="T"),]$liter2_in_2hr)
  #   
  #   dtest<-xgb.DMatrix(data=as.matrix(x_mt_sub[which(y_mt$part73=="V"),]),
  #                      label=y_mt_sub[which(y_mt_sub$part73=="V"),]$liter2_in_2hr)
  # 
  #   gbm_cv <- xgb.cv(hyper_param,
  #                    dtrain,
  #                    objective = objective,
  #                    metrics = eval_metric,
  #                    maximize = TRUE,
  #                    nrounds=1000,
  #                    nfold = 5,
  #                    early_stopping_rounds = 100,
  #                    print_every_n = 100,
  #                    prediction = T) #keep cv results
  #   
  #   cv_result<-data.frame(ENCOUNTER_NUM = row.names(x_mt_sub)[(y_mt_sub$part73=="T")],
  #                         pred = gbm_cv$pred,
  #                         real = getinfo(dtrain,"label"),
  #                         stringsAsFactors = F)
  #   
  #   roc_i<-pROC::roc(cv_result$real,cv_result$pred)
  #   
  #   i<-i+1
  #   pred_step[[paste0("top",i)]]<-cv_result
  #   roc_obj[[paste0("top",i)]]<-roc_i
  #   
  #   auc_inc[i]<-roc_i$auc - roc_opt$auc
  #   auc_incp[i]<-pROC::roc.test(roc_opt,roc_i,method='delong')$p.value
  #   
  #   auc_inc_step[i]<-roc_i$auc - roc_obj[[paste0("top",(i-1))]]$auc
  #   auc_incp_step[i]<-pROC::roc.test(roc_obj[[paste0("top",(i-1))]],roc_i,method='delong')$p.value
  #   
  #   if(roc_i$auc > roc_opt$auc){
  #     roc_opt<-roc_i
  #     update<-c(update,1)
  #   }else{
  #     update<-c(update,0)
  #   }
  # }
  # 
  #####################################################################################################################################
  
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
  gbm_out<-list(valid_out=rbind(valid_cv,valid),
                model=xgb_tune,
                var_imp=var_imp,
                part_eff=part_eff_topk,
                hyper_param=hyper_param)
  
  out_grp[[paste0("grp",grp)]]<-gbm_out
}

saveRDS(out_grp,file="./output/subgrp_analysis.rda")
# saveRDS(out_grp,file="./output/subgrp_analysis_w_lac.rda")




