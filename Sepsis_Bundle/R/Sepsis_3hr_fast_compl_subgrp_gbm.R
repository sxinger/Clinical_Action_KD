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


##==============load data=================
Xy_sparse<-readRDS("./data/Xy_sp_rec.rda")
sample_idx<-readRDS("./data/sample_idx.rda")

#select sampling scheme
rs_idx<-sample_idx$rs %>%
  unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_")

# ds_idx<-sample_idx$ds %>%
#   unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_")
# 
# ms_idx<-sample_idx$ms %>%
#   unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_")


##=============global parameters==============
eval_metric<-"auc"
objective<-"binary:logistic"
grid_params_tree<-expand.grid(
  # max_depth=c(6,10),
  max_depth=10,
  # eta=c(0.3,0.1,0.02,0.01),
  eta=0.02,
  min_child_weight=1,
  subsample=0.8,
  colsample_bytree=0.8, 
  gamma=1
)

##==============modeling=====================
rs_results<-list()
for(rs in 1:10){
  cat("Start resample:",rs,".\n")
  start_rs<-Sys.time()
  
  x_mt<-Xy_sparse$x_mt
  y_mt<-Xy_sparse$y_mt
  
  out_grp<-list()
  for(trigger_ind in c(0,1)){
    bm<-c()
    bm_nm<-c()
    
    cat("...resample",rs,"; subgroup",trigger_ind,".\n")
    start_subgrp<-Sys.time()
    
    #--partition
    start<-Sys.time()
    
    x_tr<-x_mt[which(rownames(x_mt) %in% rs_idx[rs_idx$rs_cv!=rs,]$PAT_ENC),]
    y_tr<-y_mt[which(y_mt$PAT_ENC %in% rs_idx[rs_idx$rs_cv!=rs,]$PAT_ENC),]
    
    x_ts<-x_mt[which(rownames(x_mt) %in% rs_idx[rs_idx$rs_cv==rs,]$PAT_ENC),]
    y_ts<-y_mt[which(y_mt$PAT_ENC %in% rs_idx[rs_idx$rs_cv==rs,]$PAT_ENC),]

    dtrain<-xgb.DMatrix(data=x_tr[as.numeric(!is.na(y_tr$IV_TRIGGER_SINCE_TRIAGE))==trigger_ind,],
                        label=y_tr[as.numeric(!is.na(y_tr$IV_TRIGGER_SINCE_TRIAGE))==trigger_ind,]$TRT3HR_COMPLT_FAST_IND)
    
    dtest<-xgb.DMatrix(data=x_ts[as.numeric(!is.na(y_ts$IV_TRIGGER_SINCE_TRIAGE))==trigger_ind,],
                       label=y_ts[as.numeric(!is.na(y_ts$IV_TRIGGER_SINCE_TRIAGE))==trigger_ind,]$TRT3HR_COMPLT_FAST_IND)
    
    lapse<-Sys.time()-start
    cat("...resample",rs,"; subgroup",trigger_ind,"...partitioned in",lapse,units(lapse),".\n")
    
    bm<-c(bm,paste(round(lapse),units(lapse)))
    bm_nm<-c(bm_nm,"partition")
    
    
    #--tune
    start<-Sys.time()
    
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
      # param$scale_pos_weight=mean(as.numeric(as.character(y_mt$TRT3HR_COMPLT_FAST_IND))) #inbalance sampling
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
    valid_cv<-data.frame(PAT_ENC = y_tr[as.numeric(!is.na(y_tr$IV_TRIGGER_SINCE_TRIAGE))==trigger_ind,]$PAT_ENC,
                         valid_type = 'T',
                         pred = bst_grid_cv[,which.max(bst_grid$metric)],
                         real = getinfo(dtrain,"label"),
                         stringsAsFactors = F)
    
    lapse<-Sys.time()-start
    cat("...resample",rs,"; subgroup",trigger_ind,"...tuned in",lapse,units(lapse),".\n")
    
    bm<-c(bm,paste0(lapse,units(lapse)))
    bm_nm<-c(bm_nm,"tunning")
    
    
    #--validation
    start<-Sys.time()
    
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
    
    valid<-data.frame(PAT_ENC = y_ts[as.numeric(!is.na(y_ts$IV_TRIGGER_SINCE_TRIAGE))==trigger_ind,]$PAT_ENC,
                      valid_type = 'V',
                      pred = predict(xgb_tune,newdata=dtest),
                      real = getinfo(dtest,"label"),
                      stringsAsFactors = F)
    
    lapse<-Sys.time()-start
    cat("...resample",rs,"; subgroup",trigger_ind,"...validated in",lapse,units(lapse),".\n")
    
    bm<-c(bm,paste0(lapse,units(lapse)))
    bm_nm<-c(bm_nm,"validation")    
    
    
    #--variable importance
    start<-Sys.time()
    
    var_imp<-xgb.importance(colnames(x_tr),model=xgb_tune) %>%
      filter(Gain > 0) %>%
      mutate(Gain_rescale = round((Gain/Gain[1])*100)) %>%
      dplyr::mutate(rank = 1:n())
      
    # save results
    gbm_out<-list(valid_out=valid,
                  model=xgb_tune,
                  var_imp=var_imp,
                  hyper_param=hyper_param)
      
    out_grp[[paste0("grp",trigger_ind)]]<-gbm_out
    }
    rs_results[[rs]]<-out_grp
}

saveRDS(rs_results,file="./output/subgrp_analysis_gbm_rs.rda")



# top k partial
feat_dict<-readRDS("./data/feat_at_enc.rda") %>%
  bind_rows(readRDS("./data/feat_bef_enc.rda"))

k<-min(50,nrow(var_imp))
part_eff_topk<-c()
for(j in seq_len(k)){
  feat_j<-c(var_imp %>% 
              dplyr::select(Feature) %>%
              dplyr::slice(j) %>% unlist)
  
  #breaking points are saved in data dictionary
  pred_grid<-feat_dict %>% ungroup %>%
    filter(grepl(VARIABLE,feat_j[1])) %>%
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



