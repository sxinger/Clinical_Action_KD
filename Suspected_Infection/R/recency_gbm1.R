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
                    "rBayesianOptimization"
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

#--tune hyperparameter (less rounds, early stopping)
xgb_cv_bayes <- function(max_depth, min_child_weight, subsample, 
                         eta=0.05,colsample_bytree=0.8,lambda=1,alpha=0,gamma=1) {
  cv <- xgb.cv(params = list(booster = "gbtree",
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, 
                             eta = eta,
                             colsample_bytree = colsample_bytree,
                             lambda = lambda,
                             alpha = alpha,
                             gamma = gamma,
                             objective = "binary:logistic",
                             eval_metric = "auc"),
               data = dtrain,
               nround = 100,
               # folds = folds,
               nfold = 5,
               prediction = TRUE,
               showsd = TRUE,
               early_stopping_rounds = 5,
               maximize = TRUE,
               verbose = 0)
  
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
       Pred = cv$pred)
}

OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max_depth = c(4L,10L),
                                              min_child_weight = c(1L,10L),
                                              subsample = c(0.5,0.8)),
                                init_grid_dt = NULL,
                                init_points = 10,
                                n_iter = 10,
                                acq = "ucb",
                                kappa = 2.576,
                                eps = 0.0,
                                verbose = TRUE)

#--determine number of trees, or steps (more rounds, early stopping)
bst <- xgb.cv(param=data.frame(max_depth=OPT_Res$Best_Par[1],
                               min_child_weight=OPT_Res$Best_Par[2],
                               subsample=OPT_Res$Best_Par[3],
                               eta=0.05,
                               colsample_bytree=0.8,
                               lambda=1,
                               alpha=0,
                               gamma=1),
              dtrain,
              objective = "binary:logistic",
              metrics = "auc",
              maximize = TRUE,
              nrounds=1000,
              folds = folds,
              early_stopping_rounds = 50,
              print_every_n = 50,
              prediction = F)

steps<-which(bst$evaluation_log$test_auc_mean==max(bst$evaluation_log$test_auc_mean))

##================================ validation =====================================
watchlist<-list(test=dtest)
xgb_tune<-xgb.train(data=dtrain,
                    max_depth=OPT_Res$Best_Par[1],
                    min_child_weight=OPT_Res$Best_Par[2],
                    subsample=OPT_Res$Best_Par[3],
                    maximize = TRUE,
                    eta=0.01,
                    nrounds=steps,
                    eval_metric="auc",
                    objective="binary:logistic",
                    verbose = 0)

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


##========================== save results ============================
gbm_out<-list(data=Xy_sparse,
              valid_out=rbind(valid_cv,valid),
              model=xgb_tune,
              var_imp=feat_sel,
              opt_hyper=OPT_Res,
              opt_steps=steps)

saveRDS(gbm_out,file=paste0("./output/gbm1_rec_fs",k,".rda"))

