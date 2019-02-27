#### predict SI: latest value abstraction, glm ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "h2o"
                  ))

##==============load data=================
Xy_sparse<-readRDS("./data/Xy_sp_rec.rda")
x_mt<-Xy_sparse$x_mt
y_mt<-Xy_sparse$y_mt

##================partition==================
sample_idx<-readRDS("./data/sample_idx.rda")

train_mt<-cbind(x_mt[which(sample_idx$rs$rs_part73=="T"),],
                label=1-y_mt[which(sample_idx$rs$rs_part73=="T"),]$CASE_CTRL)

test_mt<-cbind(x_mt[which(sample_idx$rs$rs_part73=="V"),],
               label=1-y_mt[which(sample_idx$rs$rs_part73=="V"),]$CASE_CTRL)

##=================tune=========================
#initialize h2o
h2o.init(nthreads=-1)

## global parameters
hyper_params<-list(alpha=c(1,0.8,0.5,0.3))

pred_idx<-which(!colnames(train_mt) %in% c("label"))
target_idx<-which(colnames(train_mt)=="label")

col_encode<-data.frame(col_name=colnames(train_mt),
                       col_code=paste0("C",1:ncol(train_mt)),
                       stringsAsFactors = F)
colnames(train_mt)<-col_encode$col_code

#in case break--
# h2o.remove_all()
train_h2o<-as.h2o(as.matrix(train_mt)) #column order doesn't change
alpha_grid<-h2o.grid(x=pred_idx,
                     y=target_idx,  
                     training_frame=train_h2o,
                     algorithm = "glm",
                     grid_id = "elastnet_alpha_grid",
                     family="binomial",
                     solver="COORDINATE_DESCENT",   #same optimization method as glmnet
                     # fold_column = paste0("C",fold_idx),
                     nfolds=5,
                     lambda_search=TRUE,
                     early_stopping = TRUE,
                     # standardize = TRUE,
                     # missing_values_handling="Skip",
                     remove_collinear_columns=TRUE,
                     keep_cross_validation_predictions =T,
                     hyper_params = hyper_params,
                     search_criteria = list(strategy = "Cartesian"))

# alpha search for elastic net
sortedGrid<-h2o.getGrid("elastnet_alpha_grid",sort_by = "auc",decreasing = TRUE)
alpha_opt_model<-h2o.getModel(sortedGrid@model_ids[[1]])
alpha_opt<-alpha_opt_model@parameters$alpha

##================================ validation
bst_grid_cv<-h2o.getFrame(alpha_opt_model@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])
valid_cv<-data.frame(ENCOUNTER_NUM = row.names(train_mt),
                     valid_type = 'T',
                     pred = as.data.frame(bst_grid_cv)$p1,
                     real = train_mt[,target_idx],
                     stringsAsFactors = F)

colnames(test_mt)<-col_encode$col_code
test_h2o<-as.h2o(as.matrix(test_mt[,pred_idx]))
bst_grid_fit<-h2o.predict(alpha_opt_model,newdata=test_h2o)
valid<-data.frame(ENCOUNTER_NUM = row.names(test_mt),
                  valid_type = 'V',
                  pred = as.data.frame(bst_grid_fit)$p1,
                  real = test_mt[,target_idx],
                  stringsAsFactors = F)

##============================= variable importance
feat_dict<-readRDS("./data/feat_uni_aug.rda")

var_imp<-h2o.varimp(alpha_opt_model) %>%
  left_join(col_encode,by=c("names"="col_code")) %>%
  dplyr::filter(coefficients != 0) %>%
  dplyr::select(col_name,coefficients,sign) %>%
  dplyr::rename(Feature=col_name) %>%
  dplyr::mutate(rank=1:n()) %>%
  left_join(feat_dict,by=c("Feature"="VARIABLE"))

var_imp2<-var_imp %>% filter(!is.na(NAME_CHAR)) %>%
  bind_rows(var_imp %>% filter(is.na(NAME_CHAR)&!grepl("\\_(\\-)?[0-9]+yr$",Feature)) %>%
              dplyr::select(-NAME_CHAR,-CONCEPT_PATH,-enc_wi,-odds_ratio_emp,-log_odds_ratio_sd,-uni_p_val) %>%
              left_join(feat_dict %>% dplyr::select(-VARIABLE),
                        by=c("Feature"="CONCEPT_CD"))) %>%
  bind_rows(var_imp %>% filter(is.na(NAME_CHAR)&grepl("\\_(\\-)?[0-9]+yr$",Feature)) %>%
              dplyr::select(-NAME_CHAR,-CONCEPT_PATH,-enc_wi,-odds_ratio_emp,-log_odds_ratio_sd,-uni_p_val) %>%
              dplyr::mutate(Feature2=gsub("\\_(\\-)?[0-9]+yr$","",Feature)) %>%
              left_join(feat_dict %>% dplyr::select(-CONCEPT_CD),
                        by=c("Feature2"="VARIABLE")) %>%
              dplyr::select(-Feature2))

var_imp2 %<>%
  dplyr::select(-CONCEPT_CD) %>%
  # filter(!is.na(NAME_CHAR)) %>%
  group_by(Feature) %>%
  arrange(rank) %>%
  dplyr::slice(1:1) %>%
  ungroup %>%
  arrange(rank) %>%
  dplyr::mutate(uni_p_val=round(uni_p_val,6))

##=============================finalize results==================================
# save results
glm_out<-list(valid_out=rbind(valid_cv,valid),
              model=alpha_opt_model,
              var_imp=var_imp2,
              hyper_param=alpha_opt)

pROC::auc(valid$real,valid$pred) #0.9086
pROC::ci.auc(valid$real,valid$pred) #95% CI: 0.9036-0.9136 (DeLong)

k<-nrow(var_imp) #968
saveRDS(glm_out,file=paste0("./output/rev_glm1_rec_fs",k,".rda"))


#close h2o instance
h2o.shutdown(prompt = FALSE)


##=============================review results==========================
k<-968
glm_out<-readRDS(paste0("./output/glm1_rec_fs",k,".rda"))
var_imp<-glm_out$var_imp
valid<-glm_out$valid_out %>% filter(valid_type=="V")
pROC::ci.auc(valid$real,valid$pred)


