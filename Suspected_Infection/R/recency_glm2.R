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
                    "h2o"
                  ))

##==============load data=================
Xy_sparse<-readRDS("./data/Xy_sp_rec.rda")
x_mt<-Xy_sparse$x_mt
y_mt<-Xy_sparse$y_mt

##==============minimal set exploration=============
#select variables
glm_out<-readRDS("./output/glm1_rec_fs1251.rda")
var_imp<-glm_out$var_imp
P<-nrow(var_imp)
k<-200 #0.9146-0.9192 (DeLong)
# k<-100 # 0.894-0.90 (DeLong)
# k<-50 # 0.8815-0.8868 (DeLong)
# k<-25 #0.8754-0.881 (DeLong)

demo<-c("AGE_GRP","RACE_","SEX_MALE","ETHNICITY","MARRIED_STATUS","LANGUAGE")  
var_lst<-c(which(grepl(paste0("(",paste(demo,collapse=")|("),")"),colnames(x_mt))),
           which(colnames(x_mt) %in% var_imp$Feature[seq_len(k)]))

#traing and testing sets
train_mt<-cbind(x_mt[which(y_mt$part73=="T"),var_lst],
                label=y_mt[which(y_mt$part73=="T"),]$CASE_CTRL)

test_mt<-cbind(x_mt[which(y_mt$part73=="V"),var_lst],
               label=y_mt[which(y_mt$part73=="V"),]$CASE_CTRL)

pred_idx<-which(!colnames(train_mt) %in% c("label"))
target_idx<-which(colnames(train_mt)=="label")

col_encode<-data.frame(col_name=colnames(train_mt),
                       col_code=paste0("C",1:ncol(train_mt)),
                       stringsAsFactors = F)
colnames(train_mt)<-col_encode$col_code

#initialize h2o
h2o.init(nthreads=-1)

h2o.removeAll()
train_h2o<-as.h2o(as.matrix(train_mt)) #column order doesn't change
fit_glm<-h2o.glm(x=pred_idx,
                 y=target_idx,  
                 training_frame=train_h2o,
                 model_id = paste0("alpha_",glm_out$hyper_param,"_k_",k),
                 family="binomial",
                 solver="COORDINATE_DESCENT",   #same optimization method as glmnet
                 nfolds=5,
                 alpha=glm_out$hyper_param,
                 lambda_search=TRUE,
                 early_stopping = TRUE,
                 remove_collinear_columns=TRUE,
                 keep_cross_validation_predictions =T)

##================================ validation =================================
bst_grid_cv<-h2o.getFrame(fit_glm@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])
valid_cv<-data.frame(ENCOUNTER_NUM = row.names(train_mt),
                     valid_type = 'T',
                     pred = as.data.frame(bst_grid_cv)$p1,
                     real = train_mt[,target_idx],
                     stringsAsFactors = F)

colnames(test_mt)<-col_encode$col_code
test_h2o<-as.h2o(as.matrix(test_mt[,pred_idx]))
bst_grid_fit<-h2o.predict(fit_glm,newdata=test_h2o)
valid<-data.frame(ENCOUNTER_NUM = row.names(test_mt),
                  valid_type = 'V',
                  pred = as.data.frame(bst_grid_fit)$p1,
                  real = test_mt[,target_idx],
                  stringsAsFactors = F)

##============================= variable importance =================================
feat_dict<-readRDS("./data/feat_at_enc.rda") %>%
  dplyr::select(VARIABLE,CONCEPT_CD,NAME_CHAR,CONCEPT_PATH,enc_wi,odds_ratio_emp)

var_imp<-h2o.varimp(fit_glm) %>%
  left_join(col_encode,by=c("names"="col_code")) %>%
  dplyr::filter(coefficients != 0) %>%
  dplyr::select(col_name,coefficients,sign) %>%
  dplyr::rename(Feature=col_name) %>%
  dplyr::mutate(rank=1:n()) %>%
  left_join(feat_dict,by=c("Feature"="VARIABLE"))

var_imp2<-var_imp %>% filter(!is.na(NAME_CHAR)) %>%
  bind_rows(var_imp %>% filter(is.na(NAME_CHAR)) %>%
              dplyr::select(-NAME_CHAR,-CONCEPT_PATH,-enc_wi,-odds_ratio_emp) %>%
              left_join(feat_dict %>% dplyr::select(-VARIABLE),
                        by=c("Feature"="CONCEPT_CD")))

var_imp2<-var_imp2 %>% filter(!is.na(NAME_CHAR)) %>%
  bind_rows(var_imp2 %>% filter(is.na(NAME_CHAR)) %>%
              dplyr::select(-NAME_CHAR,-CONCEPT_PATH,-enc_wi,-odds_ratio_emp) %>%
              mutate(Feature2=gsub("_[^\\_]+$","",Feature)) %>%
              left_join(feat_dict %>% dplyr::select(-CONCEPT_CD),
                        by=c("Feature2"="VARIABLE")) %>%
              dplyr::select(-Feature2))

var_imp2<-var_imp2 %>% filter(!is.na(NAME_CHAR)) %>%
  bind_rows(var_imp2 %>% filter(is.na(NAME_CHAR)) %>%
              dplyr::select(-NAME_CHAR,-CONCEPT_PATH,-enc_wi,-odds_ratio_emp) %>%
              mutate(Feature2=gsub("_[^\\_]+$","",gsub("_[^\\_]+$","",Feature))) %>%
              left_join(feat_dict %>% dplyr::select(-CONCEPT_CD),
                        by=c("Feature2"="VARIABLE")) %>%
              dplyr::select(-Feature2))

var_imp2 %<>%
  dplyr::select(-CONCEPT_CD) %>%
  group_by(Feature) %>%
  arrange(rank) %>%
  dplyr::slice(1:1) %>%
  ungroup %>%
  arrange(rank)

##=============================finalize results==================================
#check performace before saving the results
pROC::ci.auc(valid$real,valid$pred)

# save results
glm_out<-list(valid_out=rbind(valid_cv,valid),
              model=fit_glm,
              var_imp=var_imp2,
              hyper_param=glm_out$hyper_param)

k<-nrow(var_imp2)
saveRDS(glm_out,file=paste0("./output/glm1_rec_fs",k,".rda"))
write.csv(var_imp2,file=paste0("./output/glm1_rec_fs",k,".csv"),
          row.names=F)

#close h2o instance
h2o.shutdown(prompt = FALSE)

