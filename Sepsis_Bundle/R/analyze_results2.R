##################################
#### analyze modeling results ####
##################################
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "ResourceSelection",
                    "pROC",
                    "ROCR",
                    "PRROC",
                    "devtools",
                    "xgboost"
                    ))
# install_github("AppliedDataSciencePartners/xgboostExplainer")


## load data
out_grp<-readRDS("./output/subgrp_analysis_gbm2.rda")

## Load in fact_stack and pat_tbl
fact_stack<-readRDS("./data/sepsis_presel_long_ohc.rda")
rand_sample<-readRDS("./data/sepsis_y_rs_idx.rda")

#try out SHAP values
library(xgboostExplainer)

#reload training and testing sets
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
  mutate(subgrp2=ifelse(subgrp > 1, 2, subgrp)) %>%
  arrange(ENCOUNTER_NUM)

all(row.names(x_mt)==y_mt$ENCOUNTER_NUM)

#group1
summary(y_mt[which((y_mt$part73=="T")&(y_mt$subgrp2==1)),]$fast_trt3hr_3comp) #0.1


dtrain1<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="T")&(y_mt$subgrp2==1)),],
                     label=y_mt[which((y_mt$part73=="T")&(y_mt$subgrp2==1)),]$fast_trt3hr_3comp)

dtest1<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==1)),],
                    label=y_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==1)),]$fast_trt3hr_3comp)

#group2
summary(y_mt[which((y_mt$part73=="T")&(y_mt$subgrp2==2)),]$fast_trt3hr_4comp) #0.02

dtrain2<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="T")&(y_mt$subgrp2==2)),],
                     label=y_mt[which((y_mt$part73=="T")&(y_mt$subgrp2==2)),]$fast_trt3hr_4comp)

dtest2<-xgb.DMatrix(data=x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==2)),],
                    label=y_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==2)),]$fast_trt3hr_4comp)

# #group3
# dtest3_y1<-xgb.DMatrix(data=rbind(x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==1)),],
#                                   x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==2)),]),
#                        label=c(y_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==1)),]$fast_trt3hr_3comp,
#                                y_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==2)),]$fast_trt3hr_3comp))
# 
# dtest3_y2<-xgb.DMatrix(data=rbind(x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==1)),],
#                                   x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==2)),]),
#                        label=c(y_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==1)),]$fast_trt3hr_4comp,
#                                y_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==2)),]$fast_trt3hr_4comp))

summary(getinfo(dtest1,"label")) #11.3%
summary(getinfo(dtest2,"label")) #1.8%

#collinearity check
var_lst<-colnames(x_mt)
var_grp<-unique(gsub("_.*","",var_lst))

cor_plot<-list()
for(i in seq_along(var_grp)){
  x_sub<-x_mt[,which(grepl(paste0("^",var_grp[i]),var_lst))]
  x_sub_cor<-cor(as.matrix(x_sub),method="spearman")
  if(dim(x_sub_cor)[1]>1){
    cor_plot[[var_grp[i]]]<-x_sub_cor
  }
}
saveRDS(cor_plot,file="./output/cor_plot.rda")


# #### exlpainer model 1
# explainer1<-buildExplainer(out_grp$grp1$model,dtrain1,type="binary",base_score=0.1,trees_idx=NULL) 
# #### explainer model 2
# explainer2<-buildExplainer(out_grp$grp2$model,dtrain2,type="binary",base_score=0.02,trees_idx=NULL)
# 
# saveRDS(list(explainer1=explainer1,explainer2=explainer2),
#         "./output/explainer.rda")



explainer<-readRDS("./output/explainer.rda")

#model1
pred_brkdn11<-explainPredictions(out_grp$grp1$model,explainer$explainer1,dtest1)
sel_col<-colnames(pred_brkdn11)[colSums(pred_brkdn11 != 0) > 0]

#model2
pred_brkdn22<-explainPredictions(out_grp$grp2$model,explainer$explainer2,dtest2)
sel_col<-colnames(pred_brkdn22)[colSums(pred_brkdn22 != 0) > 0]

n1<-nrow(dtest1)
n2<-nrow(dtest2)

pred_brkdn11_b<-c()
pred_brkdn22_b<-c()

pred_perf1_b<-c()
pred_calib1_b<-c()

pred_perf2_b<-c()
pred_calib2_b<-c()

x_val1_b<-c()
x_val2_b<-c()

#boostrapping parameters
boots<-100

for(b in 1:boots){
  start<-Sys.time()
  
  #model1
  idxset1<-sample(1:n1,n1,replace=T)
  dtest1_b<-xgboost::slice(dtest1,idxset1)
  pred_brkdn11_b_df<-pred_brkdn11[idxset1,]
  x_val1_b_df<-as.data.frame(as.matrix(x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==1)),]))[idxset1,]
  
  x_val1_b %<>%
    bind_rows(cbind(x_val1_b_df,boot=b,idx=idxset1))
  
  pred_brkdn11_b %<>%
    bind_rows(cbind(pred_brkdn11_b_df,boot=b,idx=idxset1))
  
  pred_perf1_b %<>%
    bind_rows(cbind(get_perf_summ(pred=exp(rowSums(pred_brkdn11_b_df[,-751]))/(1+exp(rowSums(pred_brkdn11_b_df[,-751]))),
                                  real=getinfo(dtest1_b,"label"))$perf_summ,
                    boot=b))
  
  pred_calib1_b %<>%
    bind_rows(cbind(get_calibr(pred=exp(rowSums(pred_brkdn11_b_df[,-751]))/(1+exp(rowSums(pred_brkdn11_b_df[,-751]))),
                               real=getinfo(dtest1_b,"label"),
                               n_bin=10),
                    boot=b))
  
  #model2
  idxset2<-sample(1:n2,n2,replace=T)
  dtest2_b<-xgboost::slice(dtest2,idxset2)
  pred_brkdn22_b_df<-pred_brkdn22[idxset2,]
  x_val2_b_df<-as.data.frame(as.matrix(x_mt[which((y_mt$part73=="V")&(y_mt$subgrp2==2)),]))[idxset2,]
  
  x_val2_b %<>%
    bind_rows(cbind(x_val2_b_df,boot=b,idx=idxset2))

  pred_brkdn22_b %<>%
    bind_rows(cbind(pred_brkdn22_b_df,boot=b,idx=idxset2))
  
  pred_perf2_b %<>%
    bind_rows(cbind(get_perf_summ(pred=exp(rowSums(pred_brkdn22_b_df[,-751]))/(1+exp(rowSums(pred_brkdn22_b_df[,-751]))),
                                  real=getinfo(dtest2_b,"label"))$perf_summ,
                    boot=b))
  
  pred_calib2_b %<>%
    bind_rows(cbind(get_calibr(pred=exp(rowSums(pred_brkdn22_b_df[,-751]))/(1+exp(rowSums(pred_brkdn22_b_df[,-751]))),
                               real=getinfo(dtest2_b,"label"),
                               n_bin=10),
                    boot=b))
  
  lapse<-Sys.time()-start
  cat("finish bootstrapped sample",b,"in",lapse,units(lapse),".\n")
}


####aggregate bootstrapped results
#model1
pred_brkdn1<-c()
var_lst<-colnames(pred_brkdn11_b)[-(751:753)]
for(v in seq_along(var_lst)){
  pred_brkdn1 %<>%
    bind_rows(pred_brkdn11_b %>%
                dplyr::select(var_lst[v],boot,idx) %>%
                dplyr::mutate(val=x_val1_b[,var_lst[v]]) %>%
                group_by(boot,val) %>%
                dplyr::summarise(effect=mean(get(var_lst[v]))) %>%
                ungroup %>%
                dplyr::mutate(var=var_lst[v]))
}

pred_perf1_b %>%
  group_by(overall_meas) %>%
  dplyr::summarise(lower=quantile(meas_val,probs=0.025,na.rm=T),
                   mid=median(meas_val,na.rm=T),
                   upper=quantile(meas_val,probs=0.975,na.rm=T)) %>%
  ungroup %>%
  View

pred_brkdn1 %>%
  dplyr::filter(var=="age") %>%
  View

#model2
pred_brkdn2<-c()
var_lst<-colnames(pred_brkdn22_b)[-(751:753)]
for(v in seq_along(var_lst)){
  pred_brkdn2 %<>%
    bind_rows(pred_brkdn22_b %>%
                dplyr::select(var_lst[v],boot,idx) %>%
                dplyr::mutate(val=x_val2_b[,var_lst[v]]) %>%
                group_by(boot,val) %>%
                dplyr::summarise(effect=mean(get(var_lst[v]))) %>%
                ungroup %>%
                dplyr::mutate(var=var_lst[v]))
}  

pred_perf2_b %>%
  group_by(overall_meas) %>%
  dplyr::summarise(lower=quantile(meas_val,probs=0.025,na.rm=T),
                   mid=median(meas_val,na.rm=T),
                   upper=quantile(meas_val,probs=0.975,na.rm=T)) %>%
  ungroup %>%
  View


data4plot<-list(partial_eff=list(pred_brkdn1=pred_brkdn1,
                                 pred_brkdn2=pred_brkdn2),
                perf_summ=list(pred_perf1=pred_perf1_b,
                               pred_perf2=pred_perf2_b),
                calibr=list(pred_calib1_b=pred_calib1_b,
                            pred_calib2_b=pred_calib2_b))

saveRDS(data4plot,file="./output/model_explainer.rda")






