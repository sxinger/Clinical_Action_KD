#### case study ####

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

dat<-readRDS("./data/rand_sample_idx.rda") %>%
  filter(part73=="V")

#pick out two encounters to follow
##948436 (encounter1), 966532(encounter2)

#collect hourly data
load("./data/sepsis_presel_cat.Rdata")
load("./data/sepsis_presel_num.Rdata")
model<-readRDS("./output/subgrp_analysis.rda")

presel_num_o<-presel_num
presel_cat_o<-presel_cat

pred<-c()
for(hr in 1:24){
  start_i<-Sys.time()
  
  presel_num<-presel_num_o %>%
    filter(ENCOUNTER_NUM %in% c(7268907,7614754)) %>%
    filter(START_SINCE_TRIAGE <= hr) %>%
    dplyr::filter(grepl("_inc",VARIABLE) | ((!grepl("_inc",VARIABLE) & NVAL > 0))) #remove suspecious 0
  
  presel_cat<-presel_cat_o %>%
    filter(ENCOUNTER_NUM %in% c(7268907,7614754)) %>%
    filter(START_SINCE_TRIAGE <= hr)
  
  # aggregate values within time-window t (if multiple values present)
  presel_num2<-presel_num %>%
    dplyr::select(ENCOUNTER_NUM, VARIABLE, NVAL,START_SINCE_TRIAGE) %>%
    unique %>% group_by(ENCOUNTER_NUM, VARIABLE) %>%
    arrange(START_SINCE_TRIAGE) %>%
    do(mutate(.
              ,agg_min = ifelse(length(NVAL)==0, NA, min(NVAL,na.rm=T))
              ,agg_max = ifelse(length(NVAL)==0, NA, max(NVAL,na.rm=T))
              ,agg_mean = ifelse(length(NVAL)==0, NA, mean(NVAL,na.rm=T))
              ,agg_initial = NVAL[1]
    )) %>%
    arrange(ENCOUNTER_NUM) %>% ungroup %>%
    dplyr::select(ENCOUNTER_NUM, VARIABLE
                  ,agg_min, agg_max, agg_mean, agg_initial
                  # ,agg_sd
    ) %>% 
    unique %>%
    gather(agg,NVAL,-ENCOUNTER_NUM,-VARIABLE) %>%
    unite("VARIABLE_agg",c("VARIABLE","agg")) %>%
    filter(!is.na(NVAL)) %>%
    bind_rows(presel_num %>%
                dplyr::select(ENCOUNTER_NUM, VARIABLE, NVAL,START_SINCE_TRIAGE) %>%
                mutate(abs_hr = abs(START_SINCE_TRIAGE)) %>%
                group_by(ENCOUNTER_NUM, VARIABLE) %>%
                top_n(n=-1,wt=abs_hr) %>% ungroup %>%
                dplyr::rename(VARIABLE_agg = VARIABLE) %>%
                dplyr::select(ENCOUNTER_NUM, VARIABLE_agg, NVAL) %>%
                unique %>% arrange(ENCOUNTER_NUM))
  
  ## collect and process categorical variables
  # aggregate values within time-window t
  presel_cat %<>%
    dplyr::select(ENCOUNTER_NUM, VARIABLE, TVAL,START_SINCE_TRIAGE) %>%
    mutate(abs_hr = abs(START_SINCE_TRIAGE)) %>%
    group_by(ENCOUNTER_NUM, VARIABLE) %>%
    dplyr::mutate(distinct_state = length(unique(TVAL))) %>%
    group_by(ENCOUNTER_NUM, VARIABLE, TVAL) %>%
    top_n(n=-1,abs_hr) %>% ungroup %>%  #get most recent value
    dplyr::select(ENCOUNTER_NUM, VARIABLE, distinct_state, TVAL, START_SINCE_TRIAGE) %>%
    arrange(ENCOUNTER_NUM)
  
  
  # use distinct_state as a new numerical variable
  dist_state<-presel_cat %>% 
    group_by(ENCOUNTER_NUM, VARIABLE, distinct_state) %>%
    top_n(n=1,wt=START_SINCE_TRIAGE) %>%
    ungroup %>%
    dplyr::select(ENCOUNTER_NUM, VARIABLE, distinct_state, START_SINCE_TRIAGE) %>%
    unique %>%
    dplyr::rename(NVAL = distinct_state) %>%
    dplyr::mutate(VARIABLE_agg = paste0(VARIABLE,"_distincts")) %>%
    dplyr::select(ENCOUNTER_NUM, VARIABLE_agg, NVAL) %>%
    unique %>%
    filter(NVAL > 1)
  
  # attach to numerical stack
  presel_num2 %<>%
    bind_rows(dist_state)
  
  # hot-encoding (use separate indicators for different categories)
  presel_cat2 <- presel_cat %>%
    dplyr::select(-distinct_state) %>%
    mutate(NVAL = 1) %>%
    unite("VARIABLE_agg",c("VARIABLE","TVAL")) %>%
    dplyr::select(ENCOUNTER_NUM,VARIABLE_agg,NVAL) %>%
    unique
  
  # put together
  data_long<-presel_num2 %>%
    bind_rows(presel_cat2) %>%
    dplyr::select(ENCOUNTER_NUM,VARIABLE_agg,NVAL) %>%
    arrange(ENCOUNTER_NUM,VARIABLE_agg)
  
  # apply model 1:
  feat<-data.frame(VARIABLE_agg=model$grp1$model$feature_names,
                    stringsAsFactors = F) 
  
  feat_fill<-feat %>%
    anti_join(data_long,by="VARIABLE_agg")
  
  x_mt1<-data_long %>%
    semi_join(feat,by="VARIABLE_agg") %>%
    bind_rows(data.frame(ENCOUNTER_NUM=0,
                         VARIABLE_agg=feat_fill$VARIABLE_agg,
                         NVAL=0,
                         stringsAsFactors = F)) %>% 
    long_to_sparse_matrix(.,
                          id="ENCOUNTER_NUM",
                          variable="VARIABLE_agg",
                          val="NVAL")
  
  # apply model 2:
  feat<-data.frame(VARIABLE_agg=model$grp2$model$feature_names,
                    stringsAsFactors = F) 
  
  feat_fill<-feat %>%
    anti_join(data_long,by="VARIABLE_agg")
  
  x_mt2<-data_long %>%
    semi_join(feat,by="VARIABLE_agg") %>%
    bind_rows(data.frame(ENCOUNTER_NUM=0,
                         VARIABLE_agg=feat_fill$VARIABLE_agg,
                         NVAL=0,
                         stringsAsFactors = F)) %>%
  long_to_sparse_matrix(.,
                        id="ENCOUNTER_NUM",
                        variable="VARIABLE_agg",
                        val="NVAL")
  
  pred_hr<-data.frame(ENCOUNTER_NUM=row.names(x_mt1)[-1],
                      pred1=predict(model$grp1$model,newdata=x_mt1[-1,]),
                      pred2=predict(model$grp2$model,newdata=x_mt2[-1,]),
                      hr_since_triage=hr,
                      stringsAsFactors = F)
  
  pred %<>% bind_rows(pred_hr)
  
  
  lapse_i<-Sys.time()-start_i
  cat("get predictions for",hr,lapse_i,units(lapse_i),".\n")
}

pred %<>%
  dplyr::mutate(lift=pred1/0.01,
                lift2=pred2/0.001)

saveRDS(pred,file="./data/case_study.rda")






