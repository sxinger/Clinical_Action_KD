#### Determine censor points for controls ####
##========prepare
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"ROracle"
                     ,"DBI"))

config_file<-read.csv('./config.csv')
conn<-connect_to_db("Oracle","OCI",config_file)

##=======load cohort=========
enroll<-dbGetQuery(conn,"select * from SI_CASE_CTRL")
N<-length(unique(enroll$ENCOUNTER_NUM))
P<-sum(enroll$CASE_CTRL)
# saveRDS(enroll,file="./data/SI_enroll.rda")

##==============load data at encounter================
chunk_id<-dbGetQuery(conn,"select distinct concept_prefix from SI_OBS_AT_ENC")
chunk_id %<>% 
  filter(!CONCEPT_PREFIX %in% c("KUH|HOSP_ADT_CLASS",
                                "KUMC|VISITDETAIL|HSPSERVICES",
                                "KUMC|REPORTS|NOTETYPES",
                                "KUMC|DischargeDisposition",
                                "CPT",
                                "KUH|ED_EPISODE"))

data_at_enc<-c()
feat_at_enc<-c()
for(i in seq_along(chunk_id$CONCEPT_PREFIX)){
  start_i<-Sys.time()
  
  chk_i<-dbGetQuery(conn,
                    paste0("select * from SI_OBS_AT_ENC where CONCEPT_PREFIX ='",
                           chunk_id$CONCEPT_PREFIX[i],"'")) %>%
    left_join(enroll %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL),
              by=c("PATIENT_NUM","ENCOUNTER_NUM"))

  #--re-construct variables
  if(chunk_id$CONCEPT_PREFIX[i] %in% c("KUH|MEDICATION_ID")){
    data_i<-chk_i %>%
      filter(is.na(NVAL_NUM) & is.na(UNITS_CD)) %>%
      unite("VARIABLE",c("CONCEPT_CD","MODIFIER_CD"),sep="@") %>%
      dplyr::mutate(NVAL_NUM=1) %>%
      dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL) %>% 
      unique %>%
      bind_rows(chk_i %>%
                  filter(is.na(NVAL_NUM) & !is.na(UNITS_CD)) %>%
                  unite("UNIT_MOD",c("UNITS_CD","MODIFIER_CD")) %>%
                  unite("VARIABLE",c("CONCEPT_CD","UNIT_MOD"),sep="@") %>%
                  dplyr::mutate(NVAL_NUM=1) %>%
                  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL) %>% 
                  unique) %>%
      bind_rows(chk_i %>%
                  filter(!is.na(NVAL_NUM)) %>%
                  unite("VARIABLE",c("CONCEPT_CD","UNITS_CD","MODIFIER_CD"),sep="@") %>%
                  dplyr::mutate(NVAL_NUM=1) %>%
                  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL) %>% 
                  unique)
  
  }else if(chunk_id$CONCEPT_PREFIX[i] %in% c("KUH|DI","KUH|DX_ID","KUH|PROC_ID")){
    data_i<-chk_i %>%
      unite("VARIABLE",c("CONCEPT_CD","MODIFIER_CD"),sep="@") %>%
      dplyr::mutate(NVAL_NUM=1) %>%
      dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL) %>% 
      unique
    
  }else if(chunk_id$CONCEPT_PREFIX[i] %in% c("KUH|COMPONENT_ID")){
    data_i<-chk_i %>%
      filter(MODIFIER_CD!="@") %>%
      unite("VARIABLE",c("CONCEPT_CD","UNITS_CD"),sep="@") %>%
      dplyr::mutate(NVAL_NUM=1) %>%
      dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL) %>% 
      unique
    
  }else if(chunk_id$CONCEPT_PREFIX[i] %in% c("KUH|FLO_MEAS_ID+hash","KUH|FLO_MEAS_ID+LINE")){
    data_i<-chk_i %>%
      dplyr::mutate(TVAL_CHAR=gsub(".*_","",CONCEPT_CD),
                    VARIABLE=str_replace(CONCEPT_CD,"_[0-9]+$","")) %>%
      dplyr::mutate(NVAL_NUM=ifelse(is.na(NVAL_NUM),1,NVAL_NUM)) %>%
      dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL) %>% 
      unique
    
  }else{
    data_i<-chk_i %>%
      dplyr::mutate(VARIABLE=CONCEPT_CD) %>%
      dplyr::mutate(TVAL_CHAR=NA) %>%
      dplyr::mutate(NVAL_NUM=ifelse(is.na(NVAL_NUM),1,NVAL_NUM)) %>%
      dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL) %>% 
      unique
  }
  
  #--collect summary
  feat_i<-data_i %>%
    group_by(VARIABLE) %>%
    dplyr::mutate(distinct_val=length(unique(NVAL_NUM))) %>%
    dplyr::summarize(enc_wi = length(unique(ENCOUNTER_NUM)),
                     enc_wo = N-length(unique(ENCOUNTER_NUM)),
                     pos_wi = length(unique(ENCOUNTER_NUM*CASE_CTRL))-1,
                     pos_wo = P-(length(unique(ENCOUNTER_NUM*CASE_CTRL))-1),
                     q_min=ifelse(distinct_val[1]==1,0,min(NVAL_NUM,na.rm=T)),
                     q_1=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.05)[1]),
                     q_2=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.1)[1]),
                     q_3=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.15)[1]),
                     q_4=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.2)[1]),
                     q_5=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.25)[1]),
                     q_6=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.3)[1]),
                     q_7=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.35)[1]),
                     q_8=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.4)[1]),
                     q_9=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.45)[1]),
                     q_10=ifelse(distinct_val[1]==1,
                                 round(length(unique(ENCOUNTER_NUM))/N,2),
                                 median(NVAL_NUM, na.rm=T)),
                     q_11=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.55)[1]),
                     q_12=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.6)[1]),
                     q_13=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.65)[1]),
                     q_14=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.7)[1]),
                     q_15=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.75)[1]),
                     q_16=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.8)[1]),
                     q_17=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.85)[1]),
                     q_18=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.9)[1]),
                     q_19=ifelse(distinct_val[1]==1,NA,quantile(NVAL_NUM,probs=0.95)[1]),
                     q_max=ifelse(distinct_val[1]==1,NVAL_NUM,max(NVAL_NUM,na.rm=T))) %>%
    ungroup %>%
    dplyr::mutate(neg_wi=enc_wi-pos_wi,
                  neg_wo=enc_wo-pos_wo) %>%
    dplyr::mutate(pos_p_wi=pos_wi/enc_wi,
                  pos_p_wo=pos_wo/enc_wo) %>%
    dplyr::mutate(odds_ratio_emp=round(pos_p_wi/pos_p_wo,2),
                  log_odds_ratio_sd=sqrt(1/pos_wi+1/pos_wo+1/neg_wi+1/neg_wo)) %>%
    dplyr::mutate(odds_ratio_emp_low=exp(log(odds_ratio_emp)-1.96*log_odds_ratio_sd),
                  odds_ratio_emp_low=exp(log(odds_ratio_emp)+1.96*log_odds_ratio_sd))
  
  #--stack results
  data_at_enc %<>% bind_rows(data_i)
  feat_at_enc %<>% bind_rows(feat_i)
  
  lapse_i<-Sys.time()-start_i
  cat("finish collecting data and feature summaries for",i,"in",lapse_i,units(lapse_i),".\n")
}


#=====pre-filter: frequency (0.5%)
freq_filter_rt<-0.005
data_at_enc %<>% 
  dplyr::select(-CASE_CTRL) %>%
  semi_join(feat_at_enc %>% filter(enc_wi >= round(freq_filter_rt*N)),
            by="VARIABLE")


#========save data
saveRDS(data_at_enc,file="./data/data_at_enc.rda")
saveRDS(feat_at_enc,file="./data/feat_at_enc.rda")

rm(data_at_enc,feat_at_enc); gc()

##==============load data before encounter (simple)=====================
chunk_id<-dbGetQuery(conn,"select distinct concept_prefix from SI_OBS_BEF_ENC")
chunk_id %<>%
  filter(!CONCEPT_PREFIX %in% c("KUH|PAT_ENC",
                                "KUH|MEDICATION_ID",
                                "NDC",
                                "KUH|PROC_ID"))

data_bef_enc<-c()
feat_bef_enc<-c()
for(i in seq_along(chunk_id$CONCEPT_PREFIX)){
  start_i<-Sys.time()
  
  chk_i<-dbGetQuery(conn,
                    paste0("select distinct patient_num,encounter_num,concept_cd,day_bef_triage 
                            from SI_OBS_BEF_ENC where CONCEPT_PREFIX ='",
                           chunk_id$CONCEPT_PREFIX[i],"'")) %>%
    left_join(enroll %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL),
              by=c("PATIENT_NUM","ENCOUNTER_NUM"))
  
  #--re-construct variables
  data_i<-chk_i %>%
    dplyr::mutate(VARIABLE=CONCEPT_CD) %>%
    dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,DAY_BEF_TRIAGE,CASE_CTRL) %>% 
    unique
  
  #--take first and most recent occurence
  data_i %<>%
    group_by(ENCOUNTER_NUM,VARIABLE) %>%
    arrange(DAY_BEF_TRIAGE) %>%
    dplyr::slice(c(1,n())) %>%
    ungroup
  
  #--collect summary
  feat_i<-data_i %>%
    group_by(VARIABLE) %>%
    dplyr::summarize(enc_wi = length(unique(ENCOUNTER_NUM)),
                     enc_wi_p = round(length(unique(ENCOUNTER_NUM))/N,2),
                     enc_wo = N-length(unique(ENCOUNTER_NUM)),
                     pos_wi = length(unique(ENCOUNTER_NUM*CASE_CTRL))-1,
                     pos_wo = P-(length(unique(ENCOUNTER_NUM*CASE_CTRL))-1)) %>%
    ungroup %>%
    dplyr::mutate(neg_wi=enc_wi-pos_wi,
                  neg_wo=enc_wo-pos_wo) %>%
    dplyr::mutate(pos_p_wi=pos_wi/enc_wi,
                  pos_p_wo=pos_wo/enc_wo) %>%
    dplyr::mutate(odds_ratio_emp=round(pos_p_wi/pos_p_wo,2),
                  log_odds_ratio_sd=sqrt(1/pos_wi+1/pos_wo+1/neg_wi+1/neg_wo)) %>%
    dplyr::mutate(odds_ratio_emp_low=exp(log(odds_ratio_emp)-1.96*log_odds_ratio_sd),
                  odds_ratio_emp_low=exp(log(odds_ratio_emp)+1.96*log_odds_ratio_sd))
  
  #--stack results
  data_bef_enc %<>% bind_rows(data_i)
  feat_bef_enc %<>% bind_rows(feat_i)
  
  lapse_i<-Sys.time()-start_i
  cat("finish collecting data and feature summaries for",i,"in",lapse_i,units(lapse_i),".\n")
}


#=====pre-filter: frequency
freq_filter_rt<-0.005
data_bef_enc %<>% 
  dplyr::select(-CASE_CTRL) %>%
  semi_join(feat_bef_enc %>% filter(enc_wi >= round(freq_filter_rt*N)),
            by="VARIABLE")

#========save data
saveRDS(data_bef_enc,file="./data/data_bef_enc.rda")
saveRDS(feat_bef_enc,file="./data/feat_bef_enc.rda")



