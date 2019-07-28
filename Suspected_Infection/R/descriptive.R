rm(list=ls())
gc()

setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"stringr",
                     "fuzzyjoin"))


#--load data
pat_at_enc<-readRDS("./data/pat_at_enc.rda")
enroll<-readRDS("./data/SI_enroll_clean.rda")

#--load infection dx concepts
infect_icd<-read.csv("./ref/Infect_ICD.csv",stringsAsFactors = F) %>%
  dplyr::mutate(DX_CD = paste0("\\\\",gsub("\\.","\\\\.",gsub(" .*","",Infection_DX)))) %>%
  dplyr::mutate(DX_CD = case_when(!grepl("\\.",DX_CD) ~ paste0(DX_CD,"\\."),
                                  TRUE ~ DX_CD))

infect_cd_dx<-readRDS("./data/dx_cd_at_enc.rda") %>%
  dplyr::filter(CONCEPT_TYPE %in% c("KUH|DX_ID","ICD")) %>%
  regex_inner_join(infect_icd,by=c(CONCEPT_PATH = "DX_CD")) %>%
  dplyr::mutate(MODIFIER=gsub(".*@","",VARIABLE))

#--load all infection dx at the encounter
infect_dx<-readRDS("./data/dx_at_enc.rda") %>%
  inner_join(infect_cd_dx %>% dplyr::select(VARIABLE,Infection_DX, DX_CD, MODIFIER),
             by="VARIABLE") 

#--describe demographics
demo_master<-pat_at_enc %>%
  inner_join(enroll %>% dplyr::select(PATIENT_NUM, ENCOUNTER_NUM, STATUS_AT,STATUS_30D,C_ABX_LOC,CASE_CTRL),
             by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  mutate(AGE_GRP=case_when(AGE<30 ~ "20s",
                           AGE>=30 & AGE <40 ~ "30s",
                           AGE>=40 & AGE <50 ~ "40s",
                           AGE>=50 & AGE <60 ~ "60s",
                           AGE>=60 & AGE <70 ~ "70s",
                           AGE>=70 & AGE <80 ~ "60s",
                           AGE>=80 ~ "80s"))


#--age in numeric
demo_master %>%
  dplyr::select(AGE,CASE_CTRL,C_ABX_LOC) %>%
  group_by(CASE_CTRL,C_ABX_LOC) %>%
  dplyr::summarise(age_mean=mean(AGE,na.rm=T),
                   age_sd=sd(AGE,na.rm=T)) %>%
  ungroup %>% View

#--other categorical
demo_master2<-demo_master %>%
  dplyr::select(-AGE,-DEATH_DATE) %>%
  gather(demo_type,demo_val,-PATIENT_NUM,-ENCOUNTER_NUM,-CASE_CTRL,-C_ABX_LOC) %>%
  group_by(CASE_CTRL,C_ABX_LOC,demo_type) %>%
  dplyr::mutate(enc_all=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  group_by(CASE_CTRL,C_ABX_LOC,demo_type,demo_val,enc_all) %>%
  dplyr::mutate(enc_at=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  unite("C_ABX_LOC_CASE_CTRL",c("C_ABX_LOC","CASE_CTRL"),sep="_") %>%
  dplyr::mutate(demo_val2=case_when(enc_at<=10&!(demo_type %in% c("AGE_GRP","SEX_MALE","STATUS_30D","STATUS_AT")) ~ "other",
                                    demo_val %in% c("@","u","unknown") ~ "unknown",
                                    TRUE ~ demo_val)) %>%
  group_by(C_ABX_LOC_CASE_CTRL,demo_type,demo_val2,enc_all) %>%
  dplyr::summarize(enc_at=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  mutate(enc_prop=round(enc_at/enc_all,3))


demo_master2 %>% dplyr::select(demo_type,demo_val2,C_ABX_LOC_CASE_CTRL,enc_prop) %>%
  spread(C_ABX_LOC_CASE_CTRL,enc_prop,fill=0) %>%
  View


demo_master %>%
  dplyr::select(-AGE,-DEATH_DATE) %>%
  gather(demo_type,demo_val,-PATIENT_NUM,-ENCOUNTER_NUM,-CASE_CTRL) %>%
  group_by(CASE_CTRL,demo_type) %>%
  dplyr::mutate(enc_all=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  group_by(CASE_CTRL,demo_type,demo_val,enc_all) %>%
  dplyr::mutate(enc_at=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  dplyr::mutate(demo_val2=case_when(enc_at<1000&!(demo_type %in% c("AGE_GRP","SEX_MALE","STATUS_30D","STATUS_AT")) ~ "other",
                                    demo_val %in% c("@","u","unknown") ~ "unknown",
                                    TRUE ~ demo_val)) %>%
  group_by(CASE_CTRL,demo_type,demo_val2,enc_all) %>%
  dplyr::summarize(enc_at=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  mutate(enc_prop=round(enc_at/enc_all,3)) %>% 
  dplyr::select(demo_type,demo_val2,CASE_CTRL,enc_prop) %>%
  spread(CASE_CTRL,enc_prop,fill=0) %>%
  View


#--describe culture and antibiotic timings
enroll %>% 
  group_by(C_ABX_LOC) %>%
  dplyr::summarize(enc_cnt=n(),
                   mortality_at=sum(STATUS_AT),
                   mortality_30d=sum(STATUS_30D),
                   c_time_min=min(C_SINCE_TRIAGE,na.rm=T),
                   c_time_5perc=quantile(C_SINCE_TRIAGE,probs=0.05,na.rm=T),
                   c_time_25perc=quantile(C_SINCE_TRIAGE,probs=0.25,na.rm=T),
                   c_time_50perc=quantile(C_SINCE_TRIAGE,probs=0.5,na.rm=T),
                   c_time_75perc=quantile(C_SINCE_TRIAGE,probs=0.75,na.rm=T),
                   c_time_85perc=quantile(C_SINCE_TRIAGE,probs=0.85,na.rm=T),
                   c_time_95perc=quantile(C_SINCE_TRIAGE,probs=0.95,na.rm=T),
                   c_time_max=max(C_SINCE_TRIAGE,na.rm=T),
                   abx_time_min=min(ABX_SINCE_TRIAGE,na.rm=T),
                   abx_time_5perc=quantile(ABX_SINCE_TRIAGE,probs=0.05,na.rm=T),
                   abx_time_25perc=quantile(ABX_SINCE_TRIAGE,probs=0.25,na.rm=T),
                   abx_time_50perc=quantile(ABX_SINCE_TRIAGE,probs=0.5,na.rm=T),
                   abx_time_75perc=quantile(ABX_SINCE_TRIAGE,probs=0.75,na.rm=T),
                   abx_time_85perc=quantile(ABX_SINCE_TRIAGE,probs=0.85,na.rm=T),
                   abx_time_95perc=quantile(ABX_SINCE_TRIAGE,probs=0.95,na.rm=T),
                   abx_time_max=max(ABX_SINCE_TRIAGE,na.rm=T)) %>%
  ungroup %>%
  bind_rows(enroll %>% 
              dplyr::filter(C_ABX_LOC %in% c("T_C_ABX",
                                             "T_C",
                                             "ABX_T_C",
                                             "C_ABX_T",
                                             "C_T",
                                             "C_T_ABX",
                                             "No_C_ABX_T")) %>%
              group_by(C_ABX_LOC,ED_DEST_ASSGM) %>%
              dplyr::summarize(enc_cnt=n(),
                               mortality_at=sum(STATUS_AT),
                               mortality_30d=sum(STATUS_30D),
                               c_time_min=min(C_SINCE_TRIAGE,na.rm=T),
                               c_time_5perc=quantile(C_SINCE_TRIAGE,probs=0.05,na.rm=T),
                               c_time_25perc=quantile(C_SINCE_TRIAGE,probs=0.25,na.rm=T),
                               c_time_50perc=quantile(C_SINCE_TRIAGE,probs=0.5,na.rm=T),
                               c_time_75perc=quantile(C_SINCE_TRIAGE,probs=0.75,na.rm=T),
                               c_time_85perc=quantile(C_SINCE_TRIAGE,probs=0.85,na.rm=T),
                               c_time_95perc=quantile(C_SINCE_TRIAGE,probs=0.95,na.rm=T),
                               c_time_max=max(C_SINCE_TRIAGE,na.rm=T),
                               abx_time_min=min(ABX_SINCE_TRIAGE,na.rm=T),
                               abx_time_5perc=quantile(ABX_SINCE_TRIAGE,probs=0.05,na.rm=T),
                               abx_time_25perc=quantile(ABX_SINCE_TRIAGE,probs=0.25,na.rm=T),
                               abx_time_50perc=quantile(ABX_SINCE_TRIAGE,probs=0.5,na.rm=T),
                               abx_time_75perc=quantile(ABX_SINCE_TRIAGE,probs=0.75,na.rm=T),
                               abx_time_85perc=quantile(ABX_SINCE_TRIAGE,probs=0.85,na.rm=T),
                               abx_time_95perc=quantile(ABX_SINCE_TRIAGE,probs=0.95,na.rm=T),
                               abx_time_max=max(ABX_SINCE_TRIAGE,na.rm=T)) %>%
              ungroup) %>%
  View

#--describe confirmed infection status
infect_dx %>%
  group_by(CASE_CTRL) %>%
  dplyr::mutate(tot=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  group_by(CASE_CTRL,Infection_DX,tot) %>%
  dplyr::summarize(infect_cnt=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  dplyr::mutate(infect_dx_prop=round(infect_cnt/tot,2)) %>%
  arrange(desc(CASE_CTRL),desc(infect_dx_prop),desc(infect_cnt)) %>%
  filter(infect_cnt>=100) %>%
  View
  

enroll %>%
  dplyr::select(PATIENT_NUM, ENCOUNTER_NUM, STATUS_AT, STATUS_30D) %>%
  left_join(infect_dx, by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  group_by(CASE_CTRL, Infection_DX) %>%
  dplyr::summarize(enc_cnt=n(),
                   si_cnt=sum(CASE_CTRL),
                   mort_at=sum(STATUS_AT),
                   mort_30d=sum(STATUS_30D)) %>%
  ungroup %>% 
  View
  

  