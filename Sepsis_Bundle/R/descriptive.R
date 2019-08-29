rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"stringr"))


cohort<-readRDS("./data/sepsis_enroll.rda") %>%
  left_join(readRDS("./data/pat_at_enc.rda"),by=c("PATIENT_NUM","ENCOUNTER_NUM"))

#---demographic description (group 1 vs group 2, case vs control)


#---



##mortality quick view
cohort %>%
  # filter(!is.na(TRT3HR_COMPLT3)) %>%
  filter(TRT3HR_COMPLT_FAST_IND==1) %>%
  group_by(is.na(IV_TRIGGER_SINCE_TRIAGE)) %>%
  dplyr::summarize(n=n(),
                   mort_at=sum(MORT_AT),
                   mort_30d=sum(MORT_30D),
                   age_mean=mean(AGE),
                   sepsis_onset_med=median(SEPSIS_ONSET),
                   od_1_med=median(OD_1_SINCE_TRIAGE,na.rm=T),
                   od_2_med=median(OD_2_SINCE_TRIAGE,na.rm=T)) %>%
  ungroup %>%
  dplyr::mutate(mort_at_rt=mort_at/n,
                mort_30d_rt=mort_30d/n) %>%
  View
