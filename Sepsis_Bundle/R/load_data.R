#### Determine censor points for controls ####
##========prepare
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"stringr"
                     ,"ROracle"
                     ,"DBI"))

config_file<-read.csv('./config.csv')
conn<-connect_to_db("Oracle","OCI",config_file)

##==============load cohort and define targets=========
enroll<-dbGetQuery(conn,"select * from ED_SI_SEPSIS_STAGE") %>%
  filter(DT_CLASS != 'U') %>%
  filter(!(is.na(SI_SINCE_TRIAGE) & !is.na(ABX_SINCE_TRIAGE))) %>% #remove ambiguous nonSI cases
  filter(SEPSIS_IND==1 & SEPSIS_SINCE_TRIAGE<=48 & SI_SINCE_TRIAGE<=13) %>%   #all sepsis patients
  dplyr::mutate(SEPSIS_IND=as.numeric(!is.na(SIRS_2_SINCE_TRIAGE)*
                                      !is.na(OD_1_SINCE_TRIAGE)*
                                      !is.na(SI_SINCE_TRIAGE)),
                SEPSIS_ONSET=pmax(SIRS_2_SINCE_TRIAGE,
                                  SI_SINCE_TRIAGE,
                                  pmin(OD_1_SINCE_TRIAGE,
                                       OD_2_SINCE_TRIAGE,
                                       OD_3_SINCE_TRIAGE,
                                       OD_4_SINCE_TRIAGE,
                                       OD_5_SINCE_TRIAGE,
                                       OD_6_SINCE_TRIAGE,
                                       OD_7_SINCE_TRIAGE,na.rm=T))) %>%
  dplyr::mutate(TRT3HR_INIT=pmin(C_SINCE_TRIAGE,
                                 ABX_SINCE_TRIAGE,
                                 LACTATE_SINCE_TRIAGE,
                                 IV_COMPLT_SINCE_TRIAGE,na.rm=T),
                TRT3HR_COMPLT=ifelse(!is.na(IV_TRIGGER_SINCE_TRIAGE),
                                     pmax(C_SINCE_TRIAGE,
                                          ABX_SINCE_TRIAGE,
                                          LACTATE_SINCE_TRIAGE,
                                          IV_COMPLT_SINCE_TRIAGE),
                                     pmax(C_SINCE_TRIAGE,
                                          ABX_SINCE_TRIAGE,
                                          LACTATE_SINCE_TRIAGE))) %>%
  dplyr::mutate(TRT3HR_COMPLT_IND=as.numeric(!is.na(TRT3HR_COMPLT)),
                TRT3HR_COMPLT_FAST_IND=case_when(((!is.na(IV_TRIGGER_SINCE_TRIAGE)&(IV_2_SINCE_TRIAGE-IV_0_SINCE_TRIAGE)<=2)|
                                                  (is.na(IV_TRIGGER_SINCE_TRIAGE)&!is.na(TRT3HR_COMPLT)))&
                                                  ABX_SINCE_TRIAGE <= 3 ~ 1,
                                                 TRUE ~ 0))

N<-length(unique(enroll$ENCOUNTER_NUM))
P<-sum(enroll$TRT3HR_COMPLT_FAST_IND) 
cat(N,P,P/N,".\n") #12970 1050 0.08095605

saveRDS(enroll,file="./data/sepsis_enroll.rda")

##==============load patient-level data==================
pat_at_enc<-dbGetQuery(conn,"select * from SI_PAT_AT_ENC") %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%    #patient filter
  filter(!duplicated(ENCOUNTER_NUM))

saveRDS(pat_at_enc,file="./data/pat_at_enc.rda")


