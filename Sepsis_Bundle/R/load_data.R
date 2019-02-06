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


##=======load cohort and define targets=========
enroll<-dbGetQuery(conn,"select * from ED_SI_SEPSIS_STAGE") %>%
  filter(DT_CLASS != 'U') %>%
  dplyr::mutate(SEPSIS_IND=as.numeric(!is.na(SIRS_2_SINCE_TRIAGE)*
                                      !is.na(OD_1_SINCE_TRIAGE)*
                                      !is.na(OD_2_SINCE_TRIAGE)*
                                      !is.na(OD_3_SINCE_TRIAGE)*
                                      !is.na(OD_4_SINCE_TRIAGE)*
                                      !is.na(OD_5_SINCE_TRIAGE)*
                                      !is.na(OD_6_SINCE_TRIAGE)*
                                      !is.na(OD_7_SINCE_TRIAGE)*
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
                TRT3HR_COMPLT=pmax(C_SINCE_TRIAGE,
                                   ABX_SINCE_TRIAGE,
                                   LACTATE_SINCE_TRIAGE,
                                   IV_COMPLT_SINCE_TRIAGE))
  
N<-length(unique(enroll$ENCOUNTER_NUM))
P<-sum(enroll$SEPSIS_IND)
saveRDS(enroll,file="./data/SI_enroll.rda")


##==============load patient-level data==================
pat_at_enc<-readRDS("../Suspected_Infection/data/pat_at_enc.rda") %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM"))

saveRDS(pat_at_enc,file="./data/pat_at_enc.rda")


##=============load data at encounter===================
data_at_enc<-readRDS("../Suspected_Infection/data/data_at_enc.rda") %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM"))

saveRDS(data_at_enc,file="./data/data_at_enc.rda")


##=============load data before encounter===================
data_bef_enc<-readRDS("../Suspected_Infection/data/data_bef_enc.rda") %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM"))

saveRDS(data_bef_enc,file="./data/data_bef_enc.rda")



##===========cut out Sepsis cohort for Lakmal============##
#--UHC Diagnosis assigned: UHC|CCSICD9DIAG:2
#--Specific types of antibiotics were administered within 24 hours since triage start
#established connection (run line 14,15)
uhc_ccs2<-dbGetQuery(conn,"select s.patient_num, s.encounter_num
                           from ED_SI_SEPSIS_STAGE s
                           where exists (select 1 from blueherondata.observation_fact obs
                                         where obs.encounter_num = s.encounter_num and 
                                               obs.concept_cd like 'UHC|CCSICD9DIAG:2%')")

# 36,356/56,870 has UHC diagnosis (64%)

enroll_sub<-readRDS("./data/SI_enroll.rda") %>%
  semi_join(uhc_ccs2,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  filter(grepl("((CEFEPINE)|(LEVOFLOXACIN)|(PIPERACILLIN)|(TAZOBACTAM)|(TOBRAMYCIN)|(VANCOMYCIN))+",
               ANTIBIO_GENERIC)) %>%
  filter(ABX_SINCE_TRIAGE <= 24) %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM)

#16,391/56,870 (28%)

dbWriteTable(conn,"SEPSIS_PAT_ENC_NUM",enroll_sub,overwrite=T)




