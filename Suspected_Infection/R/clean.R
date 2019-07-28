#### Cleaning ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"
))

##=========clean up===========
enroll<-readRDS("./data/SI_enroll.rda") %>%
  dplyr::mutate(ANTIBIO_SUBTYPE_SHORT=gsub("\\ .*","",ANTIBIO_SUBTYPE)) %>% #short ABX name
  dplyr::mutate(C_ABX_LOC=case_when(is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE)~ "C_D",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE<TRANS_SINCE_TRIAGE~ "C_T",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE~ "T_C",
                                    is.na(TRANS_SINCE_TRIAGE)&is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)~ "ABX_D",
                                    !is.na(TRANS_SINCE_TRIAGE)&is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&ABX_SINCE_TRIAGE<TRANS_SINCE_TRIAGE~ "ABX_T",
                                    !is.na(TRANS_SINCE_TRIAGE)&is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&ABX_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE~ "T_ABX",
                                    is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)~ "C_ABX_D",
                                    C_SINCE_TRIAGE<TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE<TRANS_SINCE_TRIAGE ~ "C_ABX_T",
                                    C_SINCE_TRIAGE<TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE ~ "C_T_ABX",
                                    C_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE<TRANS_SINCE_TRIAGE ~ "ABX_T_C",
                                    C_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE ~ "T_C_ABX",
                                    !is.na(TRANS_SINCE_TRIAGE)&is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE) ~ "No_C_ABX_T",
                                    TRUE ~ "NonSI_D")) %>%  #identify culture and abx location
  dplyr::mutate(PRED_POINT = case_when(!is.na(SI_SINCE_TRIAGE) ~ SI_SINCE_TRIAGE,
                                       is.na(SI_SINCE_TRIAGE)&!is.na(TRANS_SINCE_TRIAGE) ~ TRANS_SINCE_TRIAGE,
                                       TRUE ~ END_SINCE_TRIAGE)) %>% #identify the prediction point
  left_join(readRDS("./data/pat_at_enc.rda") %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,DEATH_DATE),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  dplyr::mutate(STATUS_AT=case_when(DEATH_DATE <= pmax(LAST_FLOSHT_DT,ENC_END,na.rm=T) & DEATH_DATE >= TRIAGE_START ~ 1,
                                    TRUE ~ 0),
                STATUS_30D=case_when(DEATH_DATE <= pmax(LAST_FLOSHT_DT,ENC_END,na.rm=T)+30*60*60*24 & DEATH_DATE >= TRIAGE_START ~ 1,
                                     TRUE ~ 0)) #mortaily outcome for the encounter


#--suspeciously long transition time (in ED for more than 3 days) suggests systematic error in this field
quantile(enroll$TRANS_SINCE_TRIAGE,0:20/20,na.rm=T) #quick and dirty, less than 5% is above 15 hours

bef<-nrow(enroll)
enroll %<>% dplyr::filter(TRANS_SINCE_TRIAGE<=15|is.na(TRANS_SINCE_TRIAGE)) #inner upper bound = 15
aft<-nrow(enroll)
bef-aft #2,723

#--negative or suspeciously long ED discharge time (more than 3 days before dischraged from ED), suggests systematic error in this field
quantile(enroll$END_SINCE_TRIAGE[enroll$DT_CLASS=="D"],0:20/20,na.rm=T) #quick and dirty, less than 5% is above 15 hours

bef<-nrow(enroll)
enroll %<>% dplyr::filter((END_SINCE_TRIAGE>0&END_SINCE_TRIAGE<=13&DT_CLASS=="D")|DT_CLASS!="D") #inner upper bound = 13
aft<-nrow(enroll)
bef-aft #8,189


#--estimate a time point to separate community-acquired infection from hospital-acquired
quantile(enroll$C_SINCE_TRIAGE,0:20/20,na.rm=T) 

#18 hours is 85th percentile and inner upper bound ~ skew the number a bit to 24hours
bef<-nrow(enroll)
enroll %<>% dplyr::filter((C_SINCE_TRIAGE<=24)|is.na(C_SINCE_TRIAGE<=24)) 
aft<-nrow(enroll)
bef-aft #3,602

#--save the cleaned data set
saveRDS(enroll,file="./data/SI_enroll_clean.rda")



