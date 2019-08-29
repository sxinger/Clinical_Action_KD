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

##current cohort
cohort<-readRDS("./data/SI_enroll.rda") %>%
  filter(SI_SINCE_TRIAGE<=13) %>%
  dplyr::mutate(AtLeast_Init=pmin(C_SINCE_TRIAGE,ABX_SINCE_TRIAGE,LACTATE_SINCE_TRIAGE,IV_0_SINCE_TRIAGE,na.rm=T),
                AtLeast_ABX_IV=pmin(ABX_SINCE_TRIAGE,IV_0_SINCE_TRIAGE,na.rm=T),
                TRT3HR_COMPLT3=pmax(C_SINCE_TRIAGE,ABX_SINCE_TRIAGE,LACTATE_SINCE_TRIAGE),
                TRT3HR_COMPLT_IND=as.numeric(!is.na(TRT3HR_COMPLT)),
                TRT3HR_COMPLT_FAST_IND=case_when(((!is.na(IV_TRIGGER_SINCE_TRIAGE)&(IV_2_SINCE_TRIAGE-IV_0_SINCE_TRIAGE)<=2)|
                                                    (is.na(IV_TRIGGER_SINCE_TRIAGE)&!is.na(TRT3HR_COMPLT)))&
                                                   pmax(C_SINCE_TRIAGE,ABX_SINCE_TRIAGE,LACTATE_SINCE_TRIAGE,na.rm=T) <= 3 ~ 1,
                                                 TRUE ~ 0)) %>%
  left_join(readRDS("./data/pat_at_enc.rda"),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  dplyr::mutate(MORT_AT=(!is.na(DEATH_DATE)&DEATH_DATE<=TRIAGE_START+(END_SINCE_TRIAGE+24)*60*60&DEATH_DATE>=TRIAGE_START)*1,
                MORT_30D=(!is.na(DEATH_DATE)&DEATH_DATE<=TRIAGE_START+(END_SINCE_TRIAGE+30*24)*60*60&DEATH_DATE>=TRIAGE_START)*1)

mean(cohort$TRT3HR_COMPLT_FAST_IND)
mean(cohort$MORT_AT) #5%
mean(cohort$MORT_30D) #11%

################################################################################
##send over to de-id server to collect their current encounter number
# dbWriteTable(conn,"SEPSIS_ENC_OLD",cohort,temporary=T,overwrite=T)

##add patient number
##save the mapping real encounter and patient id on id server
##collect additional outcome variables: Mortality, 30d Mortality
##patient table supplement: SEPSIS_ENC_SUPP
################################################################################


##collect the supplemental data
# enc_outcome<-dbGetQuery(conn,"select * from SEPSIS_ENC_SUPP")


##collect supplemental statistics
#mortality quick view
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




