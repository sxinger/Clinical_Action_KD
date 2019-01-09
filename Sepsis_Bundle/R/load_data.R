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

##=======load cohort=========
enroll<-dbGetQuery(conn,"select * from ED_SI_SEPSIS_STAGE")
N<-length(unique(enroll$ENCOUNTER_NUM))
P<-sum(enroll$SEPSIS_IND)
saveRDS(enroll,file="./data/SI_enroll.rda")

##==============load patient-level data==================
pat_at_enc<-readRDS("../Suspected_Infection/data/pat_at_enc.rda") %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM"))



##=============load data at encounter===================
data_at_enc<-readRDS("../Suspected_Infection/data/data_at_enc.rda") %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM"))



##=============load data before encounter===================
data_bef_enc<-readRDS("..Suspected_Infection/data/data_bef_enc.rda") %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM"))





