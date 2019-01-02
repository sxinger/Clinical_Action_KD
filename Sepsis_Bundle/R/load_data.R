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
readRDS("../Suspected_Infection/data/pat_at_enc.rda")

##=============load data at encounter===================
saveRDS(data_at_enc,file="./data/data_at_enc.rda")
saveRDS(feat_at_enc2,file="./data/feat_at_enc.rda")


##=============load data before encounter===================
saveRDS(data_bef_enc,file="./data/data_bef_enc.rda")
saveRDS(feat_bef_enc,file="./data/feat_bef_enc.rda")


##==============load all historical dx=====================
historical_dx<-dbGetQuery(conn,"select * from SI_HIST_DX")

#========save data
saveRDS(hist_dx,file="./data/hist_dx.rda")



