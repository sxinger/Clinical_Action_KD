###############################################################################
# This script loads the 3hr bundle details from oracle and make various labels#
# Note that the table TRT3HR_TIMESTAMPS were collected in Oracle by sql and   # 
# already existed in my schema                                                #
###############################################################################

#clean the slate
rm(list=ls()); gc()

#set up working directory
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"ROracle"
                     ,"DBI"))

## Connect to Oracle database
config_file<-read.csv('../config.csv')
conn<-connect_to_db("Oracle",config_file)


## load targets -- 3-hour treatment completion
# TRT3HR_TIMESTAMPS
trt3hr<-dbGetQuery(c_connect,"select * from TRT3HR_TIMESTAMPS") %>%
  dplyr::select(-OD1_TYPE) %>% unique %>%
  dplyr::mutate(TRT3HR_BEGIN = ifelse(is.na(BOLUS_TRIGGER),pmin(BLOOD_C,ABX,LAC1,na.rm=T),TRT3HR_BEGIN),
                TRT3HR_END1 = ifelse(is.na(BOLUS_TRIGGER),pmax(BLOOD_C,ABX,LAC1,na.rm=F),TRT3HR_END1),
                TRT3HR_END2 = ifelse(is.na(BOLUS_TRIGGER),pmax(BLOOD_C,ABX,LAC1,na.rm=F),TRT3HR_END2)) %>% #bundle completion defined differently for non-hypotensive cases
  dplyr::mutate(trt3hr_begin = ifelse(is.na(TRT3HR_BEGIN),0,1),
                trt3hr_end1 = ifelse(is.na(TRT3HR_END1),0,1),
                trt3hr_end2 = ifelse(is.na(TRT3HR_END2),0,1),
                trt3hr_end1_3hr = ifelse((is.na(TRT3HR_END1) | TRT3HR_END1 > 3),0,1),
                trt3hr_end2_3hr = ifelse((is.na(TRT3HR_END2) | TRT3HR_END2 > 3),0,1),
                liter2_in_2hr = ifelse(is.na(LITER2_IN_2HR),0,1)) %>%
  dplyr::mutate(fast_trt3hr = ifelse(is.na(BOLUS_TRIGGER),trt3hr_end1_3hr,liter2_in_2hr)) 

summary(trt3hr %>% dplyr::select(-ENCOUNTER_NUM,-TRIGGER_TYPE))


#   trt3hr_end2     trt3hr_end1_3hr trt3hr_end2_3hr liter2_in_2hr      fast_trt3hr    
#  Min.   :0.0000   Min.   :0.00    Min.   :0.000   Min.   :0.00000   Min.   :0.0000  
#  1st Qu.:0.0000   1st Qu.:0.00    1st Qu.:0.000   1st Qu.:0.00000   1st Qu.:0.0000  
#  Median :1.0000   Median :0.00    Median :0.000   Median :0.00000   Median :0.0000  
#  Mean   :0.6352   Mean   :0.23    Mean   :0.218   Mean   :0.02994   Mean   :0.2444  
#  3rd Qu.:1.0000   3rd Qu.:0.00    3rd Qu.:0.000   3rd Qu.:0.00000   3rd Qu.:0.0000  
#  Max.   :1.0000   Max.   :1.00    Max.   :1.000   Max.   :1.00000   Max.   :1.0000 


# save results
save(trt3hr,file="./data/sepsis_target_trt3hr.Rdata")


