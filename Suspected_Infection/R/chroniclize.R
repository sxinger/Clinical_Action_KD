#### discretize ####
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


##==============load data==============
data_bef_enc<-readRDS("./data/data_bef_enc.rda")
enroll<-readRDS("./data/SI_enroll.rda")


##=============chroniclize historical records====================##
data_bef_enc %<>%
  dplyr::filter(DAY_BEF_TRIAGE<=-31) %>% # maintain a 30-day time buffer
  dplyr::mutate(CONCEPT_CD=VARIABLE,
                TVAL_CHAR=as.character(round(DAY_BEF_TRIAGE/365.25))) %>%
  dplyr::mutate(VARIABLE=paste0(VARIABLE,"_",TVAL_CHAR,"yr"))


#------------augment feature dictionary with the discretized features-------------
N<-nrow(enroll)
P<-sum(enroll$CASE_CTRL)
feat_bef_enc2<-data_bef_enc %>%
  left_join(enroll %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  group_by(VARIABLE,CONCEPT_CD) %>%
  dplyr::summarize(enc_wi = length(unique(ENCOUNTER_NUM)),
                   enc_wo = N-length(unique(ENCOUNTER_NUM)),
                   pos_wi = length(unique(ENCOUNTER_NUM*CASE_CTRL))-1,
                   pos_wo = P-(length(unique(ENCOUNTER_NUM*CASE_CTRL))-1),
                   q_min=0,q_10=round(length(unique(ENCOUNTER_NUM))/N,2),q_max=1) %>%
  ungroup %>%
  dplyr::mutate(neg_wi=enc_wi-pos_wi,
                neg_wo=enc_wo-pos_wo) %>%
  dplyr::mutate(pos_p_wi=pos_wi/enc_wi,
                pos_p_wo=pos_wo/enc_wo) %>%
  dplyr::mutate(odds_ratio_emp=round(pos_p_wi/pos_p_wo,2),
                log_odds_ratio_sd=sqrt(1/pos_wi+1/pos_wo+1/neg_wi+1/neg_wo)) %>%
  dplyr::mutate(odds_ratio_emp_low=exp(log(odds_ratio_emp)-1.96*log_odds_ratio_sd),
                odds_ratio_emp_low=exp(log(odds_ratio_emp)+1.96*log_odds_ratio_sd))

feat_bef_enc2 %<>%
  bind_rows(readRDS("./data/feat_bef_enc.rda"))

saveRDS(feat_bef_enc2,file="./data/feat_bef_enc_aug.rda")
#----------------------------------------------------------------------------------

#========save data============
saveRDS(data_bef_enc,file="./data/data_bef_enc_yearly.rda")


  