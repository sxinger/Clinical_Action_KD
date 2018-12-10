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
data_at_enc<-readRDS("./data/data_at_enc.rda")
rs_idx<-readRDS("./data/rand_idx.rda")

##=============pick out numerical variables=============
num_var<-readRDS("./data/feat_at_enc.rda") %>%
  filter(!is.na(q_1))

data_at_enc_discrt<-data_at_enc %>%
  inner_join(num_var %>% dplyr::select(VARIABLE,q_5,q_10,q_15),
             by="VARIABLE") %>%
  group_by(VARIABLE) %>%
  dplyr::mutate(NVAL_NUM2=case_when(NVAL_NUM < q_5 ~ "low",
                                    NVAL_NUM >= q_5 & NVAL_NUM < q_10 ~ "mid_low",
                                    NVAL_NUM >= q_10 & NVAL_NUM < q_15 ~ "mid_high",
                                    NVAL_NUM > q_15 ~ "high",
                                    TRUE ~ "miss")) %>%
  ungroup %>%
  mutate(VARIABLE2=VARIABLE) %>%
  unite("TVAL_CHAR",c("VARIABLE2","NVAL_NUM2"),sep="_") %>%
  dplyr::mutate(VARIABLE=paste0("num_",VARIABLE)) %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE)

data_at_enc %<>%
  anti_join(num_var,by="VARIABLE") %>%
  bind_rows(data_at_enc_discrt)


#======attach discretized age=====
pat_at_enc<-readRDS("./data/pat_at_enc.rda")
age_discrt<-pat_at_enc %>%
  mutate(AGE_GRP=case_when(AGE<30 ~ "20s",
                           AGE>=30 & AGE <40 ~ "30s",
                           AGE>=40 & AGE <50 ~ "40s",
                           AGE>=50 & AGE <60 ~ "60s",
                           AGE>=60 & AGE <70 ~ "70s",
                           AGE>=70 & AGE <80 ~ "60s",
                           AGE>=80 ~ "80s")) %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,AGE_GRP) %>%
  gather(VARIABLE,TVAL_CHAR,-ENCOUNTER_NUM,-PATIENT_NUM) %>%
  mutate(TVAL_CHAR2=TVAL_CHAR) %>%
  unite("VARIABLE",c("VARIABLE","TVAL_CHAR2")) %>%
  mutate(NVAL_NUM=1,START_SINCE_TRIAGE=0)

data_at_enc %<>% 
  filter(VARIABLE!="AGE") %>%
  filter(!(VARIABLE=="SEX_MALE"&NVAL_NUM==0)) %>%
  bind_rows(age_discrt)

#========save data
saveRDS(data_at_enc,file="./data/data_at_enc_discrt.rda")



  