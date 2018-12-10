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
  unite("TVAL_CHAR",c("VARIABLE","NVAL_NUM2"),sep="_") %>%
  dplyr::mutate(VARIABLE=paste0("num_",VARIABLE)) %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE,CASE_CTRL)

data_at_enc<-data_at_enc %>%
  anti_join(data_at_enc_discrt,by="VARIABLE") %>%
  bind_rows(data_at_enc_discrt)

saveRDS(data_at_enc,file="./data/data_at_enc_discrt.rda")
  