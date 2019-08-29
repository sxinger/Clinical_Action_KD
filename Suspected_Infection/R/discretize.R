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
enroll<-readRDS("./data/SI_enroll_clean.rda")


##=============pick out numerical variables=============
num_var<-readRDS("./data/feat_at_enc.rda") %>%
  filter(!is.na(q_1)&distinct_val > 5) %>%
  dplyr::select(-NAME_CHAR,-CONCEPT_PATH) %>%
  unique

#--load Q1 and Q3 of each variable
data_at_enc_discrt<-readRDS("./data/data_at_enc.rda") %>%   #already applied frequency filter
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>% #exclusions for enroll_clean
  inner_join(num_var %>% dplyr::select(VARIABLE,q_5,q_10,q_15),
             by="VARIABLE")

#--discretize the variables
data_at_enc_discrt1<-data_at_enc_discrt %>%
  group_by(VARIABLE) %>%
  dplyr::mutate(NVAL_NUM2=case_when(NVAL_NUM < q_5 ~ "low",
                                    NVAL_NUM >= q_5 & NVAL_NUM < q_10 ~ "mid_low",
                                    NVAL_NUM >= q_10 & NVAL_NUM < q_15 ~ "mid_high",
                                    NVAL_NUM >= q_15 ~ "high")) %>%
  ungroup %>%
  filter(!is.na(NVAL_NUM2)) %>%
  mutate(VARIABLE2=VARIABLE) %>%
  unite("TVAL_CHAR",c("VARIABLE2","NVAL_NUM2"),sep="_") %>%
  dplyr::mutate(VARIABLE=paste0("num_",VARIABLE)) %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE)

#--identify the "missing" category
chunk_n<-50
pat_chunk<-enroll %>% 
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM) %>%
  dplyr::mutate(chunk=sample(1:chunk_n,n(),replace=T),
                k=1)

num_var_miss<-num_var %>%
  dplyr::select(VARIABLE) %>% unique %>%
  dplyr::mutate(TVAL_CHAR=paste0(VARIABLE,"_miss"),
                VARIABLE=paste0("num_",VARIABLE),
                k=1)

data_at_enc_discrt2<-c()
for(i in 1:chunk_n){
  data_at_enc_discrt2 %<>%
    bind_rows(pat_chunk %>%
                filter(chunk==i) %>%
                full_join(num_var_miss,by="k")%>%
                anti_join(data_at_enc_discrt1,by=c("PATIENT_NUM","ENCOUNTER_NUM","VARIABLE")) %>%
                dplyr::mutate(NVAL_NUM = 1))
  
  cat("finish chunk",i,".\n")
}

ldata_at_enc_discrt<-data_at_enc_discrt1 %>%
  bind_rows(data_at_enc_discrt2)

rm(data_at_enc_discrt1,data_at_enc_discrt2)
gc()


#-----------augment feature dictionary with the discretized features-----------
N<-nrow(enroll)
P<-sum(enroll$CASE_CTRL)
feat_enc_dscrt<-data_at_enc_discrt %>%
  left_join(enroll %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  group_by(VARIABLE,TVAL_CHAR) %>%
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
                odds_ratio_emp_low=exp(log(odds_ratio_emp)+1.96*log_odds_ratio_sd)) %>%
  dplyr::rename(CONCEPT_CD=TVAL_CHAR) %>%
  left_join(num_var %>% dplyr::select(VARIABLE,NAME_CHAR,CONCEPT_PATH,CONCEPT_TYPE,CODE) %>%
              mutate(VARIABLE=paste0("num_",VARIABLE)),
            by="VARIABLE")

feat_at_enc<-readRDS("./data/feat_at_enc.rda") %>%
  bind_rows(feat_enc_dscrt)

#--------------------------------------------------------------------------------------


#======attach discretized age==========
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
  filter(!(VARIABLE=="SEX_MALE"&NVAL_NUM==0))

data_at_enc_discrt %<>%
  bind_rows(age_discrt)

#------------augment feature dictionary with the discretized features-------------
N<-nrow(enroll)
P<-sum(enroll$CASE_CTRL)
feat_enc_dscrt<-age_discrt %>%
  left_join(enroll %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  group_by(VARIABLE,TVAL_CHAR) %>%
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
                odds_ratio_emp_low=exp(log(odds_ratio_emp)+1.96*log_odds_ratio_sd)) %>%
  dplyr::rename(CONCEPT_CD=TVAL_CHAR) %>%
  dplyr::mutate(NAME_CHAR=CONCEPT_CD,
                CONCEPT_PATH="patient_dimension")

feat_at_enc %<>%
  bind_rows(feat_enc_dscrt)
#----------------------------------------------------------------------------------


#========save data============
saveRDS(feat_at_enc,file="./data/feat_at_enc_aug.rda")
saveRDS(data_at_enc,file="./data/data_at_enc2.rda")
saveRDS(data_at_enc_discrt,file="./data/data_at_enc_discrt2.rda")


  