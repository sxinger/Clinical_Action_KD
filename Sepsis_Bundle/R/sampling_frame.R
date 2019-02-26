#### random sampling ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"                   
                  ))

## load patient data and attach demographic info used for stratified sampling
enroll<-readRDS("./data/SI_enroll.rda") %>%
  left_join(readRDS("./data/pat_at_enc.rda") %>%
              dplyr::mutate(AGE_GRP=case_when(AGE<30 ~ "20s and below",
                                              AGE>=30 & AGE <40 ~ "30s",
                                              AGE>=40 & AGE <50 ~ "40s",
                                              AGE>=50 & AGE <60 ~ "50s",
                                              AGE>=60 & AGE <70 ~ "60s",
                                              AGE>=70 ~ "70s and above"),
                            RACE=case_when(RACE %in% c("amerian ind",
                                                       "pac islander",
                                                       "two races",
                                                       "asian") ~ "other",
                                           TRUE ~ RACE)) %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,
                            AGE_GRP,SEX_MALE,RACE),
            by=c("PATIENT_NUM","ENCOUNTER_NUM"))


## random sampling
rsample_idx<-enroll %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM) %>%
  dplyr::mutate(rs_part73 = sample(c("T","V"),prob=c(0.7,0.3),n(),replace=T),
                rs_cv10 = sample(1:10,n(),replace=T))

enroll %<>% 
  semi_join(rsample_idx %>% filter(rs_part73=="T"),
            by=c("PATIENT_NUM","ENCOUNTER_NUM"))

## down sampling - outcome
ns<-20
dsample_idx<-c()
enroll0<- enroll %>% filter(CASE_CTRL==0) %>% 
  dplyr::select(PATIENT_NUM, ENCOUNTER_NUM) %>%
  dplyr::mutate(ds_cv20=sample(1:ns,n(),replace=T))

for (i in seq_len(ns)){
  dsample_idx %<>%
    bind_rows(enroll %>% filter(CASE_CTRL==1) %>%
                dplyr::select(PATIENT_NUM,ENCOUNTER_NUM) %>%
                dplyr::mutate(ds_cv20=i)) %>%
    bind_rows(enroll0 %>% filter(ds_cv20==i))
}

dsample_idx %<>% 
  bind_rows(rsample_idx %>% filter(rs_part73=="V") %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM) %>%
              dplyr::mutate(ds_cv20=0)) 

## exact match sampling - outcome+age+sex+race
ms<-20
msample_idx<-c()
enroll0<- enroll %>% filter(CASE_CTRL==0) %>% 
  group_by(AGE_GRP,SEX_MALE,RACE) %>%
  dplyr::mutate(rn=1:n()) %>% ungroup %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,AGE_GRP,SEX_MALE,RACE,rn) %>%
  dplyr::rename(PATIENT_NUM0=PATIENT_NUM,ENCOUNTER_NUM0=ENCOUNTER_NUM)

for(i in 1:ms){
  msample_idx %<>%
    bind_rows(enroll %>% filter(CASE_CTRL==1) %>%
                left_join(enroll0,by=c("AGE_GRP","SEX_MALE","RACE")) %>%
                group_by(PATIENT_NUM,ENCOUNTER_NUM) %>%
                dplyr::filter(rn==sample(1:max(rn,na.rm=T),1)) %>%
                ungroup %>%
                dplyr::select(PATIENT_NUM0,ENCOUNTER_NUM0) %>%
                dplyr::rename(ENCOUNTER_NUM=ENCOUNTER_NUM0,PATIENT_NUM=PATIENT_NUM0) %>%
                dplyr::mutate(ms_cv20=i)) %>%
    bind_rows(enroll %>% filter(CASE_CTRL==1) %>%
                dplyr::select(PATIENT_NUM,ENCOUNTER_NUM) %>%
                dplyr::mutate(ms_cv20=i))
}

msample_idx %<>% 
  bind_rows(rsample_idx %>% filter(rs_part73=="V") %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM) %>%
              mutate(ms_cv20=0)) 
  
## put together sampling indices
sample_idx<-list(rs=rsample_idx,
                 ds=dsample_idx,
                 ms=msample_idx)
  
## save result
saveRDS(sample_idx,file="./data/sample_idx.rda")

