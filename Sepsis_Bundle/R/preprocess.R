###########################################################
# This script preprocessed the selected variables, by     #
# - selecting observations before certain perdiction point#
# - aggregating multiple values for the same variable     #
# - encoding categorical variables                        #
###########################################################

rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",
                    "fuzzyjoin"))

## Load in fact_stack and pat_tbl
enroll<-readRDS("./data/SI_enroll.rda")
data_at_enc<-readRDS("./data/data_at_enc.rda")
data_bef_enc<-readRDS("./data/data_bef_enc.rda")
sample_idx<-readRDS("./data/sample_idx.rda")

##======feature pre-selection==========
cd_out<-c("KUH\\|FLO_MEAS_ID",          #flowsheet facts
          "KUH\\|PROC_ID",              #order
          "KUH\\|MEDICATION_ID",        #medication
          "KUH\\|DX_ID",                #diagnosis
          "KUMC\\|REPORTS\\|NOTETYPES", #notetypes
          "RELIGION",                   #religion
          "LANGUAGE",                   #language
          'KUH\\|COMPONENT_ID\\:2015',  #lactate
          'KUH\\|COMPONENT_ID\\:2016'   #lactate
          )                   

#identify diagnostic folders in KU ED
ed_cd<-readRDS("../Suspected_Infection/data/feat_at_enc.rda") %>% 
  filter(grepl("Flowsheet\\\\KU\\\\ED",CONCEPT_PATH)) %>%
  dplyr::select(VARIABLE,CONCEPT_CD,CONCEPT_PATH)

#hand-pick flowsheet concepts of interest
cd_in<-unique(c(ed_cd[grepl("\\\\TMP VITAL SIGNS",ed_cd$CONCEPT_PATH)&!grepl("(( SOURCE )|( METHOD )|( POSITION )|( EST ))+",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #vital in ED
                # ed_cd[grepl("\\\\TMP AIRWAY/BREATHING",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #airway/breathing in ED
                # ed_cd[grepl("\\\\TMP CIRCULATION NAV",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #circulation in ED
                ed_cd[grepl("\\\\TMP CORE MEASURES",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED core measures
                # ed_cd[grepl("\\\\TMP LDA CRITICAL",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED critical
                ed_cd[grepl("\\\\TMP DISABILITY",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED disability
                # ed_cd[grepl("\\\\TMP SAFETY",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED fall risk
                ed_cd[grepl("\\\\TMP NEURO\\\\",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED neuro
                # ed_cd[grepl("\\\\TMP PAIN ASSESSMENT",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED pain assessment
                ed_cd[grepl("\\\\TMP NAV PRIMARY ASSESSMENT",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED primary assessment
                # ed_cd[grepl("\\\\TMP RESPIRATORY",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED respiratory
                ed_cd[grepl("\\\\TMP SEPSIS SCREEN",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED sepsis screen
                ed_cd[grepl("\\\\TMP SKIN/WOUND",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED skin/wound
                c("KUH|FLO_MEAS_ID:5_SYSTOLIC","KUH|FLO_MEAS_ID:5_DIASTOLIC")))
ed_cd %<>% 
  semi_join(data.frame(CONCEPT_CD=cd_in,stringsAsFactors=F),
            by="CONCEPT_CD")

data_at_enc2<- data_at_enc %>%
  filter(!grepl(paste0("(",paste(cd_out,collapse=")|("),")"),VARIABLE)) %>%
  bind_rows(data_at_enc %>% semi_join(ed_cd,by="VARIABLE"))

rm(data_at_enc); gc()


##======feature engineering============
#---SIRS and OD events
data_at_enc_eng<-enroll %>%
  dplyr::select("PATIENT_NUM","ENCOUNTER_NUM",
                paste0("SIRS_",1:4,"_SINCE_TRIAGE"),
                paste0("OD_",1:4,"_SINCE_TRIAGE")) %>%
  gather(TVAL_CHAR,START_SINCE_TRIAGE,-ENCOUNTER_NUM,-PATIENT_NUM) %>%
  filter(!is.na(START_SINCE_TRIAGE)) %>%
  dplyr::mutate(TVAL_CHAR=gsub("_SINCE_TRIAGE","",TVAL_CHAR),
                VARIABLE=gsub("_.*","",TVAL_CHAR),
                NVAL_NUM=1)

#---Charlson Comorbidity Index
cci_icd<-read.csv("./src/charlson_ICD.csv",stringsAsFactors = F) %>%
  dplyr::mutate(DX_CODE=paste0("\\\\",DX_CODE))

feat_cd<-readRDS("./data/feat_at_enc.rda") %>%
  dplyr::select(CONCEPT_CD,NAME_CHAR,CONCEPT_PATH) %>%
  dplyr::filter(grepl("(\\\\ICD(9|10))+",CONCEPT_PATH)) %>%
  fuzzy_inner_join(cci_icd,by=c("CONCEPT_PATH"="DX_CODE"),match_fun=str_detect) %>%
  dplyr::select(CONCEPT_CD,NAME_CHAR,DX,WEIGHT) %>% unique

data_bef_enc_cci<-data_bef_enc %>% 
  inner_join(feat_cd %>% dplyr::select(CONCEPT_CD,DX,WEIGHT),by=c("VARIABLE"="CONCEPT_CD")) %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,DX) %>%
  arrange(abs(DAY_BEF_TRIAGE)) %>%
  dplyr::slice(1:1) %>%
  ungroup

data_all<-data_at_enc2 %>% 
  bind_rows(data_at_enc_eng) %>%
  bind_rows(data_bef_enc_cci %>%
              group_by(PATIENT_NUM,ENCOUNTER_NUM) %>%
              dplyr::summarize(NVAL_NUM=sum(WEIGHT)) %>%
              ungroup %>%
              dplyr::mutate(VARIABLE="CCI",START_SINCE_TRIAGE=-1) %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,START_SINCE_TRIAGE)) %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM,TVAL_CHAR,START_SINCE_TRIAGE) %>%
  left_join(enroll %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,SEPSIS_SINCE_TRIAGE,IV_0_SINCE_TRIAGE),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) 

saveRDS(data_at_enc2,file="./data/data_all.rda")


##======temporal filter (at encounter)===============
#---filter by time
data_at_enc2 %<>%
  dplyr::filter(START_SINCE_TRIAGE<=pmin(coalesce(IV_0_SINCE_TRIAGE,TRT3HR_CMPLT),
                                         SEPSIS_SINCE_TRIAGE,na.rm=T)) # prediction point


##======feature abstraction===========
#---set aside time-invariant variables
data_num_inv<-data_at_enc2 %>%
  group_by(VARIABLE) %>%
  dplyr::mutate(distinct_val=length(unique(NVAL_NUM))) %>%
  ungroup %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,distinct_val) %>%
  dplyr::mutate(pt_freq=length(unique(START_SINCE_TRIAGE))) %>%
  ungroup %>% group_by(VARIABLE,distinct_val) %>%
  dplyr::summarize(avg_pt_freq = mean(pt_freq,na.rm=T),
                   sd_pt_freq = sd(pt_freq,na.rm=T),
                   median_pt_freq = median(pt_freq,na.rm=T),
                   q3_pt_freq=quantile(pt_freq,probs=0.75,na.rm=T)) %>%
  ungroup %>%
  mutate(invar_ind = ifelse(q3_pt_freq<=1|distinct_val==1,1,0))

#---aggregate values within time-window t (if multiple values present)
data_num_agg<-data_at_enc2 %>%
  semi_join(data_num_inv %>% filter(invar_ind == 0),by="VARIABLE") %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM, VARIABLE, NVAL_NUM,START_SINCE_TRIAGE) %>%
  unique %>% group_by(PATIENT_NUM,ENCOUNTER_NUM, VARIABLE) %>%
  arrange(START_SINCE_TRIAGE) %>%
  do(mutate(.
            ,agg_min = ifelse(length(NVAL_NUM)==0, NA, min(NVAL_NUM,na.rm=T))
            ,agg_max = ifelse(length(NVAL_NUM)==0, NA, max(NVAL_NUM,na.rm=T))
            ,agg_mean = ifelse(length(NVAL_NUM)==0, NA, mean(NVAL_NUM,na.rm=T))
            # ,agg_sd = ifelse(length(NVAL_NUM) <= 1, 0, sd(NVAL_NUM,na.rm=T))
            ,agg_initial = NVAL_NUM[1]
            # ,init_t = START_SINCE_TRIAGE[1]
  )) %>%
  # mutate(min_t = ifelse(NVAL==agg_min,START_SINCE_TRIAGE,NA),
  #        max_t = ifelse(NVAL==agg_max,START_SINCE_TRIAGE,NA)) %>%
  arrange(PATIENT_NUM,ENCOUNTER_NUM) %>% ungroup %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM, VARIABLE
                ,agg_min, agg_max, agg_mean, agg_initial
                # ,agg_sd
  ) %>% 
  unique %>%
  gather(agg,NVAL_NUM,-ENCOUNTER_NUM,-PATIENT_NUM,-VARIABLE) %>%
  unite("VARIABLE_agg",c("VARIABLE","agg")) %>%
  filter(!is.na(NVAL_NUM))

#---collect time-invariant variables
data_num_fix<-data_at_enc2 %>%
  semi_join(data_num_inv %>% filter(invar_ind == 1),by="VARIABLE") %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM, VARIABLE, NVAL_NUM, TVAL_CHAR,START_SINCE_TRIAGE) %>%
  mutate(abs_hr = abs(START_SINCE_TRIAGE)) %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM, VARIABLE) %>%
  top_n(n=-1,wt=abs_hr) %>% ungroup %>%
  dplyr::mutate(VARIABLE_agg = ifelse(!is.na(TVAL_CHAR),paste0(VARIABLE,"_",TVAL_CHAR),VARIABLE)) %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM, VARIABLE_agg, NVAL_NUM) %>%
  unique %>% arrange(PATIENT_NUM,ENCOUNTER_NUM)


#---put together
fact_stack<-data_num_agg %>%
  bind_rows(data_num_fix) %>%
  bind_rows(data_bef_enc %>% 
              filter(DAY_BEF_TRIAGE < 0) %>%
              group_by(PATIENT_NUM, ENCOUNTER_NUM, VARIABLE) %>%
              arrange(DAY_BEF_TRIAGE) %>% dplyr::slice(1:1) %>%
              dplyr::mutate(NVAL_NUM=1) %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,NVAL_NUM) %>%
              dplyr::rename(VARIABLE_agg = VARIABLE)) %>%
  bind_rows(data_bef_enc_cci %>%
              dplyr::mutate(NVAL_NUM=1) %>%
              dplyr::mutate(VARIABLE_agg=DX) %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE_agg,NVAL_NUM)) %>%
  semi_join(enroll,by=c("PATIENT_NUM","ENCOUNTER_NUM"))

x_mt<- fact_stack %>%
  unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_") %>%
  long_to_sparse_matrix(.,
                        id="PAT_ENC",
                        variable="VARIABLE_agg",
                        val="NVAL_NUM",
                        binary=F)

dim(x_mt)
# 12084  1360

y_mt<-enroll %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,TRT3HR_COMPLT_FAST_IND,IV_TRIGGER_SINCE_TRIAGE) %>%
  semi_join(fact_stack,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_") %>%
  filter(!duplicated(PAT_ENC)) %>%
  arrange(PAT_ENC)

mean(y_mt$TRT3HR_COMPLT_FAST_IND) #12%

all(row.names(x_mt)==y_mt$PAT_ENC) #alignment check

Xy_sparse<-list(x_mt=x_mt,y_mt=y_mt)
saveRDS(Xy_sparse,"./data/Xy_sp_rec.rda")

