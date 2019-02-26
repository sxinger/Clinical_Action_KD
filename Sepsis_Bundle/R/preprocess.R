###########################################################
# This script preprocessed the selected variables, by     #
# - selecting observations before certain perdiction point#
# - aggregating multiple values for the same variable     #
# - encoding categorical variables                        #
###########################################################

rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

#### Preprocessing ####
source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"))

## Load in fact_stack and pat_tbl
enroll<-readRDS("./data/SI_enroll.rda")
data_at_enc<-readRDS("./data/data_at_enc.rda")
data_bef_enc<-readRDS("./data/data_bef_enc.rda")
sample_idx<-readRDS("./data/sample_idx.rda")


##======feature pre-selection==========
cd_out<-c("KUH\\|FLO_MEAS_ID",          #flowsheet facts
          "KUH\\|MEDICATION_ID",        #medication
          "KUH\\|DX_ID",                #diagnosis
          "KUMC\\|REPORTS\\|NOTETYPES", #notetypes
          "RELIGION",                   #religion
          "LANGUAGE")                   #language

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
                # ed_cd[grepl("\\\\TMP SEPSIS SCREEN",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED sepsis screen
                ed_cd[grepl("\\\\TMP SKIN/WOUND",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED skin/wound
                c("KUH|FLO_MEAS_ID:5_SYSTOLIC","KUH|FLO_MEAS_ID:5_DIASTOLIC")))
ed_cd %<>% 
  semi_join(data.frame(CONCEPT_CD=cd_in,stringsAsFactors=F),
            by="CONCEPT_CD")

data_at_enc2<- data_at_enc %>%
  filter(!grepl(paste0("(",paste(cd_out,collapse=")|("),")"),VARIABLE)) %>%
  bind_rows(data_at_enc %>% semi_join(ed_cd,by="VARIABLE"))

rm(data_at_enc); gc()


##=====feature engineering============
data_at_enc_eng<-enroll %>%
  dplyr::select("PATIENT_NUM",
                paste0("SIRS_",1:4,"_SINCE_TRIAGE"),
                paste0("OD_",1:4,"_SINCE_TRIAGE")) %>%
  gather(VARIABLE,START_SINCE_TRIAGE,-PATIENT_NUM) %>%
  filter(!is.na(START_SINCE_TRIAGE)) %>%
  dplyr::mutate(VARIABLE=gsub("_SINCE_TRIAGE","",VARIABLE)) %>%
  left_join(enroll %>%
              dplyr::select("PATIENT_NUM",
                            paste0("OD_",1:4,"_TVAL")) %>%
              gather(VARIABLE,TVAL_CHAR,-PATIENT_NUM) %>%
              filter(!is.na(TVAL_CHAR)) %>%
              dplyr::mutate(VARIABLE=gsub("_TVAL","",VARIABLE)),
            by=c("PATIENT_NUM","VARIABLE")) %>%
  dplyr::mutate(VARIABLE=case_when(!is.na(TVAL_CHAR) ~ TVAL_CHAR,
                                   TRUE ~VARIABLE),
                NVAL_NUM=1) %>%
  dplyr::select(PATIENT_NUM,VARIABLE,NVAL_NUM,START_SINCE_TRIAGE) %>%
  



##======temporal filter===============




##======feature abstraction===========



## pre-filter: use facts before treatment completion1/sepsis onset
presel_num %<>%
  left_join(trt3hr %>% 
              dplyr::select(ENCOUNTER_NUM,TRT3HR_END1,SEPSIS_SINCE_TRIAGE),
            by="ENCOUNTER_NUM") %>%
  mutate(pred_point = ifelse(is.na(TRT3HR_END1),SEPSIS_SINCE_TRIAGE,TRT3HR_END1)) %>%
  filter(START_SINCE_TRIAGE <= pred_point) %>%
  dplyr::filter(grepl("_inc",VARIABLE) | ((!grepl("_inc",VARIABLE) & NVAL > 0))) #remove suspecious 0


presel_cat %<>%
  left_join(trt3hr %>% 
              dplyr::select(ENCOUNTER_NUM,TRT3HR_END1,SEPSIS_SINCE_TRIAGE),
            by="ENCOUNTER_NUM") %>%
  mutate(pred_point = ifelse(is.na(TRT3HR_END1),SEPSIS_SINCE_TRIAGE,TRT3HR_END1)) %>%
  filter(START_SINCE_TRIAGE <= pred_point)


## process numerical variables
# set aside time-invariant variables
presel_num2_invar<-presel_num %>%
  group_by(ENCOUNTER_NUM,VARIABLE) %>%
  dplyr::mutate(pt_freq=length(unique(START_SINCE_TRIAGE))) %>%
  ungroup %>% group_by(VARIABLE) %>%
  dplyr::summarize(avg_pt_freq = mean(pt_freq,na.rm=T),
                   sd_pt_freq = sd(pt_freq,na.rm=T)) %>%
  ungroup %>%
  mutate(invar_ind = ifelse(avg_pt_freq==1 & sd_pt_freq==0,1,0))


# aggregate values within time-window t (if multiple values present)
presel_num2_var<-presel_num %>%
  semi_join(presel_num2_invar %>% filter(invar_ind == 0),by="VARIABLE") %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE, NVAL,START_SINCE_TRIAGE) %>%
  unique %>% group_by(ENCOUNTER_NUM, VARIABLE) %>%
  arrange(START_SINCE_TRIAGE) %>%
  do(mutate(.
            ,agg_min = ifelse(length(NVAL)==0, NA, min(NVAL,na.rm=T))
            ,agg_max = ifelse(length(NVAL)==0, NA, max(NVAL,na.rm=T))
            ,agg_mean = ifelse(length(NVAL)==0, NA, mean(NVAL,na.rm=T))
            # ,agg_sd = ifelse(length(NVAL) <= 1, 0, sd(NVAL,na.rm=T))
            ,agg_initial = NVAL[1]
            # ,init_t = START_SINCE_TRIAGE[1]
            )) %>%
  # mutate(min_t = ifelse(NVAL==agg_min,START_SINCE_TRIAGE,NA),
  #        max_t = ifelse(NVAL==agg_max,START_SINCE_TRIAGE,NA)) %>%
  arrange(ENCOUNTER_NUM) %>% ungroup


# collect time-invariant variables
presel_num2_fix<-presel_num %>%
  semi_join(presel_num2_invar %>% filter(invar_ind == 1),by="VARIABLE") %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE, NVAL,START_SINCE_TRIAGE) %>%
  mutate(abs_hr = abs(START_SINCE_TRIAGE)) %>%
  group_by(ENCOUNTER_NUM, VARIABLE) %>%
  top_n(n=-1,wt=abs_hr) %>% ungroup %>%
  dplyr::rename(VARIABLE_agg = VARIABLE) %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE_agg, NVAL) %>%
  unique %>% arrange(ENCOUNTER_NUM)


# merge variable with aggregate function
presel_num2_var %<>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE
               ,agg_min, agg_max, agg_mean, agg_initial
               # ,agg_sd
               ) %>% 
  unique %>%
  gather(agg,NVAL,-ENCOUNTER_NUM,-VARIABLE) %>%
  unite("VARIABLE_agg",c("VARIABLE","agg")) %>%
  filter(!is.na(NVAL))


## collect and process categorical variables
# aggregate values within time-window t
presel_cat %<>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE, TVAL,START_SINCE_TRIAGE) %>%
  mutate(abs_hr = abs(START_SINCE_TRIAGE)) %>%
  group_by(ENCOUNTER_NUM, VARIABLE) %>%
  dplyr::mutate(distinct_state = length(unique(TVAL))) %>%
  group_by(ENCOUNTER_NUM, VARIABLE, TVAL) %>%
  top_n(n=-1,abs_hr) %>% ungroup %>%  #get most recent value
  dplyr::select(ENCOUNTER_NUM, VARIABLE, distinct_state, TVAL, START_SINCE_TRIAGE) %>%
  arrange(ENCOUNTER_NUM)


# use distinct_state as a new numerical variable
dist_state<-presel_cat %>% 
  group_by(ENCOUNTER_NUM, VARIABLE, distinct_state) %>%
  top_n(n=1,wt=START_SINCE_TRIAGE) %>%
  ungroup %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE, distinct_state, START_SINCE_TRIAGE) %>%
  unique %>%
  dplyr::rename(NVAL = distinct_state) %>%
  dplyr::mutate(VARIABLE_agg = paste0(VARIABLE,"_distincts")) %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE_agg, NVAL) %>%
  unique %>%
  filter(NVAL > 1)


# attach to numerical stack
presel_num2_var %<>%
  bind_rows(dist_state)
  

# hot-encoding (use separate indicators for different categories)
presel_cat2 <- presel_cat %>%
  dplyr::select(-distinct_state) %>%
  mutate(NVAL = 1) %>%
  unite("VARIABLE_agg",c("VARIABLE","TVAL")) %>%
  dplyr::select(ENCOUNTER_NUM,VARIABLE_agg,NVAL) %>%
  unique


# put together
fact_stack_ohc<-presel_num2_var %>%
  bind_rows(presel_num2_fix) %>%
  bind_rows(presel_cat2) %>%
  dplyr::select(ENCOUNTER_NUM,VARIABLE_agg,NVAL) %>%
  arrange(ENCOUNTER_NUM,VARIABLE_agg)


## save final results
save(fact_stack_ohc,file=paste0("./data/sepsis_presel_long_ohc.Rdata"))

