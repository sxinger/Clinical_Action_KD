#### Preprocessing ####
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
data_at_enc<-readRDS("./data/data_at_enc_discrt.rda")
data_bef_enc<-readRDS("./data/data_bef_enc.rda")
rs_idx<-readRDS("./data/rand_idx.rda") %>% 
  filter(PRED_POINT <= 48)

#===============filter====================
data_at_enc %<>% 
  semi_join(rs_idx, by=c("PATIENT_NUM","ENCOUNTER_NUM"))
data_bef_enc %<>%
  semi_join(rs_idx, by=c("PATIENT_NUM","ENCOUNTER_NUM"))


cd_out<-c("KUMC\\|VISITDETAIL\\|POS\\(O2\\)",
          "KUH\\|FLO_MEAS_ID",
          "KUH\\|MEDICATION_ID")

#identify diagnostic folders in KU ED
ed_cd<-readRDS("./data/feat_at_enc.rda") %>% 
  filter(grepl("Flowsheet\\\\KU\\\\ED",CONCEPT_PATH)) %>%
  dplyr::select(VARIABLE,CONCEPT_CD,CONCEPT_PATH,TVAL_CHAR)

cd_in<-unique(c(ed_cd[grepl("\\\\TMP VITAL SIGNS",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #vital in ED
                ed_cd[grepl("\\\\TMP AIRWAY/BREATHING",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #airway/breathing in ED
                ed_cd[grepl("\\\\TMP CIRCULATION NAV",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #circulation in ED
                ed_cd[grepl("\\\\TMP CORE MEASURES",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED core measures
                ed_cd[grepl("\\\\TMP LDA CRITICAL",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED critical
                ed_cd[grepl("\\\\TMP DISABILITY",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED disability
                ed_cd[grepl("\\\\TMP SAFETY",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED fall risk
                ed_cd[grepl("\\\\TMP NEURO\\\\",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED neuro
                ed_cd[grepl("\\\\TMP PAIN ASSESSMENT",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED pain assessment
                ed_cd[grepl("\\\\TMP NAV PRIMARY ASSESSMENT",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED primary assessment
                ed_cd[grepl("\\\\TMP RESPIRATORY",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED respiratory
                ed_cd[grepl("\\\\TMP SEPSIS SCREEN",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED sepsis screen
                ed_cd[grepl("\\\\TMP SKIN/WOUND",ed_cd$CONCEPT_PATH),]$CONCEPT_CD, #ED skin/wound
                ed_cd[grepl("\\\\TMP TABACCO USE",ed_cd$CONCEPT_PATH),]$CONCEPT_CD #ED tabacco use
                ))
cd_in<-gsub("\\|","\\\\\\|",cd_in)
cd_in<-gsub("\\+","\\\\\\+",cd_in)
cd_in<-gsub("\\:","\\\\\\:",cd_in)
cd_in<-gsub("\\_","\\\\\\_",cd_in)

data_at_enc %<>%
  filter(!grepl(paste0("(",paste(cd_out,collapse=")|("),")"),VARIABLE)|
         grepl(paste0("(",paste(cd_in,collapse=")|("),")"),VARIABLE))


data_at_enc %<>%
  filter(!grepl(paste0("(",paste(cd_out,collapse=")|("),")"),VARIABLE))


##==============feature aggregation: recency=========
x_mt<-data_at_enc %>%
  #filter first
  group_by(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE) %>%
  arrange(desc(START_SINCE_TRIAGE)) %>%
  dplyr::slice(1:1) %>% ## latest values
  ungroup %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,TVAL_CHAR) %>%
  bind_rows(data_bef_enc %>%
              dplyr::filter(DAY_BEF_TRIAGE<=-31) %>%
              dplyr::mutate(TVAL_CHAR=as.character(round(DAY_BEF_TRIAGE/365.25))) %>%
              dplyr::mutate(VARIABLE=paste0(VARIABLE,"_",TVAL_CHAR,"yr"))) %>%
  unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_") %>%
  #then transform
  mutate(VARIABLE=case_when(grepl("(FLO_MEAS_ID\\+hash)|(FLO_MEAS_ID\\+LINE)+",VARIABLE) ~ paste0(VARIABLE,"_",TVAL_CHAR),
                            grepl("(VISITDETAIL\\|POS)+",VARIABLE) ~ paste0(VARIABLE,":",TVAL_CHAR),
                            grepl("num_",VARIABLE) ~ TVAL_CHAR,
                            TRUE ~ VARIABLE)) %>%
  long_to_sparse_matrix(.,
                        id="PAT_ENC",
                        variable="VARIABLE",
                        val="NVAL_NUM",
                        binary=T)
dim(x_mt)
# 453578   2163

y_mt<-rs_idx %>% 
  semi_join(data_at_enc,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_") %>%
  filter(!duplicated(PAT_ENC)) %>%
  arrange(PAT_ENC)
  
mean(y_mt$CASE_CTRL) #12%

all(row.names(x_mt)==y_mt$PAT_ENC) #alignment check

Xy_sparse<-list(x_mt=x_mt,y_mt=y_mt)
saveRDS(Xy_sparse,"./data/Xy_sp_rec.rda")


##==============feature aggregation: ==============


