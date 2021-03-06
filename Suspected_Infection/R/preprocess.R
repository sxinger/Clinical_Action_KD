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

##==============load features================
#--augmented data matrix
feat_dict<-readRDS("./data/feat_at_enc_aug.rda") %>%
  bind_rows(readRDS("./data/feat_bef_enc_aug.rda")) %>%
  dplyr::select(VARIABLE,CONCEPT_CD,NAME_CHAR,CONCEPT_PATH,
                enc_wi,odds_ratio_emp,log_odds_ratio_sd) %>%
  dplyr::mutate(uni_p_val=ifelse(log_odds_ratio_sd==0, 1e-10,
                                 ifelse(log_odds_ratio_sd==Inf,1,
                                        2*pnorm(abs(log(odds_ratio_emp)/log_odds_ratio_sd),lower.tail = F))))
#422,605

feat_dict %<>% filter(uni_p_val<=0.01)
length(unique(feat_dict$VARIABLE))
#43,235

saveRDS(feat_dict,file="./data/feat_uni_aug.rda")

##==============load data==============
data_at_enc<-readRDS("./data/data_at_enc2.rda") %>%
  bind_rows(readRDS("./data/data_at_enc_discrt.rda"))

sample_idx<-readRDS("./data/sample_idx.rda")

#===============filter====================
data_at_enc %<>% 
  semi_join(sample_idx$rs, by=c("PATIENT_NUM","ENCOUNTER_NUM"))

cd_out<-c("KUH\\|GEN_ALERT",            #some general alerts
          "KUH\\|FLO_MEAS_ID",          #flowsheet facts
          "KUH\\|MEDICATION_ID",        #medication
          "KUH\\|DX_ID",                #diagnosis
          "KUMC\\|REPORTS\\|NOTETYPES", #notetypes
          "RELIGION",                   #religion
          "LANGUAGE")                   #language

#identify diagnostic folders in KU ED
ed_cd<-readRDS("./data/feat_at_enc_aug.rda") %>% 
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

#only keep variables with p_val < 0.01
data_at_enc2 %<>% 
  semi_join(readRDS("./data/feat_uni_aug.rda") %>% 
              filter(uni_p_val<=0.01),by="VARIABLE")

readRDS("./data/feat_uni_aug.rda") %>%


##==============feature aggregation: recency=========
x_mt<-data_at_enc2 %>%
  #filter first
  group_by(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE) %>%
  arrange(desc(START_SINCE_TRIAGE)) %>%
  dplyr::slice(1:1) %>% ## latest values
  ungroup %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,TVAL_CHAR) %>%
  bind_rows(readRDS("./data/data_bef_enc_yearly.rda") %>%
              semi_join(sample_idx$rs, by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
              dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,TVAL_CHAR)) %>%
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
# 383430   2575

y_mt<-readRDS("./data/SI_enroll.rda") %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL) %>%
  semi_join(data_at_enc2,by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_") %>%
  filter(!duplicated(PAT_ENC)) %>%
  arrange(PAT_ENC)
  
mean(y_mt$CASE_CTRL) #3.4%

all(row.names(x_mt)==y_mt$PAT_ENC) #alignment check

Xy_sparse<-list(x_mt=x_mt,y_mt=y_mt)
saveRDS(Xy_sparse,"./data/Xy_sp_rec.rda")


##==============feature aggregation: ==============


