#### Determine censor points for controls ####

rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"ROracle"
                     ,"DBI"))

config_file<-read.csv('../config.csv')
conn<-connect_to_db("Oracle",config_file)

## load cohort
dat<-dbGetQuery(conn,"select * from SI_CASE_CONTROL")
freq_bd<-round(0.01*length(unique(dat$ENCOUNTER_NUM)))
# saveRDS(dat,file="./data/SI_initial.rda")


## load SI_ServDep for identifying transtion of department
servdep<-dbGetQuery(conn,"select * from SI_ServDep") %>%
  filter(DAYS_SINCE_TRIAGE>=0) %>%
  dplyr::select(ENCOUNTER_NUM,SERVDEP_NAME,DAYS_SINCE_TRIAGE) %>%
  left_join(readRDS("./data/SI_initial.rda") %>%
              dplyr::select(ENCOUNTER_NUM,TRIAGE_START,SI_SINCE_TRIAGE),
            by = "ENCOUNTER_NUM") %>%
  unique %>%
  group_by(ENCOUNTER_NUM) %>%
  arrange(DAYS_SINCE_TRIAGE) %>%
  dplyr::mutate(servdep_lag=lag(SERVDEP_NAME,n=1L)) %>%
  ungroup %>%
  mutate(ED_prev=ifelse(grepl("(Emergency)+",servdep_lag),1,
                        ifelse(is.na(servdep_lag),0,-1))) %>%
  group_by(ENCOUNTER_NUM) %>%
  arrange(desc(ED_prev)) %>% dplyr::slice(1:1) %>%
  ungroup %>%
  mutate(time_bd=ifelse(!is.na(SI_SINCE_TRIAGE),SI_SINCE_TRIAGE,
                        case_when(ED_prev==0 ~ Inf,
                                  ED_prev==1 ~ DAYS_SINCE_TRIAGE)),
         real=ifelse(!is.na(SI_SINCE_TRIAGE),1,0)) %>%
  dplyr::select(ENCOUNTER_NUM,real,time_bd) %>%
  unique

saveRDS(servdep,file="./data/define_censor.rda")


## load data
chunk_id<-dbGetQuery(conn,"select distinct concept_prefix from SI_OBS_AT_ENC")

data_at_enc<-c()
for(i in seq_along(chunk_id)){
  chk_i<-dbGetQuery(conn,
                    paste0("select * from SI_OBS_AT_ENC where",
                           concept_prefix,"='",chunk_id[i],"'"))
  
  #--pre-filter1: time
  feat_i<-chk_i %>%
    dplyr::select(ENCOUNTER_NUM,CONCEPT_CD,NVAL_NUM,MODIFIER_CD,START_SINCE_TRIAGE)
    inner_join(servdep, by="ENCOUNTER_NUM") %>%
    filter(START_SINCE_TRIAGE < time_bd) #strictly less than
  
  if(chunk_id[i] %in% c()){
    
  }else if()
    group_by(VARIABLE_agg) %>%
    dplyr::summarize(enc_cnt_pos = length(unique(ENCOUNTER_NUM)),
                     complt_cnt_pos = length(unique(ENCOUNTER_NUM*as.numeric(as.character(complt))))-1,
                     distinct_val=length(unique(NVAL)),
                     q_min=ifelse(length(unique(NVAL))==1,NVAL,min(NVAL,na.rm=T)),
                     q_1=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.05)[1]),
                     q_2=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.1)[1]),
                     q_3=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.15)[1]),
                     q_4=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.2)[1]),
                     q_5=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.25)[1]),
                     q_6=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.3)[1]),
                     q_7=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.35)[1]),
                     q_8=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.4)[1]),
                     q_9=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.45)[1]),
                     q_mid=ifelse(length(unique(NVAL))==1,
                                  round(length(unique(ENCOUNTER_NUM))/enc_cnt_all,2),
                                  median(NVAL, na.rm=T)),
                     q_11=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.55)[1]),
                     q_12=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.6)[1]),
                     q_13=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.65)[1]),
                     q_14=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.7)[1]),
                     q_15=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.75)[1]),
                     q_16=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.8)[1]),
                     q_17=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.85)[1]),
                     q_18=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.9)[1]),
                     q_19=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.95)[1]),
                     q_max=ifelse(length(unique(NVAL))==1,NVAL,max(NVAL,na.rm=T))) %>%
    dplyr::mutate(enc_cnt_neg = enc_cnt_all-enc_cnt_pos,
                  complt_cnt_neg = comple_all - complt_cnt_pos) %>%
    dplyr::mutate(incomplt_cnt_pos = enc_cnt_pos - complt_cnt_pos,
                  incomplt_cnt_neg = enc_cnt_neg - complt_cnt_neg) %>%
    dplyr::mutate(complt_rate_pos = round(complt_cnt_pos/enc_cnt_pos,3),
                  incomplt_rate_pos = round(incomplt_cnt_pos/enc_cnt_pos,3),
                  complt_rate_neg = round(complt_cnt_neg/enc_cnt_neg,3),
                  incomplt_rate_neg = round(incomplt_cnt_neg/enc_cnt_neg,3)) %>%
    dplyr::mutate(pos_odd = complt_rate_pos/incomplt_rate_pos,
                  neg_odd = complt_rate_neg/incomplt_rate_neg) %>%
    dplyr::mutate(empirical_odd_ratio = pos_odd/neg_odd) %>%
    dplyr::select(VARIABLE_agg, distinct_val,
                  enc_cnt_pos,complt_rate_pos, complt_rate_neg,
                  pos_odd, neg_odd, empirical_odd_ratio,
                  q_min,q_1,q_2,q_3,q_4,q_5,q_6,q_7,q_8,q_9,q_mid,
                  q_11,q_12,q_13,q_14,q_15,q_16,q_17,q_18,q_19,q_max) %>%
    unique %>% arrange(desc(empirical_odd_ratio))
}

#--pre-filter2: frequency
data_at_enc %<>% 
  dplyr::select(-real,-time_bd) %>%
  semi_join(feat_at_enc %>% filter(overall >= freq_bd),
            by="VARIABLE")

#--remove encounters with no frequent features 
servdep %<>% semi_join(data_at_enc,by="ENCOUNTER_NUM")

#--save data
saveRDS(fact_stack,file="./data/data_at_enc.rda")
saveRDS(feat_at_enc,file="./data/feat_at_enc.rda")
saveRDS(servdep,file="./data/define_censor.rda")

