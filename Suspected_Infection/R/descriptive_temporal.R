rm(list=ls())
gc()

setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c( "dplyr"
                    ,"tidyr"
                    ,"magrittr"
                    ,"stringr"))

rs_idx<-readRDS("./data/rand_idx.rda")

#------------------------demographics--------------------------------------------------
demo_master<-pat_at_enc %>%
  left_join(rs_idx %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  mutate(STATUS=ifelse(is.na(DEATH_DATE),0,1))

demo_master %<>%
  mutate(AGE_GRP=case_when(AGE<30 ~ "20s",
                           AGE>=30 & AGE <40 ~ "30s",
                           AGE>=40 & AGE <50 ~ "40s",
                           AGE>=50 & AGE <60 ~ "60s",
                           AGE>=60 & AGE <70 ~ "70s",
                           AGE>=70 & AGE <80 ~ "60s",
                           AGE>=80 ~ "80s"))
#--age in numeric
demo_master %>%
  dplyr::select(AGE,CASE_CTRL) %>%
  group_by(CASE_CTRL) %>%
  dplyr::summarise(age_mean=mean(AGE,na.rm=T),
                   age_sd=sd(AGE,na.rm=T)) %>%
  ungroup %>% View

#--other categorical
demo_master2<-demo_master %>%
  dplyr::select(-AGE,-DEATH_DATE) %>%
  gather(demo_type,demo_val,-PATIENT_NUM,-ENCOUNTER_NUM,-CASE_CTRL) %>%
  group_by(CASE_CTRL,demo_type) %>%
  dplyr::mutate(enc_all=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  group_by(CASE_CTRL,demo_type,demo_val,enc_all) %>%
  dplyr::summarise(enc_at=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  mutate(enc_prop=round(enc_at/enc_all,3))

demo_master2 %>%
  dplyr::select(demo_type,demo_val,enc_at,CASE_CTRL) %>%
  spread(CASE_CTRL,enc_at,fill=0) %>%
  left_join(demo_master2 %>%
              dplyr::select(demo_type,demo_val,enc_prop,CASE_CTRL) %>%
              spread(CASE_CTRL,enc_prop,fill=0),
            by=c("demo_type","demo_val")) %>%
  dplyr::rename("enc_cnt_0"=`0.x`,
                "enc_cnt_1"=`1.x`,
                "enc_prop_0"=`0.y`,
                "enc_prop_1"=`1.y`) %>%
  dplyr::select(demo_type,demo_val,
                enc_cnt_0,enc_prop_0,
                enc_cnt_1,enc_prop_1) %>%
  arrange(demo_type,desc(enc_cnt_0)) %>%
  View
#---------------------------------------------------------------------------------------


#--------------observation time window-------------
brks<-c(seq(0,24,1),48,72,Inf)

#--si timing
si_tm<-rs_idx %>%
  filter(CASE_CTRL==1) %>%
  mutate(SI_time_grp=cut(SI_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         C_time_grp=cut(C_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         ABX_time_grp=cut(ABX_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         early_transition=case_when(!is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE < TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "Culture and ABX within ED; Transitioned",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE < TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE ~ "Culture within ED; ABX after ED; Transitioned",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "ABX within ED; Culture after ED; Transitioned",
                                    TRUE ~ "Culture and ABX within ED; Discharged"),
         CALYR=as.numeric(format(TRIAGE_START,"%Y")))

saveRDS(si_tm,file="./data/SI_timing_dist.rda")

#identify outlier and collect summary
si_tm %>%
  dplyr::mutate(si_loc=case_when(SI_SINCE_TRIAGE<=TRANS_SINCE_TRIAGE|is.na(TRANS_SINCE_TRIAGE)~"ED_discharge",
                                 SI_SINCE_TRIAGE>TRANS_SINCE_TRIAGE ~ SERVDEP_NAME)) %>%
  group_by(si_loc) %>%
  dplyr::summarise(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   si_min=min(SI_SINCE_TRIAGE,na.rm=T),
                   si_p05=quantile(SI_SINCE_TRIAGE,probs=0.05,na.rm=T),
                   si_q1=quantile(SI_SINCE_TRIAGE,probs=0.25,na.rm=T),
                   si_median=median(SI_SINCE_TRIAGE,na.rm=T),
                   si_q3=quantile(SI_SINCE_TRIAGE,probs=0.75,na.rm=T),
                   si_p95=quantile(SI_SINCE_TRIAGE,probs=0.95,na.rm=T),
                   si_max=max(SI_SINCE_TRIAGE,na.rm=T)) %>%
  ungroup %>% arrange(desc(enc_cnt)) %>%
  View


#--transfer timing
nonsi_tm<-rs_idx %>%
  filter(CASE_CTRL==0) %>%
  mutate(TRANS_time_grp=cut(TRANS_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         C_time_grp=cut(C_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         ABX_time_grp=cut(ABX_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         early_transition=case_when(!is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE < TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "Culture and ABX within ED; Transitioned",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE < TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE ~ "Culture within ED; ABX after ED; Transitioned",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "ABX within ED; Culture after ED; Transitioned",
                                    is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "Culture and ABX within ED; Discharged",
                                    is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE<TRANS_SINCE_TRIAGE ~ "Culture within ED; Discharged",
                                    is.na(TRANS_SINCE_TRIAGE)&is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "ABX within ED; Discharged",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE) ~ "No ABX with Culture; Transitioned",
                                    is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE) ~ "No ABX with Culture; Discharged",
                                    !is.na(TRANS_SINCE_TRIAGE)&is.na(C_SINCE_TRIAGE)&is.na(ABX_SINCE_TRIAGE) ~ "No ABX and No Culture; Transitioned",
                                    TRUE ~ "No ABX and No Culture; Discharged"),
         CALYR=as.numeric(format(TRIAGE_START,"%Y")))

saveRDS(nonsi_tm,file="./data/NONSI_timing_dist.rda")

#identify outlier and collect summary
nonsi_tm %>%
  group_by(SERVDEP_NAME) %>%
  dplyr::summarise(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   si_min=min(TRANS_SINCE_TRIAGE,na.rm=T),
                   si_p05=quantile(TRANS_SINCE_TRIAGE,probs=0.05,na.rm=T),
                   si_q1=quantile(TRANS_SINCE_TRIAGE,probs=0.25,na.rm=T),
                   si_median=median(TRANS_SINCE_TRIAGE,na.rm=T),
                   si_q3=quantile(TRANS_SINCE_TRIAGE,probs=0.75,na.rm=T),
                   si_p95=quantile(TRANS_SINCE_TRIAGE,probs=0.95,na.rm=T),
                   si_max=max(TRANS_SINCE_TRIAGE,na.rm=T)) %>%
  ungroup %>%
  View


#--discharge timing
rs_idx %>%
  filter(END_SINCE_TRIAGE==PRED_POINT) %>% 
  dplyr::summarise(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   si_min=min(END_SINCE_TRIAGE,na.rm=T),
                   si_p05=quantile(END_SINCE_TRIAGE,probs=0.05,na.rm=T),
                   si_q1=quantile(END_SINCE_TRIAGE,probs=0.25,na.rm=T),
                   si_median=median(END_SINCE_TRIAGE,na.rm=T),
                   si_q3=quantile(END_SINCE_TRIAGE,probs=0.75,na.rm=T),
                   si_p95=quantile(END_SINCE_TRIAGE,probs=0.95,na.rm=T),
                   si_max=max(END_SINCE_TRIAGE,na.rm=T)) %>%
  View

  