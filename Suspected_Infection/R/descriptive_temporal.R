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
demo_master<-readRDS("./data/pat_at_enc.rda") %>%
  inner_join(rs_idx %>% dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,CASE_CTRL),
            by=c("PATIENT_NUM","ENCOUNTER_NUM")) %>%
  mutate(STATUS=ifelse(is.na(DEATH_DATE),0,1))

demo_master %<>%
  mutate(AGE_GRP=case_when(AGE<30 ~ "20s and below",
                           AGE>=30 & AGE <40 ~ "30s",
                           AGE>=40 & AGE <50 ~ "40s",
                           AGE>=50 & AGE <60 ~ "50s",
                           AGE>=60 & AGE <70 ~ "60s",
                           AGE>=70 ~ "70s and above"))
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
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE >= TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE >= TRANS_SINCE_TRIAGE ~ "Culture and ABX after ED; Transitioned",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE < TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE ~ "Culture within ED; ABX after ED; Transitioned",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE>=TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "ABX within ED; Culture after ED; Transitioned",
                                    TRUE ~ "Culture and ABX within ED; Discharged"),
         CALYR=as.numeric(format(TRIAGE_START,"%Y")))

saveRDS(si_tm,file="./data/SI_timing_dist.rda")

#distribution
descdist(si_tm$SI_SINCE_TRIAGE,discrete = F)
descdist(si_tm[si_tm$early_transition=="Culture and ABX within ED; Transitioned",]$SI_SINCE_TRIAGE,discrete = F)
descdist(si_tm[si_tm$early_transition=="Culture within ED; ABX after ED; Transitioned",]$SI_SINCE_TRIAGE,discrete = F)
descdist(si_tm[si_tm$early_transition=="ABX within ED; Culture after ED; Transitioned",]$SI_SINCE_TRIAGE,discrete = F)
descdist(si_tm[si_tm$early_transition=="Culture and ABX within ED; Discharged",]$SI_SINCE_TRIAGE,discrete = F)
descdist(si_tm[si_tm$SI_SINCE_TRIAGE<=24,]$SI_SINCE_TRIAGE,discrete = F)

#outlier identification
si_tm %<>%
  mutate(log_SI_SINCE_TRIAGE=ifelse(SI_SINCE_TRIAGE==0,log(0.01),log(SI_SINCE_TRIAGE)),
         log_ABX_SINCE_TRIAGE=ifelse(ABX_SINCE_TRIAGE==0,log(0.01),log(ABX_SINCE_TRIAGE)))

si_ol<-si_tm %>%
  dplyr::summarize(log_lb=median(log_SI_SINCE_TRIAGE,na.rm = T)-1.5*IQR(log_SI_SINCE_TRIAGE,na.rm = T),
                   log_ub=median(log_SI_SINCE_TRIAGE,na.rm = T)+1.5*IQR(log_SI_SINCE_TRIAGE,na.rm = T),
                   log_tm_median=median(log_SI_SINCE_TRIAGE,na.rm = T)) %>%
  mutate(lb=exp(log_lb),ub=exp(log_ub),tm_median=exp(log_tm_median))

#-1.683487,4.053067,1.18479,0.1857253,57.57375,3.27
#upper: 4.053067-->58 hours-->down to 48 hours

ggplot(si_tm,aes(x=log_SI_SINCE_TRIAGE))+
  geom_density()+stat_ecdf()+
  geom_vline(xintercept = c(log(3),log(13),log(24),log(48),4.053067),linetype=2)+
  geom_text(data=data.frame(x=c(log(3),log(13),log(24),log(48),4.053067),
                            y=c(0.4,0.75,0.8,0.9,0.95),
                            label=c("3hr","13hr","24hr","48hr","upper bound (56hr)")),
            aes(x=x,y=y,label=label))

si_tm %<>% filter(SI_SINCE_TRIAGE<=48)

calyrs<-unique(si_tm$CALYR)
si_calyr<-si_tm %>%
  filter(SI_SINCE_TRIAGE<=48) %>%
  group_by(CALYR) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   log_lb=median(log_SI_SINCE_TRIAGE,na.rm = T)-1.5*IQR(log_SI_SINCE_TRIAGE,na.rm = T),
                   log_ub=median(log_SI_SINCE_TRIAGE,na.rm = T)+1.5*IQR(log_SI_SINCE_TRIAGE,na.rm = T),
                   log_tm_median=median(log_SI_SINCE_TRIAGE,na.rm = T)) %>%
  ungroup %>%
  mutate(lb=exp(log_lb),ub=exp(log_ub),tm_median=exp(log_tm_median))

si_tm %<>%
  mutate(CALYR_grp=case_when(CALYR<=2008 ~ "2007-2008",
                             CALYR<=2010&CALYR>2008 ~ "2009-2010",
                             CALYR<=2012&CALYR>2010 ~ "2011-2012",
                             CALYR<=2014&CALYR>2012 ~ "2013-2014",
                             CALYR<=2016&CALYR>2014 ~ "2015-2016",
                             CALYR<=2018&CALYR>2016 ~ "2017-2018"))

scenarios<-unique(si_tm$early_transition)
si_grp<-si_tm %>%
  group_by(CALYR_grp,early_transition) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   log_lb=median(log_SI_SINCE_TRIAGE,na.rm = T)-1.5*IQR(log_SI_SINCE_TRIAGE,na.rm = T),
                   log_ub=median(log_SI_SINCE_TRIAGE,na.rm = T)+1.5*IQR(log_SI_SINCE_TRIAGE,na.rm = T),
                   log_tm_median=median(log_SI_SINCE_TRIAGE,na.rm = T),
                   log_tm_90th=quantile(log_SI_SINCE_TRIAGE,probs=0.9,na.rm = T)) %>%
  ungroup %>%
  mutate(lb=exp(log_lb),ub=exp(log_ub),
         tm_median=exp(log_tm_median),
         tm_90th=exp(log_tm_90th)) %>%
  mutate(label_med=round(tm_median,1),
         label_90th=round(tm_90th,1),
         label_pos_x_med=log_tm_median,
         label_pos_x_90th=log_tm_90th)

si_time_grp<-si_tm %>%
  group_by(CALYR_grp,early_transition,SI_time_grp) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  mutate(log10_enc_cnt=log10(enc_cnt)) %>%
  mutate(early_transition=recode(early_transition,
                                 `Culture and ABX within ED; Discharged`="1.Culture and ABX within ED; Discharged",
                                 `Culture within ED; ABX after ED; Transitioned`="2.Culture within ED; ABX after ED; Transitioned",
                                 `ABX within ED; Culture after ED; Transitioned`="3.ABX within ED; Culture after ED; Transitioned",
                                 `Culture and ABX within ED; Transitioned`="4.Culture and ABX within ED; Transitioned"))

#compare scenatios w.r.t. real times
ggplot(si_time_grp,aes(x=SI_time_grp,y=enc_cnt,fill=early_transition))+
  geom_bar(position="stack",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="SI onset (hours since triage)",y="Number of Encounters",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)

ggplot(si_time_grp,aes(x=SI_time_grp,y=log10_enc_cnt,fill=early_transition))+
  geom_bar(position="dodge",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="SI onset (hours since triage)",y="log10(Number of Encounters)",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)


#compare scenarios over time
ggplot(si_tm,aes(x=log_SI_SINCE_TRIAGE,color=early_transition))+
  stat_ecdf()+
  geom_point(data=si_grp,aes(x=log_tm_median,y=0.5,size=enc_cnt))+
  geom_point(data=si_grp,aes(x=log_tm_90th,y=0.9,size=enc_cnt))+
  geom_label_repel(data=si_grp,aes(x=label_pos_x_med,y=0.5,label=label_med))+
  geom_label_repel(data=si_grp,aes(x=label_pos_x_90th,y=0.9,label=label_90th))+
  labs(x="log(hours_since_triage)",y="cumulative probability",
       color="SI location",size="Number of Encounters")+
  facet_wrap(~CALYR_grp)

#compare abx time
abx_grp<-si_tm %>%
  group_by(CALYR_grp,early_transition) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   log_lb=median(log_ABX_SINCE_TRIAGE,na.rm = T)-1.5*IQR(log_ABX_SINCE_TRIAGE,na.rm = T),
                   log_ub=median(log_ABX_SINCE_TRIAGE,na.rm = T)+1.5*IQR(log_ABX_SINCE_TRIAGE,na.rm = T),
                   log_tm_median=median(log_ABX_SINCE_TRIAGE,na.rm = T),
                   log_tm_90th=quantile(log_ABX_SINCE_TRIAGE,probs=0.9,na.rm = T)) %>%
  ungroup %>%
  mutate(lb=exp(log_lb),ub=exp(log_ub),
         tm_median=exp(log_tm_median),
         tm_90th=exp(log_tm_90th)) %>%
  mutate(label_med=round(tm_median,1),
         label_90th=round(tm_90th,1),
         label_pos_x_med=log_tm_median,
         label_pos_x_90th=log_tm_90th)

abx_time_grp<-si_tm %>%
  group_by(CALYR_grp,early_transition,ABX_time_grp) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  mutate(log10_enc_cnt=log10(enc_cnt)) %>%
  mutate(early_transition=recode(early_transition,
                                 `Culture and ABX within ED; Discharged`="1.Culture and ABX within ED; Discharged",
                                 `Culture within ED; ABX after ED; Transitioned`="2.Culture within ED; ABX after ED; Transitioned",
                                 `ABX within ED; Culture after ED; Transitioned`="3.ABX within ED; Culture after ED; Transitioned",
                                 `Culture and ABX within ED; Transitioned`="4.Culture and ABX within ED; Transitioned"))

#compare scenatios w.r.t. real times
ggplot(abx_time_grp,aes(x=ABX_time_grp,y=enc_cnt,fill=early_transition))+
  geom_bar(position="stack",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="ABX time (hours since triage)",y="Number of Encounters",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)

ggplot(abx_time_grp,aes(x=ABX_time_grp,y=log10_enc_cnt,fill=early_transition))+
  geom_bar(position="dodge",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="Anchor Point (hours since triage)",y="log10(Number of Encounters)",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)


#compare scenarios over time
ggplot(si_tm,aes(x=log_ABX_SINCE_TRIAGE,color=early_transition))+
  stat_ecdf()+
  geom_point(data=abx_grp,aes(x=log_tm_median,y=0.5,size=enc_cnt))+
  geom_point(data=abx_grp,aes(x=log_tm_90th,y=0.9,size=enc_cnt))+
  geom_label_repel(data=abx_grp,aes(x=label_pos_x_med,y=0.5,label=label_med))+
  geom_label_repel(data=abx_grp,aes(x=label_pos_x_90th,y=0.9,label=label_90th))+
  labs(x="log(ABX_hours_since_triage)",y="cumulative probability",
       color="SI location",size="Number of Encounters")+
  facet_wrap(~CALYR_grp)


#--transfer timing
nonsi_tm<-rs_idx %>%
  filter(CASE_CTRL==0) %>%
  mutate(TRANS_time_grp=cut(TRANS_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         C_time_grp=cut(C_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         ABX_time_grp=cut(ABX_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F),
         early_transition=case_when(!is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE < TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE < TRANS_SINCE_TRIAGE ~ "Culture and ABX within ED; Transitioned",
                                    !is.na(TRANS_SINCE_TRIAGE)&!is.na(C_SINCE_TRIAGE)&!is.na(ABX_SINCE_TRIAGE)&C_SINCE_TRIAGE >= TRANS_SINCE_TRIAGE&ABX_SINCE_TRIAGE >= TRANS_SINCE_TRIAGE ~ "Culture and ABX after ED; Transitioned",
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
nonsi_tm<-readRDS("./data/NONSI_timing_dist.rda")

nonsi_tm %<>% filter(PRED_POINT<=48&PRED_POINT>=0) %>%
  mutate(log_PRED_POINT=ifelse(PRED_POINT==0,log(0.01),log(PRED_POINT))) %>%
  mutate(CALYR_grp=case_when(CALYR<=2008 ~ "2007-2008",
                             CALYR<=2010&CALYR>2008 ~ "2009-2010",
                             CALYR<=2012&CALYR>2010 ~ "2011-2012",
                             CALYR<=2014&CALYR>2012 ~ "2013-2014",
                             CALYR<=2016&CALYR>2014 ~ "2015-2016",
                             CALYR<=2018&CALYR>2016 ~ "2017-2018")) %>%
  mutate(trans_disch=gsub(".*; ","",early_transition))

pred_ol_grp<-nonsi_tm %>%
  group_by(CALYR_grp,trans_disch,early_transition) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   log_lb=median(log_PRED_POINT,na.rm = T)-1.5*IQR(log_PRED_POINT,na.rm = T),
                   log_ub=median(log_PRED_POINT,na.rm = T)+1.5*IQR(log_PRED_POINT,na.rm = T),
                   log_tm_median=median(log_PRED_POINT,na.rm = T),
                   log_tm_90th=quantile(log_PRED_POINT,probs=0.9,na.rm = T)) %>%
  ungroup %>%
  mutate(lb=exp(log_lb),ub=exp(log_ub),
         tm_median=exp(log_tm_median),
         tm_90th=exp(log_tm_90th)) %>%
  mutate(label_med=round(tm_median,1),
         label_90th=round(tm_90th,1),
         label_pos_x_med=log_tm_median,
         label_pos_x_90th=log_tm_90th)

pred_time_grp<-nonsi_tm %>%
  filter(PRED_POINT<=48&PRED_POINT>=0) %>%
  mutate(PRED_POINT_grp=cut(ABX_SINCE_TRIAGE,breaks=brks,include.lowest=T,labels=F)) %>%
  group_by(CALYR_grp,early_transition,PRED_POINT_grp) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  mutate(log10_enc_cnt=log10(enc_cnt))

#compare scenatios w.r.t. real times
ggplot(pred_time_grp,aes(x=PRED_POINT_grp,y=enc_cnt,fill=early_transition))+
  geom_bar(position="stack",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="ABX time (hours since triage)",y="Number of Encounters",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)

ggplot(pred_time_grp,aes(x=PRED_POINT_grp,y=log10_enc_cnt,fill=early_transition))+
  geom_bar(position="dodge",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="ABX time (hours since triage)",y="log10(Number of Encounters)",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)

#compare scenarios over time
ggplot(nonsi_tm,aes(x=log_PRED_POINT,color=early_transition))+
  stat_ecdf(aes(linetype=trans_disch))+
  geom_point(data=pred_ol_grp,aes(x=log_tm_median,y=0.5,size=enc_cnt))+
  geom_point(data=pred_ol_grp,aes(x=log_tm_90th,y=0.9,size=enc_cnt))+
  geom_label_repel(data=pred_ol_grp,aes(x=label_pos_x_med,y=0.5,label=label_med))+
  geom_label_repel(data=pred_ol_grp,aes(x=label_pos_x_90th,y=0.9,label=label_90th))+
  labs(x="log(hours_since_triage)",y="cumulative probability",
       color="SI location",size="Number of Encounters",linetype="Disposition")+
  facet_wrap(~CALYR_grp)

#compare abx time
abx_ol_grp<-nonsi_tm %>%
  filter(early_transition %in% c("Culture and ABX within ED; Transitioned",
                                 "Culture within ED; ABX after ED; Transitioned",
                                 "ABX within ED; Culture after ED; Transitioned")) %>%
  mutate(log_ABX_SINCE_TRIAGE=ifelse(ABX_SINCE_TRIAGE==0,log(0.01),log(ABX_SINCE_TRIAGE))) %>%
  group_by(CALYR_grp,early_transition) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM)),
                   log_lb=median(log_ABX_SINCE_TRIAGE,na.rm = T)-1.5*IQR(log_ABX_SINCE_TRIAGE,na.rm = T),
                   log_ub=median(log_ABX_SINCE_TRIAGE,na.rm = T)+1.5*IQR(log_ABX_SINCE_TRIAGE,na.rm = T),
                   log_tm_median=median(log_ABX_SINCE_TRIAGE,na.rm = T),
                   log_tm_90th=quantile(log_ABX_SINCE_TRIAGE,probs=0.9,na.rm = T)) %>%
  ungroup %>%
  mutate(lb=exp(log_lb),ub=exp(log_ub),
         tm_median=exp(log_tm_median),
         tm_90th=exp(log_tm_90th)) %>%
  mutate(label_med=round(tm_median,1),
         label_90th=round(tm_90th,1),
         label_pos_x_med=log_tm_median,
         label_pos_x_90th=log_tm_90th)

abx_time_grp<-nonsi_tm %>%
  filter(PRED_POINT<=48&PRED_POINT>=0) %>%
  filter(early_transition %in% c("Culture and ABX within ED; Transitioned",
                                 "Culture within ED; ABX after ED; Transitioned",
                                 "ABX within ED; Culture after ED; Transitioned")) %>%
  group_by(CALYR_grp,early_transition,ABX_time_grp) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>%
  mutate(log10_enc_cnt=log10(enc_cnt))

#compare scenatios w.r.t. real times
ggplot(abx_time_grp,aes(x=ABX_time_grp,y=enc_cnt,fill=early_transition))+
  geom_bar(position="stack",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="ABX time (hours since triage)",y="Number of Encounters",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)

ggplot(abx_time_grp,aes(x=ABX_time_grp,y=log10_enc_cnt,fill=early_transition))+
  geom_bar(position="dodge",stat="identity")+
  scale_x_continuous(breaks=seq_along(brk_label),labels=brk_label)+
  labs(x="ABX time (hours since triage)",y="log10(Number of Encounters)",fill="SI location")+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
  facet_wrap(~CALYR_grp)


#compare scenarios over time
ggplot(nonsi_tm %>%
         filter(early_transition %in% c("Culture and ABX within ED; Transitioned",
                                        "Culture within ED; ABX after ED; Transitioned",
                                        "ABX within ED; Culture after ED; Transitioned")) %>%
         mutate(log_ABX_SINCE_TRIAGE=ifelse(ABX_SINCE_TRIAGE==0,log(0.01),log(ABX_SINCE_TRIAGE))),
       aes(x=log_ABX_SINCE_TRIAGE,color=early_transition))+
  stat_ecdf()+
  geom_point(data=abx_ol_grp,aes(x=log_tm_median,y=0.5,size=enc_cnt))+
  geom_point(data=abx_ol_grp,aes(x=log_tm_90th,y=0.9,size=enc_cnt))+
  geom_label_repel(data=abx_ol_grp,aes(x=label_pos_x_med,y=0.5,label=label_med))+
  geom_label_repel(data=abx_ol_grp,aes(x=label_pos_x_90th,y=0.9,label=label_90th))+
  labs(x="log(ABX_hours_since_triage)",y="cumulative probability",
       color="SI location",size="Number of Encounters")+
  facet_wrap(~CALYR_grp)

