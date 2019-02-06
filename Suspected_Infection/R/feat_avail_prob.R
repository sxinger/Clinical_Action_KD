#### predict SI: latest value abstraction, glm ####
#### estimate availability at admission ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "survival"
))

#load data
enroll<-readRDS("./data/SI_enroll.rda")
sample_idx<-readRDS("./data/sample_idx.rda")
dat<-readRDS("./data/data_at_enc_discrt.rda") %>%
  group_by(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE) %>%
  arrange(desc(START_SINCE_TRIAGE)) %>%
  dplyr::slice(1:1) %>% ## latest values
  ungroup %>%
  dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,VARIABLE,TVAL_CHAR,START_SINCE_TRIAGE) %>%
  unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_") %>%
  mutate(VARIABLE=case_when(grepl("(FLO_MEAS_ID\\+hash)|(FLO_MEAS_ID\\+LINE)+",VARIABLE) ~ paste0(VARIABLE,"_",TVAL_CHAR),
                            grepl("(VISITDETAIL\\|POS)+",VARIABLE) ~ paste0(VARIABLE,":",TVAL_CHAR),
                            grepl("num_",VARIABLE) ~ TVAL_CHAR,
                            TRUE ~ VARIABLE))

saveRDS(dat,file="./data/data_all_time.rda")

#load minimal set
k<-119
dat<-readRDS("./data/data_all_time.rda")
glm_out<-readRDS(paste0("./output/glm2_rec_fs",k,".rda"))

#estimate likelihood of observing at time t
obs_t<-c(0,5,15,30,60,120,180,Inf)/60

var_t<-c()
for(v in 1:20){
  dat_v<-dat %>% 
    filter(VARIABLE==glm_out$var_imp$Feature[v]) %>%
    right_join(enroll %>% 
                dplyr::select(PATIENT_NUM,ENCOUNTER_NUM,PRED_POINT) %>%
                unite("PAT_ENC",c("PATIENT_NUM","ENCOUNTER_NUM"),sep="_"),
              by="PAT_ENC") %>%
    dplyr::mutate(time=coalesce(START_SINCE_TRIAGE,PRED_POINT),
                  status=as.numeric(!is.na(START_SINCE_TRIAGE))) %>%
    filter(time > 0)
  
  fit_v<-survfit(Surv(time,status)~1,data=dat_v)
  timing<-data.frame(avail_t=fit_v$time,
                     prob_of_obs=1 - fit_v$surv) %>%
    dplyr::mutate(cut=cut(avail_t,breaks=obs_t,labels = F,include.lowest = T)) %>%
    group_by(cut) %>%
    top_n(n=1L,wt=-prob_of_obs) %>%
    ungroup
  
  var_t<-rbind(c(Feature=glm_out$var_imp$Feature[v],
                 median_t=median(dat_v$START_SINCE_TRIAGE,na.rm=T),
                 IQR_t=IQR(dat_v$START_SINCE_TRIAGE,na.rm=T),
                 round(timing$prob_of_obs,4)))
  
  cat("finish",v,".\n")
}
colnames(var_t)<-c("Feature","median_t","IQR_t",
                   paste0())
var_t %<>%
  inner_join(glm_out$var_imp, by="Feature")


write.csv(var_t,file="./output/variable_timing20.csv",row.names=F)


