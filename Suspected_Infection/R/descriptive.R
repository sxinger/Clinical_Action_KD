rm(list=ls())
gc()

pat_at_enc<-readRDS("./data/pat_at_enc.rda")
rs_idx<-readRDS("./data/rand_idx.rda")

#
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
#age in numeric
demo_master %>%
  dplyr::select(AGE,CASE_CTRL) %>%
  group_by(CASE_CTRL) %>%
  dplyr::summarise(age_mean=mean(AGE,na.rm=T),
                   age_sd=sd(AGE,na.rm=T)) %>%
  ungroup %>% View

#other categorical
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





  