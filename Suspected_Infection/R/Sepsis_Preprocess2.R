#### Preprocessing ####
setwd("~/sepsis")
rm(list=ls()); gc()
source("./helper_functions.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"                    
                 ))

## Load in fact_stack and pat_tbl
load("../data/EMR_num.Rdata")
load("../data/EMR_cat_struct.Rdata")
load("../data/EMR_cat_flowsht.Rdata")
load("./data/EMR_dict.Rdata")
load("./data/rand_sample_idx.Rdata")

unique(emr_dict$LEV1)

## pre-filter: use selected data types and their facts within certain time window
emr_num %<>%
  #pre-selected data types
  left_join(emr_dict %>% dplyr::select(CONCEPT_CD,LEV1),by="CONCEPT_CD") %>%
  dplyr::filter(!(LEV1 == "Medications" & MODIFIER != "MedObs:Historical")) %>% 
  dplyr::filter(LEV1 %in% c("Alerts",
                            "Allergy",
                            "Cardiology Lab Results",
                            "Flowsheets",
                            "Laboratory Tests (KUH Hierarchy)",
                            "Laboratory Tests (LOINC Hierarchy)")) %>%
  #facts should occur before choice of anchor points
  left_join(trt3hr %>% 
            dplyr::select(ENCOUNTER_NUM,TRT3HR_END1,SEPSIS_SINCE_TRIAGE),
          by="ENCOUNTER_NUM") %>%
  mutate(pred_point = ifelse(is.na(TRT3HR_END1),SEPSIS_SINCE_TRIAGE,TRT3HR_END1)) %>%
  filter(START_SINCE_TRIAGE <= pred_point)

emr_num %<>%
  dplyr::mutate(VARIABLE=paste0(CONCEPT_CD,"@",UNITS,"@",MODIFIER)) #

# set aside time-invariant variables (only single entry is available within the time frame)
num2_invar<-emr_num %>%
  group_by(ENCOUNTER_NUM,VARIABLE) %>%
  #recording frequency at individual level
  dplyr::mutate(pt_freq=length(unique(START_DATE))) %>%
  ungroup %>% group_by(VARIABLE) %>%
  dplyr::summarize(avg_pt_freq = mean(pt_freq,na.rm=T),
                   sd_pt_fre1 = sd(pt_freq,na.rm=T)) %>%
  ungroup %>%
  mutate(invar_ind = ifelse(avg_pt_freq==1 & (sd_pt_fre1==0 | is.na(sd_pt_fre1)),1,0))

# only aggregate values for variables with multiple entries
num2_var<-emr_num %>%
  semi_join(num2_invar %>% filter(invar_ind == 0),by="VARIABLE") %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE, NVAL,START_SINCE_TRIAGE) %>%
  unique %>% group_by(ENCOUNTER_NUM, VARIABLE) %>%
  arrange(START_SINCE_TRIAGE) %>%
  do(mutate(.
            ,agg_min = ifelse(length(NVAL)<2, NA, min(NVAL,na.rm=T))
            ,agg_max = ifelse(length(NVAL)<2, NA, max(NVAL,na.rm=T))
            ,agg_mean = ifelse(length(NVAL)<2, NA, mean(NVAL,na.rm=T))
            # ,agg_sd = ifelse(length(NVAL) <= 1, 0, sd(NVAL,na.rm=T))
            ,agg_initial = NVAL[1]
            # ,init_t = START_SINCE_TRIAGE[1]
  )) %>%
  # mutate(min_t = ifelse(NVAL==agg_min,START_SINCE_TRIAGE,NA),
  #        max_t = ifelse(NVAL==agg_max,START_SINCE_TRIAGE,NA)) %>%
  arrange(ENCOUNTER_NUM) %>% ungroup

# collect time-invariant variables
num2_fix<-emr_num %>%
  semi_join(num2_invar %>% filter(invar_ind == 0),by="VARIABLE") %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE, NVAL,START_SINCE_TRIAGE) %>%
  mutate(abs_hr = abs(START_SINCE_TRIAGE),
         var_t = START_SINCE_TRIAGE) %>%
  group_by(ENCOUNTER_NUM, VARIABLE) %>%
  top_n(n=-1,abs_hr) %>% ungroup %>%
  arrange(ENCOUNTER_NUM)

# merge variable with aggregate function
num2_var %<>%
  dplyr::select(
                ENCOUNTER_NUM, VARIABLE
                ,agg_min, agg_max, agg_initial,agg_mean
                # ,agg_sd,min_t, max_t, init_t
                ) %>% unique %>%
  gather(agg,NVAL_NUM,-ENCOUNTER_NUM,-VARIABLE) %>%
  unite("VARIABLE_agg",c("VARIABLE","agg")) %>%
  filter(!is.na(NVAL_NUM))

num2_fix2<- num2_fix %>%
  dplyr::rename(VARIABLE_agg=VARIABLE) %>%
  dplyr::select(ENCOUNTER_NUM, VARIABLE_agg, NVAL) %>% unique 
  # bind_rows(num2_fix %>%
  #             dplyr::mutate(VARIABLE_agg=paste0(VARIABLE,"_t"),
  #                    NVAL=START_SINCE_TRIAGE) %>%
  #             dplyr::select(ENCOUNTER_NUM, VARIABLE_agg, NVAL) %>% unique)
  

# collect and process categorical variables
#pre-selected data types
emr_cat_struct %<>%
  semi_join(emr_dict,by="CONCEPT_CD") %>%
  #facts should occur before completion and within 3 hours since triage
  left_join(rand_sample %>% 
              dplyr::select(ENCOUNTER_NUM,TRT3HR_END1,SEPSIS_SINCE_TRIAGE),
            by="ENCOUNTER_NUM") %>%
  mutate(pred_point = ifelse(is.na(TRT3HR_END1),SEPSIS_SINCE_TRIAGE,TRT3HR_END1)) %>%
  filter(START_SINCE_TRIAGE <= pred_point)

emr_cat_flowsht %<>%
  semi_join(emr_dict,by="CONCEPT_CD") %>%
  #facts should occur before completion and within 3 hours since triage
  left_join(rand_sample %>% 
              dplyr::select(ENCOUNTER_NUM,TRT3HR_END1,SEPSIS_SINCE_TRIAGE),
            by="ENCOUNTER_NUM") %>%
  mutate(pred_point = ifelse(is.na(TRT3HR_END1),SEPSIS_SINCE_TRIAGE,TRT3HR_END1)) %>%
  filter(START_SINCE_TRIAGE <= pred_point)

# presel_cat2<-presel_cat %>%
#   arrange(ENCOUNTER_NUM, START_DT) %>%
#   mutate(within_t = ifelse(START_SINCE_TRIAGE<=t,1,0)) %>%
#   filter(within_t == 1)
# 
# # aggregate values within time-window t
# presel_cat2 %<>%
#   dplyr::select(ENCOUNTER_NUM, VARIABLE, TVAL,START_SINCE_TRIAGE) %>%
#   mutate(abs_hr = abs(START_SINCE_TRIAGE),
#          var_t = START_SINCE_TRIAGE) %>%
#   group_by(ENCOUNTER_NUM, VARIABLE, TVAL) %>%
#   top_n(n=-1,abs_hr) %>% ungroup %>%  #get most recent value
#   arrange(ENCOUNTER_NUM)
# 
# max(presel_cat2$START_SINCE_TRIAGE)

# # hot-encoding
# presel_cat3 <- presel_cat2 %>%
#   mutate(NVAL = 1) %>%
#   unite("VARIABLE_agg",c("VARIABLE","TVAL")) %>%
#   dplyr::select(ENCOUNTER_NUM,VARIABLE_agg,NVAL) %>%
#   unique
# 
# # enumerate
# presel_cat4 <- presel_cat2 %>%
#   dplyr::rename(VARIABLE_agg = VARIABLE,
#                 NVAL = TVAL) %>%
#   dplyr::select(ENCOUNTER_NUM,VARIABLE_agg,NVAL) %>%
#   unique

# put together
load(paste0("./data/sepsis_presel_long_ohc.Rdata"))
fact_stack_add<-num2_var %>%
  bind_rows(fact_stack_ohc) %>%
  bind_rows(num2_fix2) %>%
  # bind_rows(presel_cat3) %>%
  arrange(ENCOUNTER_NUM,VARIABLE_agg)

# fact_stack_enum<-presel_num2_var %>%
#   mutate(NVAL = as.character(NVAL),
#          num = 1) %>%
#   bind_rows(presel_num2_fix2 %>% 
#               mutate(NVAL = as.character(NVAL),
#                      num = 1)) %>%
#   bind_rows(presel_cat4 %>% mutate(num = 0)) %>%
#   arrange(ENCOUNTER_NUM,VARIABLE_agg)

# save
save(fact_stack_add,file=paste0("./data/sepsis_presel_long_aug_ohc.Rdata"))
