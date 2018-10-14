###########################################################
# This script preprocessed the selected variables, by     #
# - selecting observations before certain perdiction point#
# - aggregating multiple values for the same variable     #
# - encoding categorical variables                        #


#### Preprocessing ####
setwd("~/sepsis")
rm(list=ls()); gc()
source("./helper_functions.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"))

## Load in fact_stack and pat_tbl
load("./data/sepsis_presel_cat.Rdata")
load("./data/sepsis_presel_num.Rdata")
load("./data/sepsis_target_trt3hr.Rdata")


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

