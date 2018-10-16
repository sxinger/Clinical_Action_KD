######################################################################
# This script attached an index column, which randomly               # 
# partition patient cohort into training (70%) and testing (30%) sets#
######################################################################
# set up
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle/")
rm(list=ls()); gc()
source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"                   
                  ))

## load data
load("./data/sepsis_presel_long_ohc.Rdata")
load("./data/sepsis_target_trt3hr.Rdata")

rand_sample<-fact_stack_ohc %>% 
  dplyr::select(ENCOUNTER_NUM) %>% unique %>% 
  left_join(trt3hr,by="ENCOUNTER_NUM") %>%
  dplyr::mutate(subgrp = ifelse(!is.na(BOLUS_TRIGGER),1,2)) %>%
  group_by(subgrp) %>% # stratified sampling
  dplyr::mutate(part73 = sample(c("T","V"),prob=c(0.7,0.3),n(),replace=T)) %>%
  ungroup

## save result
save(rand_sample,file="./data/rand_sample_idx.Rdata")

