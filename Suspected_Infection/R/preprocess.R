###########################################################
# This script preprocessed the selected variables, by     #
# - selecting observations before certain perdiction point#
# - aggregating multiple values for the same variable     #
# - encoding categorical variables                        #

#### Preprocessing ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
library("Matrix")
require_libraries(c("dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"))

## Load in fact_stack and pat_tbl
data_at_enc<-readRDS("./data/data_at_enc.rda")
feat_at_enc<-readRDS("./data/feat_at_enc.rda")
target<-readRDS("./data/define_censor.rda")


## latest values
data_at_enc_latest %<>%
  group_by(ENCOUNTER_NUM,VARIABLE) %>%
  arrange(desc(START_SINCE_TRIAGE)) %>%
  dplyr::slice(1:1) %>%
  ungroup %>%
  long_to_sparse_matrix()