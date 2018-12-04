#### random sampling ####
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

## load data
cohort<-readRDS("./data/SI_enroll.rda") %>%
  dplyr::mutate(part73 = sample(c("T","V"),prob=c(0.7,0.3),n(),replace=T),
                cv10 = sample(1:10,n(),replace=T))

## save result
saveRDS(cohort,file="./data/rand_idx.rda")

