#### random sampling ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
library("Matrix")
require_libraries(c(
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr"                   
                  ))

## load data
target<-readRDS("./data/define_censor.rda")

## assigne sampling indices
target %<>% 
  dplyr::mutate(part73 = sample(c("T","V"),prob=c(0.7,0.3),n(),replace=T),
                cv10 = sample(1:10,n(),replace=T))

## save result
saveRDS(target,file="./data/target_sidx.rda")

