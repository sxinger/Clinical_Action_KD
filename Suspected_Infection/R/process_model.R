##========prepare
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c("data.table",
                    "Matrix",
                    "pROC",
                    "dplyr",
                    "tidyr",
                    "stringr",
                    "magrittr",
                    "ggplot2"))

enroll<-readRDS("./data/SI_enroll.rda") %>%
  dplyr::filter(!is.na(SI_SINCE_TRIAGE))

