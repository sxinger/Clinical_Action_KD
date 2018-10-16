###########################################################################
# This script loads the study cohort and pre-selected variables from HERON#
# Note that the tables: SEPSIS_INITIAL_SI3,PreSel_cat_var,PreSel_num_var  #
# were collected in Oracle by sql and already existed in my schema        #
###########################################################################

#clean the slate
rm(list=ls()); gc()

#set up working directory
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c( "dplyr"
                     ,"tidyr"
                     ,"magrittr"
                     ,"ROracle"
                     ,"DBI"))

## Connect to Oracle database
config_file<-read.csv('../config.csv')
conn<-connect_to_db("Oracle",config_file)


#get pre-selected facts from heron (saved tables in HERON under "XSONG"'s schema)
# PreSel_cat_var - pre-selected categorical features stack
# PreSel_num_var - pre-selected numerical features stack
# SEPSIS_INITIAL_SI3 - final severe sepsis cohort for analysis

presel_cat<-dbGetQuery(c_connect,"select fc.* from xsong.PreSel_cat_var fc
                                   where exists (select 1 from xsong.SEPSIS_INITIAL_SI3 s
                                                 where s.encounter_num = fc.encounter_num and
                                                       s.sepsis_ind = 1)") %>%
  arrange(ENCOUNTER_NUM, START_DT)


presel_num<-dbGetQuery(c_connect,"select fn.* from xsong.PreSel_num_var fn
                                   where exists (select 1 from xsong.SEPSIS_INITIAL_SI3 s
                                                 where s.encounter_num = fn.encounter_num and
                                                       s.sepsis_ind = 1) and
                                         NVAL is not null") %>%
  arrange(ENCOUNTER_NUM, START_DT)

#eyeball an example
presel_cat %>% filter(ENCOUNTER_NUM == 1480) %>% View
presel_num %>% filter(ENCOUNTER_NUM == 1480) %>% View

#save tables locally
save(presel_cat, file="./data/sepsis_presel_cat.Rdata")
save(presel_num, file="./data/sepsis_presel_num.Rdata")

