#### Preprocessing ####
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "h2o"
))

##==============load data==============
data_at_enc<-readRDS("./data/data_at_enc_discrt.rda")
# data_bef_enc<-readRDS("./data/data_bef_enc.rda")
rs_idx<-readRDS("./data/rand_idx.rda")


#===============filter====================
# cd_out<-c("KUH\\|DX_ID","ICD","KUH\\|PROC_ID")
# data_at_enc %<>%
#   filter(!grepl(paste0("(",paste(cd_out,collapse=")|("),")"),VARIABLE))


##==============feature aggregation: recency=========
x_mt<-data_at_enc %>%
  group_by(ENCOUNTER_NUM,VARIABLE) %>%
  arrange(desc(START_SINCE_TRIAGE)) %>%
  dplyr::slice(1:1) %>% ## latest values
  ungroup %>%
  mutate(VARIABLE=case_when(grepl("(FLO_MEAS_ID\\+hash)|(FLO_MEAS_ID\\+LINE)+",VARIABLE) ~ paste0(VARIABLE,"_",TVAL_CHAR),
                            grepl("(VISITDETAIL\\|POS)+",VARIABLE) ~ paste0(VARIABLE,":",TVAL_CHAR),
                            grepl("num_",VARIABLE) ~ TVAL_CHAR,
                            TRUE ~ VARIABLE)) %>%
  long_to_sparse_matrix(.,
                        id="ENCOUNTER_NUM",
                        variable="VARIABLE",
                        val="NVAL_NUM",
                        binary=T)
dim(x_mt)
# [1] 464765   4178

y_mt<-rs_idx %>% 
  semi_join(data_at_enc,by="ENCOUNTER_NUM") %>%
  arrange(ENCOUNTER_NUM)

mean(y_mt$CASE_CTRL) #12%

all(row.names(x_mt)==y_mt$ENCOUNTER_NUM) #alignment check

Xy_sparse<-list(x_mt=x_mt,y_mt=y_mt)
saveRDS(Xy_sparse,"./data/Xy_sp_rec.rda")


##==============feature aggregation: ==============


