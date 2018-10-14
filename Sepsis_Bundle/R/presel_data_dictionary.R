#################################################
# This script collects basic variable summaries #
#################################################
## set up
setwd("~/sepsis")
rm(list=ls()); gc()
source("./helper_functions.R")
require_libraries(c( "dplyr"
                    ,"tidyr"
                    ,"magrittr"
                    # ,"entropy"
                    ,"broom"
))

## load data
load("./data/sepsis_presel_long_ohc.Rdata")
load("./data/sepsis_target_trt3hr.Rdata")

#create feature dictionary
enc_cnt_all<-nrow(rand_sample %>% semi_join(fact_stack_ohc,by="ENCOUNTER_NUM"))
comple_all<-nrow(rand_sample %>% semi_join(fact_stack_ohc,by="ENCOUNTER_NUM") %>%
                   dplyr::filter(fast_trt3hr == 1))

feat_dict<-fact_stack_ohc %>%
  inner_join(trt3hr %>%
               dplyr::mutate(complt = fast_trt3hr) %>%
               dplyr::select(ENCOUNTER_NUM,complt),
             by="ENCOUNTER_NUM") %>%
  group_by(VARIABLE_agg) %>%
  dplyr::summarize(enc_cnt_pos = length(unique(ENCOUNTER_NUM)),
                   complt_cnt_pos = length(unique(ENCOUNTER_NUM*as.numeric(as.character(complt))))-1,
                   distinct_val=length(unique(NVAL)),
                   q_min=ifelse(length(unique(NVAL))==1,NVAL,min(NVAL,na.rm=T)),
                   q_1=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.05)[1]),
                   q_2=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.1)[1]),
                   q_3=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.15)[1]),
                   q_4=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.2)[1]),
                   q_5=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.25)[1]),
                   q_6=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.3)[1]),
                   q_7=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.35)[1]),
                   q_8=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.4)[1]),
                   q_9=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.45)[1]),
                   q_mid=ifelse(length(unique(NVAL))==1,
                                round(length(unique(ENCOUNTER_NUM))/enc_cnt_all,2),
                                median(NVAL, na.rm=T)),
                   q_11=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.55)[1]),
                   q_12=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.6)[1]),
                   q_13=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.65)[1]),
                   q_14=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.7)[1]),
                   q_15=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.75)[1]),
                   q_16=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.8)[1]),
                   q_17=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.85)[1]),
                   q_18=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.9)[1]),
                   q_19=ifelse(length(unique(NVAL))==1,NA,quantile(NVAL,probs=0.95)[1]),
                   q_max=ifelse(length(unique(NVAL))==1,NVAL,max(NVAL,na.rm=T))) %>%
  dplyr::mutate(enc_cnt_neg = enc_cnt_all-enc_cnt_pos,
                complt_cnt_neg = comple_all - complt_cnt_pos) %>%
  dplyr::mutate(incomplt_cnt_pos = enc_cnt_pos - complt_cnt_pos,
                incomplt_cnt_neg = enc_cnt_neg - complt_cnt_neg) %>%
  dplyr::mutate(complt_rate_pos = round(complt_cnt_pos/enc_cnt_pos,3),
                incomplt_rate_pos = round(incomplt_cnt_pos/enc_cnt_pos,3),
                complt_rate_neg = round(complt_cnt_neg/enc_cnt_neg,3),
                incomplt_rate_neg = round(incomplt_cnt_neg/enc_cnt_neg,3)) %>%
  dplyr::mutate(pos_odd = complt_rate_pos/incomplt_rate_pos,
                neg_odd = complt_rate_neg/incomplt_rate_neg) %>%
  dplyr::mutate(empirical_odd_ratio = pos_odd/neg_odd) %>%
  dplyr::select(VARIABLE_agg, distinct_val,
                enc_cnt_pos,complt_rate_pos, complt_rate_neg,
                pos_odd, neg_odd, empirical_odd_ratio,
                q_min,q_1,q_2,q_3,q_4,q_5,q_6,q_7,q_8,q_9,q_mid,
                q_11,q_12,q_13,q_14,q_15,q_16,q_17,q_18,q_19,q_max) %>%
  unique %>% arrange(desc(empirical_odd_ratio))


##univariate analysis for numerical variables
or_uni_num<-fact_stack_ohc %>%
  inner_join(feat_dict %>% filter(distinct_val > 1) %>%
              dplyr::select(VARIABLE_agg,pos_odd),
             by="VARIABLE_agg") %>%
  left_join(rand_sample %>%
              dplyr::mutate(y = liter2_in_2hr) %>%
              dplyr::select(ENCOUNTER_NUM,y),
            by="ENCOUNTER_NUM") %>%
  group_by(VARIABLE_agg) %>%
  dplyr::mutate(baseline=log(pos_odd)) %>%
  do(fit_cd=glm(y~NVAL+offset(baseline),data=.,family="binomial"))

cd_OR_unadj<-tidy(or_uni_num,fit_cd) %>%
  filter(term == "NVAL") %>%
  mutate(odd_ratio_unadj=exp(estimate)) %>%
  ungroup

rm(or_uni_num); gc()

feat_dict %<>%
  left_join(cd_OR_unadj,by="VARIABLE_agg")


## save results
save(feat_dict,file=paste0("./data/presel_data_dict.Rdata"))
