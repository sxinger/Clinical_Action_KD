##################################
#### analyze modeling results ####
##################################
# set up
source("./helper_functions.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "ResourceSelection"
))

## load data
load("./data/subgrp_analysis.Rdata")

## 
valid_out<-c()
fs<-c()
fs10_part<-c()
for(grp in 1:3){
  valid_out %<>%
    bind_rows(out_grp[[paste0("grp",grp)]]$valid %>%
                dplyr::mutate(subgrp=grp))
  
  fs %<>%
    bind_rows(out_grp[[paste0("grp",grp)]]$var_imp %>%
                dplyr::mutate(subgrp=grp))
  
  fs10_part %<>%
    bind_rows(out_grp[[paste0("grp",grp)]]$part_eff %>%
                dplyr::mutate(subgrp=grp))
}


## auc
auc_out<-valid_out %>%
  group_by(subgrp) %>%
  dplyr::summarize(low95=pROC::ci.auc(real,pred)[1],
                   auc=pROC::ci.auc(real,pred)[2],
                   up95=pROC::ci.auc(real,pred)[3])

## sensitivity
## specificity
best_thresh<-valid_out %>%
  group_by(subgrp) %>%
  dplyr::summarize(thresh=pROC::coords(roc(real,pred),x="best",best.method="closest.topleft")[1],
                   spec=pROC::coords(roc(real,pred),x="best",best.method="closest.topleft")[2],
                   sens=pROC::coords(roc(real,pred),x="best",best.method="closest.topleft")[3])


## calibration
n_bin<-10

calib_out<-valid_out %>%
  group_by(subgrp) %>%
  arrange(pred) %>%
  do(mutate(.,pred_bin = cut(pred,breaks=quantile(pred,0:n_bin/n_bin),include.lowest=T,labels=F))) %>%
  ungroup %>% group_by(subgrp,pred_bin) %>%
  dplyr::summarize(expos=n(),
                   real_agg = sum(real),
                   pred_agg = sum(pred))

hl_out<-valid_out %>%
  group_by(subgrp) %>%
  dplyr::summarize(chi2=hoslem.test(real,pred)[["statistic"]],
                   df=hoslem.test(real,pred)[["parameter"]],
                   pval=hoslem.test(real,pred)[["p.value"]])


## check selected features
# number of features
fs_n<-fs %>%
  group_by(subgrp) %>%
  dplyr::summarize(fs_dim=max(rank)) %>%
  ungroup

# top10 features
fs_top10<-fs %>%
  group_by(subgrp) %>%
  slice(1:10)

