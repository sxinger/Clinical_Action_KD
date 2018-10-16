##################################
#### analyze modeling results ####
##################################
rm(list=ls()); gc()
setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "ResourceSelection",
                    "ROCR",
                    "PRROC"
))

## load data
# out_grp<-readRDS("./output/subgrp_analysis.rda")
out_grp<-readRDS("./output/subgrp_analysis_w_lac.rda")

## collect various modeling results
perf_mat<-c()
fs_all<-c()
fs20_part<-c()
for(grp in 1:2){
  #-- performance matrix
  valid<-out_grp[[paste0("grp",grp)]]$valid %>%
    dplyr::mutate(subgrp=grp)
  
  pred<-ROCR::prediction(valid$pred,valid$real)
  
  prc<-performance(pred,"prec","rec")
  roc<-performance(pred,"sens","spec")
  nppv<-performance(pred,"ppv","npv")
  pcfall<-performance(pred,"pcfall")
  acc<-performance(pred,"acc")
  fscore<-performance(pred,"f")
  mcc<-performance(pred,"phi")
  
  perf_tbl<-data.frame(cutoff=prc@alpha.values[[1]],
                       prec=prc@y.values[[1]],
                       rec_sens=prc@x.values[[1]],
                       stringsAsFactors = F) %>% 
    arrange(cutoff) %>%
    left_join(data.frame(cutoff=nppv@alpha.values[[1]],
                         ppv=nppv@y.values[[1]],
                         npv=nppv@x.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    dplyr::mutate(prec_rec_dist=abs(prec-rec_sens)) %>%
    left_join(data.frame(cutoff=fscore@x.values[[1]],
                         fscore=fscore@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    left_join(data.frame(cutoff=roc@alpha.values[[1]],
                         spec=roc@x.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    dplyr::mutate(Euclid_meas=sqrt((1-rec_sens)^2+(0-(1-spec))^2),
                  Youden_meas=rec_sens+spec-1) %>%
    left_join(data.frame(cutoff=pcfall@x.values[[1]],
                         pcfall=pcfall@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    left_join(data.frame(cutoff=acc@x.values[[1]],
                         acc=acc@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    left_join(data.frame(cutoff=mcc@x.values[[1]],
                         mcc=mcc@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    filter(prec > 0 & rec_sens > 0 & spec > 0)
  
  #--performance summary
  lab1<-valid$pred[valid$real==1]
  lab0<-valid$pred[valid$real==0]
  pr<-pr.curve(scores.class0 = lab1,
               scores.class1 = lab0,curve=F)
  roc_ci<-pROC::ci.auc(valid$real,valid$pred)
  
  perf_summ<-data.frame(overall_meas=c("roauc_low",
                                       "roauc",
                                       "roauc_up",
                                       "opt_thresh",
                                       "opt_sens",
                                       "opt_spec",
                                       "opt_acc",
                                       "opt_ppv",
                                       "opt_npv",
                                       "prauc1",
                                       "prauc2",
                                       "opt_prec",
                                       "opt_rec",
                                       "opt_fscore",
                                       "size"),
                        meas_val=c(roc_ci[[1]],
                                   roc_ci[[2]],
                                   roc_ci[[3]],
                                   perf_tbl$cutoff[which.min(perf_tbl$Euclid_meas)],
                                   perf_tbl$rec_sens[which.min(perf_tbl$Euclid_meas)],
                                   perf_tbl$spec[which.min(perf_tbl$Euclid_meas)],
                                   perf_tbl$acc[which.min(perf_tbl$prec_rec_dist)],
                                   perf_tbl$ppv[which.min(perf_tbl$prec_rec_dist)],
                                   perf_tbl$npv[which.min(perf_tbl$prec_rec_dist)],
                                   pr$auc.integral,
                                   pr$auc.davis.goadrich,
                                   perf_tbl$prec[which.min(perf_tbl$prec_rec_dist)],
                                   perf_tbl$rec_sens[which.min(perf_tbl$prec_rec_dist)],
                                   perf_tbl$fscore[which.min(perf_tbl$prec_rec_dist)],
                                   length(unique(valid$PATIENT_NUM))),
                        stringsAsFactors = F)
  
  perf_summ %<>%
    bind_rows(perf_tbl %>% 
                dplyr::summarize(prec=mean(prec),
                                 sens=mean(rec_sens),
                                 spec=mean(spec),
                                 ppv=mean(ppv),
                                 npv=mean(npv),
                                 acc=mean(acc),
                                 fscore=mean(fscore),
                                 mcc=mean(mcc)) %>%
                gather(overall_meas,meas_val))
  

  perf_overall<-perf_summ %>% 
    filter(overall_meas %in% c("roauc",
                               "prauc1",
                               "opt_acc",
                               "mcc",
                               "opt_sens",
                               "opt_spec",
                               "opt_fscore",
                               "opt_ppv",
                               "opt_npv")) %>%
    dplyr::mutate(overall_meas=recode(overall_meas,
                                      roauc="1. ROAUC",
                                      prauc1="2. PRAUC",
                                      mcc="3. Matthews Correlation Coefficient",
                                      opt_acc="4. Accuracy",
                                      opt_sens="5. Sensitivity",
                                      opt_spec="6. Specificity",
                                      opt_fscore="7. F-score",
                                      opt_ppv="8. Positive Predictive Value",
                                      opt_npv="9. Negative Predictive Value"))
  
  #--stack results
  perf_mat %<>% bind_rows(perf_overall %>% mutate(subgrp=grp))
  

  fs_all %<>% bind_rows(out_grp[[paste0("grp",grp)]]$var_imp %>%
                        dplyr::mutate(subgrp=grp))
    
  
  fs20_part %<>% bind_rows(out_grp[[paste0("grp",grp)]]$part_eff %>%
                             dplyr::mutate(subgrp=grp))
    
}

out_plot<-list(perf=perf_mat,
               fs_all=fs_all,
               fs20_part=fs20_part)

saveRDS(out_plot,file="./output/presel.rda")
# saveRDS(out_plot,file="./output/presel_w_lac.rda")
