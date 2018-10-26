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
                    "PRROC",
                    "lmPerm"
))

#model list
model<-c("glm","rf","nnet","gbm")
n_bin<-20

## collect various modeling results
perf_mat<-c()
calib_equal_bin<-c()
fs_all<-c()

for(m in 1:4){
  out_grp<-readRDS(paste0("./output/subgrp_analysis_",model[m],".rda"))
  
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
    hl<-hoslem.test(valid$pred,valid$real,g=20)
    gof<-aovp(real~pred,data=valid)
    gof<-summary(gof)
    gof_pt<-gof[[1]][["R Mean Sq"]][2]

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
                                         "calib_chi",
                                         "calib_p",
                                         "calib_pt",
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
                                     hl[["statistic"]],
                                     hl[["p.value"]],
                                     gof_pt,
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
                                 "opt_thresh",
                                 "opt_sens",
                                 "opt_spec",
                                 "opt_fscore",
                                 "opt_ppv",
                                 "opt_npv")) %>%
      dplyr::mutate(overall_meas=recode(overall_meas,
                                        roauc="1. ROAUC",
                                        prauc1="2. PRAUC",
                                        mcc="3. Matthews Correlation Coefficient",
                                        opt_thresh="4.Threshold",
                                        opt_acc="5. Accuracy",
                                        opt_sens="6. Sensitivity",
                                        opt_spec="7. Specificity",
                                        opt_fscore="8. F-score",
                                        opt_ppv="9. Positive Predictive Value",
                                        opt_npv="10. Negative Predictive Value"
                                        ))
    
    #--calibration
    calib<-data.frame(pred=valid$pred,
                      real=valid$real) %>%
      arrange(pred) %>%
      dplyr::mutate(pred_bin = cut(pred,
                                   breaks=unique(quantile(pred,0:(n_bin)/(n_bin))),
                                   include.lowest=T,
                                   labels=F)) %>%
      ungroup %>% group_by(pred_bin) %>%
      dplyr::summarize(expos=n(),
                       bin_lower=min(pred),
                       bin_upper=max(pred),
                       bin_mid=median(pred),
                       real_agg = sum(real),
                       pred_p = mean(pred),
                       pred_p_sd = sd(pred)) %>%
      dplyr::mutate(real_p=real_agg/expos) %>%
      dplyr::mutate(binCI_lower = pmax(0,pred_p-1.96*sqrt(real_p*(1-real_p)/expos)),
                    binCI_upper = pred_p+1.96*sqrt(real_p*(1-real_p)/expos))
  
    
    #--stack results
    perf_mat %<>% bind_rows(perf_overall %>% mutate(subgrp=grp,model=model[m]))
    
    calib_equal_bin %<>% bind_rows(calib %>% mutate(subgrp=grp,model=model[m]))
    
    fs_all %<>% bind_rows(out_grp[[paste0("grp",grp)]]$var_imp %>%
                            dplyr::mutate(subgrp=grp,model=model[m]))
    
    
  }
}

out_plot<-list(perf=perf_mat,
               calib=calib_equal_bin,
               fs_all=fs_all)

saveRDS(out_plot,file="./output/presel_multimodel.rda")

