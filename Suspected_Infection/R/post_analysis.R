#### survival-like prediction model ####
setwd("~/home_host/AKI_CDM")

rm(list=ls()); gc()

source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "scales",
                    "broom",
                    "Matrix",
                    "xgboost",
                    "lme4"))

##-----set up-----
params_site<-list(
  KUMC=list(site="KUMC"),
  MCW=list(site="MCW"),  #MCW - lab match on encounterid (patid not populated)
  UNMC=list(site="UNMC"),
  UTSW=list(site="UTSW"),
  MU=list(site="MU") #MU - date and time is recorded as 5-digits number (seconds), bp are all null
) 

##task parameters
#-----prediction point
pred_in_d_opt<-c(1,2)

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

#-----feature selection type
fs_type_opt<-c("no_fs","rm_scr_bun")
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '48642-3','48643-1', #eGFR
          '3097-3','44734-2','BUN_SCR')

##-----important variables variations-------
varimp<-c()
for(i in 1:length(params_site)){
  params<-params_site[[i]]
  
  for(pred_in_d in pred_in_d_opt){
  
    for(pred_task in pred_task_lst){
      
      for(fs_type in fs_type_opt){
        
        varimp %<>%
          bind_rows(readRDS(paste0("./model_ref/",params$site,"/",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))$feat_imp %>%
                      dplyr::mutate(cum_gain=cumsum(Gain),
                                    rank=rank(-Gain),
                                    fs_type=fs_type,
                                    pred_in_d=pred_in_d,
                                    pred_task=pred_task,
                                    site=params$site
                                    ))
      }
    }
  }
}
saveRDS(varimp, file="./data/validation/varimp_site.rda")


##quick scan
varimp<-readRDS("./data/validation/varimp_site.rda")
k<-100
varimp %>% 
  filter(pred_in_d==2&pred_task=="stg2up"&fs_type=="no_fs"&rank<=k) %>%
  group_by(Feature) %>%
  dplyr::mutate(common_site=round(length(unique(site))/5,2)) %>%
  ungroup %>%
  dplyr::select(Feature,rank,common_site,site) %>%
  gather(var,val,-rank,-site) %>%
  unite("site_var",c("site","var")) %>%
  spread(site_var,val) %>%
  View



##------cross-site validation--------
ext_valid<-c()
for(i in 1:length(params_site)){
  params<-params_site[[i]]
  
  for(pred_in_d in pred_in_d_opt){
    
    ext_valid %<>%
      bind_rows(readRDS(paste0("./model_ref/",params$site,"/site_perfsumm_",pred_in_d,"d.rda")) %>%
                  dplyr::mutate(fs_type=fs_type,
                                pred_in_d=pred_in_d,
                                pred_task=pred_task,
                                ref_site=params$site
                  ))
  }
}
saveRDS(ext_valid, file="./data/validation/ext_valid.rda")

##quick scan
ext_valid<-readRDS("./data/validation/ext_valid.rda")
ext_valid %>% 
  filter(grp=="Overall"&pred_in_d==2&pred_task=="stg2up") %>% 
  View


##------important feature distribution discrepancy-------
sample_p<-0.1
  
#2d, AKI2
pred_in_d<-2
pred_task<-"stg2up"
ref<-"KUMC"

#top number of variables
topk<-150

v_dist_stk<-c()
v_dist_joint_stk<-c()

for(fs_type in fs_type_opt){
  
  #important variable list based on KUMC model
  kumc_var<-readRDS(paste0("./model_ref/KUMC/",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))$feat_imp %>%
    dplyr::mutate(rank=rank(-Gain),
                  cum_Gain=cumsum(Gain))
  
  kumc_dat<-readRDS(paste0("./data/validation/KUMC/data_ds_",pred_in_d,"d.rda"))[[pred_task]]
  kumc_feat<-kumc_dat[[1]] %>%
    left_join(kumc_dat[[2]],by=c("ENCOUNTERID","dsa_y")) %>%
    semi_join(kumc_dat[[2]] %>% dplyr::select(ENCOUNTERID) %>% 
                unique %>% sample_frac(sample_p),
              by="ENCOUNTERID")
  
    for(v in c(1:52,54:topk)){
    start<-Sys.time()
    #----------------------------------------------------------------------------------------------
    #ref-q(x)
    pop_ref<-nrow(kumc_feat)
    
    q<-kumc_feat %>% 
      dplyr::filter(key==kumc_var$Feature[v]&!is.na(value)) %>%
      arrange(value) %>%
      distinct(ENCOUNTERID,dsa_y,y,.keep_all = TRUE)
    
    a<-nrow(unique(kumc_feat[,c("ENCOUNTERID","dsa_y")]))
    ay<-sum(unique(kumc_feat[,c("ENCOUNTERID","dsa_y","y")])$y)
    n<-length(q$value)
    ny<-sum(q$y)
    k<-length(unique(q$value))
    
    x_q<-c(q$value,rep(median(q$value),a-n))
    ref_kernal<-cross_kernel(x_q,x_q)
    
    #conditional kernal (y=1)
    if(ny==0){
      x_q_c<-rep(max(q$value)+1e-04,ay)
    }else{
      x_q_c<-c(q$value[q$y==1],rep(median(q$value[q$y==1]),ay-ny))
    }
    ref_kernal_c<-cross_kernel(x_q_c,x_q_c)
    
    #some set up for density distribution estimation
    if(k<=1){
      var_type<-"binary"
      brk<-data.frame(bin=1:2,
                      brk_val=c(0,1))
    }else if(k>1&k<=20){
      var_type<-"discrete"
      brk<-data.frame(bin=seq_len(k),
                      brk_val=unique(q$value)[order(unique(q$value))])
    }else{
      var_type<-"continuous"
      #use Freedman and Diaconis to estimate reference density
      brk<-bin_fd(q$value)
    }
    
    v_dist<<-c()
    for(i in 1:length(params_site)){
      comp<-data.frame(y=q$y,x=q$value,site="ref",stringsAsFactors = F) %>%
        dplyr::mutate(k=1) %>%
        full_join(brk %>% dplyr::mutate(k=1),by="k") %>%
        dplyr::select(-k)
      
      site<-params_site[[i]]$site
      site_dat<-readRDS(paste0("./data/validation/",site,"/data_ds_",pred_in_d,"d.rda"))[[pred_task]]
      site_feat<-site_dat[[1]] %>%
        left_join(site_dat[[2]],by=c("ENCOUNTERID","dsa_y")) %>%
        semi_join(site_dat[[2]] %>% dplyr::select(ENCOUNTERID) %>% 
                    unique %>% sample_frac(sample_p),
                  by="ENCOUNTERID")
      pop_site<-nrow(site_feat)
      
      p<-site_feat %>%
        dplyr::filter(key==kumc_var$Feature[v]&!is.na(value)) %>%
        arrange(value) %>%
        distinct(ENCOUNTERID,dsa_y,y,.keep_all = TRUE)
      
      b<-nrow(unique(site_feat[,c("ENCOUNTERID","dsa_y")]))
      by<-sum(unique(site_feat[,c("ENCOUNTERID","dsa_y","y")])$y)
      
      #if KUMC, get a bootstrapped sample
      if(ref==site){
        p %<>% dplyr::slice(sample(1:nrow(p),b,replace=T)) %>%
          arrange(value)
      }
      m<-length(p$value)
      my<-sum(unique(p[,c("ENCOUNTERID","dsa_y","y")])$y)
      
      #track result
      out_site<-c(kumc_var$Feature[v],var_type,site,a,ay,n,ny,b,by,m,my)
      
      if(nrow(p)>1){
        #compare missing
        miss_mt<-q %>% dplyr::select(value,y) %>% 
          dplyr::mutate(value=1) %>%
          bind_rows(kumc_feat %>% 
                      dplyr::filter(!(key==kumc_var$Feature[v]&!is.na(value))) %>%
                      anti_join(q,by=c("ENCOUNTERID","dsa")) %>%
                      dplyr::select(ENCOUNTERID,dsa_y,y) %>%
                      unique %>% dplyr::mutate(value=0) %>%
                      dplyr::select(value,y)) %>% 
          dplyr::mutate(site="ref") %>%
          bind_rows(p %>% dplyr::select(value,y) %>%
                      dplyr::mutate(value=1) %>%
                      bind_rows(site_feat %>% 
                                  dplyr::filter(!(key==kumc_var$Feature[v]&!is.na(value))) %>%
                                  anti_join(p,by=c("ENCOUNTERID","dsa")) %>%
                                  dplyr::select(ENCOUNTERID,dsa_y,y) %>%
                                  unique %>% dplyr::mutate(value=0) %>%
                                  dplyr::select(value,y)) %>%
                      dplyr::mutate(site=site))
        
        miss_mt %<>%
          dplyr::mutate(site=relevel(as.factor(site),ref="ref"))
        
        fit_na<-glm(value ~ site, data=miss_mt, family="binomial")
        fit_summ_na<-summary(fit_na) #whether missing distribution is different
        
        fit_na_c<-glm(value ~ site, data=miss_mt %>% dplyr::filter(y==1), family="binomial")
        fit_summ_na_c<-summary(fit_na_c) #y=1
        
        comp %<>%
          dplyr::mutate(site=as.character(site)) %>%
          bind_rows(data.frame(y=p$y,x=p$value,site=site,stringsAsFactors = F) %>%
                      dplyr::mutate(k=1) %>%
                      full_join(brk %>% dplyr::mutate(k=1),by="k") %>%
                      dplyr::select(-k)) 
        
        if(var_type=="continuous"){
          comp %<>% dplyr::filter(x>=brk_lb&x<brk_ub) %>%
            unite("brk_val",c("brk_lb","brk_ub"),sep=",")
        }else if(var_type=="discrete"){
          comp %<>% dplyr::filter(x==brk_val)
        }else if(var_type=="binary"){
          comp<-miss_mt %>% left_join(brk,by=c("value"="brk_val")) %>%
            dplyr::mutate(brk_val=value)
        }else{
          stop("data type is not supported for current analysis!")
        }
        
        #value mean
        comp %<>%
          dplyr::mutate(site=relevel(as.factor(site),ref="ref"))
        
        if(var_type!="binary"){
          fit_val<-glm(x ~ site, data=comp %>% dplyr::filter(!is.na(x)), family="gaussian")
          fit_summ_val<-summary(fit_val) #whether value distribution is different
          
          if(my<1){
            fit_summ_val_c<-list(coefficients=data.frame(intercept=rep(NA,2),
                                                         slope=rep(NA,2),
                                                         std_err=rep(NA,2),
                                                         pval=rep(NA,2)))
          }else{
            fit_val_c<-glm(x ~ site, data=comp %>% dplyr::filter(!is.na(x)&y==1), family="gaussian")
            fit_summ_val_c<-summary(fit_val_c) #y=1
          }
        }else{
          fit_summ_val<-fit_summ_na
          fit_summ_val_c<-fit_summ_na_c
        }
        
        #kl divergence
        p_na<-1-exp(sum(fit_summ_na$coefficients[,1]))/(1+exp(sum(fit_summ_na$coefficients[,1])))
        q_na<-1-exp(fit_summ_na$coefficients[1,1])/(1+exp(fit_summ_na$coefficients[1,1]))
        
        kl_div<-comp %>% 
          group_by(site) %>%
          dplyr::mutate(n=n()) %>%
          ungroup %>%
          group_by(site,brk_val,bin,n) %>%
          dplyr::summarize(freq=n()) %>%
          ungroup %>%
          dplyr::mutate(px=ifelse(site=="ref",freq/n*(1-q_na),freq/n*(1-p_na))) %>%
          dplyr::select(bin,px,site) %>%
          spread(site,px,fill=0) %>%
          # dplyr::filter(!(ref==0&get(site)>0)) %>%
          dplyr::mutate(ref=ifelse(ref==0,1/n,ref)) %>%
          dplyr::filter(get(site)>0) %>%
          dplyr::mutate(lr=ifelse(get(site)==0&ref==0,0,get(site)/ref)) %>%
          dplyr::mutate(prod=get(site)*log(lr))
        
        # hybrid kl divergence (with missing pattern)
        kl_div_out<-sum(kl_div$prod)+p_na*log(p_na/q_na)
        
        #kl divergence (y=1)
        p_na_c<-1-exp(sum(fit_summ_na_c$coefficients[,1]))/(1+exp(sum(fit_summ_na_c$coefficients[,1])))
        q_na_c<-1-exp(fit_summ_na_c$coefficients[1,1])/(1+exp(fit_summ_na_c$coefficients[1,1]))
        
        if(my>0){
          kl_div_c<-comp %>% 
            filter(y==1) %>%
            group_by(site) %>%
            dplyr::mutate(n=n()) %>%
            ungroup %>%
            group_by(site,brk_val,bin,n) %>%
            dplyr::summarize(freq=n()) %>%
            ungroup %>%
            dplyr::mutate(px=ifelse(site=="ref",freq/n*(1-q_na_c),freq/n*(1-p_na_c))) %>%
            dplyr::select(bin,px,site) %>%
            spread(site,px,fill=0) %>%
            # dplyr::filter(!(ref==0&get(site)>0)) %>%
            dplyr::mutate(ref=ifelse(ref==0,1/ny,ref)) %>%
            dplyr::filter(get(site)>0) %>%
            dplyr::mutate(lr=ifelse(get(site)==0&ref==0,0,get(site)/ref)) %>%
            dplyr::mutate(prod=get(site)*log(lr))
          
          # hybrid kl divergence (with missing pattern)
          kl_div_c_out<-sum(kl_div_c$prod)+p_na_c*log(p_na_c/q_na_c)
          
        }else{
          if(ny==0){
            kl_div_c_out<-0
          }else{
            kl_div_c<-comp %>% 
              filter(y==1) %>%
              group_by(site) %>%
              dplyr::mutate(n=n()) %>%
              ungroup %>%
              group_by(site,brk_val,bin,n) %>%
              dplyr::summarize(freq=n()) %>%
              ungroup %>%
              dplyr::mutate(qx=freq/n)
            
            kl_div_c_out<-1/max(kl_div_c$qx)
          }
        }
        
        #marginal mmd
        x_p<-c(p$value,rep(median(p$value),b-m))
        site_kernal<-cross_kernel(x_p,x_p)
        x_kernal<-cross_kernel(x_p,x_q)
        mmd<-sqrt(max(ref_kernal+site_kernal-2*x_kernal,0))
        
        #conditional mmd (y=1)
        if(my==0){
          x_p_c<-rep(coalesce(c(median(q$value[q$y==1])),max(q$value)+1e-04),by)
        }else if(my==1){
          x_p_c<-rep(p$value[p$y==1],by)
        }else{
          x_p_c<-c(p$value[p$y==1],rep(median(p$value[p$y==1]),by-my))
        }
        x_kernal_c<-cross_kernel(x_p_c,x_q_c)
        site_kernal_c<-cross_kernel(x_p_c,x_p_c)
        mmd_c<-sqrt(max(ref_kernal_c+site_kernal_c-2*x_kernal_c,0))
        
        #--collect results
        out_site<-c(out_site,
                    as.numeric(fit_summ_na$coefficients[2,]),
                    as.numeric(fit_summ_na_c$coefficients[2,]),
                    as.numeric(fit_summ_val$coefficients[2,]),
                    as.numeric(fit_summ_val_c$coefficients[2,]),
                    as.numeric(kl_div_out),
                    as.numeric(kl_div_c_out),
                    as.numeric(mmd),
                    as.numeric(mmd_c))
      }else{
        
        if(var_type=="continuous"){
          comp %<>% unite("brk_val",c("brk_lb","brk_ub"),sep=",")
        }else if(var_type=="discrete"){
          comp %<>% dplyr::filter(x==brk_val)
        }else if(var_type=="binary"){
          comp<-miss_mt %>% left_join(brk,by=c("value"="brk_val")) %>%
            dplyr::mutate(brk_val=value)
        }else{
          stop("data type is not supported!")
        }
        
        #kl divergence: when variable is completely missing, impute with a constant
        kl_div<-comp %>% 
          group_by(site) %>%
          dplyr::mutate(n=n()) %>%
          ungroup %>%
          group_by(site,brk_val,bin,n) %>%
          dplyr::summarize(freq=n()) %>%
          ungroup %>%
          dplyr::mutate(qx=freq/n)
        
        kl_div_out<-1/max(kl_div$qx)
        
        #kl divergence (y=1): when variable is completely missing, impute with a constant
        if(ny==0){
          kl_div_c_out<-0
        }else{
          kl_div_c<-comp %>% 
            filter(y==1) %>%
            group_by(site) %>%
            dplyr::mutate(n=n()) %>%
            ungroup %>%
            group_by(site,brk_val,bin,n) %>%
            dplyr::summarize(freq=n()) %>%
            ungroup %>%
            dplyr::mutate(qx=freq/n)
          
          kl_div_c_out<-1/max(kl_div_c$qx)
        }
        
        #mmd: when variable is completely missing, assume a simple imputation with a constant
        x_q<-rep(median(q$value),b)
        site_kernal<-cross_kernel(x_q,x_q)
        x_kernal<-cross_kernel(x_p,x_q)
        mmd<-sqrt(max(ref_kernal+site_kernal-2*x_kernal,0))
        
        #mmd(y=1): when variable is completely missing, assume a simple imputation with a constant
        x_p_c<-rep(coalesce(c(median(q$value[q$y==1])),max(q$value)+1e-04),by)
        site_kernal_c<-cross_kernel(x_p_c,x_p_c)
        x_kernal_c<-cross_kernel(x_p_c,x_q_c)
        mmd_c<-sqrt(max(ref_kernal_c+site_kernal_c-2*x_kernal_c,0))
        
        #--collect results
        out_site<-c(out_site,
                    rep(NA,16),
                    as.numeric(kl_div_out),
                    as.numeric(kl_div_c_out),
                    as.numeric(mmd),
                    as.numeric(mmd_c))
      }
      
      names(out_site)<-NULL
      v_dist<-rbind(v_dist,out_site) 
    }
    
    ##------model join distribution diparities among sites
    colnames(v_dist)<-c("var","var_type","site",
                        "ref_overall_support","ref_case_support",
                        "ref_overall_support_with","ref_case_support_with",
                        "overall_support","case_support",
                        "overall_support_with","case_support_with",
                        paste0("ind_",c("est","std","zval","pval")),
                        paste0("cond_ind_",c("est","std","zval","pval")),
                        paste0("val_",c("est","std","zval","pval")),
                        paste0("cond_val_",c("est","std","zval","pval")),
                        "kl_div",
                        "cond_kl_div",
                        "mmd",
                        "cond_mmd") 
    rownames(v_dist)<-NULL
    
    #collect marginal distribution difference
    v_dist<-as.data.frame(v_dist)
    v_dist_df<-data.frame(v_dist[,1:3] %>%
                            mutate_all(list(~as.character)),
                          v_dist[,-(1:3)] %>%
                            mutate_all(list(~as.character)) %>%
                            mutate_all(list(~as.numeric)))
    v_dist_stk %<>% bind_rows(v_dist_df)
    
    #collect joint distribution difference
    v_dist_stk2<-v_dist_stk %>%
      left_join(kumc_var,by=c("var"="Feature")) %>%
      # group_by(site) %>%
      # dplyr::mutate(Gain_adj=Gain/max(cum_Gain)) %>%
      # ungroup %>%
      dplyr::mutate(kl_div_wt=kl_div*Gain,
                    cond_kl_div_wt=cond_kl_div*Gain,
                    mmd_wt=mmd*Gain,
                    cond_mmd_wt=cond_mmd*Gain) %>%
      arrange(site,rank)
    
    v_dist_joint<-v_dist_stk2 %>%
      group_by(site) %>%
      dplyr::summarize(v_incld=max(rank,na.rm=T),
                       kl_div_joint=sum(kl_div_wt,na.rm=T),
                       kl_div_joint_c=sum(cond_kl_div_wt,na.rm=T),
                       mmd_joint=sum(mmd_wt,na.rm=T),
                       mmd_joint_c=sum(cond_mmd_wt,na.rm=T))
    
    v_dist_joint_stk %<>% bind_rows(v_dist_joint)
    
    #----------------------------------------------------------------------------------------------
    lapse<-Sys.time()-start
    cat(fs_type,"...finish compare variable",v,":",kumc_var$Feature[v],", in",lapse,units(lapse),".\n")
  }
  
  v_out<-list(v_dist=v_dist_stk,
              v_dist_joint=v_dist_joint_stk)
  
  saveRDS(v_out,file=paste0("./data/validation/",pred_task,"_",pred_in_d,"d_",fs_type,".rda"))
}




