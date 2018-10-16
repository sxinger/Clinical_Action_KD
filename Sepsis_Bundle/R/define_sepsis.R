#### Define Sepsis cases and controls ####

define_sepsis<-function(conn,
                        db_link,
                        db_name,
                        db_schema,
                        within_d=2,
                        remote_db=F,
                        verb=T){

  #check if cdm_db_server has been specified when remote_db=T
  if(remote_CDM & is.null(db_link)){
    warning("must specify the db_link for CDM when remote_db=T!")
  }
  
  #execute the following sql snippets according to the specified order
  statements<-paste0(
    "./inst/",
    c("SEP_SIRS",   #stage the onsets of SIRS
      "SEP_AMS",    #stage the onset of altered mental status
      "SEP_BP",   
      "SEP_HYPOT",  #stage the onset of hypotension
      "SEP_BLR_AT", 
      "SEP_BLR_BEF",
      "SEP_BLR_BL",
      "SEP_LF",     #stage the onset of liver failure
      "SEP_SCR_AT", 
      "SEP_SCR_BEF",
      "SEP_SCR_BL",
      "SEP_KF",     #stage the onset of kidney failure
      "SEP_PLT",
      "SEP_INR",
      "SEP_PTT",
      "SEP_DDIMER",
      "SEP_AC",     #stage the onset of abnormal coagulation
      "SEP_SPO2",
      "SEP_PAO2",
      "SEP_PAFIO2",
      "SEP_HYPOX",  #stage the onset of hypoxemia
      "SEP_LACT",  
      "SEP_IL",     #stage onset of increase lactate
      "SEP_OD"),
    ".sql"
  )
  
  execute_batch_sql(conn,statements,verb,
                    db_link=db_link,
                    db_name=db_name,
                    db_schema=db_schema,
                    within_d=within_d)
  
  #query back final cohort
  tbl1_nm<-"SEP_OD"
  sep_od<-dbGetQuery(conn,
                     paste("select * from",tbl1_nm))
  
  #clean out intermediate tables
  for(i in 1:(length(statements)-1)){
    parse_out<-parse_sql(statements[i])
    if(parse_out$action=="write"){
      drop_temp<-paste("drop table",parse_out$tbl_out,"purge")
      dbSendQuery(conn,drop_temp)
    }else{
      warning("no temporary table was created by this statment!")
    }
    if(verb){
      cat("temp table",toupper(parse_out$tbl_out),"dropped. \n")
    }
  }
  
  return(sep_od)
}

