#### 3-Hour Bundle ####

define_trt3hr<-function(conn,
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
  statements<-paste0("./inst/",
    c("cohort_initial",
      "cohort_all_SCr",
      "cohort_enc_SCr",
      "cohort_baseline_SCr",
      "cohort_exclude",
      "cohort_eligib",
      "cohort_AKI_staging",
      "cohort_final"),
    ".sql"
    )
  
  execute_batch_sql(conn,statements,verb,
                    db_link=db_link,
                    db_name=db_name,
                    db_schema=db_schema,
                    within_d=within_d)
  
  #collect attrition info
  attrition<-dbGetQuery(conn,
                        parse_sql(paste0("./inst/",DBMS_type,
                                         "/consort_diagram.sql"))$statement)
  
  #query back final cohort
  tbl1_nm<-"AKI_onsets"
  if(DBMS_type=="tSQL"){
    tbl1_nm<-paste0("#",tbl1_nm) #special syntax for tSQL
  }
  aki_enc<-dbGetQuery(conn,
                      paste("select * from",tbl1_nm))
  
  #clean out intermediate tables
  for(i in 1:(length(statements)-1)){
    parse_out<-parse_sql(statements[i])
    if(parse_out$action=="write"){
      if(DBMS_type=="Oracle"){
        drop_temp<-paste("drop table",parse_out$tbl_out,"purge") # purge is only required in Oracle for completely destroying temporary tables
      }else{
        drop_temp<-paste("drop table",parse_out$tbl_out)
      }
      dbSendQuery(conn,drop_temp)
    }else{
      warning("no temporary table was created by this statment!")
    }
    if(verb){
      cat("temp table",toupper(parse_out$tbl_out),"dropped. \n")
    }
  }
  
  #output
  out<-list(aki_enc=aki_enc,
            attrition=attrition)
  
  return(out)
}

