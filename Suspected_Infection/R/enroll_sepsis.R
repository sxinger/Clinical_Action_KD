#### extract Sepsis cohort ####

enroll_sepsis<-function(conn,
                        db_link,
                        db_name,
                        db_schema,
                        start_date="2007-01-01",
                        end_date="2018-12-31",
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
    c("ED_betwn",
      "ED_18up",
      "ED_eligb",
      "ED_Culture",
      "ED_Antibio",
      "ED_SI",
      "ED_Temp",
      "ED_HR",
      "ED_RR",
      "ED_WBC",
      "ED_AnySIRS",
      "SEP_INITIAL"),
    ".sql"
  )
  
  execute_batch_sql(conn,statements,verb,
                    db_link=db_link,
                    db_name=db_name,
                    db_schema=db_schema,
                    start_date=start_date,
                    end_date=end_date,
                    within_d=within_d)
  
  #collect attrition info
  attrition<-dbGetQuery(conn,
                        parse_sql(paste0("./inst/consort_diagram.sql"))$statement)
  
  #query back final cohort
  tbl1_nm<-"SEP_INITIAL"
  sep_init<-dbGetQuery(conn,
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
  
  #output
  out<-list(sep_init=sep_init,
            attrition=attrition)
  
  return(out)
}

