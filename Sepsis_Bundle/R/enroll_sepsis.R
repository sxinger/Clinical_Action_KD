#### extract Sepsis cohort ####
rm(list=ls())
gc()

setwd("~/proj_sepsis/Clinical_Actions_KD/Sepsis_Bundle")

##---load libraries
source("./R/util.R")
#load libraries
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "knitr"))

##---establish connections
config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db("Oracle","OCI",config_file)

##---set parameters
start_date<-"2007-01-01"
end_date<-"2018-12-31"
within_d<-2

##------------ identify sepsis progress and treatment pathways by executing the following sql snippets in order --------------
pressor_cd<-read.csv("./src/pressor_cd.csv",stringsAsFactors = F,na.strings = c(""," "))
dbWriteTable(conn,"PRESSOR_CD",pressor_cd,temporary=T,overwrite=T)

statements<-paste0(
  "./inst/",
  c("ED_SI_Temp",
    "ED_SI_HR",
    "ED_SI_RR",
    "ED_SI_WBC",
    "ED_SI_SIRS", #critical event
    "ED_SI_GCS",
    "ED_SI_LOC",
    "ED_SI_BP",
    "ED_SI_BILRB",
    "ED_SI_SCR",
    "ED_SI_DDIMER",
    "ED_SI_INR",
    "ED_SI_PTT",
    "ED_SI_PLATLT",
    "ED_SI_SPO2",
    "ED_SI_PAO2",
    "ED_SI_PAFIO2",
    "ED_SI_LACTATE",
    "ED_SI_OD",   #critical event
    "ED_SI_3HR_screen",
    "ED_SI_3HR_ABX",
    "ED_SI_3HR_IV", 
    "ED_SI_6HR_screen",
    "ED_SI_6HR_PRESSOR", #need to access clarity for infusion rates
    "ED_SI_6HR_CARDIAC"),
  ".sql"
)

#--excecute single snippet
sql<-parse_sql(statements[25],
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema,
               start_date=start_date,
               end_date=end_date,
               within_d=within_d)

execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))

#--batch execution
execute_batch_sql(conn,statements,verb=T,
                  db_link=NULL,
                  i2b2_db_schema=config_file$i2b2_db_schema,
                  start_date=start_date,
                  end_date=end_date,
                  within_d=within_d)

##--------------- vasopressor details from clarity ---------------------------------------------------
config_file_path<-"./config_nh.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db("Oracle","OCI",config_file)

dbWriteTable(conn,"PRESSOR_CD",pressor_cd,temporary=T,overwrite=T)
sql<-parse_sql("./inst/ED_SI_6HR_PRESSOR_NH.sql",
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema,
               start_date=start_date,
               end_date=end_date,
               within_d=within_d)
execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))

config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db("Oracle","OCI",config_file)
sql<-parse_sql("./inst/ED_SI_6HR_PRESSOR_BH.sql",
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema)
execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))


##----------------- Stage sepsis and extract the final --------------------
sql<-parse_sql("./inst/ED_SI_SEPSIS_STAGE.sql",
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema,
               start_date=start_date,
               end_date=end_date,
               within_d=within_d)
execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))

##----------------- get consort table ------------------------------
sql<-parse_sql(paste0("./inst/consort_diagram.sql"))
consort<-execute_single_sql(conn,
                            statement=sql$statement,
                            write=(sql$action=="write"),
                            table_name=toupper(sql$tbl_out))

write.csv(consort,file="./result/consort_diagram.csv",row.names = F)

