#### extract Sepsis cohort ####
rm(list=ls())
gc()

setwd("~/proj_sepsis/Clinical_Actions_KD/Suspected_Infection")

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

##--extract SI cohort by executing the following sql snippets in order
statements<-paste0(
  "./inst/",
  c("ED_betwn",
    "ED_18up",
    "ED_eligb",
    "ED_Culture",
    "ED_Antibio",
    "ED_SI",
    "SI_ServDep",
    "SI_case_ctrl"),
  ".sql"
)
#--excecute single snippet
sql<-parse_sql(statements[8],
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema,
               start_date=start_date,
               end_date=end_date)

execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))

#--batch execution
# execute_batch_sql(conn,statements,verb=T,
#                   db_link=NULL,
#                   i2b2_db_schema=config_file$i2b2_db_schema,
#                   start_date=start_date,
#                   end_date=end_date)

##---get consort table
sql<-parse_sql(paste0("./inst/consort_diagram.sql"))
consort<-execute_single_sql(conn,
                            statement=sql$statement,
                            write=(sql$action=="write"),
                            table_name=toupper(sql$tbl_out))

##---collect patient level info
sql<-parse_sql(paste0("./inst/collect_pat_fact.sql"),
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema)

execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))

##---collect clinical facts at encounter
sql<-parse_sql(paste0("./inst/collect_fact_at_enc.sql"),
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema)

execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))

##---collect clinical facts before encounter
sql<-parse_sql(paste0("./inst/collect_fact_bef_enc.sql"),
               db_link=NULL,
               i2b2_db_schema=config_file$i2b2_db_schema,
               start_date=start_date)

execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))

