#' ---
#' title: "R and Databases"
#' author: "Alex F. Bokov"
#' date: "09/23/2016"
#' ---
#+ echo=TRUE,cache=TRUE
#' Load libraries
require(RSQLite);require(xtable);
#' Go to the directory where the .db file lives on YOUR system. You may need to
#' change the path as appropriate.
setwd('/tmp/2016-Fall-TSCI-5050/');
#' Open connection
con <- dbConnect(SQLite(),'/tmp/2016-Fall-TSCI-5050/Session_03_SQL_HDF5/exampleinput.db');
#' See what tables are available in this db file
dbListTables(con)
#' Select a table
#+ results="asis"
dbGetQuery(con,'select * from patient_dimension');
print(xtable(dat),type='html');
#' The following need to be annotated by me, still -- Alex
#' 
#' Open a connection
#' View a table as a `data.frame`
dbGetQuery(con,'select * from concept_dimension');
dbGetQuery(con,'select * from observation_fact');
head(dbGetQuery(con,'select patient_num,concept_cd from observation_fact'));
head(dbGetQuery(con,'select patient_num,concept_cd from observation_fact where concept_cd = "NDC:00005306343"'));
head(dbGetQuery(con,"select patient_num,concept_cd from observation_fact where concept_cd = 'NDC:00005306343'"));
head(dbGetQuery(con,"select patient_num,concept_cd from observation_fact where concept_cd like '%NDC:00005306343'"));
#'
#'
