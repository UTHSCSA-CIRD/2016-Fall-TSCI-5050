#' ---
#' title: "R and Databases"
#' author: "Alex F. Bokov"
#' date: "09/23/2016"
#' ---
#+ echo=TRUE,cache=TRUE
#' Load libraries
require(RSQLite);require(xtable);
#' Go to the directory where the .db file lives on YOUR system
setwd('/tmp/2016-Fall-TSCI-5050/');
#' Open connection
con <- dbConnect(SQLite(),'/tmp/2016-Fall-TSCI-5050/Session_03_SQL_HDF5/exampleinput.db');
#' Select a table
#+ results="asis"
dat <- dbGetQuery(con,'select * from patient_dimension');
print(xtable(dat),type='html');
