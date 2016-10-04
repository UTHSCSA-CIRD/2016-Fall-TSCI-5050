#' ---
#' title: "Data Storage and Manipulation"
#' author: "Alex F. Bokov"
#' date: "10/04/2016"
#' ---
#+ echo=TRUE,cache=TRUE
#' Load libraries
require(RSQLite);require(data.table);require(plyr);

#' Open connection (remember to change the path to match your location)
con <- dbConnect(SQLite(),'/tmp/2016-Fall-TSCI-5050/Session_04_Data_S_n_M/Session_04_Nour.db');
#' See tables
print(.tmp<-dbListTables(con));
#' Let's look at what is contained in `r .tmp[1]` and `r .tmp[2]`.
head(dbReadTable(con,'io'));
head(dbReadTable(con,'kc'));

dt <- dbReadTable(con,'kc');
#' Convert to numeric because for some reason SQLite thinks its not
dt <- sapply(dt,as.numeric);
