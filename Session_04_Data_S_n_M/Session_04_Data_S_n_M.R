#' ---
#' title: "Data Storage and Manipulation"
#' author: "Alex F. Bokov"
#' date: "10/04/2016"
#' ---
#+ echo=TRUE,cache=TRUE
#' Load libraries
require(RSQLite);require(data.table);require(plyr);require(ggplot2);

#' Open connection (remember to change the path to match your location)
con <- dbConnect(SQLite(),'/tmp/2016-Fall-TSCI-5050/Session_04_Data_S_n_M/Session_04_Nour.db');
#' See tables
print(.tmp<-dbListTables(con));
#' Let's look at what is contained in `r .tmp[1]` and `r .tmp[2]`.
head(dbReadTable(con,'io'));
head(dbReadTable(con,'kc'));

dt <- dbReadTable(con,'kc');
#' Convert to numeric because for some reason SQLite thinks its not
dt[,] <- sapply(dt,as.numeric,simplify = F);
#' Any missing values?
is.na(dt);
#' Apparently `is.numeric()` converts blanks to missing values (`NA`).
#' Let's check:
as.numeric(c('1','2.3','4','','13.8'));
#' Fix it! It happens to be a zero in real life.
dt[11,1] <- 0;
#' Now reshape it to the long format
reshape(dt,idvar='y',v.names='x',varying=list(2:9),direction='long') %>% 
  setNames(c('Dose','Replicate','Signal')) -> dt1;
#' ...notice the use of the `%>%` pipe operator (from the `plyr` package)
head(dt1);
#' Check the data types
sapply(dt1,class);
#' Make replicate into a factor to avoid `lm()` and friends from over-interpreting it
dt1$Replicate<- as.factor(dt1$Replicate);
#' Now let's try some plots.
gp <- ggplot(data=dt1,aes(x=Dose,y=Signal));
gp + geom_line(aes(group=Replicate,colour=Replicate));
gp + geom_line(aes(x=log(Dose),group=Replicate,colour=Replicate));
gp + geom_line(aes(x=log(Dose),y=c(0,diff(Signal)),group=Replicate,colour=Replicate));
#' But we need to diff _within_ each replicate. For that we can use `data.table`
tab1 <- data.table(dt1);
#' Do a bunch of interesting transformations.
tab1[,list(Dose,
           logDose=log(Dose),
           Signal,
           DiffSignal=c(0,diff(Signal))
           ),
     by=Replicate];

#' One last thing: dataset io
dbReadTable(con,'io') %>% 
  transform(x=as.numeric(x),y=as.numeric(y),z=as.numeric(z)) -> io;
io$y_x <- with(io,y - x);
plot(y_x~z,io,pch='.',cex=2,col='#00000060');

#' Open connection (remember to change the path to match your location)
con <- dbConnect(SQLite(),'/tmp/2016-Fall-TSCI-5050/Session_04_Data_S_n_M/Session_04_Nour.db');
#' See tables
print(.tmp<-dbListTables(con));
#' Let's look at what is contained in `r .tmp[1]` and `r .tmp[2]`.
head(dbReadTable(con,'io'));
head(dbReadTable(con,'kc'));
