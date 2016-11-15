#' ---
#' title: "96 Well Plates in R"
#' author: "Alex F. Bokov"
#' date: "October 18, 2016"
#' ---

#' Load some libraries we'll be using for this
require('platetools');  
require('xlsx');         # also reads xlsm and xls
require('magrittr');     # provides the %>% operator
require('nlme');         # mixed-effect models
require('ggplot2');

#' Declare session variables
pmfile <- 'MI00200 MISSION microRNA Mimic v.17 Plate Map.xls';
#' Unless you run Linux, always check out the git repo to the /tmp
#' directory where you push and delete it after working, you will 
#' likely want to modify these paths if nothing else
path <- '/tmp/2016-Fall-TSCI-5050/'
datapath <- paste0(path,'Session_05_Plates/screen/');
setwd(path);

#' Create  the well labels
wells <- outer(LETTERS[1:8],t(1:12),paste0)[,1,]

#' Read the plate map file
read.xlsx(paste0(datapath,pmfile),sheetIndex = 1) %>% 
  subset(Symbol!='EMPTY') %>% 
  transform(ID=gsub('[^0-9]','',Plate.ID)) -> map;
levels(map$Plate.Location) <- gsub('0([0-9])$','\\1',levels(map$Plate.Location))

#' Function for converting from 8x12 layout to the 96x2 (or 384x2) one
unplate <- function(dat,plate=96) {
  nrow<-8; ncol<-12;
  if(plate==384) {nrow<-2*nrow; ncol<-2*ncol;}
  data.frame(well=num_to_well(1:plate),value=c(t(dat[1:nrow,1:ncol])))
}

#' We read the platest into a nested list-- top level are
#' files, second level are replicates of the same plate,
#' first 3 are controls, second three are vincristin
(list.files(pattern = 'series.xlsm',path = datapath,full.names=T)->filenames) %>% 
  # Outer pipeline run on the who set of plates
  lapply(
    function(xx) {
      # Inner pipeline run on each plate
      1:6 %>% 
        lapply(function(yy) read.xlsx(xx,yy)) %>%
        setNames(rep(paste0(c('c','c','c','v','v','v'),1:6)));
      # End inner pipeline 
    }) %>% 
  setNames(paste0('pl',gsub('.*//([0-9]{1,2}) series.xlsm$','\\1',filenames))) -> dat;

do.call('c',dat) %>% sapply(unplate,simplify=F) -> dat2;

#' Combine everything into one big dataframe preserving
#' replicate, plate, and treatment info
sapply(names(dat2),function(xx) {
  dat2[[xx]]$repid<-xx;dat2[[xx]]
  },simplify=F) %>% do.call('rbind',.) -> dat3;

dat3$ID <- gsub('^pl([0-9]{1,2}).*','\\1',dat3$repid);
dat3$treat <- gsub('[^vc]','',dat3$repid);
dat3$Plate.Location <- dat3$well;
levels(dat3$Plate.Location) <- gsub('^([A-H])0([0-9])$','\\1\\2',levels(dat3$well));
dat3$col<-gsub('[^0-9]','',dat3$Plate.Location);
dat3$row<-gsub('[0-9]','',dat3$Plate.Location);
dat3$nwell <- rep(1:96,length(unique(dat3$repid)));

merge(dat3,transform(map[,c(1:9,13)],Plate.ID=as.character(Plate.ID)),
      by = c('Plate.Location','ID'),all.x=T) %>% 
  transform(Symbol=as.character(Symbol)) -> dat4;

dat4[dat4$well %in% c('A12','B12'),'Symbol'] <- 'siPLK';

dat4[grepl('^[A-H]1$',dat4$Plate.Location),'Symbol'] <- 'Mock';

dat4[is.na(dat4$Symbol),'Symbol']<- 'Control';

split(dat4,dat4$repid) %>% sapply(function(xx) {
  xx$c1sd <- sd(subset(xx,col==1)$value); 
  xx$c1ctr <- rep(subset(xx,col==1)$value,each=12);
  xx$c1med <- median(subset(xx,col==1)$value);
  xx},simplify=F) %>% do.call('rbind',.) -> dat5;

#' Get a matrix of plate-wise standard deviations
qc.scale<-sapply(dat,function(xx) 
  sapply(xx,function(yy) 
    sd(apply(yy[,1:11],1,function(zz) zz[-1]-zz[1]))));

#' Ditto for means
qc<-sapply(dat,function(xx) 
  sapply(xx,function(yy) 
    mean(apply(yy[,1:11],1,function(zz) zz[-1]-zz[1]))));

#' Mixed effects as alternative to manual normalization. The idea is you fit an
#' `lme` model and then treat the _residuals_ as your response variable without 
#' further normalization
fit <- lme(value~treat,dat6<-subset(dat5,!Symbol%in%c('Mock','siPLK','Control')),
           random=~1|ID/repid/col);
dat7 <- groupedData(value~treat|ID,dat6);
fit0 <- lme(value~treat,dat7,random=pdBlocked(list(pdIdent(~1),pdIdent(~row-1),pdIdent(~col-1))));
summary(fit);
plot(fit,level=3);
dat6$norm<-residuals(fit,level=3,type = 'pearson');
#ggplot(dat6,aes(x=Symbol,y=norm,col=treat))+geom_boxplot();

#' Long story, will explain later, but this is kind of interesting
#ggplot(transform(subset(dat5,ID!='1'&Symbol!='siPLK'),row=as.numeric(factor(row)),col=as.numeric(col),ID=as.numeric(ID),value=(value-c1med)/c1sd),aes(x=nwell,y=value,color=treat,group=repid))+geom_line()+facet_wrap(~ID);

samplePlot <- function(data,val='norm',col='treat',pch='.',...){
  plot(data[[val]],col=factor(data[[col]]),pch=pch,...);
}

#' ## New topic! Regression (for really weird reasons)
#' 
#' Read in new data.
zdat <- read.csv(paste0(datapath,'ratio and z score.csv'));
#' This contains physical plates as columns, plate-IDs and 
#' wells as rows. Also well-wise averages for control and
#' vincristine replicate-sets, called `avg` and `avg.1`.
#' Let's calculate the product and ratio of these for each
#' miRNA and name those columns `VC` and `V_C`, respectively.
#' Yes, sounds crazy, just go with it.
zdat$VC <- with(zdat,avg.1*avg);
zdat$V_C <- with(zdat,avg.1/avg);
#' Now let's fit a linear regression model to them
VClm <- lm(VC~0+V_C,zdat);
#' Now let's extract the residuals from this model and add
#' them to our dataset as a new variable, `tox`
zdat$tox <- resid(VClm);
#' Now lets rank them, again new column
zdat$ranktox <- order(zdat$tox,decreasing = F);
#' Prediction-- if you plot your data as you usually
#' do, and then color in the points in order of increasing 
#' toxicity (i.e. from 1 to 962) you will see points getting
#' filled in from the lower-left quadrant first.
