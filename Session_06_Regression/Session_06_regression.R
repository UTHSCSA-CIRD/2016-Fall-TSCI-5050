#' ---
#' title: "Regression Crash Course"
#' author: "Nour Abdelfattah"
#' date: "November 29, 2016"
#' ---

#' Load the usual goodies
library(magrittr);

#' Load the data
ragdata<- read.csv("regdata.csv");

#' Plot the data to see what relationships between
#' variables become apparent
data.matrix(ragdata) %>% jitter %>% as.data.frame %>% plot(pch='.',cex=3);

#' Let's say we want to characterize the dose-concentration
#' relationship for this drug (Theophiline)
#' Treating dose as a dichotomous variables (fDs) we see...
stripchart(conc~fDs, ragdata,vertical=T,method='jitter');

#' Fit the equivalent linear regression model
lm0 <-lm(conc~fDs, ragdata);
lm0;
confint(lm0);
#' Make special note of the `Coefficients` table, that's
#' the hypothesis tests.
summary(lm0);
#' You can pull out just the coefficient table from 
#' `summary(lm0)` using the `$` operator, since `summary()`
#' returns a list (invisibly, as usual in R)
summary(lm0)$coefficient;
#' With a single dichotomous predictor, a linear model
#' gives the same result a t-test
t.test(conc~fDs,ragdata);
#' ...not significant, in both cases.

