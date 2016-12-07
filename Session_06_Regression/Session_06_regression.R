#' ---
#' title: "Regression Crash Course"
#' author: "Nour Abdelfattah"
#' date: "November 29, 2016"
#' ---
.workdir <- 'Session_06_Regression';
if(basename(getwd())!=.workdir){
  if(.workdir %in% list.files('.')) setwd(.workdir) else
    if(.workdir %in% list.files('..')) setwd(file.path('..',.workdir)) else
      setwd(file.path('..','..',.workdir))
}

#' Load the usual goodies
library(magrittr); library(ggplot2); library(nlme);

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
#' How about if we add `fWt` (low/high) as a predictor...
#' We can use the `update` function to avoid retyping the whole model
lm1 <- update(lm0,.~.+fWt);
summary(lm1);
#' Still insignificant.
#' How about if weight is the only predictor?
lm2 <- update(lm0,.~fWt);
#' Oh, by the way, there is a `Time` variable in the data.
#' Time has an annoying way of influencing everything. To
#' see if time might be a significant effect on the model,
#' try plotting its residuals against the candidate variable.
#' 
#' Notice the `plot(x,y)` syntax here instead of the `plot(y~x,data)` syntax
plot(ragdata$Time,residuals(lm0));
#' Yes, time seems to have a big effect. This is what an unimportant
#' variable would have looked like, in contrast:
plot(rnorm(nrow(ragdata)),residuals(lm0));
#' Let's try using Time as the main predictor of `conc`
lm3 <- lm(conc~Time,ragdata, subset = Time>3);
summary(lm3);
#' Now we do see a good fit on the model! Notice how the p-value
#' at the bottom is, for the first time, significant. This is 
#' goodness-of-fit test and what it's telling you here is that
#' the model fits significantly better than an "intercept-only" model
#' , i.e. `conc~1`... the most uninformative model possible.
#'
#' Does including dose further improve model fit? Let's try
#' the numeric version of the variable, `Dose`
lm4 <- update(lm3,.~.+Dose);
summary(lm4);
#' Even better.
#' Now let's compare the fit to `lm3`
anova(lm3,lm4);
#' Significantly better than `lm3`, so we keep lm4
#' What `lm4` tells us is that per unit of time, `conc`
#' goes down by -0.29 and this slope is significantly different
#' from 0. Also, per unit of dose, the curve is shifted up by
#' 0.77 and that too is significant. But how to we know that
#' `Dose` only shifts the intercept? What if the `Time` slope
#' is also dependent on `Dose`? For that we need to consider 
#' an interaction term between `Time` and `Dose`. Just add `Time*Dose` 
#' to the right side of the formula and it will expand out into
#' `Time + Dose + Time:Dose`.
lm5 <- update(lm3,.~Time*Dose);
summary(lm5);
#' Does it fit better?
anova(lm4,lm5);
#' Yes.
#' 
#' Body weight is also available as `Wt` (we're abandoning the 
#' `fWt` and `fDs` variables because they were only there to make the point that
#' discretizing numeric variables can make your model less sensitive).
#' But what about interactions? Do we _really_ need `Time*Dose*Wt` and the 
#' insane `Time + Dose + Wt + Time:Dose + Dose:Wt + Time:Wt + Time:Dose:Wt`
#' formula it will expand into? It can get tedious doing a separate F-test
#' (what it is we're really doing when we call the `anova()` command) for each 
#' addition or deletion of a variable. Furthermore, the F-test is not always valid.
#' So instead we use the `step()` function:
lm6<-step(lm5,scope=list(lower=.~1,upper=.~(.)*Wt));
#' Let's do an instant replay...
lm6<-step(
  # whee... we just called the mighty step function
  lm5,
  # ...upon the lm5 fitted model object
  scope = 
    # and we use the scope optional parameter
    list(
      # which takes a list with two values:
      lower=.~1,
      # lower, which represents the simplest model we 
      # are willing to consider (we use the null model 
      # described above, but we could use something else... 
      # for example force it to keep the variable 
      # representing the main intervention whether or not 
      # it contributes to model fit)
      # Note that we are using the update syntax, where 
      # '.' represents "what's already there"
      upper=.~(.)*Wt
      # And the second value, named 'upper' represents the
      # most complex model we are willing to consider. 
      # Reminder: the '~(.)' expands to '~ Time + Dose + Time:Dose'
      # and then with '*Wt' we add every possible interaction of
      # those with the 'Wt' variable. But we won't necessarily end
      # up keeping them all, this is just the upper bound on model
      # scope.
      ) # Done with scope argument
  ); # Done with expression.
#' So here is what `lm6` tells us:
summary(lm6);
#' It explains about 79% of the variation, and is significantly
#' better than a null model (it better be, since it's by definition
#' at least as good as `lm5` which is better than `lm4` which is better
#' than a null model).
#' 
#' Also it tells us that the slope of `conc` over `Time`
#' depends on `Dose` (falls faster with larger `Dose`), 
#' and that the slope of `conc` over `Wt`
#' also depends on `Dose` (rises slower with larger `Dose`)
#' The 'main effects' -- `Time`, `Dose`, and `Wt`... are 
#' less trustworthy... each is estimated while holding the
#' values of the others at 0. A dose of 0 is not represented 
#' in the data, and a weight of 0 is not even possible.
#' 
#' Let's center the numeric values (other than `Time`, where a
#' 0 is possible, though we are excluding it.)
summary(update(lm6,data=transform(ragdata,Dose=Dose-3.1,Wt=Wt-median(Wt))));
#' Thank you for participating. For the next (and final?) session I
#' _hope_ to cover:
#' 
#' * Using fitted models to predict new data.
#' * Plotting the predictions of fitted models.
#' * Taking into account that we have multiple individuals being measured over time, not just one, meaning that they might have random individual variation in slope and intercept.
#' * Having discrete and continuous variables in the same model.
#' * Maybe: transforming `conc` to make the data behave like cell counts in the context of multi-drug synergy and then trying either `lme()` with a function of `conc` or `nlme()`

#' # Session 7!
#' Make a durable version of our increasingly transformed `data.frame`
transform(ragdata,Dose = Dose - 3.1,Wt = Wt - median(Wt)) %>% 
  subset(Time > 3) -> ragdata0;
ragdata1 <- ragdata0[order(ragdata0$Time),];

#' # How to do confidence intervals on a fitted model
cbind(confint(lm6),summary(lm6)$coef[,1])[,c(1,3,2)];
#' Breaking it down:
 
# bind the columns these two functions give you into one convenient matrix
cbind(
  # this produces the lower and upper confidence bounds
  # giving 90% confidence bounds to demonstrate how to change from the
  # default 95%
  confint(lm6,level = 0.9),
  # the summary of lm6 returns an object like everything in R almost
  # which has a subobject called coefficients, which we can extract in
  # the same command... and that object is a matrix, whose first column
  # are the effect estimates from our model, so we extract that with [,1]
  summary(lm6)$coef[,1]
  # but the resulting 3-column matrix might be less convenient than
  # lower/estimate/upper form, so we rearrange it with [,c(1,3,2)]
)[,c(1,3,2)] -> lm6confint;

#' If we transformed the response variable, e.g. with `log()` (which we 
#' didn't, but for illustrative purposes.. ) we can back-transform this 
#' entire matrix back to linear scale with `exp()`

exp(lm6confint);

#' TODO: insert exploration up to this point
#' "OKay, okay, but what if your data is non-linear?"
#' Switching gears, let's load the 'Inucyte' dataset.
indat <- read.csv('Incucyte.csv');
#' Get the first-differences
indat0 <- data.frame(Elapsed=indat$Elapsed[-1],sapply(indat[,-1],diff));

#' Now let's make long versions of both these.
#' Notice we did first-differences (first derivatives) of these _first_
#' because it's easier while the samples are still in separate columns
lapply(names(indat)[-1],
       function(ii) indat[,c('Elapsed',ii)] %>% 
         # notice the setNames() call-- causes all the individual data.frames
         # being combined to have the same column names so that rbind() does 
         # not fail
         cbind(ID=ii) %>% setNames(c('t','count','id'))) %>% 
  do.call(rbind,.) -> lindat;

#' Repeat the above for the first-differenced version
lapply(names(indat0)[-1],
       function(ii) indat0[,c('Elapsed',ii)] %>% 
         cbind(ID=ii) %>% setNames(c('t','count','id'))) %>% 
  do.call(rbind,.) -> lindat0;

#' Create the treatment group variable by taking the prefix of each sample ID
lindat$group<-factor(gsub('_[GH][0-9]{1,2}','',lindat$id));
lindat0$group<-factor(gsub('_[GH][0-9]{1,2}','',lindat0$id));

#' Prediction: the first-difference version will be better suited to a linear
#' model.
inplot <- ggplot(lindat,aes(x=t,y=count,group=id,col=group))+geom_line()+geom_point();
inplot0 <- ggplot(lindat0,aes(x=t,y=count,group=id,col=group))+geom_line()+geom_point();
inplot;
inplot0;
#' Now let's try restricting them to the 21 - 71 hour time window.
#' Raw values
inplot + scale_x_continuous(limits = c(21,71));
#' First differences (rate of change)
inplot0 + scale_x_continuous(limits = c(21,71));
#' So, you could fit a linear model mixed effect model on the first-differenced 
#' data like this:
in0lm <- lme(count~t+group,lindat0,random=~1|id);
#' Or you could fit a non-linear model, e.g. logistic like this:
#' First, get starting values
ininit<-getInitial(count~SSlogis(t,Asym,xmid,sdal),data=lindat);
innlm <- nlme(
  # model the response variable count as a logistic function of
  # parameters t (time, the primary covariate) and whatever
  # values best fit for Asym, xmid, and scal(e)
  count~SSlogis(t,Asym,xmid,scal),
  # from our lindat data.frame
  lindat,
  # Asym, xmid, and scal all are potentiall influenced by group
  # keeping in mind that group is really treatment
  fixed=Asym+xmid+scal~group,
  # ...not to be confused with the grouping variable
  # which for this data is each individual replicate, i.e.
  # well,represented by the id column
  groups=~id,
  # we provide the starting values, but we have to
  # repeat them four times, because there are four treatment
  # groups (scram,sirna1,sirna2,umrna)
  start=rep(ininit,4),
  # the below simply says that sample-to-sample
  # variation (i.e. random effects) in Asym, xmid, and scal
  # are independent of each other.
  random=pdDiag(Asym+xmid+scal~1)
  );
#' How do they look?
#' 
#' The mixed-effect linear residuals...
plot(in0lm);
#' The mixed-effect nonlinear residuals...
plot(innlm);
#' In short, they look terrible.
#' But we're keeping in all that noise in the beginning.
#' Let's try limiting the data to only after the 20th hour
in0lm1 <- update(in0lm,subset=t>20);
innlm1 <- update(innlm,subset=t>20);
#' Let's try again...
#' 
#' Linear on growth rate
plot(in0lm1);
#' Non-linear on raw count
plot(innlm1);
#' Better, but weird spike in the 45-55 range of predicted 
#' values. Why is that? It's because the `urna` group
#' does not obey the logistic model like the other 
#' treatments do. You can tell by coloring the dots
#' on a plot of the original data:
plot(count~t,getData(innlm1));
#' Note the use of the `getData()` command. Instead of
#' trying to recreate and properly align the original data
#' with the subset used in the `innlm1` model, it extracts
#' the actual data-set that the model has been fitted to and
#' is retained within the model object.
#' 
#' Now let's color the dots for which the predicted values 
#' are in the 45-55 range
points(
  # same formula-style first argument for plot as before
  count~t,
  # same accessing of the data except now we subset it
  getData(innlm1)[
    # which tells us which predicted values return true for 
    # the expression in its argument
    which(
      # the expression is:
      # the absolute value of
      abs(
        # the difference between 50 and the predicted
        # value according to the innlm1 model...
        50 - predict(innlm1)
        # ...must be less than 5
        # in other words 45 < X < 55
        )<5
      # close off the parentheses and brackets for the
      # data argument to the points() function
      ),],
  # specify a color to distinguish these points from the rest
  col='green');
#' So, you could spend your time learning how to write a modified
#' `SSlogis()` function. Or you could ask yourself, does it matter?
#' I would aruge that the imperfect fit of the `urna` group does not
#' alter your _main_ hypothesis: that it outperforms the 
#' siRNA knock-downs. Forcing a logistic model to fit to this actually
#' biases the results to make the effect size seem smaller and your micro
#' RNA _still_ outperforms.

#' Here are the hypothesis tests for the nonlinear model.
#' (in a regular `lm()` this would be `summary(foo)$coef` 
#' but this is nlme/lme):
summary(innlm1)$tTest
#' The heck does this mean? To be continued...
