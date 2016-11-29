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
