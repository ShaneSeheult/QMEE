# Assignment 05

## JD Just FYI: You had a space at the end of your directory name, which messes up scripts, so I removed it.

# This Assignment is meant to be run from the main repo directory

## ---- libraries --------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(performance)
library(plotrix)
library(dotwhisker)
library(randomForest)
library(emmeans)

## ---- Import and Read Data (RDS) ---------------------------------------------
dd <- readRDS("PupBirthdaysClean.rds")

## ---- Formulate a clear hypothesis about your data ---------------------------

# Note, A hypothesis for my project can be found within the following directory:
#       Assignment04/QMEE_Assignment_04.pdf
## JD: It's harder for me to open if you don't spell it right (capitalization was wrong)

# I have copied the text from this assignment and pasted it below:
#       My null hypothesis (h0) is that there is no difference between birthdays of pupsborn to wild-
# caught or captive bats. In other words, the pups are drawn from the same population. 
# My alternative hypothesis (h1) is that the difference in birthdays between the two  groups 
# is not equal to 0 and that there is an effect of being born to a mother that has spent extended
# time in captivity.

## JD: The alternative hypothesis you list has no clear semantic content beyond the null, so it's not clear what the point is. If you're just looking to see _if you see_ clear differences of any kind, you can stop with the null I guess (or say that you are looking for differences in the mean).
## If you have an actual hypothesis (one group will give birth earlier, for example), it's OK to state that.

## ---- Linear Model -----------------------------------------------------------
dd$Days <- as.numeric(dd$Switch - dd$Birthday)
# Note, this is creating a new variable called 'Days' which is a difference between day of first switch and birthday.
summary(dd)

## JD: What is first switch? Why do we want this difference?

lin.mod <- lm(as.numeric(Days) ~ as.factor(Group), data = dd)
summary(lin.mod)

## --- Diagnostic Plots (performance) ------------------------------------------
plot(check_posterior_predictions(lin.mod)) # Specific
# posterior predictions: checks for discrepancies between data and fitted models. It helps to examine if the type of model being used 
# fits the model well. This plot is simulating data using models and comparing discrepancies between real and simulated data.
# This figure suggests that the model has a relatively good fit to the data, but that the observed data is left skewed relative to the 
# models predictions. This suggests that there may be a model that better predicts the observed data.

## JD: Dude, this all looks actually crazy. How could the square root of the standardized residuals be > 1e13? Something is badly wrong here, and there's no point going further. It's important to look carefully at things like the values on axes.

## It may just be that your model is too simple for the performance tools; it's just a two-group model, right? You could try something like the default plots:

## Also, weren't there scary warnings; try not to ignore warnings.

plot(lin.mod)

## … or even just some simpler plots, like a boxplot.

plot(check_heteroscedasticity(lin.mod)) # General
# heteroscedasticity: checks for the assumption of homogeneity in variance. This plot suggests that there is not 
# equal variance as the reference line is not flat and horizontal.

# To address this, I have transformed my data using a log function:
dd.log <- (dd
           %>% mutate(Days = log(as.numeric(Days)))
)

## JD: I'm not a big fan of adding an offset and then taking a log, but in this case, where you're counting in discrete days, you could try adding 1/2 day or 1 day, taking the log, and seeing if it makes your qq plots better. 

lin.mod.log <- lm(is.finite(Days) ~ as.factor(Group), data = dd.log)
plot(check_heteroscedasticity(lin.mod.log)) # General
# This correction seems to have "flattened out" the line, addressing the lack of heteroscedasticity in the un-transformed data
# However, I had to omit data  using this approach, and do not believe this is appropriate.

# Note, The above plots are all diagnostic plots. I have not adjusted the visual aspects o these graphs (good practice to be able to
#     examine and summarize on the fly). These plots are all visualizing assumptions/errors associated with the model fit for the data and
#     evaluate assumptions on the model. Visualizing these tests is important as it aids in interpretation - here, we are looking for deviations/
#     absence of patterns in the residuals to evaluate how well the model is fitting the data. 
# Some of these diagnostics are using generic methods (e.g. residuals vs fitted model) and some are using specific methods (residuals 
#     vs predictors). I have outlined which plots are doing which.

## ---- Inferential Plot (Emmeans) ---------------------------------------------
emm.lin.mod <- emmeans(lin.mod, spec = ~ Group)
plot(pairs(emm.lin.mod)) + geom_vline(xintercept = c(0,-5,5), lty = 2) + xlim(-5,5) 

# This plot is showing the confidence interval of the estimate for the contrast between the Group variable (i.e. 
# Captive and Wild-Caught bats). The results of this inferential plot suggest that the effect of Group is not statistically
# different than 0 (as the confidence interval — the blue shading — crosses over 0). The figure also suggests that the effect
# of group is relatively small, given the span of the confidence interval of the estimate. 

# I have set equivalence bounds to be +/- 5 days (as noted by the dotted vertical lines). These equivalence bounds are borne
# from the research question and were chosen arbitrarily. Since the confidence intervals do not cross either the positive 
# or negative equivalence bounds, we can conclude that the estimate is statistically equivalent to 0.

## Grade 1.9/3
