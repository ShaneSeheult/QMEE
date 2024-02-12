# Assignment 05

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
summary(dd)

## ---- Formulate a clear hypothesis about your data ---------------------------

# Note, A hypothesis for my project can be found within the following directory:
#       ShaneSeheult/QMEE/Assignment04/QMEE_assignment_04.pdf

# I have copied the text from this assignment and pasted it below:
#       My null hypothesis (h0) is that there is no difference between birthdays of pupsborn to wild-
# caught or captive bats. In other words, the pups are drawn from the same population. 
# My alternative hypothesis (h1) is that the difference in birthdays between the two  groups 
# is not equal to 0 and that there is an effect of being born to a mother that has spent extended
# time in captivity.

## ---- Linear Model -----------------------------------------------------------
dd$Days <- dd$Switch - dd$Birthday
# Note, this is creating a new variable called 'Days' which is a difference between day of first switch and birthday.

Days.df <- (dd 
            %>% group_by(Group)
            %>% summarise(Mean.Days = mean(Days, na.rm = TRUE), Std.Err.Days = std.error(Birthday, na.rm = TRUE))
)

lin.mod <- lm(as.numeric(Days) ~ as.factor(Group), data = dd)
summary(lin.mod)

plot(check_posterior_predictions(lin.mod)) # Specific
plot(check_heteroscedasticity(lin.mod)) # General
plot(check_distribution(lin.mod)) # Specific / General
plot(check_normality(lin.mod)) # Specific

# Note, The above plots are all diagnostic plots. I have not adjusted the visual aspects o these graphs (good practice to be able to
#     examine and summarize on the fly). These plots are all visualizing assumptions/errors associated with the model fit for the data and
#     evaluate assumptions on the model. Visualizing these tests is important as it aids in interpretation - here, we are looking for deviations/
#     absence of patterns in the residuals to evaluate how well the model is fitting the data. 
# Some of these diagnostics are using generic methods (e.g. residuals vs fitted model) and some are using specific methods (residuals 
#     vs predictors). I have outlined which plots are doing which.

# posterior predictions: checks for discrepancies between data and fitted models. It helps to examine if the type of model being used 
#     fits the model well
# heteroscedasticity: checks for the assumption of equal variance. This plot suggests that there is not equal variance as the reference
#     line is not flat and horizontal
# normality: this checks whether the residuals of the regression model are normally distributed 

emm.lin.mod <- emmeans(lin.mod, spec = ~ Group)
plot(pairs(emm.lin.mod)) + geom_vline(xintercept = c(0,-5,5), lty = 2) + xlim(-5,5) 

# This plot is showing the confidence interval of the estimate for the contrast between the Group variable (i.e. 
# Captive and Wild-Caught bats). The results of this inferential plot suggest that the effect of Group is not statistically
# different than 0 (as the confidence interval — the blue shading — crosses over 0). The figure also suggests that the effect
# of group is relatively small, given the span of the confidence interval of the estimate. 

# I have set equivalence bounds to be +/- 5 days (as noted by the dotted vertical lines). These equivalence bounds are borne
# from the research question and were chosen arbitrarily. Since the confidence intervals do not cross either the positive 
# or negative equivalence bounds, we can conclude that the estimate is statistically equivalent to 0.
