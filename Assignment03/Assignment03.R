# Author: Shane Seheult
# Script Name: Assignment03.R

# This Script is meant to be run from the directory ShaneSeheult/QMEE/Assignment03/Assignment03.R

# ---- libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(performance)
library(plotrix)
library(dotwhisker)
#library(qqplotr)
# ---- Import & Read Data (RDS) ------------------------------------------------
dd <- readRDS("PupBirthdaysClean.rds") 
summary(dd)

## ---- Figure Theme Object ----------------------------------------------------
theme.obj <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.4)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
# This code can be added to plots for visual aesthetic. I have added this theme object to all applicable ggplot
#     figures as I believe it ads some visual clarity.
# Another option would be to use theme_bw - which is the classic dark-on-light ggplot2 theme. I have opted out of using
#     theme_bw as I like (and am familiar with) the control I have by creating  my own theme and being able to set the parameters
#     for things like text size and line width. Note, my theme object also presents dark-on-white figures.

## ---- Figure - Mean +/- Std.Error of Birthdays -------------------------------
dd$Days <- dd$Switch - dd$Birthday
# Note, this is creating a new variable called 'Days' which is a diffeence between day of first switch and birthday.

Days.df <- (dd 
            %>% group_by(Group)
            %>% summarise(Mean.Days = mean(Days, na.rm = TRUE), Std.Err.Days = std.error(Birthday, na.rm = TRUE))
)
# Days.df is taking the averages and standard errors for Days across the two groups - Captive and Wild-Caught.

Fig.01 <- ggplot(data = Days.df, aes(y = Mean.Days, x = Group)) +
  geom_point() +
  theme.obj +
  scale_y_continuous(limits = c(0,5)) +
  ylab("Average number of days until first switch") +
  geom_errorbar(aes(ymin = Mean.Days - Std.Err.Days, ymax = Mean.Days + Std.Err.Days), color = "black", width = 0.1)
print(Fig.01)

ggsave("Fig01.pdf", plot = Fig.01, device = "pdf", width = 80, height = 100, units = "mm")

# Fig.01 is trying to show the mean (+/- standard error) for the number of days from birth a nursing pup takes
#     to switch which teat it is latched to (i.e. the pup is found attached to the opposite teat from which it was first attached too) for
#     pups born to moms in captivity compared to pups born to wild-caught moms. The dots represent the means for each group and the error bars
#     represent the standard error for each mean, respectively. 
# Note, Fig.01 is showing inferential information about the data. In other words, Fig.01 is showing summary data (i.e. means and standard
#     errors) by analyzing a sample of data and making predictions about larger populations. 
# Note, the means (i.e. the dots) are represented by position along a common scale (y axis) which is considered the best way to visualize
#     data for accurate understand, according to Cleveland (1984). Seeing as the dots are at a relatively close position along the y-axis,
#     it is considerably easy to determine/visualize that the average number of days until first switch are similar between groups.
# Note, I have saved the plot as a PDF using the ggsave() function. I have chosen to save the plot with a width < height. My rationale
#     is due to the proximity of comparisons. Since these two means are being compared between groups, I want the points to be in a
#     relatively close proximity to one another to make visual interpretation and understanding easier.
## ---- Figure - Boxplots ------------------------------------------------------
Fig.02 <- (ggplot(data = dd, aes(y = Days, x = Group, na.rm = T)) 
  + theme.obj 
  + scale_y_continuous(breaks = seq(0,8,1)) 
  + geom_boxplot(fill = "gray") 
  + geom_jitter(color = "black", alpha = 0.8, width = 0.2)
)
print(Fig.02)

ggsave("Fig02.pdf", plot = Fig.02, device = "pdf", width = 100, height = 100, units = "mm")

# Note, box plots are considered descriptive. Here, we see what is occurring in the population we are studying. In other words,
#     Fig.02 is showing data and describing the population we have sampled, showing the distribution of the data points for this population.
# Neither box plots or mean +/- standard error plots are superior to the other, they are just better for different things.
# Note, I have specified the y axis to be continuous, to remove a warning message that states that R does not know how to pick
#     a scale for type <difftime> and  defaults to continuous anyways. 
# Note, I have set the y-axis scale to be in the order of days with the breaks spaced out by days (i.e. integer number). It is easy for
#     viewers to understand/visualize whole days as opposed to thinking about what non-integer days are (e.g. what is 4.35 days?) 
# Note, I have used grey scale for the box plot colors ## see https://clauswilke.com/dataviz/avoid-line-drawings.html
# Box plots are displaying the median (solid black line), the IQR (top and bottom edges of each box) and the whiskers (the lines 
#     extending from the box plots) which are 1.5 the distance of the IQR. The dot is NOT an outlier, but simply a data point that is
#     outside of this range.
# I have put the two boxplots on the same y-axis scale. My rationale is that it is easier to compare across groups and visualize data 
#     that are positioned along a common axis (See Cleveland (1984)). This also makes it easier to compare the distributions between the 
#     two groups - Captive and Wild-Caught.


## ---- Figure - Switch ~ Birthday ---------------------------------------------
Fig.03 <- (ggplot(data = dd, aes(y = Switch, x = Birthday, shape = Group, color = Group))
  + geom_point(position = position_jitterdodge(jitter.width = 0.3, jitter.height = 0.3), size = 4)  
  + theme.obj
  + scale_y_date(limits = c(ymd("2023-06-13"), NA))
  + geom_abline(slope = 1, linetype = "dashed")
  + scale_shape_manual(values = c(15, 1))
  + scale_color_grey()
)
print(Fig.03)

ggsave("Fig03.pdf", plot = Fig.03, device = "pdf", width = 100, height = 100, units = "mm")

# Note, the dotted line has a slope of 1. Data points on this line suggest that the pup switched
#     which teat it was suckling on the day it was born. By definition, data points cannot fall below this
#     dotted line (i.e. pups cannot switch teats before they are born). Some data points may be slightly 
#     below this line because of the vertical jitter applied to the data points.
# I have chosen to use both shapes and color (in grey scale) to distinguish data points between the two groups 
#     (Captive and wild-caught). I argue that using two methods for discriminating group (in this case, color and shape)
#     help to alleviate any ambiguity in the interpretation of data points.

## ---- Linear Model -----------------------------------------------------------
lin.mod <- lm(Days ~ Group, data = dd)
check_model(lin.mod)


methods("check_model")
## debug(performance:::check_model.default)
## .check_assumptions_linear(x, minfo, verbose, 
##         ...)
##+ Error in Ops.difftime((f - mean(f)), 2) : 
##  '^' not defined for "difftime" objects

lin.mod2 <- lm(as.numeric(Days) ~ as.factor(Group), data = dd)
check_model(lin.mod2)
## debug(performance:::.check_assumptions_linear)
## + Error in stats::qf(0.5, ncol(x), nrow(x) - ncol(x)) : 
##  Non-numeric argument to mathematical function


lin.mod3 <- lme4::lmer(as.numeric(Days) ~ Group + (1 | Sex), data = dd)
check_model(lin.mod3)

# Note, I get an error. Error: `check_model()` not implemented for models of class `lm` yet.
# Note, currently of type <difftime> as seen through str(dd$Days). I have tried to re-format the variable 'Days' using:
#   (1) as.numeric, and (2) as.integer. I have also tried formulating a model using lme4:lmer, but was unsuccessful using
#   this method as well. I have left this code in - despite the fact it produces errors - so that you can view.

# Note, this model using base-R data (data = mtcars) is able to create the plot. 
m1 <- lm(mpg ~ as.factor(cyl), data = mtcars)
check_model(m1)

## performance::check_model is doing something weird, such that converting
## the variable to numeric *within the formula* screws it up (it shouldn't!)
lin.mod3 <- lm(Days ~ Group, data = mutate(dd, across(Days, as.numeric)))
check_model(lin.mod3)

## see https://github.com/easystats/performance/issues/678

# For my analysis I will continue using lin.mod2 which treats Day as numeric and Group as factor:
# Despite the check_model() function not working, I am able to create (some) of the plots from the performance package
#     in separate command functions as shown below. Although this is not ideal, as it creates more lines of code compared to the
#     one line of check_model(). I apologize that it is not working on my computer.
# I have organized the figures in a similar order as seen in the check_model() function.

plot(check_posterior_predictions(lin.mod2)) # Specific
plot(check_heteroscedasticity(lin.mod2)) # General
plot(check_distribution(lin.mod2)) # Specific / General
plot(check_normality(lin.mod2)) # Specific

# Note, I get an error saying 'For confidence bands, please install 'qqplotr'. I have done this, loaded it in the library and still
# receive this error. I am sorry for this. I have kept it in as it still - for me - displays the plot.

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
