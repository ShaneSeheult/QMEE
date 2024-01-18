#Author: Shane Seheult 

## ---- libraries --------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gganimate)
## ---- Import Data ------------------------------------------------------------
teat_project_csv <- "Teat_Preferance_QMEE_Project.csv"
dd <- read_csv(teat_project_csv)
print(dd)

## ---- Do Birthdays differ, on average, between Groups? -----------------------
Wild_Caught.df <- filter(dd, Group == "Wild-Caught")
Captive.df <- filter(dd, Group == "Captive")

t.test(x = Wild_Caught.df$'Julian Date', y = Captive.df$'Julian Date')

# The results suggest that there is a significant difference (t(26.9) = 3.37, p = 0.02, 95%CI [0.70, 2.90])
# between birthdays of pups born to wild-caught females and pups born to females in captivity.
# The mean Birthday for pups born from wild-caught females is 168.74 (Julian Day) and the mean Birthday for
# pups born from captive females is 166.94 (Julian Day).

## ---- Plot the Data & look for anomalies ------------------------------------

fig01 <- ggplot(data = dd, aes(y = dd$`Julian Date`, x = Group, na.rm = T)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = "black", size = 0.4)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  ylab('Julian Date') + xlab('Group') +
        geom_boxplot()
fig01

# The box plots show no outliers (i.e. there are no data points exceeding the whiskers of the boxplots)
# However, the Julian Dates for the captive group tend to be skewed to lower values (as evidenced by the bold line
# of the box plot).

## ---- Looking for Normality in Data ------------------------------------------
qqnorm(dd$`Julian Date`, main = "Julian Date");qqline(dd$`Julian Date`)

## And in the residuals 
lin.mod <- lm(dd$`Julian Date`~ dd$Group)
qqnorm(residuals(lin.mod)); qqline(residuals(lin.mod))

## Histogram of Data 
fig02 <- ggplot(data = dd, aes(x = dd$`Julian Date`)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = "black", size = 0.4)) +
  ylab('Density') + xlab('Day') +
  scale_y_continuous(limits = c(0,20), expand = c(0,0)) +
  geom_histogram(bins = 20) +
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank()) 
fig02

# The data appear to be normally distributed as seen by the qqnorm plot (i.e. the data points
# do not drastically deviate from the qqline). and as per the histogram plot (fig02), although the 
# data do seem to be slightly right skewed. 

shapiro.test(dd$`Julian Date`)

# The results of the Shapiro-Wilk Normality test suggest that the data are not normally
# distributed. W = 0.950, p = 0.015

## ---- Trying Log Transformed data --------------------------------------------
dd.02 <- dd %>% mutate(log_JulianDate = log(dd$`Julian Date`)) 

qqnorm(dd.02$log_JulianDate, main = "log Julian Date");qqline(dd.02$`log_JulianDate`)

fig03 <- ggplot(data = dd.02, aes(x = dd.02$`log_JulianDate`)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = "black", size = 0.4)) +
  ylab('Density') + xlab('Day') +
  scale_y_continuous(limits = c(0,20), expand = c(0,0)) +
  geom_histogram() +
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank()) 
fig03

shapiro.test(dd.02$`log_JulianDate`)

# The log transformed data is not normal either, likely because I am using Julian 
# Date as the depedent variable. 

# sqrt(max(x+1) - x) for negatively skewed data - TRY IT!

## ---- Do the proportions of Same Sex and Different Sex pup pairs differ? -----
Matches <- dd$Sex...2 == dd$`Sibling Sex`
summary(Matches)
# From here, we can see that the total number of observations is 60 (37 + 19 + 4), which is consistent
# with using summary(df) to see that Bat.ID has a length of 60. We can then calculate the proportion Same-
# Sibling Matches (i.e. TRUE) and the proportion of Different Sibling Matches (i.e. FALSE).

prop_SameSib <- 19 / 60
prop_DiffSib <- 37 / 60

# We can also test if the two proportions differ from one another:

prop.test(c(19,37), c(60,60))

# The results suggest that there is a higher proportion of Different Sex Siblings compared to Same Sex Siblings
# (X^2 = 9.68, df = 1, p = 0.002, 95% CI [-0.487, -0.113]).
