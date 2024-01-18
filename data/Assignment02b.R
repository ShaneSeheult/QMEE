# Name: Assignment02b.R 
# Author: Shane Seheult

# ---- libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gganimate)

# ---- Import Data (RDS) -------------------------------------------------------
dd <- readRDS("PupBirthdaysClean.rds") 
summary(dd)

## ---- Do Birthdays differ, on average, between Groups? -----------------------
Wild_Caught.df <- subset(dd, Group == "Wild-Caught")
Captive.df <- subset(dd, Group == "Captive")

t.test(x = Wild_Caught.df$Julian.Date, y = Captive.df$Julian.Date)

# The results suggest that there is a significant difference (t(26.92) = 3.37, p = 0.002, 95%CI [0.70, 2.90])
# between birthdays of pups born to wild-caught females and pups born to females in captivity.
# The mean Birthday for pups born from wild-caught females is 168.74 (Julian Day) and the mean Birthday for
# pups born from captive females is 166.94 (Julian Day).

## ---- Plot the Data & look for anomalies ------------------------------------

fig01 <- ggplot(data = dd, aes(y = Julian.Date, x = Group, na.rm = T)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.4)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  ylab('Julian Date') + xlab('Group') +
  geom_boxplot()
fig01

# The box plots show no outliers (i.e. there are no data points exceeding the whiskers of the boxplots)
# However, the Julian Dates for the captive group tend to be skewed to lower values (as evidenced by the bold line
# of the box plot).

## ---- Looking for Normality in Data ------------------------------------------
lin.mod <- lm(dd$Julian.Date~ dd$Group)
qqnorm(residuals(lin.mod)); qqline(residuals(lin.mod))

# Shapiro-Wilk Test
shapiro.test(residuals(lin.mod))
# The results of the Shapiro-Wilk normality test suggest that the residuals of the
# linear model are normally distributed. W = 0.967, p = 0.105

## Histogram of Data 
fig02 <- ggplot(data = dd, aes(x = Julian.Date)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = "black", size = 0.4)) +
  ylab('Count') + xlab('Julian Date') +
  scale_y_continuous(limits = c(0,20), expand = c(0,0)) +
  scale_x_continuous(limits = c(162, 175)) +
  geom_histogram(bins = 30) +
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank()) 
fig02

# The data appear to be normally distributed as seen by the qqnorm plot (i.e. the data points
# do not drastically deviate from the qqline) and the histogram plot (fig02), although the 
# data do seem to be slightly right skewed.


## ---- Do the proportions of Same Sex and Different Sex pup pairs differ? -----
dd$Matches <- dd$Sex == dd$Sib.Sex
summary(dd$Matches)
# From here, we can see that the total number of observations is 60 (37 + 19 + 4), which is consistent
# with using summary(df) to see that Bat.ID has a length of 60. We can then calculate the proportion Same-
# Sibling Matches (i.e. TRUE) and the proportion of Different Sibling Matches (i.e. FALSE).

prop_SameSib <- 19 / 60
prop_DiffSib <- 37 / 60

# We can also test if the two proportions differ from one another:

prop.test(c(19,37), c(60,60))

# The results suggest that there is a higher proportion of Different Sex Siblings compared to Same Sex Siblings
# (X^2 = 9.68, df = 1, p = 0.002, 95% CI [-0.487, -0.113]). The proportion of Same-Sibling pups is 0.32 and the 
# the proportion of Different-Sex pups is 0.62.
