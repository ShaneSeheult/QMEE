# Author: Shane Seheult
# Script Name: Assignment03.R

# ---- libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gganimate)
library(performance)
library(plotrix)
# ---- Import & Read Data (RDS) ------------------------------------------------
dd <- readRDS("PupBirthdaysClean.rds") 
summary(dd)

## ---- Figure Theme Object ----------------------------------------------------
theme.obj <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.4)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))

## ---- Figure - Mean +/- Std.Error of Birthdays -------------------------------
print(dd 
  %>% group_by(Group)
  %>% summarise(Mean.Birthday = mean(Birthday, na.rm = TRUE), Std.Err.Birthday = std.error(Birthday, na.rm = TRUE),
                Mean.Switch = mean(Switch, na.rm = TRUE), Std.Err.Switch = std.error(Switch, na.rm = TRUE)))

dd$Days <- dd$Switch - dd$Birthday

Days.df <- (dd 
            %>% group_by(Group)
            %>% summarise(Mean.Days = mean(Days, na.rm = TRUE), Std.Err.Days = std.error(Birthday, na.rm = TRUE)))

Fig.01 <- ggplot(data = Days.df, aes(y = Mean.Days, x = Group)) +
  geom_point() +
  theme.obj +
  scale_y_continuous(limits = c(0,5)) +
  ylab("Average number of days until first switch") +
  geom_errorbar(aes(ymin = Mean.Days - Std.Err.Days, ymax = Mean.Days + Std.Err.Days), color = "black", width = 0.1)
Fig.01

## ---- Figure - Switch ~ Birthday ---------------------------------------------
Fig.02 <- ggplot(data = dd, aes(y = Switch, x = Birthday, color = Group)) +
  geom_point(position = position_dodge2(width = 0.4)) + 
  theme.obj + 
  geom_smooth(method = "lm")
Fig.02 

## ---- Figure - Histogram of Day of First Switch ------------------------------
#Fig.03 <- ggplot(data = dd, aes(x = Switch)) +
# theme.obj +
#  ylab('Count') + xlab('Day of First Switch') +
#  geom_histogram(bins = 10) +
#  theme(legend.position = c(0.85, 0.85), legend.title = element_blank()) 
#Fig.03

## ---- Linear Model -----------------------------------------------------------
lin.mod <- lm(dd$Switch~ dd$Group)
check_model(lin.mod)