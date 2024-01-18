#Author: Shane Seheult 

# Designed to be run from ShaneSeheult / QMEE / data

## ---- libraries --------------------------------------------------------------
library(tidyverse)

## ---- Import Original Data ---------------------------------------------------
teat_project_csv <- "Teat_Preferance_QMEE_Project.csv"
dd <- read_csv(teat_project_csv)
print(dd)
summary(dd)

## This is a good way to see summary of character variables (There is a more-complicated, but updated version)
summary(dd %>% mutate_if(is.character, as.factor))

# FIXME: Look for problems in the data set (dd).
problems(dd) 
# The tibble of problems has 0 rows, thus there are no problems in my data set (dd).

# ----- Clean up data and ensure that columns are what you expect --------------
## ---- Correct Column Names for Sex -------------------------------------------
dd <- select(dd, -Sex...6)
# Note, The column used to identify the bats sex was repeated twice - redundancy! 
# This line of code removes the second occurernce of the column

dd$Birthday <- ymd(as.character(dd$Birthday))
# This formats the birthday variable into type <date> 

## ---- Correct Column Types ---------------------------------------------------
dd <- rename(dd, c(Bat.ID = `Bat ID`, Sex = Sex...2, Julian.Date = `Julian Date`, Sib.Sex = `Sibling Sex`))
# This code renames the columns, making it easier to call them in  future code

dd <- (dd
  %>% mutate(Bat.ID = as.factor(Bat.ID))
  %>% mutate(Sex = as.factor(Sex))
  %>% mutate(Group = as.factor(Group))
  %>% mutate(Sib.Sex = as.factor(Sib.Sex)))
summary(dd)

## --- SaveRDS after cleaning data ---------------------------------------------
saveRDS(dd, "PupBirthdaysClean.rds")
