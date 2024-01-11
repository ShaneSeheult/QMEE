#Author: Shane Seheult 

teat_project_csv <- "Teat_Preferance_QMEE_Project.csv"
df <- read.csv(teat_project_csv)
summary(df)

## ---- Do Birthdays differ, on average, between Groups? -----------------------
Wild_Caught.df <- subset(df, Group == "Wild-Caught")
Captive.df <- subset(df, Group == "Captive")

t.test(x = Wild_Caught.df$Julian.Date, y = Captive.df$Julian.Date)

# The results suggest that there is a significant difference (t(26.9) = 3.37, p = 0.02, 95%CI [0.70, 2.90])
# between birthdays of pups born to wild-caught females and pups born to females in captivity.
# The mean Birthday for pups born from wild-caught females is 168.74 (Julian Day) and the mean Birthday for
# pups born from captive females is 166.94 (Julian Day).

## ---- Do the proportions of Same Sex and Different Sex pup pairs differ? -----
Matches <- df$Sex.1 == df$Sibling.Sex
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
