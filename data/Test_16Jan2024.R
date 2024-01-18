# Test - 16 Jan 2023

#dir.create("data")
download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")

#list.files("data")

library(tidyverse)
dd <- read_csv("data/portal_data_joined.csv")
print(dd)

## select (grabs columns)
select(dd, plot_id, species_id, weight)
select(dd, -record_id, -species_id)

## filter (include/exclude observations/rows)

filter(dd, year == 1995)
#filter(dd, year = 1995) # Note, this will produce an error as you need to use "=="

## pipe, %>$ and |> - do something (some function)
# Take dd and pass it to the select function and select this column
dd %>% select(record_id)

dd.02 <- (dd
  %>% select(year, record_id) #You need to select Year to be able to filter year
  %>% filter(year == 1995))

## mutate - modify an old variable or make a new variable

dd %>% mutate(weight_kg = weight/1000, squared_weight = weight_kg^2) # Can be chained or put into separate lines (mulitple %>% mutate lines)

# ---- RDS Files ------
# saveRDS(x, "x.rds") - x is a dataframe and you can name it anything (as long as it is followed by .rds)
# Right click in Git tab and let it know it should not try to save a copy of this file
# .gitignore should pop back up with an M (modified) - You can make patterns in this file, ignore anything that ends in .rds
# You then want to commit .gitignore
# x <- readRDS("x.rds") - read this object back in
# save("x", "y", file = "mystuff.rda")
# L <- load ("mystuff.rda") - Loads .rds files that you have saved into work space!
