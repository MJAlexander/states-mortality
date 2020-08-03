library(tidyverse)

# download
download.file("https://data.nber.org/natality/2017/natl2017.csv.zip", destfile = "natl2017.csv.zip")

# read in and select
d <- read_csv("natl2017.csv.zip")
head(d)
d <- d %>%
  select(mager, mracehisp, meduc, bmi, sex, combgest, dbwt, ilive)

# only use a sample of the births
set.seed(853)
ds <- d[sample(1:nrow(d), nrow(d)*0.001),]

ds <- ds %>% mutate(dbwt= dbwt/1000)
write_rds(ds, path = "births_2017_sample.RDS")