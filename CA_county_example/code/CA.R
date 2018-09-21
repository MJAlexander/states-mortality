## Code to estimate age-specific mortality rates by county in California for 1999--2016. 
## Monica Alexander, September 2018

# load in packages and functions
library(tidyverse)
library(rjags)
library(R2jags)
source("./code/functions/F_traceplot.R") # function to make traceplots of posterior samples
source("./code/functions/derive_lifetable_values.R") # function to calculate life expectancy etc


# read in deaths data and clean up

d <- read_csv("./data/CA.csv")
d <- d %>% 
  rename(county = County,
         code = `County Code`,
         age_group = `Age Group`,
         year = Year,
         deaths = Deaths,
         pop = Population)

d <- d %>% 
  filter(age_group!= "Not Stated", county!="Sierra County, CA") %>% 
  rowwise() %>% 
  mutate(age = ifelse(age_group== "< 1 year", 0, 
                      ifelse(age_group == "85+ years", 85, 
                             as.numeric(strsplit(age_group, "-")[[1]][1]))),
         mx = deaths/pop) 


# read in pop data and clean up

pop <- read_csv("./data/CA_pop.csv")

pop <- pop %>% 
  rename(county = County,
         code = `County Code`,
         age_group = `Age Group`,
         year = `Yearly July 1st Estimates`,
         pop = Population) %>% 
  filter(age_group!= "Not Stated", county!="Sierra County, CA")

# need to get the age groups so they are consistent with deaths dataframe 

age_crosswalk <- tibble(age_group = unique(pop$age_group), age = c(0, 1, 5, 10, 15, 20, 
                                                  25, 25, 35, 35, 
                                                  45, 45, 55, 55, 
                                                  65, 65, 75, 75, 85), 
       age_label = c("< 1 year" ,   "1-4 years",   "5-9 years",   "10-14 years", "15-19 years", "20-24 years",
                     rep("25-34 years",2), rep("35-44 years", 2), rep("45-54 years", 2),
                     rep("55-64 years",2), rep("65-74 years",2), rep("75-84 years",2), "85+ years"  ))


pop <- pop %>% 
  rowwise() %>% 
  mutate(age = age_crosswalk$age[which(age_crosswalk$age_group== age_group)],
         age_label = age_crosswalk$age_label[which(age_crosswalk$age_group== age_group)])

pop <- pop %>% 
  group_by(county, year, age_label, age) %>% 
  summarise(pop = sum(pop)) %>% 
  rename(age_group = age_label) %>% 
  ungroup()



# get the meta data about unique age groups, years and counties. 

age_groups <- unique(d$age_group)
ages <- c(0, 1, seq(5, 25, by = 5), seq(35, 85, by = 10))
years <- unique(d$year)
counties <- unique(d$county)

# make sure counties overlap across the two datasets
pop <- pop %>% filter(county %in% counties) 

## Get the data into JAGS format, i.e. arrays of dimension age x time x area x state

# initialise arrays (note I only have one state)
y.xtas <- array(NA, c(length(age_groups), length(years), length(counties), 1))
pop.xtas <- array(NA, c(length(age_groups), length(years), length(counties), 1))

for(j in 1:length(years)){
    this_value <- as.matrix(d %>% filter(year == years[j]) %>% select(county, age, deaths) %>% spread(county, deaths) %>% select(-age))
    this_pop <- as.matrix(pop %>% filter(year == years[j]) %>% select(county, age, pop) %>% spread(county, pop) %>% select(-age))
    these_counties <- colnames(this_value)
    these_counties_pop <- colnames(this_pop)
    for(k in 1:length(these_counties)){
      y.xtas[,j,which(counties==these_counties[k]),1] <- this_value[,k]
    }
    for(l in 1:length(these_counties_pop)){
      pop.xtas[,j,which(counties==these_counties_pop[l]),1] <- this_pop[,l]
    }
}


# load in principal components
pcs <- read.csv("./data/US_state.csv")[,1:3]

jags.data <- list(y.xtas = y.xtas, 
                  pop.xtas = pop.xtas, 
                  Yx = pcs,
                  S = 1, X= length(age_groups), T = length(years), 
                  n.a=length(counties), n.amax=length(counties), P=3 )

# parameters to monitor
parnames <- c("beta.tas", "mu.beta" ,"sigma.beta", "tau.mu", "u.xtas", "mx.xtas")

# run model
mod <- jags(data = jags.data, 
            parameters.to.save=parnames, 
            n.iter = 30000,
            model.file = "./code/model.txt")

# summary
mod$BUGSoutput$summary

# check all Rhats are less than 1.1
max(mod$BUGSoutput$summary[,"Rhat"])

# pull out the posterior samples
mcmc.array <- mod$BUGSoutput$sims.array

# you can check the traceplots of samples; make sure mixing is adequate
PlotTrace("mx.xtas[1,5,2,1]", mcmc.array)


# get Mx estimates and 95% CIs for 2016 for each county
res <- tibble(age = rep(unique(d$age), each = length(counties)), 
              county = rep(counties, times = length(age_groups)),
              median = rep(NA, length(age_groups)*length(counties)), 
              upper = rep(NA, length(age_groups)*length(counties)), 
              lower = rep(NA, length(age_groups)*length(counties)))
for(i in 1:length(age_groups)){
  for(j in 1:length(counties)){
    sms <-  c(mcmc.array[,,paste0("mx.xtas[",i,",18,",j,",1]")]  ) # 2016 == years[18]
    res$median[res$age==unique(d$age)[i]&res$county==counties[j]] <- median(log(sms))
    res$upper[res$age==unique(d$age)[i]&res$county==counties[j]] <- quantile(log(sms), 0.975)
    res$lower[res$age==unique(d$age)[i]&res$county==counties[j]] <- quantile(log(sms), 0.025)
  }
}

# plot the results
ggplot(res %>% filter(county %in% counties[seq(1, 48, by = 9)]), aes(age, median)) +
  geom_line(aes(color = county)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = county), alpha = 0.2) + 
  facet_wrap(~county) + 
  geom_point(data = d %>% filter(year==2016, county %in% counties[seq(1, 48, by = 9)]), 
             aes(age, log(mx), color = county), size = 2) + 
  theme_bw(base_size = 14) + 
  ylab("log mortality rate") + 
  ggtitle("Data and estimates for mortality rates in selected Californian counties, 2016")
ggsave("./select_counties_mx.png", width = 10, height = 6)



# get e0 estimates for Alameda
res <- c()
for(i in 1:length(years)){
  m.as <- c()
  for(j in 1:length(age_groups)){
    m.as <-  rbind(m.as, c(mcmc.array[,,paste0("mx.xtas[",j,",",i, ",",1,",1]")]  )) # alameda county is county 1
  }
  exs <- sapply(1:ncol(m.as), function(i) derive_ex_values(m.as[,i], ages)[1])
  res <- rbind(res, tibble(county = counties[3], year = years[i], median = median(exs),
                           lower = quantile(exs, 0.025),
                           upper = quantile(exs, 0.975))
  )
}

# plot the results
res %>%  
  ggplot(aes(year, median)) + 
  geom_line() + geom_point() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)+
  ylab("e0 (years)") + theme_bw(base_size = 14) + 
  ggtitle(paste("Life expectancy for", counties[1]))
ggsave("./e0_alameda.png", width = 8, height = 6)

