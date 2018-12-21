# Use the HMD data to look at lifespan inequality
# data for states are available here: https://usa.mortality.org/
# download the lifetable zipped file
# data for national are available here: https://www.mortality.org/
# download the 1x1 lifetable for both sexes (bltper_1x1.txt)


library(tidyverse)
library(geofacet)


# get a list of states 

states <- list.files("./lifetables/States")

# read in all the state life tables and create one big dataframe with all states

lt_male <- c()
lt_female <- c()


for(i in 1:length(states)){
  folder_path <- paste0("./lifetables/States/",states[i])
  state_male <- read_csv(paste0(folder_path, "/", states[i], "_mltper_1x1.csv"))
  state_female <- read_csv(paste0(folder_path, "/", states[i], "_fltper_1x1.csv"))
  
  lt_male <- rbind(lt_male, state_male)
  lt_female <- rbind(lt_female, state_female)
  
  rm(state_male)
  rm(state_female)
}


lt <- bind_rows(lt_female, lt_male)

# plot the death distributions for CA males in two years

lt %>% 
  filter(PopName=="CA", Sex == "m", Year %in% c(1960, 2010)) %>% 
  mutate(age = as.numeric(Age), year = as.factor(Year)) %>% 
  group_by(year) %>% 
  mutate(e0 = ex[row_number()==1]) %>% 
  ggplot(aes(age, dx, color = year)) + geom_line() + 
  geom_vline(aes(xintercept = e0, color = year)) + 
  theme_classic() + 
  ylab("deaths") + 
  ggtitle("Distribution of deaths, Californian males \n1960 and 2010")
ggsave("dx_CA.png", height=5,width=7)


## national life expectancy and lifespan variation
## read in data from HMD
us_lt <- read_table("bltper_1x1.txt", skip = 2)

us_lt %>% 
  group_by(Year) %>% 
  mutate(e0 = ex[Age==0], 
         age = ifelse(Age=="110+", "110", Age),
         age = as.numeric(age),
         diff_sq = ((age - e0)^2*dx)) %>% 
  group_by(Year, e0) %>% 
  summarise(sd = sqrt(sum((diff_sq)/sum(dx)))) %>% 
  filter(Year>1959) %>% 
  #gather(indicator, value, -Year) %>% 
  ggplot(aes(Year, e0, color = "life expectancy (LHS)")) + geom_line() +
  geom_line(aes(Year, sd*4, color = "lifespan variation (RHS)")) + 
  scale_y_continuous(sec.axis = sec_axis(~./4, name = "lifespan variation (years)")) + 
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "life expectancy at birth (years)",
       x = "year",
       colour = "") + 
  theme_classic() + 
  ggtitle("Life expectancy and lifespan variation \nUnited States, 1960-2016")
ggsave("USA.png", height=5,width=7)


## calculate standard deviation in age at death for states

e0_ls <- lt %>% 
  group_by(PopName, Sex, Year) %>% 
  mutate(cdx = cumsum(dx), e0 = ex[Age==0], 
         age = ifelse(Age=="110+", "110", Age),
         age = as.numeric(age),
         diff_sq = ((age - e0)^2*dx)) %>% 
  group_by(PopName, Sex, Year, e0) %>% 
  summarise(sd = sqrt(sum((diff_sq)/sum(dx))))


# facet plot  
e0_ls %>% 
  filter(Sex=="m", Year>1969, PopName!="DC") %>% 
  ggplot(aes(Year, e0, color = "life expectancy (LHS)")) + 
  facet_geo(~PopName) + 
  geom_line() +
  geom_line(aes(Year, sd*4, color = "lifespan variation (RHS)")) + 
  scale_y_continuous(sec.axis = sec_axis(~./4, name = "lifespan variation (years)")) + 
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "life expectancy at birth",
       x = "year",
       colour = "")  +
  theme_bw() + 
  scale_x_continuous(breaks=seq(1970, 2010, 20))
ggsave("facet.png", height=7,width=12)


# NH and WV

e0_ls %>% 
  filter(Sex=="m", PopName %in% c("WV", "NH"), Year>1969) %>% 
  ggplot(aes(Year, e0, color = "life expectancy (LHS)")) + 
  geom_line() +
  facet_grid(~PopName) +
  geom_line(aes(Year, sd*3.5, color = "lifespan variation (RHS)")) + 
  scale_y_continuous(sec.axis = sec_axis(~./3.5, name = "lifespan variation (years)")) + 
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "life expectancy at birth",
       x = "year",
       colour = "")  +
  theme_bw()
ggsave("NH_WV.png", height=5,width=9)


## look at two states with similar life expectancy 

View(e0_ls %>% 
  filter(Year=="2015", Sex == "m") %>% 
  arrange(-e0))

e0_ls %>% 
  filter(Sex == "m", PopName == "OH"|PopName == "GA", Year>1969) %>% 
  gather(indicator, value, -PopName, -Sex, -Year) %>% 
  ggplot(aes(Year, value, color = PopName)) + geom_line() + 
  facet_wrap(~indicator, scale = "free_y") +
  scale_color_discrete(name = "state") +
  theme_classic()
ggsave("GA_OH.png", height=5,width=9)



