## Generate principal components for Californian counties subnational mortality model
## Monica Alexander, September 2018
## the result of this is saved in ./data/US_state.csv

# NOTE you need to download the relevant information
# data are available here: https://usa.mortality.org/
# download the lifetable zipped file

library(tidyverse)

states <- list.files("./data/lifetables/States") # does not exist on github! need to download
lt <- c()

for(i in 1:length(states)){
  folder_path <- paste0("./data/lifetables/States/",states[i])
  state_both <- read_csv(paste0(folder_path, "/", states[i], "_bltper_1x1.csv"))
  
  lt <- rbind(lt, state_both)
  
  rm(state_both)
}

age_levels <- c(0, 1, seq(5, 25, by = 5), seq(35,85, by = 10), 110)
age_labels <- c("<1", "1-4 years", "5-9 years", "10-14 years",
                "15-19 years", "20-24 years", 
                "25-34 years", "35-44 years", 
                "45-54 years", "55-64 years", 
                "65-74 years", "75-84 years", 
                "85+")


lt_group <- lt %>% 
  mutate(age = as.numeric(as.character(Age))) %>% 
  mutate(age = ifelse(is.na(age), 110, age)) %>% 
  mutate(age_group = cut(age, age_levels, age_labels, include.lowest = TRUE, right = FALSE)) %>% 
  group_by(PopName, Year, age_group) %>% 
  summarise(qx = 1-prod(1-qx)) 

lt_group <- lt_group %>% 
  rowwise() %>% 
  mutate(age = ifelse(as.character(age_group)== "<1", 0, 
                      ifelse(as.character(age_group) == "85+", 85, 
                             as.numeric(strsplit(as.character(age_group), "-")[[1]][1])))) %>% 
  group_by(PopName, Year) %>% 
  mutate(n = lead(age, default = 100) - age,
         ax = n/2)

# replace first year ax's

lt_group <- lt_group %>% 
  left_join(
    lt %>% 
      filter(Age==0) %>% 
      mutate(age = as.numeric(Age)) %>% 
      select(PopName, Year, age, ax) %>% 
      rename(ax_first = ax)
  ) %>% 
  mutate(ax = ifelse(age ==0, ax_first, ax)) %>% 
  select(-ax_first)

# now calculate the mxs 

lt_group <- lt_group %>% 
  mutate(mx = qx / (n - qx*(n - ax)))

m.ga <- lt_group %>% 
  select(-age, -qx, -n, -ax) %>% 
  ungroup() %>% 
  spread(age_group, mx)  %>% 
  select(-Year, -PopName) %>% 
  as.matrix()

log_m.ga <- log(m.ga)

log_m.ga <- log_m.ga[-which(is.infinite(log_m.ga), arr.ind = T)[,1],]

pcs <-svd(log_m.ga)$v[,1:3]

plot(pcs[, 1], type = "o")
plot(pcs[, 2], type = "o")
plot(pcs[, 3], type = "o")

write_csv(as_tibble(pcs), "US_state.csv")
