# Use the HMD United states mortality data base to plot record life expectancy by year
# data are available here: https://usa.mortality.org/
# download the lifetable zipped file

library(tidyverse)
library(ggrepel)

# get a list of states 

states <- list.files("./lifetables/States")

# read in all the state life tables and create one big dataframe with all states

lt_male <- c()
lt_female <- c()

for(i in 1:length(states)){
  folder_path <- paste0("./lifetables/States/",states[i])
  state_male <- read_csv(paste0(folder_path, "/", states[i], "_mltper_1x1.csv"))
  state_female <- read_csv(paste0(folder_path, "/", states[i], "_fltper_1x1.csv"), col_types = cols(Sex = col_character()))
  
  lt_male <- rbind(lt_male, state_male)
  lt_female <- rbind(lt_female, state_female)
  
  rm(state_male)
  rm(state_female)
}


lt <- bind_rows(lt_female, lt_male)


## Oeppen-Vaupel style plot with the max life expectancy by year

lt %>% 
  filter(Age == 0, PopName!="HI") %>% 
  group_by(Year, Sex) %>% 
  summarise(max_ex = max(ex), State = PopName[ex==max(ex)]) %>% 
  arrange(Sex, Year) %>% 
  group_by(Sex) %>% 
  mutate(state_label  = case_when(
    row_number()==1 ~ State,
    State!=lag(State, n=1) ~ State,
    TRUE ~ ""
  )) %>% 
  ggplot(aes(Year, max_ex, color = State, group = Sex)) + 
  geom_point(aes(pch = Sex), size = 3) +
  geom_text_repel(aes(label = state_label), size = 5) + 
  theme_bw(base_size = 18)+
  ylab("Life expectancy at birth (years)")+
  scale_color_discrete(guide = 'none') + 
  scale_x_continuous(breaks = seq(1960, 2015, by = 5)) +
  labs(caption = "Source: United States Mortality Database https://usa.mortality.org/") + 
  ggtitle("State with highest life expectancy, 1959-2015")
ggsave("highest_le.pdf", width = 10, height = 7)
