library(tidyverse)
library(sf)
library(readxl)
library("rnaturalearth")
library("rnaturalearthdata")
source("helpers.R")

# world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# mmr data (from WHO estimates) https://www.who.int/reproductivehealth/publications/maternal-mortality-2000-2017/en/
mmr <- read_csv("data/mmr.csv")

summary(mmr$mmr)

# cut offs are 25% and 75% quartiles
mmr <- mmr %>% 
  mutate(mort_level = case_when(
    mmr<13~"low_mort",
    mmr<186~"med_mort",
    TRUE ~ "high_mort"
  )) %>% 
  mutate(mort_lev= as.factor(mort_level))

# CRVS coverage data (source: https://unstats.un.org/unsd/demographic-social/crvs/index.cshtml)

cov <- read_xls("~/Downloads/Website_final_coverage.xls", skip = 2)

# clean up and recategorize
cov <- cov %>% 
  dplyr::select(`Country or Area`, Coverage...6) %>% 
  rename(admin = `Country or Area`, coverage = Coverage...6)

cov_cut <- c("<50%", "50-74%", "75-89%", ">90%")
cov_cut_number <- c(-Inf, 49, 74, 89, Inf)

cov$cov_clean_fracs <- cut(as.numeric(cov$coverage), breaks =cov_cut_number/100, labels = cov_cut)

cov <- cov %>% 
  mutate(coverage_clean = case_when(
    coverage=="75% or more"~"75-89%",
    coverage=="less than 75%"~"50-74%",
    coverage=="less than 90%"~"50-74%",
    coverage=="less than 50%"~"<50%",
    coverage=="90% or more"~">90%",
    coverage=="75-89%"~"75-89%",
    coverage=="90-99%"~">90%",
    coverage=="70-79%"~"50-74%",
    coverage=="50-74%"~"50-74%",
    coverage=="80-89%"~"75-89%",
    coverage==">90%"~">90%",
    is.na(coverage) ~ "No data",
    TRUE ~ as.character(cov_clean_fracs)
  )) %>% 
  dplyr::select(-cov_clean_fracs) %>% 
  mutate(coverage_clean = factor(coverage_clean, 
                                 levels = c("No data", cov_cut), 
                                 labels = c("No data", cov_cut))) %>% 
  # for map categories
  mutate(apc_level = case_when(
    coverage_clean =="<50%"~"low_apc",
    coverage_clean =="50-74%"|coverage_clean =="75-89%"~"med_apc",
    coverage_clean ==">90%"~"high_apc", 
    is.na(coverage_clean) ~ "low_apc",
    TRUE ~"low_apc")) %>% 
  mutate(apc_lev=as.factor(apc_level))

## join to world data and label NAs in coverage as no data

world <- world %>% 
  left_join(mmr) %>% 
  left_join(cov) %>% 
  mutate(apc_lev = as.character(apc_lev), coverage_clean = as.character(coverage_clean)) %>% 
  mutate(apc_lev = ifelse(is.na(apc_lev), "low_apc", apc_lev),
         coverage_clean = ifelse(is.na(coverage_clean), "No data", coverage_clean)) %>% 
  mutate(apc_lev = as.factor(apc_lev),
         coverage_clean = factor(coverage_clean, levels = c("No data", cov_cut), labels = c("No data", cov_cut)))

## join the color legend (see helpers)
world <- world %>% left_join(gen_color_legend())

## map

ggplot(data = world %>% filter(!admin %in% c("Antarctica"))) +
  geom_sf(aes(fill = color_hex_a), lwd = 0.1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x=element_blank(), axis.ticks = element_blank()) + 
  scale_fill_identity(NULL, na.value = "white")

## legend
gen_hotspots_legend(c(13, 186), c(50, 90))