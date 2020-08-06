# Extracting ACS data by county for 2011-2017

# load packages
library(tidycensus)
library(tidyverse)
library(here)

# use API key
census_api_key("YOUR KEY HERE")

# 1. Get ACS variables ----------------------------------------------------------------------------

# load ACS variables to search through
v17 <- load_variables(2017, "acs5", cache = TRUE)

# list of state fip codes
data("fips_codes")
state_codes <- unique(fips_codes$state_code)
state_codes <- sort(state_codes)[1:51] # only keep mainland

# initialize empty tibble to put stuff in 
acs_vars <- tibble()

for(i in 2011:2017) {
  # get ACS
  vars_year <- get_acs(
    geography = "county",
    variables = c(
      median_inc_tot = "B19013_001",
      median_inc_nhw = "B19013H_001",
      median_inc_nhb = "B19013B_001",
      tot_in_workforce_employed_16_over = "B23025_004",
      tot_in_workforce_unemployed_16_over = "B23025_005",
      tot_not_in_workforce = "B23025_007",
      tot_hh_receiving_snap = "B22002_002",
      tot_hh_not_receiving_snap = "B22002_015",
      tot_hh_receiving_snap_nhw = "B22005H_002",
      tot_hh_not_receiving_snap_nhw = "B22005H_003",
      tot_hh_receiving_snap_nhb = "B22005B_002",
      tot_hh_not_receiving_snap_nhb = "B22005B_003"
    ),
    year = i
  )
  
  # add a year column
  vars_year$year <- i
  
  # add this year to the big tibbles
  acs_vars <- bind_rows(acs_vars, vars_year)
}


# we also want to just use the NAME variable from ACS data because it's a nicer format. 
county_code_name <- acs_vars %>% 
  select(GEOID, NAME) %>% 
  distinct() %>% 
  drop_na()

## now want to fix up county names,  rename columns to be all lowercase and fix ordering

acs_vars <- acs_vars %>% 
  select(-NAME) %>% 
  left_join(county_code_name) %>% 
  rename(county_id = GEOID, county_name = NAME) %>% 
  select(county_id, county_name, year, variable, estimate, moe)

# 2. Exlude non-mainland counties -----------------------------------------------------------------

mainland_acs = subset(acs_vars, (substr(acs_vars$county_id, 1, 2) %in% state_codes))

# 3. Save dataset ---------------------------------------------------------------------------------

write_csv(mainland_acs, here("harmonized_acs_2011_2017.csv"))
