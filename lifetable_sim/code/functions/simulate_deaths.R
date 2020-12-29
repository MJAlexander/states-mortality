simulate_deaths <- function(ages, mx, size_population, age_props){
  dd <- tibble(x = ages, mx = mx, age_props) %>%
    rowwise() %>% 
    mutate(simulated_deaths = rpois(1, mx*size_population*age_props))
  return(dd$simulated_deaths)
}