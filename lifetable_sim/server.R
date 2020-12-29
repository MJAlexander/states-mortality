library(tidyverse)
source("code/functions/make_lt.R")
source("code/functions/simulate_deaths.R")
pops <- read_rds("data/pops.rds")
mxs <- read_rds("data/mxs.rds")


# Define a server for the Shiny app
function(input, output) {
  
  run_sim <- reactive({
    # do the calculations
    
    selected_country <- input$country
    selected_year <- input$year
    
    dp <- pops %>% filter(country == selected_country, year == selected_year)
    dm <- mxs %>% filter(country == selected_country, year == selected_year)
    
    # get age vector
    ages <- unique(dm$age)
    last_age <- as.numeric(str_remove(string = ages[length(ages)], pattern = "\\+"))
    ages <- c(as.numeric(ages[1:(length(ages)-1)]), last_age)
    
    mx <- dm %>% select(mx) %>% pull()
    mx[is.na(mx)] <- 0
    
    obs_lt <- make_lt(ages, last_age, mx)
    
    age_props <- dp %>% 
      mutate(prop = population/sum(population)) %>% 
      select(prop) %>% 
      pull()
    
    total_pop <- sum(dp$population)
    
    
    size_simulated_population <- input$simulated_pop_size
    sim_mxs <- tibble()
    sim_e0s <- c()
    nsims <- 50
    
    for(i in 1:nsims){
      simulated_deaths <- simulate_deaths(ages, mx, size_simulated_population, age_props)
      simulated_mx <- simulated_deaths/(size_simulated_population*age_props)
      simulated_mx[is.nan(simulated_mx)] <- 0
      simulated_mx[simulated_mx>1] <- 1
      sim_mxs <- bind_rows(sim_mxs, tibble(age = ages, sim_mx = simulated_mx, sim = i))
      this_lt <- make_lt(ages, last_age, simulated_mx)
      sim_e0s <- c(sim_e0s, this_lt$ex[1])
    }
    
    hmd_mx <- tibble(age = ages, mx = mx)
    list(hmd_mx = hmd_mx, sim_mxs = sim_mxs, 
         obs_lt = obs_lt, sim_e0s = sim_e0s, 
         selected_country = selected_country,
         selected_year = selected_year, 
         size_simulated_population = size_simulated_population)
  })

  # plot
  output$mortalityPlot <- renderPlot({
  
    hmd_mx <- run_sim()[["hmd_mx"]]
    sim_mxs <- run_sim()[["sim_mxs"]]
    selected_country <- run_sim()[["selected_country"]]
    selected_year <- run_sim()[["selected_year"]]
    size_simulated_population <- run_sim()[["size_simulated_population"]]
    
    p <- hmd_mx %>% 
      filter(age<100) %>% 
      ggplot(aes(age, mx)) + geom_point() + 
      scale_y_log10() + 
      geom_point(data = sim_mxs %>% filter(sim<4, age<100), 
                 aes(age, sim_mx, color = factor(sim)), pch = 4) +
      theme_bw(base_size = 14) +
      scale_color_brewer(name = "simulation", palette = "Set1")+
      labs(title = "Observed and three sets of simulated mortality rates", subtitle = paste0(selected_country, ", ", 
                                                                               selected_year, ", population size = ",
                                                                               size_simulated_population), 
           caption = "HMD rates shown in black")
    
    print(p)
    
  })
  
  # print HMD le
  output$obs_le <- renderText({ 
    obs_lt <- run_sim()[["obs_lt"]]
    
    paste0("Life expectancy based on HMD mortality rates is ", round(obs_lt$ex[1], 2), ".")
  })
  
  # print simulated le
  output$sim_le <- renderText({ 
    
    sim_e0s <- run_sim()[["sim_e0s"]]
    paste0("Life expectancy based on 50 simulations is ", round(mean(sim_e0s), 2), " (2.5th and 97.5th quantiles: ",round(quantile(sim_e0s, 0.025), 2), ", ", round(quantile(sim_e0s, 0.975), 2), ").")
  })
  output$about_text <- renderText({
    paste0("test")
  })
}