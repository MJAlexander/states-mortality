library(tidyverse)
mxs <- read_rds("data/mxs.rds")
pops <- read_rds("data/pops.rds")

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Life table simulation"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    sidebarPanel(
      helpText("This app simulates deaths from populations of different sizes and calculates the range of implied life expectancy estimates. Choose from various mortality conditions from HMD data using the dropdowns below."),
      hr(),
      helpText("Choose a country and year to simulate mortality rates from, and a total population size to simulate. The results are shown to the right."),
      selectInput("country", "Country:", 
                  choices=unique(mxs$country)),
      sliderInput("year",
                  "Year:",
                  value = 1960,
                  min = 1900,
                  max = 2015, sep = ""),
      sliderInput("simulated_pop_size",
                  "Total population size:",
                  value = 100000,
                  min = 1000,
                  max = 10^6, 
                  step = 500,
                  sep = "")
      ),
    
    # Create a spot for the plot
    mainPanel(
      plotOutput("mortalityPlot") ,
      textOutput("obs_le"),
      textOutput("sim_le")
    )
    
  )
)