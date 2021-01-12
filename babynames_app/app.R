library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(ozbabynames)
library(babynames)
library(utf8)

# ontario data
d <- read_csv("ontario_top_baby_names_male_1917-2018_en_fr.csv")

# tidy up data

d <- d %>% janitor::clean_names() %>% 
  rename(name = name_nom, year = year_annee, frequency = frequency_frequence) %>% 
  mutate(name = str_to_title(name)) %>% 
  group_by(year) %>% 
  mutate(prop = frequency/sum(frequency)) %>% 
  filter(year>1989) %>% 
  mutate_if(is.character, utf8_encode)

ozbabynames <- ozbabynames %>% 
  group_by(name, sex, year) %>% 
  summarise(count = sum(count)) %>% 
  filter(count>4, name!="(Not") %>% 
  group_by(sex, year) %>% 
  mutate(prop = count/sum(count)) %>% 
  filter(year>1989) %>% 
  mutate_if(is.character, utf8_encode)

babynames <- babynames %>% 
  mutate(sex = ifelse(sex=="F", "Female", "Male")) %>% 
  filter(year>1989, n>50)  %>% 
  mutate_if(is.character, utf8_encode)
  # group_by(name, year) %>% 
  # summarise(n = sum(n)) %>% 
  # group_by(year) %>% 
  # mutate(prop = n/sum(n))


possible_names <- c(unique(ozbabynames$name), unique(babynames$name), unique(d$name))


## user interface

ui <- dashboardPage(
  dashboardHeader(title = "Trends of baby names for Australia, Ontario and the USA", titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trends", tabName = "Trends", icon = icon("th")),
      menuItem("Top Names", tabName = "Top", icon = icon("th")),
      menuItem("About", tabName = "About", icon = icon("info"))
    )
  ),
  dashboardBody(
    setBackgroundColor(color = "white", shinydashboard = TRUE),
    tabItems(
      # First tab content
      tabItem(tabName = "Trends",
              h2("Trends over time"),
              fluidRow(
                box(
                  width = 10, solidHeader = FALSE, status = "primary",
                  "Select name(s) and geography below to show trends in name popularity over time."
                )),
              sidebarLayout(
                sidebarPanel(
                  selectizeInput(inputId = 'name', 
                                 label = "Name", 
                                 choices = c(possible_names), multiple=TRUE, selected = "Edward"),
                  radioButtons(inputId = 'place',
                               label = 'Place',
                               choices = c("Australia", "Ontario", "USA"),
                               selected = "Australia")
                ),
                
                # Show a plot of time series
                mainPanel(
                  plotOutput("TrendPlot")
                )
              )
      ),
      
      # second tab
      tabItem(tabName = "Top",
              h2("Top names by year"),
              fluidRow(
                box(
                  width = 10, solidHeader = FALSE, status = "primary",
                  "Select year, gender, and geography below to show most popular names. Note: Ontario is for males only. "
                )),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = 'year', 
                                 label = "Year", 
                                 choices = 1990:2017),
                  radioButtons(inputId = 'sex',
                               label = 'Gender',
                               choices = c("Male", "Female"),
                               selected = "Male"),
                  radioButtons(inputId = 'place_2',
                               label = 'Place',
                               choices = c("Australia", "Ontario", "USA"),
                               selected = "Australia"),
                  selectInput(inputId = "number",
                              label = "Show top:",
                              choices = c(10, 20, 50, 100))
                ),
                
                # Show a plot of time series
                mainPanel(
                  tableOutput("TopTable")
                )
              )
      ),
      # Final tab content
      tabItem(tabName = "About",
              h2("About"),
              fluidRow(
                box(
                  width = 9, solidHeader = FALSE, status = "primary",
                  HTML("This app shows top baby names and trends over time for Australia, USA, and Ontario.",
                       '<br/>', '<br/>', "Australia data is from the ozbabynames package: https://github.com/ropenscilabs/ozbabynames",
                       '<br/>', '<br/>', "USA data is from the babynames package: https://cran.r-project.org/web/packages/babynames/index.html",
                       '<br/>', '<br/>', "Ontario data is from: https://data.ontario.ca/dataset/ontario-top-baby-names-male",
                       '<br/>', '<br/>', "Code to make app is here: https://github.com/MJAlexander/states-mortality",
                       '<br/>', '<br/>', "Unfortunately data are only available up to 2017 (2018 in Ontario case). You can find top name lists for more recent years elsewhere on the internet. I will incorporate them at some point.",
                       '<br/>', '<br/>', "Note the Ontario data is for males only because I haven't got around to downloading female data. Also note that the Australia data seems dodgey in parts.")
                )))
    )
  )
)

server = function(input, output) {
  output$TrendPlot <- renderPlot({
    if(input$place=="USA"){
      p1 <- babynames %>% 
        filter(name %in% input$name) %>% 
        ggplot(aes(year, prop*100, color = name, lty = sex)) + geom_line(lwd = 1.3) + 
        ggtitle("USA popularity")+
        ylab("Percent of total births") + 
        theme_bw(base_size = 16)+
        scale_color_brewer(palette = "Set1")
      p1
    }
    else{if(input$place=="Australia"){
      p1 <- ozbabynames %>% 
        filter(name %in% input$name) %>% 
        ggplot(aes(year, prop*100, color = name, lty = sex)) + geom_line(lwd = 1.3) + 
        ggtitle("Australian popularity")+
        ylab("Percent of total births") + 
        theme_bw(base_size = 16)+
        scale_color_brewer(palette = "Set1")
      p1
    }
      
      else{
        p1 <- d %>% 
          filter(name %in% input$name) %>% 
          ggplot(aes(year, prop*100, color = name)) + geom_line(lwd = 1.3) + 
          ggtitle("Ontario popularity")+
          ylab("Percent of total births") + 
          theme_bw(base_size = 16)+
          scale_color_brewer(palette = "Set1")
        p1
      }
      }
  })
  
  output$TopTable <- renderTable({
    if(input$place_2=="USA"){
      babynames %>% 
        filter(year == input$year, sex == input$sex) %>% 
        arrange(-prop) %>% 
        mutate(percent = prop*100) %>% 
        select(name, percent) %>% 
        mutate(rank = 1:n()) %>% 
        filter(rank<= as.numeric(input$number)) %>% 
        select(rank, name, percent)
    }
    else{if(input$place_2=="Australia"){
      ozbabynames %>% 
        filter(year == input$year, sex == input$sex) %>% 
        arrange(-prop) %>% 
        mutate(percent = prop*100) %>% 
        select(name, percent) %>% 
        mutate(rank = 1:n()) %>% 
        filter(rank<= as.numeric(input$number)) %>% 
        ungroup() %>% 
        select(rank, name, percent)
    }
      
      else{
        d %>% 
          filter(year == input$year) %>% 
          arrange(-prop) %>% 
          mutate(percent = prop*100) %>% 
          select(name, percent) %>% 
          mutate(rank = 1:n()) %>% 
          filter(rank<= as.numeric(input$number)) %>% 
          ungroup() %>% 
          select(rank, name, percent)
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)