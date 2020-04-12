### Loading Packages ####
library(socviz)
library(xml2)
library(httr)
library(rvest)
library(tidyverse)
library(scales)
library(plotly)

###Shiny ####
# User interface#
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h1("COVID-19 U.S. Dashboard"),
      h2("Setup"),
      
      #textInput("text", strong("Add Countries:"), 
      #          value = NULL) ,
      
      
      #usa_county_corona <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
      #counties <- unique(usa_county_corona$county)
      #counties <- counties[order(counties)]
      
      h3("Choose a variable to display:"),
      helpText("Change ... to see the growth of COVID-19 since ...."),
      selectInput("select_var", label = 'Select Var',
                  choices = list('Confirmed Cases', 'Fatalities'), 
                  selected = 'Confirmed Cases'),
      
      h3("Add counties to highlight:"),
      helpText("You can either select from the list below of most populous U.S. counties or type in 
               your selection below. Your choices will be highlighted in the line plots for more 
               comfortable viewing."),
      selectInput("select_county_1", label = 'Select County 1',
                  choices = list('New York City', 'Napa', "New London", 'Baltimore'), 
                  selected = 'New York City'),
       
      selectInput("select_county_2", label = 'Select County 2',
                  choices = list('New York City', 'Napa', "New London", 'Baltimore'), 
                  selected = 'Baltimore')
      
      
    ),
    
    mainPanel(
      h3("Total Number of Cases:"),
      plotlyOutput("tot_var"),
      br(),
      h3("Growth in Cases:"),
      plotlyOutput("var_growth")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({ 
    ## LOAD INPUTS
    noteworthy_var <- c(input$select_var)
    noteworthy_county <- c(input$select_county_1, input$select_county_2)

    ## IMPORT DATA 
    usa_county_corona <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
    
    ## PREPARE DATA
    confirmed_cases_spread <- usa_county_corona %>% 
      filter(cases > 100) %>%
      split(.$county) %>%
      purrr::map(function(x) mutate(x, x_axis = c(1:nrow(x)))) %>%
      purrr::map(mutate,
                 lag_cases = lag(cases),
                 lag_deaths = lag(deaths),
                 cases_growth = (cases - lag_cases) / lag_cases,
                 death_growth = (deaths - lag_deaths) / lag_deaths) %>%
      do.call("rbind.data.frame", .)
    
    ## PLOTS
    color_scheme <- c("#003399", "#FF2B4F", "darkgrey")
    names(color_scheme) <- c(noteworthy_county[1], noteworthy_county[2], 'Other')

    confirmed_cases_spread <- confirmed_cases_spread %>%
      mutate(county_label = case_when(
        county %in% noteworthy_county ~ county,
        T ~ "Other"
      ),
      `County` = paste(county, ", ", state, '\n',
                       format(cases, big.mark = ',', small.interval = 3), ' confirmed cases', sep = ''))
    
    ## SAVING OUTPUT
    list(confirmed_cases_spread, noteworthy_county, color_scheme)
  })
  
  
  output$tot_var <- renderPlotly({
    response <- dataInput()
    
    confirmed_cases_spread <- response[[1]]
    noteworthy_county <- response[[2]]
    color_scheme <- response[[3]]
    
    p <- confirmed_cases_spread %>%
      ggplot(aes(x_axis, cases, group = fips,  color = county_label, label = County)) +
      geom_line(data = . %>% filter(!county %in% noteworthy_county), alpha = 0.25) + #plots all other countries
      geom_line(data = . %>% filter(county %in% noteworthy_county), size = 1) +
      geom_point(data = . %>% filter(date == max(date) & county_label != "Other")) +
      scale_x_continuous(breaks = seq(0, 80, 7), limits = c(0, 31)) +
      scale_y_continuous(breaks = c(1, 100, 1000, 10000, 100000), 
                         labels = c(1, 100, 1000, 10000, "100000"), #avoid R's exponential notation
                         trans = "log"
      ) +
      scale_color_manual(values = color_scheme) + 
      labs(x = "Days since 100th Recorded Case", y = "# Confirmed Cases", color = "County")
    
    p  %>% ggplotly(tooltip = 'County')
  })
  
  output$var_growth <- renderPlotly({
    response <- dataInput()
    
    confirmed_cases_spread <- response[[1]]
    noteworthy_county <- response[[2]]
    color_scheme <- response[[3]]

    p2 <- confirmed_cases_spread %>%
      ggplot(aes(x_axis, cases_growth, group = fips,  color = county_label, label = County)) +
      geom_line(data = . %>% filter(!county %in% noteworthy_county), alpha = 0.25) + #plots all other countries
      geom_point(data = . %>% filter(date == max(date) & county_label != "Other")) +
      geom_line(data = . %>% filter(county %in% noteworthy_county), size = 1) +
      scale_x_continuous(breaks = seq(0, 80, 7), limits = c(0, 31)) +
      scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25), labels = percent) + 
      scale_color_manual(values = color_scheme) + 
      labs(x = "Days since 100th Recorded Case", y = "Growth in Confirmed Cases", color = "County")
    
    
    p2  %>% ggplotly(tooltip = 'County')
    
  })
  
  
}

shinyApp(ui, server)
