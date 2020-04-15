### Loading Packages ####
library(socviz)
library(xml2)
library(httr)
library(rvest)
library(tidyverse)
library(scales)
library(plotly)

library(shiny)

###Shiny ####
# User interface#
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h1("COVID-19 U.S. Dashboard"),
      h2("Setup"),
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
      
      tabsetPanel(
        
        type = "tabs",
                    tabPanel("Summary", p("summary")),
                    tabPanel("Map View", 
                             plotlyOutput("county_map_view")
                    ),
        
                  tabPanel("Confirmed Cases", 
                           h3("Total Number of Cases:"),
                           plotlyOutput("total_cases"),
                           br(),
                           h3("Growth in Cases:"),
                           plotlyOutput("cases_growth")
                           ),
                  tabPanel("Fatalities", 
                           h3("Total Number of Deaths:"),
                           plotlyOutput("total_deaths"),
                           br(),
                           h3("Growth in Deaths:"),
                           plotlyOutput("deaths_growth")
                  ),
                  tabPanel("Sources", p("summary"))
                           )
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
    
    ## LINE PLOT PREP ########################################################################################
    confirmed_cases_spread <- usa_county_corona %>% 
      filter(cases > 100) %>%
      split(.$county) %>%
      purrr::map(function(x) mutate(x, x_axis = c(1:nrow(x)))) %>%
      purrr::map(mutate,
                 lag_cases = lag(cases),
                 lag_deaths = lag(deaths),
                 cases_growth = (cases - lag_cases) / lag_cases,
                 deaths_growth = (deaths - lag_deaths) / lag_deaths) %>%
      do.call("rbind.data.frame", .)
    
    ## LINE PLOT AESTHETIC PREP 
    color_scheme <- c("#003399", "#FF2B4F", "darkgrey")
    names(color_scheme) <- c(noteworthy_county[1], noteworthy_county[2], 'Other')

    confirmed_cases_spread <- confirmed_cases_spread %>%
      mutate(county_label = case_when(
        county %in% noteworthy_county ~ county,
        T ~ "Other"
      ),
      `County` = paste(county, ", ", state, '\n',
                       format.Date(date, "%N %d"), '\n',
                       format(cases, big.mark = ',', small.interval = 3), ' confirmed cases', sep = ''))
    
    ## MAP PLOT PREP ######################################################
    ## IMPORT DATA 
    county_map <- socviz::county_map %>% rename(fips = id)
    
    ## PUT TOGETHER
    county_graph <- county_map %>% left_join(filter(usa_county_corona, date == max(date, na.rm = T)), 
                                             by = "fips")
    
    ## GET COUNTY NAMES FOR ALL FIPS (some missing in NYT files)
    url <- 'https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt'
    response <- read_lines(url, skip = 72)
    
    fips <- response %>% str_split('        ') %>% map(1) %>% flatten_chr()
    county <- response %>% str_split('        ') %>% map(2) %>% flatten_chr()
    
    fips_to_county <- data.frame(fips, county) %>%
      mutate_all(str_replace_all, ' {1,}', '') %>% #removes all extra spaces
      mutate(county = str_replace_all(county, 'County|Parish|Area|Borough|Census', ''))
    
    ## JOINING DATA
    county_graph <- fips_to_county %>% 
      left_join(county_map) %>% 
      left_join(filter(usa_county_corona, date == max(date, na.rm = T)), 
                by = c("fips", 'county')) %>%
      filter(!is.na(lat))
    
    ## ADDING LABEL FOR PLOTLY
    county_graph <-  county_graph %>%
      mutate(`County` = paste(county, ", ", state, '\n',
                     format.Date(date, "%N %d"), '\n',
                     format(cases, big.mark = ',', small.interval = 3), ' confirmed cases', sep = ''))
    
    ## SAVING OUTPUT
    list(confirmed_cases_spread, noteworthy_county, color_scheme, county_graph)
  })
  
  
  output$total_cases <- renderPlotly({
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
  
  output$cases_growth <- renderPlotly({
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
  
  output$total_deaths <- renderPlotly({
    response <- dataInput()
    
    confirmed_cases_spread <- response[[1]]
    noteworthy_county <- response[[2]]
    color_scheme <- response[[3]]
    
    p <- confirmed_cases_spread %>%
      ggplot(aes(x_axis, deaths, group = fips,  color = county_label, label = County)) +
      geom_line(data = . %>% filter(!county %in% noteworthy_county), alpha = 0.25) + #plots all other countries
      geom_line(data = . %>% filter(county %in% noteworthy_county), size = 1) +
      geom_point(data = . %>% filter(date == max(date) & county_label != "Other")) +
      scale_x_continuous(breaks = seq(0, 80, 7), limits = c(0, 31)) +
      scale_y_continuous(breaks = c(1, 100, 1000, 10000, 100000), 
                         labels = c(1, 100, 1000, 10000, "100000"), #avoid R's exponential notation
                         trans = "log"
      ) +
      scale_color_manual(values = color_scheme) + 
      labs(x = "Days since 100th Recorded Case", y = "# Deaths", color = "County")
    
    p  %>% ggplotly(tooltip = 'County')
  })
  
  output$deaths_growth <- renderPlotly({
    response <- dataInput()
    
    confirmed_cases_spread <- response[[1]]
    noteworthy_county <- response[[2]]
    color_scheme <- response[[3]]
    
    p2 <- confirmed_cases_spread %>%
      ggplot(aes(x_axis, deaths_growth, group = fips,  color = county_label, label = County)) +
      geom_line(data = . %>% filter(!county %in% noteworthy_county), alpha = 0.25) + #plots all other countries
      geom_point(data = . %>% filter(date == max(date) & county_label != "Other")) +
      geom_line(data = . %>% filter(county %in% noteworthy_county), size = 1) +
      scale_x_continuous(breaks = seq(0, 80, 7), limits = c(0, 31)) +
      scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25), labels = percent) + 
      scale_color_manual(values = color_scheme) + 
      labs(x = "Days since 100th Recorded Case", y = "Growth in Deaths", color = "County")
    
    
    p2  %>% ggplotly(tooltip = 'County')
    
  })
  
  output$county_map_view <- renderPlotly({
    response <- dataInput()
    county_graph <- response[[4]]

    mapplot <- county_graph %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
      #filter(state != 'Alaska') %>%
      mutate(County = paste(county, ", ", state, '\n', 
                              'Confirmed cases:', cases, sep = '')) %>%
      ggplot(aes(x = long, y = lat, fill = cases, group = fips, label = County))  + 
      geom_polygon(color = NA) + #no border colors 
      coord_equal() +
      scale_fill_continuous(
        low = "white", #white or #f7f1be
        high = "#f2161d",
        na.value = "white",
        trans = 'log',
        labels = c('', '20', '400', '8000'), #manual
      ) +
      guides(fill = guide_legend(nrow = 1)) + #breaks down legend
      labs(x= '', y ='', fill = "Total Confirmed Cases",
           title = paste('COVID-19 Spread: Confirmed Cases,', max(county_graph$date, na.rm = T))
      ) +
      theme_minimal() +
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            legend.position = "bottom")
    
    
    mapplot # %>% ggplotly(tooltip = 'County') #includeHTML
  })
  
  
}

shinyApp(ui, server)
