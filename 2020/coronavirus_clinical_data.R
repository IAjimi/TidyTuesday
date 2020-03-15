### Loading Libraries ####
library(xml2)
library(httr)
library(rvest)
library(tidyverse)

### Getting Data ####
url_influenza_tests <- "https://www.cdc.gov/flu/weekly/weeklyarchives2019-2020/data/whoAllregt_cl09.html"
url_influenza_visits <- "https://www.cdc.gov/flu/weekly/weeklyarchives2019-2020/data/senAllregt09.html"

url_to_table <- function(x) {
  resp <- read_html(x)
  raw_input <- html_table(resp)[[1]]
  new_table <- as.data.frame(raw_input, stringsAsFactors = FALSE)
  new_table <- new_table %>% 
    mutate(Week = paste(Week, "1", sep = ""),
           Date = as.Date(Week, "%Y%U%u")) %>%
    select(- Week)
  
  return(new_table)
}

influenza_visits <- url_to_table(url_influenza_visits)
influenza_tests <- url_to_table(url_influenza_tests)

influenza_visits %>%
  ggplot(aes(Date, `Total ILI`)) + 
  geom_path() +
  labs(x = "")
