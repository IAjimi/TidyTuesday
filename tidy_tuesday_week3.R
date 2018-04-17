####Tidy Tuesday Week 3####
library(readxl)
library(tidyverse)

###Loading Data
global_mortality <- read_excel("global_mortality.xlsx")

##Tidying: new column names
new_names <- vector() #create empty vector for the new column names

for (i in names(global_mortality)) { #replace names of column with shorter names
  idk <- str_split(i, " ")[[1]][1]
  new_names[i] <- str_to_lower(idk)
  new_names <- unname(new_names)
}

names(global_mortality) <- new_names

##Adding GDP Dataset
OECD_GDP <- read.csv("DP_LIVE_17042018021501126.csv", header=FALSE)
names(OECD_GDP) <- c("country_code", "Indicator", "Subject", "Masure", "Frequency", "year", 
                     "GDP", "Flags") #rename columns
OECD_GDP <- OECD_GDP[-1, c(1, 6, 7)] #select country, year, gdp

####Leading Causes of Death Worldwide####
##Numerically
leading_deaths <- global_mortality %>%  filter(!is.na(country_code))  %>% gather(cause, deaths, 4:35) %>% 
  group_by(cause) %>% summarize(av_deaths = mean(deaths, na.rm = TRUE), 
                                sd_deaths = sd(deaths, na.rm = TRUE)) %>% arrange(desc(av_deaths))
#!is.na(country_Code) because regions are included in dataset
#use gather to put df into long form, then grouby cause to get summary measure
#Cardiovascula + cancer are by far number 1

##Graphically (bar chart)
ggplot(leading_deaths) + geom_col(aes(fct_reorder(cause, av_deaths), av_deaths, fill = sd_deaths)) +
  labs(x = "", y = "% of Deaths", title = "Leading Causes of Death Worldwide, 2000-2016") +
  scale_fill_continuous(low =  "#ffd700", high = "#ea5f94",
                        breaks=c(10, 2.5, 0.5), name = "St. Dev.", guide = "legend") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), #tilts axis 
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) #hides vertical grid lines 

####Comparing US, CA, GBR####
#select columns of interest
mortality <- global_mortality %>% filter(country_code %in% c("CAN", "GBR", "USA")) %>%
  select(country_code,  year,  cardiovascular, cancers, diabetes, road, dementia, 
         "hiv/aids", suicide, homicide, alcohol, drug)
mortality_long <- mortality %>% gather(cause, deaths, 3:12) #creating a long form df for easier graphs

##Graphically
#Cancers, Cardiovascular, Dementia, Diabeties
mortality_long %>% filter(cause %in% c("cardiovascular", "cancers")) %>%
  ggplot() + geom_smooth(aes(year, deaths, color = country_code), se = FALSE) + 
  scale_color_manual(name = "Country", values = c("#ea5f94", "#ffb14e", "#9d02d7"), 
                     labels = c("Canada", "UK", "USA")) + 
  labs(x = "Year", y = "% of Deaths") + facet_grid(. ~ cause)
#deaths due to cardiovascular diseases decrceasing in the US, Canada and UK
#increase in cancer deaths

#Drug, Alcohol, Homicide, Suicide
mortality_long %>% filter(cause %in% c("drug", "alcohol", "homicide",
                                       "suicide")) %>% 
  ggplot() + geom_smooth(aes(year, deaths, color = country_code), se = FALSE) + 
  scale_color_manual(name = "Country", values = c("#ea5f94", "#ffb14e", "#9d02d7"), 
                     labels = c("Canada", "UK", "USA")) + 
  labs(x = "Year", y = "% of Deaths") + facet_grid(. ~ cause)
#large increase in drug deaths in the US, decrease in homicide rates in US and suicide
#rates in Canada

###Evolution over time in US
mortality_long %>% filter(country_code == "USA" & cause %in% c("drug", "homicide")) %>% 
  ggplot() + geom_smooth(aes(year, deaths, color = cause), se = FALSE) +
  labs(title = "Drugs surpassed Homicides in % of deaths in the US in the mid-2000s") + 
  scale_color_manual(name = "Cause", values = c("#878dac", "#3574e2"), 
                     labels = c("Drugs", "Homicide")) + 
  labs(x = "Year", y = "% of Deaths") 
#in us specifically, drug death increase + homicide decrease led to crossover in 2000s

####US Drug Epidemic####
###Drug Deaths for OECD Countries in 2016
#filtering data set
OECD <- c("AUS", "AUT", "BEL", "CAN", "CHE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC",
          "IRE", "ISL", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NOR", "NZL", "PRT", "SWE",
          "TUR", "USA")

mortality_16_OECD <- filter(global_mortality, year == 2016 & country_code %in% OECD)

#adding GDP
mortality_16_OECD <- merge(mortality_16_OECD, OECD_GDP, by = c("country_code", "year")) %>%
  arrange(drug) #arrange matters for graph labeling
mortality_16_OECD$GDP <- as.numeric(as.character(mortality_16_OECD$GDP)) #convert type

#preparing labels for graph
country_code2 <- mortality_16_OECD$country_code
country_code2[seq(0, 25, 2)] <- "" #removes 1 in 2 codes so x axis is legible

ggplot(mortality_16_OECD) + geom_col(aes(fct_reorder(country_code, drug), drug, fill = GDP)) +
  scale_x_discrete(labels= country_code2) + 
  labs(x = "", y = "% Drug Deaths", title = "Percentage of Drug-Related Deaths, OECD Countries 2016") +
  scale_fill_continuous(low =  "#ffd700", high = "#ea5f94",
                        breaks=c(100000, 50000, 25000), name = "GDP per Capita", guide = "legend") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), #tilts axis 
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) #hides vertical grid lines 
#drug deaths a primarily U.S. phenomenon? largest % of drug-related deaths in OECD

###USA: Cause of Death in 2016, Percentage Change
##reshaping dataset
usa <- global_mortality %>% filter((year == 2000 | year == 2016) & country_code == "USA") %>%
  gather(cause, deaths, 4:35) %>% spread(year, deaths)
#step 1: keeping years + country of interest only
#step 2: gather all of the death columns into CAUSE + DEATHS (df into long format)
#step 3: spread year so it becomes columns, and gather DEATHS underneat as rows
names(usa)[4:5] <- c("y2000", "y2016") #formatting column names so they arent numerical
usa <- mutate(usa, per_change = 100* (y2016 - y2000) / y2000) #add percentage change column

##bar graph
usa %>% filter(y2016 >= median(usa$y2016)) %>% ggplot() + #keep only the bigger causes of deaths 
  geom_col(aes(fct_reorder(cause, per_change), per_change, fill = y2016)) + #order by #deaths, fill w per change
  labs(x = "", y = "% Change", title = "% Change in Cause of Death between 2000-2016, USA") +
  scale_fill_continuous(low =  "#ffd28f", high = "#fa8775",
                        breaks=c(30, 8, 0.5), name = "% of Total Deaths in 2016", guide = "legend") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), #tilts axis 
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) #hides vertical grid lines 
#increase in Drug deaths by far the largest increase since 2000, almost doubled
#but remains only a small proportion of total deaths
