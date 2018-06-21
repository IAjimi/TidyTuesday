###Tidy Tuesday Week 8###
honey_0812 <- read.csv("~/Coding/honeyraw_2008to2012.csv", header=FALSE)
library("tidyverse")

###Cleaning the DataSet
#looking through the OG dataset, it appears V1 & V2 can be used to filter out certain segments
#of the dataset
#in particular, 1d to 5d contain state specific production data
#using this, I filter out those segments & make them into a new dataset

honey_0812_states <- filter(honey_0812, (V1 <= 5) & (V2 == "d")  & (V3 != ""))
#(V3 != "") removes blank entries

##Column-Type Conversion
#col 3 gets converted to character (from factor)
honey_0812_states[, 3] <- as.character(honey_0812_states[, 3])

#columns 4 to 9 are numerical, so are converted to a numerical type using a loop
for (i in c(4:9)) {
  honey_0812_states[, i] <- as.numeric(as.character(honey_0812_states[, i]))
}

##Unit Scaling
#column 4 is in 1,000 pounds, column 8 is in cents... I convert the units for personal convenience
honey_0812_states[, c(4:7, 9)] <- honey_0812_states[, c(4:7, 9)] * 1000
honey_0812_states[, 8] <- honey_0812_states[, 8] / 100

#I want to re-use my col 1 & turn it into a year column. This is easily done given that
#v1 == 1 corresponds to 2008 data, v2 == 2 2009 etc
honey_0812_states[, 1] <- honey_0812_states[, 1] + 2007

##Renaming Columns
honey_0812_states <- honey_0812_states[, -2] #I remove col 2

names(honey_0812_states) <- c("Year", "State", "Colonies", "ColonyYield", "Production", "Stocks", 
                              "PoundPrice", "ProductionValue")

###Data Qs
#Change in Yield Per Colony
honey_0812_states %>% filter(State == "United States 6/ 7/") %>% ggplot(aes(Year, ColonyYield)) + 
  geom_col() # a graph

reg <- lm(ColonyYield ~ poly(Year, 2) + PoundPrice + State, data = honey_0812_states)
summary(reg) #regression, shows slight decrease

#why?
honey_0812_states %>% filter(State != "United States 6/ 7/") %>% ggplot() + 
  geom_col(aes(Year, PoundPrice)) #increase in price per pound over time

#clear linear relationship between Q produced and production
honey_0812_states %>% filter(State != "United States 6/ 7/") %>% ggplot() + 
  geom_point(aes(Production, ProductionValue))
