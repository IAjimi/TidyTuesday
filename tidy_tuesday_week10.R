####Tidy Tuesday Week 10####
library("tidyverse")
library("lubridate")
bikes_1607 <- read.csv("~/Coding/PublicTripData/2016_07.csv")
View(bikes_1607)

###column types
bikes_1607$StartDate <- mdy(as.character(bikes_1607$StartDate)) #change into date
bikes_1607$StartTime <- hm(as.character(bikes_1607$StartTime)) #change into time
bikes_1607$StartHour <- hour(bikes_1607$StartTime) #create hour variable
bikes_1607$Day <- wday(bikes_1607$StartDate) #create weekday variable

bikes_1607$payment <- NA #creating payment  type dummy, subscriber = 2, casual = 1
bikes_1607$payment <- as.numeric(bikes_1607$PaymentPlan) - 1 #converted into binary system

###where do most trips start/end?
ggplot(bikes_1607)+geom_point(aes(StartLatitude, StartLongitude, color = PaymentPlan))
ggplot(bikes_1607)+geom_point(aes(EndLatitude, EndLongitude, color = PaymentPlan))

###at what time to most trips start?
use_dates <- bikes_1607 %>% group_by(StartHour) %>% count()
use_dates <- merge(use_dates, 
                   bikes_1607 %>% group_by(StartHour) %>% summarize(subscrib = mean(payment)))
#merging count/hour data w/ subscription type/hour data

ggplot(use_dates, aes(as.numeric(StartHour), n, color = subscrib)) + geom_path() + geom_point() +
  labs(x = "Start Hour", y = "Count")
#most people use the bikes in the afternoon (midday to 8pm) 

mean(bikes_1607$payment) #m3% of users r subscribers
ggplot(use_dates) + geom_path(aes(as.numeric(StartHour), subscrib)) +
  labs(x = "Start Hour", y = "% Subscribers") #the share of subscribers as 
#users is highest in the morning

###difference in use between subscribers and others?
#time
use_dates2 <- bikes_1607 %>% group_by(StartHour, payment) %>% count()
ggplot(use_dates2) + geom_bar(aes(as.numeric(StartHour), n, fill = payment), stat= "identity", 
                             position = position_dodge()) + 
  labs(x = "Start Hour", y = "Count") 
#both peak at the same time, but there are more casual riders than subscribers as a whole
                                                                  
#day                                   
use_days2 <- bikes_1607 %>% group_by(Day, payment) %>% count()
ggplot(use_days2) + geom_bar(aes(as.numeric(Day), n, fill = payment), stat= "identity") +
  labs(x = "Day", y = "Count") #most use on weekends
