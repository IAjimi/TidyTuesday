####Tidy Tuesday Week 4####
library("tidyverse")
australian_salary <- read.csv("~/Coding/week4_australian_salary.csv")

#divide into women & men, select relevant columns, rename
aus_women <-  australian_salary %>% filter(gender == "Female") %>% select(occupation, individuals,
                                                                          average_taxable_income)
names(aus_women) <- c("occupation", "nwomen", "women_income")
  
aus_men <-  australian_salary %>% filter(gender != "Female") %>% select(occupation, individuals,
                                                                          average_taxable_income)
names(aus_men) <- c("occupation", "nmen", "men_income")

#merge 
aus_income <- merge(aus_men, aus_women, by = "occupation")

#adding some extra columns jic
aus_income <- mutate(aus_income, propw = (nwomen / (nmen + nwomen)), 
                     paygap = men_income / women_income)

#plot
#inspired by https://twitter.com/BenYAndrew/status/990272974090465280
ggplot(aus_income, aes(men_income, women_income))+geom_point()+geom_smooth(se = FALSE) +
  labs(x = "Men", y = "Women", title = "Average Income per Occupation")

###further exploration
#selecting occupations w most women
women_topjobs <- aus_income %>% arrange(desc(propw))
women_topjobs <- women_topjobs[1:15, ] 
#occupations w highest women/men ratio are nursing, childcare, secretaries

men_topjobs <- aus_income %>% arrange(propw)
men_topjobs <- men_topjobs[1:15, ] 
#for men mainly construction, utilities

mean(women_topjobs$paygap)
mean(men_topjobs$paygap)
#the occupations with the most women have a roughly equal pay ratio (men are paid 3% more)
#while those with the most men are strikingly unequal (men are paid 42% more)

mean(aus_income$paygap)
#overall, men are paid 30% more than women in the same occupation
