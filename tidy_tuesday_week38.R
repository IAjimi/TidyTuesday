allCetaceanData <- read_csv("C:/Users/ia767/Downloads/allCetaceanData.csv")

# maintenance
## one species is spelled in 2 different ways in the data set. this fixes the issue
allCetaceanData$species <- str_replace(allCetaceanData$species, "White-sided, Pacific", 
                                       "Pacific White-Sided")

### First Graph: acquisition type per birth year ####
filter(allCetaceanData, acquisition %in% c("Born", "Capture", "Rescue")) %>% 
  ggplot(aes(originDate, fill = acquisition)) + geom_histogram() +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  labs(x ="", y = "Number of Acquired Cetaceans", fill = "Origin") +
  theme(legend.position = "top")

### Second Graph: survival rate per species, sep by acquisition type ####
species_n <- allCetaceanData %>% group_by(species) %>% count() %>% arrange(desc(n)) %>% head()
#picking species w a sufficiently high number of observations

survival_rate <- allCetaceanData %>% filter(status == "Died", 
                                            !is.na(birthYear), !is.na(statusDate)) %>% 
  mutate(DoD = year(statusDate), lifespan = DoD - as.numeric(birthYear))
#adding a 'date of death' (DoD) & lifespan variable

species_survival_rate <- survival_rate %>% filter(species %in% species_n$species,
  acquisition %in% c("Born", "Capture", "Rescue")) %>%
  group_by(species, acquisition) %>% 
  summarize(lifespan = mean(lifespan, na.rm = TRUE)) 
#calculating mean lifespan

ggplot(species_survival_rate, aes(reorder(species, lifespan), lifespan, color = acquisition)) + 
  geom_point(size = 4.5) + coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Average Lifespan of Cetacean Species by Acquisition Origin",
       subtitle = "Captured Cetaceans Live Longer Than Rescues & Individuals Born In Captivity", 
       x ="", y = "Lifespan (Years)", color = "Origin")

### Second Graph variant: indexed by time period####
#pre 1991 dummy
survival_rate$old <- NA
survival_rate$old[survival_rate$originDate <= "1991-01-01"] <- "1946-1991"
survival_rate$old[survival_rate$originDate > "1991-01-01"] <- "1991-2016"

species_survival_rate_old <- survival_rate %>% 
  filter(species %in% c("Rough-Toothed", "Bottlenose", "Common; Saddleback", "Harbor Porpoise"),
                                                  acquisition == "Rescue") %>%
  group_by(species, acquisition, old) %>% 
  summarize(lifespan = mean(lifespan, na.rm = TRUE)) 
#capture only because lack of data points for other acqisition types
#for similar reasons restricted to a few species

ggplot(species_survival_rate_old, aes(reorder(species, lifespan), lifespan, color = old)) + 
  geom_point(size = 4.5) + coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Average Lifespan of Rescued Cetaceans",
       subtitle = "Rescued Cetaceans Have A Shorter Lifespan Today Than In The Early 90s", 
       x ="", y = "Lifespan (Years)", color = "Rescue Period")
