## Get data from https://github.com/neilcaithness/brexit/blob/master/R/scrape.R script

#### plot 1: vote breakdown for v against ####
graph <- mp %>% select(-V7, -V8) %>%
  gather(issue, vote, V1:V6) %>% 
  select(issue, vote) %>%
  group_by(issue) %>%
  count(vote)

## adding text to display percentage for / against to graph
pos_text_against <- filter(graph, vote == "against")$n / 600
pos_text_for <- filter(graph, vote == "for")$n / 600

graph$pos_text <- NA
graph$pos_text[graph$vote == "against"] <- pos_text_against
graph$pos_text[graph$vote == "for"] <- pos_text_for

## labels mapping variable to issue for xticks
graph_labels <- c( "V1" = "Commit to customs union",
                   "V2" = "Require a public vote", 
                   "V3" =  "Keep close relationship",
                   "V4" =  "UK in EFTA & EEA",
                   "V5" = "Revoke article 50"  ,
                   "V6" = "Leave the EU without deal" )   

## actual graph
ggplot(graph, aes(fct_reorder(issue, -n), n/600)) + 
  geom_col(aes(fill = vote)) +
  geom_text(data = filter(graph, vote == "against"), 
            aes(y = 1 - pos_text, 
                label = paste(round(n/600,2)*100),
                fontface  = "bold"),
            nudge_y = 0.15,
            color = "white") +
  geom_text(data = filter(graph, vote == "for"), 
            aes(y = pos_text, 
                label = paste(round(n/600,2)*100),
                fontface  = "bold"),
           nudge_y = -0.05,
            color = "white") +
  coord_flip() +
  scale_x_discrete(labels=graph_labels) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("",
                    labels = c("Against", "Abstain", "For"),
                    values = c("#ff5050", "#e6e6e6", "#00b33c")) +
  labs(x = "", y = "", 
       title = "Breaking Down the Parliament's Brexit Vote")


#### plot 2: all issues broken down by party ####
## plot 2: vote breakdown per party
graph2 <- mp %>% select(party, V3) %>%
  #gather(issue, vote, V3:V6) %>% 
  #select(party, issue, vote) %>%
  group_by(party, V3) %>%
  count()  %>%
  mutate(per = n / 600 )

graph2 <- mp %>% select(party, V1:V6) %>%
  gather(issue, vote, V1:V6) %>% 
  select(party, issue, vote) %>%
  group_by(party, issue, vote) %>%
  count()  %>%
  mutate(per = n / 600 )


top_party <- mp %>% group_by(party) %>% count() %>% arrange(desc(n)) %>% head(4)
top_party <- top_party$party

# "Leave the EU without a deal" )   

graph2 %>% 
  mutate(
    colourin = case_when(
      party == "Con" ~ "#003399",
      party == "Lab" ~ "#FF2B4F",
      party == "SNP" ~ "#fcab27",
      party == "LD" ~ "#3686d3", 
      T ~ "darkgray")
  ) %>%
  ggplot(aes(vote, per, fill = colourin)) + 
  geom_col() +
  scale_fill_identity("Party",
                      guide = "legend",
                      labels = c("Con", "LD", "SNP", "Labor", "Others")) +
  scale_x_discrete(labels= c("Against", "Abstain", "For")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", 
       title = "Breaking Down the Parliament's Brexit Vote") +
  facet_wrap(~ issue, labeller= labeller(issue = graph_labels))

