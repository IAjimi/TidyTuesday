

comic_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/comic_bechdel.csv')
comic_bechdel

issue_collaborators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/issue_collaborators.csv')
issue_collaborators

## Number Series + Writer combo that pass the Bechdel test
comic_bechdel %>%
  mutate(pass_bechel_num = if_else(pass_bechdel == 'yes', 1, 0)) %>%
  group_by(series, writer) %>%
  summarise(bechdel_per = mean(pass_bechel_num),
            n_issues = n()) %>%
  arrange(desc(n_issues))

## Relationship Bechdel & # Female Characters

## Behavior Diff: Female v Male Characters
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

characters %>%
  group_by(character) %>%
  summarise_if(is.numeric, mean) %>% View()
