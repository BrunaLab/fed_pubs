



library(tidyverse)
library(janitor)

papers_need_authors <- read_rds("./data_clean/need_to_find_authors.rds") %>% 
  separate_wider_delim(filename,
                       delim = "_",
                       names = c("agency"),
                       too_many = "drop"
  ) %>% 
  mutate_all(tolower) %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(refID=as.numeric(refID))



papers_need_papers <- read_rds("./data_clean/need_to_find_papers.rds") %>% 
  separate_wider_delim(filename,
                       delim = "_",
                       names = c("agency"),
                       too_many = "drop"
  ) %>% 
  mutate_all(tolower) %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(refID=as.numeric(refID))

papers_need_papers %>% summarize(n=n_distinct(refID))

papers_need_authors$filename
SID<-papers_need_papers %>% select(SID) %>% unique()