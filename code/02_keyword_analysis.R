

# load libraries ----------------------------------------------------------

library(tidyverse)
# library(stringr)
# library(stopwords)
# library(ngram)
# library(tidytext)
# library(igraph)
library(janitor)
# library(tidystringdist)
# # tutorial
# https://www.youtube.com/watch?v=FbznaCOXbcU


# load data ---------------------------------------------------------------


keywords_all
keywords_2024 %>% group_by(original) %>% tally() %>% arrange(desc(n))
keywords_2025 %>% group_by(original) %>% tally() %>% arrange(desc(n))


# Load reference records, clean keywords and text of DE and titles -----------------
pubs$TI
complete_data <- read_csv("./data/data_clean/complete_data_clean.csv") %>%
  mutate(TI = gsub(" - ", "-", TI)) %>%
  mutate(refID = paste(refID, PY, sep = "-")) %>%
  relocate(refID, .before = 1)