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


# 
fed_pubs<-readRDS("./data_clean/analysis_fed_pubs.rds")
source("./code/keyword_splitter.R")
keywords_all <- keyword_splitter(fed_pubs)
# pubs<-all_scopus_api[[1]]
# authors<-all_scopus_api[[2]]


# Load reference records, clean keywords and text of DE and titles -----------------

# complete_data <- read_csv("./data/data_clean/complete_data_clean.csv") %>%
# fed_pubs<-readRDS("./data_clean/analysis_fed_pubs.rds")
#   mutate(TI = gsub(" - ", "-", TI)) %>%
#   mutate(refID = paste(refID, PY, sep = "-")) %>%
#   relocate(refID, .before = 1)
# # %>%
# # replace_na(list(pub_cat_2 = "temperate"))
# unique(complete_data$SO)
# unique(complete_data$pub_cat_2)
# unique(complete_data$jrnl_cat)
#
# complete_data %>% group_by(refID) %>% tally() %>% arrange(desc(n))


# tropical_data<-complete_data %>%
#   filter(SO=="bitr"|SO=="jte"|SO=="rbt"|SO=="tcs"|SO=="trec")


# split KWs up & arrange as column ----------------------------------------

source("./code/keyword_splitter.R")
keywords_all <- keyword_splitter(combined_data_papers)



pubs2025<-combined_data_papers %>% filter(PY==2025)
pubs2024<-combined_data_papers %>% filter(PY==2024)
pubs2019<-combined_data_papers %>% filter(PY==2019)

keywords_2025 <- keyword_splitter(pubs2025)
keywords_2024 <- keyword_splitter(pubs2024)
keywords_2019 <- keyword_splitter(pubs2019)

keywords_all <- distinct(keywords_all, refID, original, .keep_all = TRUE)
keywords_2025 <- distinct(keywords_2025, refID, original, .keep_all = TRUE)
keywords_2024 <- distinct(keywords_2024, refID, original, .keep_all = TRUE)
keywords_2019 <- distinct(keywords_2019, refID, original, .keep_all = TRUE)
# keywords_tropical_jrnls<-keyword_splitter(tropical_data)
write_csv(keywords_all, "./data_clean/keywords_all.csv")

# write_csv(keywords_all, "./bibliometrics/data_intermediate/keywords_tropical.csv")


# N DISTINCT KEYWORDS

keywords_all %>% 
  select(original) %>% 
  summarize(n=n_distinct(original))




top_100<-
keywords_all %>% 
  group_by(original) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_head(n=100)


top_100_2025<-
  keywords_2025 %>% 
  group_by(original) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_head(n=100)
top_100_2025

top_100_2024<-
  keywords_2024 %>% 
  group_by(original) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_head(n=100)
top_100_2024

top_100_2019<-
  keywords_2019 %>% 
  group_by(original) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_head(n=100)
top_100_2019


# ID and remove the kw that didnt split up--------------------------------------
unsplit_kw <- keywords_all %>%
  select(refID, original) %>%
  mutate(nchar = nchar(original)) %>%
  arrange(desc(nchar)) %>%
  filter((nchar > 57) |
    str_detect(original, "special feature") |
    str_detect(original, "funding was provided") |
    str_detect(original, "international journal of tropical") |
    str_detect(original, "funding was provided") |
    str_detect(original, "este trabajo"))


# remove them

keywords_all <- anti_join(keywords_all, unsplit_kw, by = "refID")
keywords_all <- keywords_all %>% mutate_all(trimws)
# rm(complete_data)

# editing keywords --------------------------------------------------------



kw_summary <- keywords_all %>%
  group_by(original) %>%
  tally() %>%
  arrange(desc(n))
kw_summary

source("./code/code_data_cleaning/keyword_editor.R")
kw_summary <- keyword_editor(kw_summary)


write_csv(kw_summary, "./data/data_intermediate/keywords_edited.csv")


kw_summary <- kw_summary %>%
  filter(!str_detect(edited, "funding was provided by grants from the academy")) %>%
  filter(!str_detect(edited, "este trabajo es parte del trabajo doctoral de la autora")) %>%
  filter(!str_detect(edited, "atom percent excess")) %>%
  filter(!str_detect(edited, "fruit census fruit trap collection")) # need to fix this one

kw_summary <- kw_summary %>% filter(nchar(edited) > 0)
kw_summary <- kw_summary %>% mutate_all(trimws)

kw_summary %>%
  group_by(edited) %>%
  tally() %>%
  arrange(desc(n))


# remove plurals ----------------------------------------------------------

source("./code/code_data_cleaning/keyword_depluralizer.R")
kw_summary <- keyword_depluralizer(kw_summary)
write_csv(kw_summary, "./data/data_intermediate/keywords_no_plurals.csv")

kw_summary %>% summarise_at(vars(edited, singular), n_distinct, na.rm = TRUE)
#
# kw_summary %>%
#   group_by(singular) %>%
#   tally() %>%
#   arrange(desc(n))


# summary(keywords$original==keywords$final)

# temporary save/reload point ---------------------------------------------
#
# write_csv(keywords, file = "./bibliometrics/data_intermediate/kw_int.csv")
# # keywords<-read_csv("./bibliometrics/data_intermediate/kw_int.csv")



source("./code/code_data_cleaning/keyword_stopword_remover.R")
kw_summary <- keyword_stopword_remover(kw_summary)
write_csv(kw_summary, "./data/data_intermediate/keywords_no_stopwords.csv")



kw_summary %>% summarise_at(vars(edited, singular, final), n_distinct, na.rm = FALSE)

#
# kw_counts<-kw_summary %>%
#   accross_all() %>%
#   # group_by(edited) %>%
#   n_distinct() %>%
#   # arrange((final))
#   arrange(desc(n))
# kw_summary


# final check for plurals ------------------------------------------------------------
# look for last s

source("./code/code_data_cleaning/keyword_s_word_finder.R")
s_words <- keyword_s_word_finder(kw_summary)
s_words
write_csv(s_words, file = "./data/data_intermediate/s_words.csv")

# write_csv(keywords, file = "./bibliometrics/data_intermediate/kw_int_no_plurals.csv")
# keywords<-read_csv("./bibliometrics/data_intermediate/kw_int_no_plurals.csv")


kw_summary %>%
  select(final) %>%
  group_by(final) %>%
  tally() %>%
  arrange(desc(n))
kw_summary


keywords_all <- left_join(keywords_all, kw_summary) %>% select(-n)
keywords_all_summary <- keywords_all %>%
  group_by(final) %>%
  tally() %>%
  arrange(desc(n))
keywords_all_summary


# dupes_kw_summary<-get_dupes(keywords_all_summary)
# kw_summary<-distinct(kw_summary,refID,original, .keep_all = TRUE)


# complete_data <- read_csv("./bibliometrics/data_clean/complete_data_clean.csv") %>%
#   relocate(refID,.before=1)
pub_cat_2 <- complete_data %>% select(refID, pub_cat_2, jrnl_cat, SO, PY)
#
# %>%
#   group_by(refID) %>%
#   tally() %>%
#   arrange(desc(n))
#


keywords_all <- left_join(keywords_all, pub_cat_2)
keywords_all[264828, ]
pub_cat_2[49962, ]
keywords_all %>% filter(refID == "1-2016")
keywords_all %>% filter(refID == "93-88-1998")


dupes <- get_dupes(keywords_all)
keywords_all <- distinct_all(keywords_all)


# remove all accents, convert to LATIN-ASCII ------------------------------


library(stringi)
keywords_all$final <- stri_trans_general(str = keywords_all$final, id = "Latin-ASCII")



# Final check for duplicates.  --------------------------------------------
# somekeywords are listed like this: "El Nino SOuthern Oscillation, ENSO". They
# were separasted at the comma, then the long form was abbreviated, meaning the
# record now has ENSO 2x. It's not many, but this will remove them.

keywords_all %>%
  group_by(final, refID) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

keywords_all <- keywords_all %>%
  distinct(refID, final, .keep_all = TRUE)

write_csv(keywords_all, "./data/data_archive/keywords.csv")
# KW PROCESSING ENDS HERE -------------------------------------------------

source("./code/code_data_cleaning/keyword_similarity_checker.R")
similarity <- keyword_stopword_remover(kw_summary)
