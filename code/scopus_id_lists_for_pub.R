library(tidyverse)

# original search ---------------------------------------------------------

scopus_id_1<-read_csv("./data_raw/agencies/agencies_redux_clean.csv")
scopus_id_2<-read_csv("./data_raw/agencies/agencies_orig_clean.csv")
scopus_id_total<-bind_rows(scopus_id_2,scopus_id_1) %>%  
  distinct(affil_id,.keep_all = TRUE) 

write_csv(scopus_id_total, "./data_clean/for_pub/scopusid_orig_srch.csv")



fed_affils1 <- read_csv("./data_raw/agencies/agency_redux.csv") %>%
  rename(affil_id = scopus_code) %>%
  distinct() %>%
  mutate(df = "redux")
fed_affils2 <- read_csv("./data_raw/agencies/agencies_redux_clean.csv") %>%
  distinct() %>%
  mutate(df = "redux")
fed_affils3 <- read_csv("./data_raw/agencies/agencies_orig_clean.csv") %>%
  distinct()
fed_affils_search <- fed_affils1 %>%
  full_join(fed_affils2) %>%
  full_join(fed_affils3, by = c("affil_id", "affiliation"))

fed_affils_search$affiliation <- replace_non_ascii(fed_affils_search$affiliation)
fed_affils_search<-fed_affils_search %>%
  mutate(city.x = if_else(is.na(city.x), city.y, city.x)) %>%
  mutate(city.y = if_else(city.x == city.y, NA, city.y)) %>%
  mutate(df.y = if_else(is.na(df.y), df.x, df.y)) %>%
  select(
    -pub_count.x,
    -pub_count.y,
    -country.x,
    -country.y,
    -df.x,
    -city.y
  ) %>%
  mutate(agency = if_else(affil_id == 60138915, "doe", agency)) %>%
  distinct(affil_id, agency, affiliation, city.x, df.y) %>%
  rename(
    city = `city.x`,
    search = `df.y`
  ) %>%
  mutate(affil_id = as.factor(affil_id)) %>% 
  mutate(cat="search")