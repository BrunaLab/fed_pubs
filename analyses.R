# load libraries ----------------------------------------------------------



library(gghighlight)
library(tidyverse)
library(janitor)

# load data ---------------------------------------------------------------

fed_authors <- readRDS("./data_clean/analysis_fed_pubs.rds") 
glimpse(fed_authors)
# %>%
#   mutate(
#     refID = as.numeric(refID),
#     PY = as.numeric(PY),
#     PM = as.numeric(PM)
#   )

# head(fed_authors, n = 10)
all_papers <- read_rds("./data_clean/scopus_papers.rds") 

# %>%
#   mutate(
#     refID = as.numeric(refID),
#     PY = as.numeric(PY),
#     PM = as.numeric(PM)
#   )
# head(all_papers, n = 10)
# names(fed_authors)



#  check affiliations -----------------------------------------------------


# Scopus Affiliation numbers
scopus_affils_df_FED <- fed_authors %>%
  filter(fed_author == TRUE) %>%
  select(affil_id, affiliation) %>%
  distinct() %>% 
  tibble()

scopus_affils_df_NOTFED <- fed_authors %>%
  filter(fed_author == FALSE) %>%
  select(affil_id, affiliation, country) %>%
  distinct() %>% 
  tibble()

# paper totals ------------------------------------------------------------


total_pubs <- all_papers %>%
  distinct(refID) %>%
  tally()
total_pubs

# papers by yr
pubs_yr <- all_papers %>%
  group_by(PY) %>%
  tally() %>%
  arrange(PY)
pubs_yr

# papers by month year
pubs_mo_yr <-
  all_papers %>%
  group_by(PM, PY) %>%
  tally() %>%
  arrange(PY, PM)
pubs_mo_yr


# author_totals -----------------------------------------------------------



## Total_authors (fed+non)
total_authors <- fed_authors %>%
  select(authorID) %>%
  distinct() %>%
  tally()
total_authors
## total fed authors
total_fed_authors <- fed_authors %>%
  filter(fed_author == TRUE) %>%
  select(authorID) %>%
  distinct() %>%
  tally()
total_fed_authors
## total non-fed authors
total_NOTfed_authors <- fed_authors %>%
  filter(fed_author == FALSE) %>%
  select(authorID) %>%
  distinct() %>%
  tally()
total_NOTfed_authors



# agency-comparisons ------------------------------------------------------
# for every pub, how many agencies on each
# with how many authors from each

agencies_author_count <- fed_authors %>%
  group_by(refID) %>%
  # distinct(agency,.keep_all = TRUE) %>%
  drop_na(agency) %>%
  select(refID, PY, PM, agency, fed_author) %>%
  mutate(fed_author = 1) %>%
  group_by(refID, PY, PM, agency) %>%
  summarize(n_agency_authors = sum(fed_author))
agencies_author_count

#  how many agencies per pub

agencies_per_pub <-
  agencies_author_count %>%
  group_by(refID) %>%
  tally() %>%
  arrange(desc(n))
agencies_per_pub

# Is there an author from an agency on a paper?
agency_authors <- fed_authors %>%
  group_by(refID) %>%
  distinct(agency, .keep_all = TRUE) %>%
  drop_na(agency) %>%
  select(refID, PY, PM, agency, fed_author) %>%
  mutate(fed_author = 1) %>%
  pivot_wider(names_from = agency, values_from = fed_author) %>%
  replace(is.na(.), 0) %>%
  # mutate(
  #   refID = as.factor(refID),
  #   PY = as.factor(PY),
  #   PM = as.factor(PM)
  # ) %>%
  arrange(PY, PM, refID)
agency_authors

names(agency_authors)


  
# agency_authors<- agency_authors %>% mutate(refID=as.factor(refID))
# agency_authors<- agency_authors %>% mutate(PY=as.factor(PY))
# agency_authors<- agency_authors %>% mutate(PM=as.factor(PM))

agency_authors<- agency_authors %>% 
  mutate(n_agencies = rowSums(pick(dod:ncua), na.rm = T)) %>%
  # rowwise() %>%
  # mutate(n_agencies = sum(c_across(where(is.numeric))), na.rm = TRUE) %>%
  # mutate(n_agencies = rowSums(across(where(is.numeric)))) %>%
  relocate(n_agencies, .after = "PM")
agency_authors

write_rds(agency_authors,"./data_clean/agency_authors.rds")

# Comparison of Jan-May publications each year,
# only "cover date" Jan-May included

yr_to_date1 <- agency_authors %>%
  group_by(PY, PM) %>%
  summarize(n_pubs = n_distinct(refID)) %>%
  filter(
    PM == 1|
    PM == 2|
    PM == 3|
    PM == 4|
    PM == 5
         ) %>%
  group_by(PY) %>%
  tally(n_pubs) %>%
  mutate(change_n = n - lag(n)) %>%
  arrange(desc(n)) %>%
  mutate(perc_change_from_previous_yr = ((change_n) / lag(n) * 100))
yr_to_date1



# Comparison of Jan-May publications each year,
# anything published in 2025 after may is added to may

yr_to_date2 <- agency_authors %>%
  group_by(PY, PM) %>%
  summarize(n_pubs = n_distinct(refID)) %>%
  mutate(PM = if_else(PY == "2025", "5", PM)) %>%
  filter(
    PM == 1|
      PM == 2|
      PM == 3|
      PM == 4|
      PM == 5
  ) %>%
  group_by(PY) %>%
  tally(n_pubs) %>%
  mutate(change_n = n - lag(n)) %>%
  arrange(desc(n)) %>%
  mutate(perc_change_from_previous = ((change_n) / lag(n) * 100))
yr_to_date2



















# agencies_on_pubs %>% summarize(n_pubs=n_distinct(refID))
# agencies_on_pubs %>% group_by(PY) %>% summarize(n_pubs=n_distinct(refID))  %>% arrange(desc(PY))
# agencies_on_pubs %>% group_by(PY,PM) %>% summarize(n_pubs=n_distinct(refID))  %>% arrange(desc(PY),desc(PM))
# agencies_on_pubs %>% group_by(PY,PM) %>% summarize(n_pubs=n_distinct(refID)) %>% filter(PM<5) %>% arrange(desc(PY),desc(PM))







# journals ----------------------------------------------------------------


journals <- all_papers %>%
  select(refID, SO, PY, PM) %>%
  mutate_all(tolower)


all_jrnls <- journals %>%
  group_by(SO) %>%
  tally() %>%
  arrange(desc(n)) 
all_jrnls


top_100_jrnls <- journals %>%
  group_by(SO) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 100)
top_100_jrnls


top_N_jrnls_peryr <-
  journals %>%
  group_by(PY, SO) %>%
  tally() %>%
  arrange(desc(PY), desc(n)) %>%
  group_by(PY) %>%
  slice_head(n = 15)
top_N_jrnls_peryr

journals %>%
  filter(SO == "science") %>%
  group_by(PY) %>%
  tally()


journals %>%
  filter(SO == "nature") %>%
  group_by(PY) %>%
  tally()

journals %>%
  filter(SO == "nature communications") %>%
  group_by(PY) %>%
  tally()
journals %>%
  filter(SO == "proceedings of the national academy of sciences of the united states of america") %>%
  group_by(PY) %>%
  tally()

names(agency_authors)
# papers per month x year per agency (no fractional authorship)
pubs_peryr_mo_per_agency <- agency_authors %>%
  ungroup() %>% 
  select(-refID) %>% 
  mutate(across("dod":"ncua",~if_else(.x==0,FALSE,TRUE))) %>% 
  # group_by(PY) %>%
  group_by(PY, PM) %>%
  summarise(across(everything(), ~ sum(., na.rm = TRUE), .names = "sum_{col}")) %>% 
  
  # summarize(across("dod":"ncua", ~ sum(.x, na.rm = TRUE), .names = "sum_{col}")) %>%
  filter(
    PM == 1|
      PM == 2|
      PM == 3|
      PM == 4|
      PM == 5
  ) 
pubs_peryr_mo_per_agency


agency_authors %>%
  # group_by(PY) %>%
  mutate(PM = if_else(PY == "2025", "5", PM)) %>%
  filter(
    PM == 1|
      PM == 2|
      PM == 3|
      PM == 4|
      PM == 5
  ) %>% 
  group_by(PY) %>%
  summarize(across("dod":"ncua", ~ sum(.x, na.rm = TRUE), .names = "sum_{col}"))


names(agencies_on_pubs)

# papers by yr and month per agency (no fractional)
pubs_yrmo_per_agency <- agency_authors %>%
  select(-n_agencies, -refID) %>%
  group_by(PY, PM) %>%
  summarise(across(where(is.numeric), sum))


# papers by yr per agency (no fractional)
pubs_yr_per_agency <- agency_authors %>%
  select(-n_agencies, -refID) %>%
  group_by(PY) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(!PY, names_to = "agency", values_to = "count")


# papers per agency (no fractional)
pubs_per_agency <- agency_authors %>%
  ungroup() %>%
  select(-n_agencies, -refID) %>%
  group_by(PY) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(!PY, names_to = "agency", values_to = "count") %>%
  group_by(agency) %>%
  summarize(n = sum(count)) %>%
  arrange(desc(n))
pubs_per_agency



### Papers per month per year 2019-2024

n_monthly <- agency_authors %>%
  ungroup() %>%
  select(refID, PY, PM) %>%
  distinct(refID, .keep_all = TRUE) %>%
  group_by(PY, PM) %>%
  tally() 

n_monthly <- n_monthly %>% 
  arrange(PY,as.numeric(PM)) %>% 
  ungroup() %>% 
  mutate(month=row_number())

n_monthly



levels(n_monthly$PY)<-as.numeric(levels(n_monthly$PY))
n_monthly<- n_monthly %>% mutate(PM=as.numeric(PM))


n_monthly %>%
  filter(month<78) %>% 

  mutate(PY=as.numeric(as.character(PY)))%>% 
  mutate(PM=as.numeric(as.character(month))) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  theme_classic() +
  # scale_x_continuous(n.breaks = 13, limits = c(1, 12)) +
  # scale_y_continuous(n.breaks = 20, limits = c(0, max(n_monthly %>% select(n)) + 500)) +
  # gghighlight(min(n) < 50)
  gghighlight(PY == 2025)


n_monthly %>%
  mutate(PY=as.numeric(as.character(PY)))%>% 
  mutate(PM=as.numeric(as.character(PM))) %>% 
  filter(PY < 2025) %>%
  ggplot(aes(x = PM, y = n, group = PY, color = PY)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  theme_classic() +
  scale_x_continuous(n.breaks = 13, limits = c(1, 12)) +
  # scale_y_continuous(n.breaks = 20, limits = c(0, max(n_monthly %>% select(n)) + 500)) +
  # gghighlight(min(n) < 50)
  gghighlight(PY == 2024)


# pubs by month 1-5
n_monthly %>%
  mutate(PY=as.numeric(as.character(PY)))%>% 
  mutate(PM=as.numeric(as.character(PM))) %>% 
  filter(PM < 6) %>%
  ggplot(aes(x = PM, y = n, group = PY, color = PY)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  theme_classic() +
  scale_x_continuous(n.breaks = 5, limits = c(1, 5))+
  # scale_y_continuous(n.breaks = 15, limits = c(0, max(n_monthly %>% select(n)) + 500)) +
  # gghighlight(min(n) < 50)
  gghighlight(PY == 2025)


### Publications from Jan-Current Month by Year

n_monthly %>%
  mutate(PY=as.numeric(as.character(PY)))%>% 
  mutate(PM=as.numeric(as.character(PM))) %>% 
  # mutate(PM=if_else(PY==2025,5,PM)) %>% ######## THIS IS KEY
  filter(PM < 6) %>%
  group_by(PY) %>%
  summarize(n = sum(n))


# data<-yr_to_date1
data <- yr_to_date2

n_2024 <- data %>%
  filter(PY == 2024) %>%
  select(n)
n_2025 <- data %>%
  ungroup() %>%
  filter(PY == 2025) %>%
  select(n)
perc_2425 <- as.numeric((n_2025 - n_2024) / n_2024 * 100)

data %>%
  ggplot(aes(x = PY, y = n)) +
  geom_bar(stat = "identity") +
  expand_limits(y = 0) +
  theme_classic() +
  annotate(
    geom = "text", x = 2024, y = max(data$n) - 700, label = n_2024,
    color = "navyblue"
  ) +
  annotate(
    geom = "text", x = 2025, y = max(data$n) - 700, label = n_2025,
    color = "navyblue"
  ) +
  annotate(
    geom = "text", x = 2025, y = max(data$n) - 2000, label = paste("(", round(perc_2425, 2), "%)", sep = ""),
    color = "red"
  ) +
  scale_y_continuous(n.breaks = 20, limits = c(0, max(data %>% select(n)) + 500)) +
  scale_x_continuous(breaks = seq(2019, 2025, by = 1)) +
  gghighlight(PY == 2025)





inst_freq <- fed_authors %>%
  filter(fed_author == TRUE) %>%
  group_by(affiliation) %>%
  tally() %>%
  arrange(desc(n))

unit_freq <- fed_authors %>%
  group_by(unit) %>%
  filter(fed_author == TRUE) %>%
  tally() %>%
  arrange(desc(n))


agency_freq <- fed_authors %>%
  filter(fed_author == TRUE) %>%
  group_by(agency) %>%
  tally() %>%
  arrange(desc(n))



first_authors <- fed_authors %>%
  filter(fed_author == TRUE) %>%
  filter(author_order == 1) %>%
  drop_na(agency)
first_authors

last_authors <- fed_authors %>%
  group_by(refID) %>%
  slice_tail() %>%
  filter(author_order != 1) %>%
  filter(fed_author == TRUE) %>%
  drop_na(agency)
last_authors



all_author_positions <- fed_authors %>%
  filter(fed_author == TRUE) %>%
  group_by(refID) %>%
  drop_na(agency)
all_author_positions


agency_subset <- pubs_per_agency %>%
  filter(n > 1000) %>%
  select(agency)
agency_n_decline <-
  # all_author_positions %>%
  first_authors %>%
  # last_authors %>%
  filter(agency %in% agency_subset$agency) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  # filter(PM<6) %>%
  group_by(agency, PY) %>%
  tally() %>%
  group_by(agency) %>%
  mutate(decline_n = (n - lag(n))) %>%
  mutate(perc_previous = ((decline_n) / lag(n)) * 100)

foo <-
  agency_n_decline %>%
  drop_na() %>%
  filter(PY > 2023)

agency_n_decline %>%
  drop_na() %>%
  filter(PY > 2023) %>%
  ggplot(aes(x = PY, y = perc_previous, group = agency, color = agency)) +
  # ggplot(aes(x=PY,y=n, group=agency, color=agency)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = c(2019, 2025)) +
  # scale_y_continuous(limits = c(0, 2000))+
  gghighlight((perc_previous < 0))

agency_n_decline %>%
  drop_na() %>%
  filter(PY > 2023) %>%
  # filter(n>200) %>%
  mutate(PY = as.factor(PY)) %>%
  ggplot(aes(x = PY, y = perc_previous, fill = agency)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  facet_wrap(vars(agency))
# scale_x_continuous(breaks = c(2024, 2025))+
# scale_y_continuous(limits = c(0, 2000))+


agency_n_decline %>%
  drop_na() %>%
  filter(PY == 2025) %>%
  ungroup() %>%
  summarize(mean = mean(perc_previous))
#
# first authors: -68.6%
# last authors:  -68.7%
# any position:  -67.6%



agency_n_decline %>%
  drop_na() %>%
  filter(PY > 2023) %>%
  # filter(n>200) %>%
  mutate(PY = as.factor(PY)) %>%
  ggplot(aes(x = agency, y = perc_previous, fill = PY)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  facet_wrap(vars(PY))
# scale_x_continuous(breaks = c(2024, 2025))+
# scale_y_continuous(limits = c(0, 2000))+













no_feds <- fed_authors %>%
  group_by(refID) %>%
  tally(fed_author) %>%
  filter(n < 1) %>%
  select(refID)

no_feds2 <- fed_authors %>%
  filter(!refID %in% no_feds$refID) %>%
  select(affiliation, country) %>%
  unique() %>%
  arrange(country, affiliation)
