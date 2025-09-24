# into --------------------------------------------------------------------
# 
# This will take the super-file of all years together, 
# add the usgs articles, remove duplicates, and check / edit federal 
# affiliations


library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)





# IDENTIFY FEDERAL AFFILIATIONS -------------------------------------------


affils_df<-read_csv("./data_raw/affils/all_affils_df_fed.csv")
scopus_ids_searched<-read_csv("./data_clean/api_affils_searched_2025-09-01.csv")
source("./code/ID_fed_affiliations.R")
affils_df<-ID_fed_affiliations(affils_df,scopus_ids_searched)
 

# ADD FEDERAL AFFILIATIONS TO AUTHORS_DF ----------------------------------

authors_df<-read_csv("./data_raw/authors/all_authors_df_fed.csv")
papers_df<-read_csv("./data_raw/papers/all_papers_df_fed.csv")

source("./code/ID_fed_authors.R")

authors_df<-ID_fed_authors(authors_df,affils_df)

# edits ---------------------------------------
# remove useless columns

authors_df<-authors_df %>% 
  select(-`afid.@_fa`
         # ,-authorID
         )

papers_df_trim<-
  papers_df %>% 
  filter(PY>2018) %>% 
  # filter(!is.na(DI)) %>% 
  filter(!is.na(DT)) %>% 
  tibble()
  
papers_df_trim<-papers_df_trim %>% 
  distinct(SO,PY,DI,TI,.keep_all = TRUE) %>% 
  mutate(PM=
         case_when(
           is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
           .default = as.numeric(PM)
         )
) %>% 
  mutate(PT=if_else(PT=="j","journal",PT))


dup_titles<-papers_df_trim %>%
  group_by(SO,PY,TI) %>% 
  tally() %>% 
  filter(n>1) %>% 
  arrange(desc(n))


unique_refid<-papers_df_trim %>% 
  tibble() %>% 
  select(refID)

# filter the authors and affils to the deduplicated refiD

authors_df_trim<-authors_df %>% 
  filter(refID %in% unique_refid$refID)

affils_df_trim<-affils_df %>% 
  filter(refID %in% unique_refid$refID)

# use ellefsen k.j. to check




# SAVE CLEAN FILES --------------------------------------------------------


affil_vector<-authors_df_trim %>% select(affil_id) %>% distinct()

affil_vector<-affils_df_trim %>% 
  filter(affil_id%in%affil_vector$affil_id) %>% 
  filter(federal==TRUE) %>% 
  select(affil_id,agency,agency_primary, federal) %>% 
  distinct()
  

affil_vector$affil_id<-as.character(affil_vector$affil_id)

authors_df_trim <-authors_df_trim  %>% 
left_join(affil_vector,by="affil_id") %>% 
  
  mutate(federal.y=if_else(federal.x==federal.y,NA,federal.y)) %>% 
  mutate(agency.y=if_else(agency.x==agency.y,NA,agency.y)) %>% 
  mutate(agency_primary.y=if_else(agency_primary.x==agency.y,NA,agency_primary.y)) %>% 
  rename(federal=federal.x,
         agency=agency.x,
         agency_primary=agency_primary.x) %>% 
  select(-federal.y,
         -agency.y,
         -agency_primary.y) %>% 
  relocate(c(federal, agency,agency_primary),.before=1)  

authors_df_trim<-authors_df_trim %>% distinct()

# use ellefsen kj to check

# use ellefsen k.j. to check

source("./code/add_missing_usgs.R")
updated_dfs<-add_missing_usgs(papers_df_trim,authors_df_trim)

papers_df_trim<-updated_dfs$papers
authors_df_trim<-updated_dfs$authors
authors_df_trim<-authors_df_trim %>% 
  select(
    -PY,
    -country,
    -state,
    -city,
    -authorID)
#  fix usgs affils

source("./code/fix_usgs_affils.R")
updated_authors<-fix_usgs_affils(authors_df_trim,papers_df_trim)
authors_df_trim<-updated_authors



# There are still some that are showing up as FEDERAL  in some articles but 
# not others, eg because USGS Coop Units are coded as university and not usgs by 
# scopus. This takes those that have records coded as both fed and non-fed and
# changes them all to fed.

# tandf<-authors_df_trim %>% select(SID,federal) %>% distinct() %>% group_by(SID) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)

FED<-authors_df_trim %>% 
  filter(federal==TRUE) %>% 
  select(SID,federal,agency,agency_primary) %>% 
  distinct()

NONFED<-authors_df_trim %>% 
  filter(federal!=TRUE|is.na(federal)) %>% 
  select(SID,federal,agency,agency_primary) %>% 
  distinct()

need_to_fix<-inner_join(NONFED,FED,by="SID") %>% 
  distinct(SID,.keep_all = TRUE) %>% 
  filter(!is.na(agency_primary.y)) %>% 
  select(SID,federal=federal.y,
         agency=agency.y,
         agency_primary=agency_primary.y)

authors_df_trim<-authors_df_trim %>% 
  left_join(need_to_fix,by="SID") %>% 
  relocate(federal.y,.after=federal.x) %>% 
  relocate(agency.y,.after=agency.x) %>% 
  relocate(agency_primary.y,.after=agency_primary.x) %>% 
  mutate(federal.x=coalesce(federal.y,federal.x)) %>% 
  mutate(agency.x=coalesce(agency.x,agency.y)) %>% 
  mutate(agency_primary.x=coalesce(agency_primary.x,agency_primary.y)) %>% 
  select(-agency_primary.y,
         -agency.y,
         -federal.y) %>% 
  rename(agency_primary=agency_primary.x,
         agency=agency.x,
         federal=federal.x) %>% 
  mutate(federal=if_else(is.na(federal),FALSE,federal))



# chose publication types or titles to remove -----------------------------

# unique(papers_df$DT)
# ARTICLE TYPES - KEEP
# "book chapter"
# "article"
# "review"
# "note"
# "data paper"
# ARTICLE TYPES - REMOVE
# "letter" # excluded below
# "editorial" # excluded below" 

# papers_df %>% filter(DT=="editorial") %>% select(TI)
# authors_df %>% filter(is.na(agency)) %>% group_by(federal) %>% tally()

papers_cat<-c("article","book chapter","data paper","note","review")

papers_df_trim <- papers_df_trim %>% 
  filter(DT%in%papers_cat) 

authors_df<-authors_df_trim %>% 
  filter(refID%in%papers_df_trim$refID)

rm(papers_cat)

# this calclulates how many papers have authors from each institution
papers_by_agency<-authors_df_trim %>% 
  select(refID,agency_primary,author_order) %>% 
  group_by(refID) %>% 
  count(agency_primary) %>% 
  pivot_wider(names_from = agency_primary, values_from = n) %>% 
  replace(is.na(.), 0)

# But wait! SCOPUS returned articles that included focal agency 
# as any affiliation (2nd, 3rd, etc). for instance for refID = 60000650_2021-13,
# the primary affiliation of all authors is 'Brigham and Women's Hospital' but 
# harvard medical school or harvard college are secondary affiliations.
# Need to 2x these to make sure should keep (i.e.) do we treat BW, Mass General
# etc as harvard or not. 
# Decision: NO primary affil only.


NA_only_pubs<-papers_by_agency %>% 
  ungroup() %>% 
  rowwise() %>%
  mutate(flag = `NA` > 0 && all(c_across(-c(refID, `NA`)) == 0)) %>%
  ungroup() %>% 
  filter(flag==TRUE)

# Filter out the ones where fed isn't primary affil
authors_df_trim<-authors_df_trim %>% filter(!refID%in%NA_only_pubs$refID) 

papers_df_trim<-papers_df_trim %>% filter(!refID%in%NA_only_pubs$refID) 

# can 2x they were removed here: 
# papers_by_agency2<-authors_df_trim %>% 
#   select(refID,agency_primary,author_order) %>% 
#   group_by(refID) %>% 
#   count(agency_primary) %>% 
#   pivot_wider(names_from = agency_primary, values_from = n) %>% 
#   replace(is.na(.), 0)

# NA_only_pubs2<-papers_by_agency2 %>%
#   ungroup() %>%
#   rowwise() %>%
#   mutate(flag = `NA` > 0 && all(c_across(-c(refID, `NA`)) == 0)) %>%
#   ungroup() %>%
#   filter(flag==TRUE)



write_rds(papers_df_trim,"./data_clean/papers_df_clean.rds")
write_rds(authors_df_trim,"./data_clean/authors_df_clean.rds")
write_rds(affils_df_trim,"./data_clean/affils_df_clean.rds")


