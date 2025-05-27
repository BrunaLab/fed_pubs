library(janitor)
library(tidyverse)
library(progress)
library(fs)

# bind the subfiles for each category and check for dupes ------------------

data_dir_affils<-"./data_raw/affils"
csv_files_affils <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")
affils_df <- csv_files_affils %>%
  map_dfr(~ read_csv(.x))


data_dir_papers<-"./data_raw/papers"
csv_files_papers <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")
papers_df <- csv_files_papers %>%
    map_dfr(~ {
      df <- read_csv(.x) 
      # Convert all columns to character type BEFORE returning
      df <- df %>% 
        mutate_all(as.character)
      
      return(df)  
    })


data_dir_authors<-"./data_raw/authors"
csv_files_authors <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")
authors_df <- csv_files_authors %>%
  map_dfr(~ read_csv(.x))


# standardize the csv column names   --------------------------------------


# names(affils_df)
source("./code/name_standardizer.R")

# ----- affils
  
affils_df<-names_standardizer(affils_df) %>%
  ungroup() %>%
  select(-"@_fa") %>% 
  distinct(affil_id,affiliation,city,country,.keep_all = TRUE) %>% 
  mutate_all(tolower) %>% 
  mutate(country=as.factor(country),
         city=as.factor(city)) %>% 
  mutate(country=
           case_when(
             country == "united states" ~ "usa",
             .default = as.character(country)
             )
         ) %>% 
  remove_empty(c("rows", "cols"))
  
write_rds(affils_df,"./data_intermediate/affils_df.rds")

# ----- papers

papers_df<-names_standardizer(papers_df) %>% 
  ungroup() %>%
  select(-"@_fa") %>% 
  # group_by(scopus_article_id,SO,TI) %>% 
  # tally() %>% 
  # arrange(desc(n)) %>% 
  distinct(scopus_article_id,SO,TI,.keep_all = TRUE) %>% 
  mutate_all(tolower) %>% 
  remove_empty(which = c("rows", "cols"))
        
write_rds(papers_df,"./data_intermediate/papers_df.rds")

# ----- authors 

authors_df<-names_standardizer(authors_df) %>%
  ungroup() %>%
  select(-"@_fa",
         -"afid.@_fa") %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  mutate_all(tolower)

write_rds(authors_df,"./data_intermediate/authors_df.rds")

# add refID ---------------------------------------------------------------
  
papers_df$refID <- seq.int(nrow(papers_df))  
papers_df<-papers_df %>% relocate(refID,.before=1)


# bring over the refiID from papers to authors ----------------------------
paper_ID_nos<-papers_df %>% select(refID,source,entry_no)

authors_df<-left_join(authors_df,paper_ID_nos) %>% 
  relocate(refID,.before=1)
affils_df<-left_join(affils_df,paper_ID_nos) %>% 
  relocate(refID,.before=1)
   
rm(paper_ID_nos)
  
# cleanup -----------------------------------------------------------------
  

source("./code/papers_df_cleanup.R")
papers_df<-papers_df_cleanup(papers_df)


# IDENTIFY FEDERAL AFFILIATIONS -------------------------------------------
  source("./code/ID_fed_affiliations.R")
affils_df_temp<-ID_fed_affiliations(affils_df)
  
# summary <- affils_df_temp %>% 
# group_by(agency) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% 
#   drop_na()

  affils_df<-affils_df_temp
  rm(affils_df_temp)
  
#   
# # this will make sure there is only one row, the "alternative names) will all be spread out
# 
# affils_df <-affils_df %>%
#   group_by(refID,affil_id, federal,agency, entry_no, city, country) %>%
#   summarize(
#     affil_name_scopus = paste(unique(affil_name_scopus), collapse = "; "),
#     alt_affiliation_name = paste(unique(alt_affiliation_name[!is.na(alt_affiliation_name)]), collapse = "; "),
#     .groups = "drop"
#   )
  

# affils_df2 %>% group_by(affil_id) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)

  

# ADD FEDERAL AFFILIATIONS TO AUTHORS_DF ----------------------------------

    source("./code/ID_fed_authors.R")
  
  authors_df<-ID_fed_authors(authors_df,affils_df)
  
  


# separate pubs with & without DOI ----------------------------------------

  pubs_no_doi<-papers_df %>% 
    filter(is.na(DI)) %>% 
    distinct(source,SO,PY,TI,.keep_all=TRUE)
  
  pubs_no_doi_dupes<-pubs_no_doi %>% 
    group_by(source,SO,PY,TI) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  
  pubs_with_doi<-papers_df %>% 
    filter(!is.na(DI)) %>% 
    distinct(source,DI,SO,PY,PG,TI,.keep_all=TRUE)
  
  pubs_with_doi_dupes<-pubs_with_doi %>% 
    group_by(source,DI,SO,PY,PG,TI) %>% 
    filter(!is.na(DI)) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  # sum(pubs_with_doi_dupes$n)  
  
  
  papers_df<-bind_rows(pubs_no_doi,pubs_with_doi)
  
  
  rm(pubs_no_doi,
     pubs_with_doi,
     pubs_with_doi_dupes,
     pubs_no_doi_dupes)
  
  
  authors_df<-authors_df %>% distinct(
                       source,
                       author_order,
                       author_url,
                       SID,
                       AF,
                       surname,
                       given_name,
                       first_middle_initials,
                       affil_id,
                       OI,
                       federal,
                       .keep_all=TRUE)
  
  # affils_df<-affils_df[!duplicated(affils_df), ]
  
  
  
  # STANDARDIZE AUTHOR NAMES & ADDRESSES ------------------------------------
  source("./code/author_name_cleaner.R")
  authors_df<-author_name_cleaner(authors_df)
  
  

  # ID PAPERS WITHOUT AUTHORS OR AUTHORS WITHOUT PAPERS ---------------------  
  
papers_df %>% summarize(n_distinct(refID))
authors_df %>% summarize(n_distinct(refID))
setdiff((papers_df$refID),(authors_df$refID))
setdiff((authors_df$refID),(papers_df$refID))



papers_df$refID<-as.character(papers_df$refID)
authors_df$refID
from_papers<-
  anti_join(papers_df %>% select(refID),
            unique(authors_df %>% select(refID)))

need_to_find_authors<- semi_join(papers_df,from_papers,by="refID")

from_authors<-
  anti_join(unique(authors_df %>% select(refID)),
            papers_df %>% select(refID))

need_to_find_papers<- semi_join(authors_df,from_authors,by="refID")
names(need_to_find_papers)

papers_df<-anti_join(papers_df,from_papers,by="refID")
authors_df<-anti_join(authors_df,from_authors,by="refID")

rm(from_papers,from_authors)

papers_df$PY<-papers_df$PY2
papers_df$PY<-as.numeric(papers_df$PY)
papers_df$PY2<-NULL

PY_binder<-papers_df %>% select(refID,PY,PM)

authors_df<-left_join(authors_df,PY_binder,by="refID")
affils_df$refID<-as.character(affils_df$refID)
affils_df<-left_join(affils_df,PY_binder)

names(PY_binder)
names(affils_df)
affil_binder<-affils_df %>% select(affil_id,federal,agency)
affil_binder$affil_id<-as.character(affil_binder$affil_id)
authors_df<-left_join(authors_df,affil_binder)
# 2x to make sure all federal have agency

write_rds(papers_df,"./data_intermediate/papers_df_preusgs.rds")
write_rds(authors_df,"./data_intermediate/authors_df_preusgs.rds")
write_rds(affils_df,"./data_intermediate/affils_df_preusgs.rds")

write_rds(need_to_find_authors,"./data_intermediate/need_to_find_authors.rds")
write_rds(need_to_find_papers,"./data_intermediate/need_to_find_papers.rds")



# MERGE WITH USGS ---------------------------------------------------------



source("./code/merge_with_usgs.R")
usgs_results<-merge_with_usgs(authors_df,papers_df)
papers_df<-usgs_results$papers
authors_df<-usgs_results$authors


# SAVE CLEAN FILES --------------------------------------------------------



authors_df %>% summarize(n=n_distinct(refID))
authors_df %>%  filter(federal==TRUE) %>% summarize(n=n_distinct(refID))


authors_df<-authors_df %>% relocate(c(federal, agency),.before=1)


write_rds(papers_df,"./data_clean/papers_df_clean.rds")
write_rds(authors_df,"./data_clean/authors_df_clean.rds")
write_rds(affils_df,"./data_clean/affils_df_clean.rds")

