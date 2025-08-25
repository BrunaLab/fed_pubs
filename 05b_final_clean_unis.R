library(janitor)
library(tidyverse)
library(progress)
library(fs)
# 
# read the files   --------------------------------------

papers_df<-read_csv("./data_raw/papers/all_papers_df_uni.csv")
affils_df<-read_csv("./data_raw/affils/all_affils_df_uni.csv")
authors_df<-read_csv("./data_raw/authors/all_authors_df_uni.csv")

# add univ affiliations ---------------------------------------------------
source("./code/ID_univ_affiliations.R")
affils_df<-ID_univ_affiliations(affils_df)


  
  # STANDARDIZE AUTHOR NAMES & ADDRESSES ------------------------------------
  source("./code/author_name_cleaner.R")
  authors_df<-author_name_cleaner(authors_df)
  
  
  

# final clean up ----------------------------------------------------------

  authors_df<-authors_df %>% 
    select(
      # -`@_fa`,
           -`afid.@_fa`) %>% 
    remove_empty(c("rows","cols")) %>% 
    distinct()
    
  
  affils_df<-affils_df %>% 
    select(
      # -`@_fa`,
           -document_count) %>% 
    remove_empty(c("rows","cols")) %>% 
    distinct()
  
  
  papers_df<-papers_df %>% 
    remove_empty(c("rows","cols")) %>% 
    distinct()
  
  
  
# SAVE CLEAN FILES --------------------------------------------------------

  
affils_df_uni<-affils_df %>% 
    select(affil_id,uni,affiliation) %>% 
    distinct(affil_id,.keep_all = TRUE) %>% 
    replace_na(list(uni="other")) %>% 
    arrange(uni)
  
authors_df<-authors_df %>% 
  left_join(affils_df_uni,by="affil_id") %>% 
  # mutate(focal_uni=if_else(is.na(uni),FALSE,TRUE)) %>% 
  relocate(uni,.before=1)



write_rds(papers_df,"./data_clean/papers_df_uni_clean.rds")
write_rds(authors_df,"./data_clean/authors_df_uni_clean.rds")
write_rds(affils_df_uni,"./data_clean/affils_df_uni_clean.rds")



