library(janitor)
library(tidyverse)
library(progress)
library(fs)




cat<-"uni"
date<-"20250901"


# cat<-"uni"
# date<-"20251010"



# read the files   --------------------------------------

# papers_df<-read_csv("./data_raw/papers/all_papers_df_uni.csv")
papers_df<-read_csv(paste("./data_raw/uni_",date,"/","all_papers_df_uni.csv",sep=""))

# affils_df<-read_csv("./data_raw/affils/all_affils_df_uni.csv")
affils_df<-read_csv(paste("./data_raw/uni_",date,"/","all_affils_df_uni.csv",sep=""))


# authors_df<-read_csv("./data_raw/authors/all_authors_df_uni.csv")
authors_df<-read_csv(paste("./data_raw/uni_",date,"/","all_authors_df_uni.csv",sep=""))


# add univ affiliations ---------------------------------------------------
source("./code/ID_univ_affiliations.R")
affils_df<-ID_univ_affiliations(affils_df)




# # search scopus for any remaining -----------------------------------------
# 
# 
# library(tidyverse)
# library(rscopus)
# # Define the folder path
# 
# api<-"---"
# 
# 
# focal_cities<-c(
#   "san diego",
#   "san francisco",
#   "los angeles",
#   "palo alto",
#   "cambridge",
#   "gainesville",
#   "columbus",
#   "chapel hill",
#   "seattle",
#   "minneapolis",
#   "st. paul",
#   "ann arbor"
# )
# 
# to_search<-affils_df %>% 
#   filter(is.na(uni)) %>% 
#   filter(city%in%focal_cities) %>% 
#   filter(country!="uk") %>% 
#   select(affil_id) %>% 
#   distinct()
# 
# search_term <- to_search$affil_id
# # search_term <- "109556305"
# term <- seq_along(search_term)
# all_affils<-data.frame()
# for (h in term){
#   
#   query_string <-search_term[h]
#   
#   
#   scopus_data_raw <- tryCatch({
#     get_affiliation_info(
#       affil_id = query_string,
#       api_key = api,
#       verbose = TRUE
#     )
#   }, error = function(e) {
#     message("Error retrieving data for affil_id: ", query_string)
#     return(NULL)
#   })
#   
#   
#   # Skip if the result is NULL or empty
#   if (is.null(scopus_data_raw)){
#     next
#   }else{
#     all_affils <- bind_rows(all_affils, scopus_data_raw)
#   }
# }
# 
# 
# scopus_info_uni_affils<-all_affils %>% 
#   select("affil_id",
#          affiliation="affil_name",
#          pub_count="document-count",
#          "city",
#          "country",
#          "document_count"=`document-count`) %>% 
#   distinct() %>% 
#   mutate(affil_id=as.numeric(affil_id))
# 
# 
# scopus_info_uni_affils<-scopus_info_uni_affils %>% 
#   mutate(pub_count=as.numeric(pub_count)) %>% 
#   arrange(city,desc(pub_count)) %>% 
#   select(-document_count)
# 
# write_csv(scopus_info_uni_affils,paste("./data_raw/affiliations_to_search/follow_up/na_uni_to_check_",Sys.Date(),".csv",sep=""))



  
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
    # select(
    #   # -`@_fa`,
    #        -document_count) %>% 
    remove_empty(c("rows","cols")) %>% 
    distinct()
  
  
  papers_df<-papers_df %>% 
    remove_empty(c("rows","cols")) %>% 
    distinct()
  
  
  
  

# duplicates --------------------------------------------------------------


  
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
               # those without a PM early online. one option is to randomly allocate:
               # is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
               # the other option is to put all in 12
               is.na(PM) ~ 12,
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
  

  
  
  # SAVE CLEAN FILES --------------------------------------------------------
  
  
  affil_vector<-authors_df_trim %>% select(affil_id) %>% distinct()
  
  affil_vector<-affils_df_trim %>% 
    filter(affil_id%in%affil_vector$affil_id) %>% 
    select(affil_id,affiliation,uni) %>% 
    distinct()
  
  
  # affil_vector$affil_id<-as.character(affil_vector$affil_id)
  
  authors_df_trim <-authors_df_trim  %>% 
    left_join(affil_vector,by="affil_id") 
  
  authors_df_trim<-authors_df_trim %>% distinct()
  
  
  
  
  
  
  
  
  
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
  papers_by_uni<-authors_df_trim %>% 
    select(refID,uni,author_order) %>% 
    group_by(refID) %>% 
    count(uni) %>% 
    pivot_wider(names_from = uni, values_from = n) %>% 
    replace(is.na(.), 0)
  
  # But wait! SCOPUS returned articles that included focal agency 
  # as any affiliation (2nd, 3rd, etc). for instance for refID = 60000650_2021-13,
  # the primary affiliation of all authors is 'Brigham and Women's Hospital' but 
  # harvard medical school or harvard college are secondary affiliations.
  # Need to 2x these to make sure should keep (i.e.) do we treat BW, Mass General
  # etc as harvard or not. 
  # Decision: NO primary affil only.
  
  
  NA_only_pubs<-papers_by_uni %>% 
    ungroup() %>% 
    rowwise() %>%
    mutate(flag = `NA` > 0 && all(c_across(-c(refID, `NA`)) == 0)) %>%
    ungroup() %>% 
    filter(flag==TRUE)
  
  # Filter out the ones where fed isn't primary affil
  authors_df_trim<-authors_df_trim %>% filter(!refID%in%NA_only_pubs$refID) 
  
  papers_df_trim<-papers_df_trim %>% filter(!refID%in%NA_only_pubs$refID) 
  
  
  
  
  

# end dedupe --------------------------------------------------------------

    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# SAVE CLEAN FILES --------------------------------------------------------

  
affils_df_uni<-affils_df_trim %>% 
    select(affil_id,uni,affiliation) %>% 
    distinct(affil_id,.keep_all = TRUE) %>% 
    replace_na(list(uni="other")) %>% 
    arrange(uni)
  

  
  
  
  
  
  
  # write_rds(papers_df_trim,"./data_clean/papers_df_uni_clean.rds")
  write_rds(papers_df_trim,paste("./data_clean/papers_df_clean","_",cat,"_",date,".rds",sep=""))
  
  # write_rds(authors_df_trim,"./data_clean/authors_df_uni_clean.rds")
  write_rds(authors_df_trim,paste("./data_clean/authors_df_clean","_",cat,"_",date,".rds",sep=""))
  
  # write_rds(affils_df_uni,"./data_clean/affils_df_uni_clean.rds")
  write_rds(affils_df_uni,paste("./data_clean/affils_df_clean","_",cat,"_",date,".rds",sep=""))
  




