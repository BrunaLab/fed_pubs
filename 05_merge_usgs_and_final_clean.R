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



# LOAD FILES -----------------------------------------------------------

# Define folder paths
# folder_path_affils <- "./data_raw/affils/year_files_uni"
# folder_path_papers <- "./data_raw/papers/year_files_uni"
# folder_path_authors <- "./data_raw/authors/year_files_uni"
# 
# folder_path_affils <- "./data_raw/affils/year_files_fed"
# folder_path_papers <- "./data_raw/papers/year_files_fed"
# folder_path_authors <- "./data_raw/authors/year_files_fed"


# 
# 
# # affil binder -----------------------------------------------------------
# 
# data_dir_affils<-folder_path_affils
# csv_files_affils_all <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")
# 
# 
# 
# dataDir <- data_dir_affils  # Update this path
# dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)
# 
# # Read and tag each file
# dt_list <- lapply(dataFls, function(file) {
#   dt <- fread(file, fill = TRUE)
#   dt[, source_file_combined := basename(file)]  # Add column with filename
#   return(dt)
# })
# 
# # Combine all tagged data tables
# affils_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
# rm(dt_list)
# 
# 
# # author binder -----------------------------------------------------------
# # data_dir_authors<-"./data_raw/scopus_api/unis_files/authors"
# data_dir_authors<-folder_path_authors
# csv_files_authors_all <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")
# 
# 
# dataDir <- data_dir_authors  # Update this path
# dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)
# 
# # Read and tag each file
# dt_list <- lapply(dataFls, function(file) {
#   dt <- fread(file, fill = TRUE)
#   dt[, source_file_combined := basename(file)]  # Add column with filename
#   return(dt)
# })
# 
# # Combine all tagged data tables
# authors_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
# rm(dt_list)
# 
# 
# 
# # papers binder -----------------------------------------------------------
# # data_dir_papers<-"./data_raw/scopus_api/unis_files/papers"
# data_dir_papers<-folder_path_papers
# csv_files_papers_all <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")
# 
# 
# dataDir <- data_dir_papers  # Update this path
# dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)
# 
# # Read and tag each file
# dt_list <- lapply(dataFls, function(file) {
#   dt <- fread(file, fill = TRUE)
#   dt[, source_file_combined := basename(file)]  # Add column with filename
#   return(dt)
# })
# 
# # Combine all tagged data tables
# papers_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
# rm(dt_list)


# folder_path_affils <- "./data_raw/affils/year_files_fed"
# folder_path_papers <- "./data_raw/papers/year_files_fed"
# folder_path_authors <- "./data_raw/authors/year_files_fed"


affils_df<-read_csv("./data_raw/affils/all_affils_df_fed.csv")
authors_df<-read_csv("./data_raw/authors/all_authors_df_fed.csv")
papers_df<-read_csv("./data_raw/papers/all_papers_df_fed.csv")

# IDENTIFY FEDERAL AFFILIATIONS -------------------------------------------

## NEED TO CONFIRM THE DOD CDC ESP. INTENRATIONAL

source("./code/ID_fed_affiliations.R")
affils_df<-ID_fed_affiliations(affils_df)
 

# find the remainder that aren't catching federal yet
# 
# check<-affils_df %>% 
#   filter(federal==FALSE) %>% 
#   
#   distinct(affil_id,.keep_all = TRUE)
# 
# check_va<-check %>% 
#   filter(str_detect(affiliation,"federal"))

# ADD FEDERAL AFFILIATIONS TO AUTHORS_DF ----------------------------------

source("./code/ID_fed_authors.R")

authors_df<-ID_fed_authors(authors_df,affils_df)
# 
# # 
# foo<-
#   # authors_data_set %>%
#   affils_df %>%
#   filter(federal==TRUE) %>%
#   group_by(federal,agency,agency_primary) %>%
#   tally() %>%
#   arrange(desc(n))
# 
# %>%
#   head(20)

authors_df

# 
# # MERGE WITH USGS ---------------------------------------------------------
# 
# source("./code/merge_with_usgs.R")
# usgs_results<-merge_with_usgs(authors_df,papers_df)
# papers_df<-usgs_results$papers
# authors_df<-usgs_results$authors



# 
# # ADD FEDERAL AFFILIATIONS TO AUTHORS_DF ----------------------------------
# 
#     source("./code/ID_fed_authors.R")
# 
#   authors_df<-ID_fed_authors(authors_df,affils_df)


  # there are some where affil not brought over as fed or not or
  # missing affil_id but can try to infer and fill in
  # authors_df %>% select(AF,federal) %>% distinct() %>% group_by(AF) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)
  # authors_df %>% select(author_url,federal) %>% distinct() %>% group_by(author_url) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)


# final edits of agency and affils ---------------------------------------
# 
# 
# papers_df<-papers_df %>% 
#   mutate(source=case_when(
#     is.na(source)~"scopus",
#     .default = as.character(source)
#   ))
# 



# remove useless columns

authors_df<-authors_df %>% 
  select(-`afid.@_fa`
         # ,-authorID
         )
# 
# papers_df$SO
# papers_df$PY
# unique(papers_df$AF)

# 
# papers_df %>% filter(is.na(DI)) %>% tally()
# 
# papers_df %>% filter(!is.na(DI)) %>% tally()
# 
# unique(papers_df$DT)
# 
# papers_df %>% filter(is.na(DT)) %>% tally()
# papers_df %>% filter(!is.na(DT)) %>% tally()


papers_df_trim<-
  papers_df %>% 
  filter(PY>2018) %>% 
  # filter(!is.na(DI)) %>% 
  filter(!is.na(DT)) %>% 
  tibble()
  
papers_df_trim<-papers_df_trim %>% 
  distinct(SO,PY,DI,TI,.keep_all = TRUE)

# 
# papers_df_trim %>% 
#   distinct(SO,PY,DI,.keep_all = TRUE)
# 
# anti_join(papers_df_trim,papers_df_trim_2)


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

# authors_df_trim %>% distinct(refID) %>% tally()
# 
# papers_df %>% distinct(refID) %>% tally()
# papers_df %>% distinct(DI) %>% tally()
# 
# papers_df_trim %>% distinct(refID) %>% tally()
# papers_df_trim %>% distinct(DI) %>% tally()





# SAVE CLEAN FILES --------------------------------------------------------
# authors_df_trim %>% summarize(n=n_distinct(refID))
# papers_df_trim %>% summarize(n=n_distinct(refID))
# affils_df_trim %>% summarize(n=n_distinct(refID))
# 
# authors_refid<-authors_df %>% select(refID) %>% distinct()
# papers_refid<-papers_df %>% select(refID) %>% distinct() %>% tibble()
# 
# 
# 
# authors_df_trim %>%  filter(federal==TRUE) %>% summarize(n=n_distinct(refID))
# 
# affils_df_trim %>%  filter(federal==TRUE) %>% summarize(n=n_distinct(affil_id))

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

write_rds(papers_df_trim,"./data_clean/papers_df_clean.rds")
write_rds(authors_df_trim,"./data_clean/authors_df_clean.rds")
write_rds(affils_df_trim,"./data_clean/affils_df_clean.rds")






