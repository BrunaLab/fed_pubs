library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)
# bind the subfiles for each category and check for dupes ------------------


# Load files and bind
# Define folder paths
folder_path_affils <- "./data_raw/affils/"
folder_path_papers <- "./data_raw/papers/"
folder_path_authors <- "./data_raw/authors/"




# affil binder -----------------------------------------------------------

data_dir_affils<-folder_path_affils
csv_files_affils_all <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")



dataDir <- data_dir_affils  # Update this path
dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)

# Read and tag each file
dt_list <- lapply(dataFls, function(file) {
  dt <- fread(file, fill = TRUE)
  dt[, source_file_combined := basename(file)]  # Add column with filename
  return(dt)
})

# Combine all tagged data tables
affils_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list)


# author binder -----------------------------------------------------------
# data_dir_authors<-"./data_raw/scopus_api/unis_files/authors"
data_dir_authors<-folder_path_authors
csv_files_authors_all <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")


dataDir <- data_dir_authors  # Update this path
dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)

# Read and tag each file
dt_list <- lapply(dataFls, function(file) {
  dt <- fread(file, fill = TRUE)
  dt[, source_file_combined := basename(file)]  # Add column with filename
  return(dt)
})

# Combine all tagged data tables
authors_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list)



# papers binder -----------------------------------------------------------
# data_dir_papers<-"./data_raw/scopus_api/unis_files/papers"
data_dir_papers<-folder_path_papers
csv_files_papers_all <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")


dataDir <- data_dir_papers  # Update this path
dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)

# Read and tag each file
dt_list <- lapply(dataFls, function(file) {
  dt <- fread(file, fill = TRUE)
  dt[, source_file_combined := basename(file)]  # Add column with filename
  return(dt)
})

# Combine all tagged data tables
papers_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list)


# MERGE WITH USGS ---------------------------------------------------------



source("./code/merge_with_usgs.R")
usgs_results<-merge_with_usgs(authors_df,papers_df)
papers_df<-usgs_results$papers
authors_df<-usgs_results$authors




# IDENTIFY FEDERAL AFFILIATIONS -------------------------------------------

## NEED TO CONFIRM THE DOD CDC ESP. INTENRATIONAL

source("./code/ID_fed_affiliations.R")
affils_df<-ID_fed_affiliations(affils_df)




# ADD FEDERAL AFFILIATIONS TO AUTHORS_DF ----------------------------------

source("./code/ID_fed_authors.R")

authors_df<-ID_fed_authors(authors_df,affils_df)

# 
# # ADD FEDERAL AFFILIATIONS TO AUTHORS_DF ----------------------------------
# 
#     source("./code/ID_fed_authors.R")
# 
#   authors_df<-ID_fed_authors(authors_df,affils_df)


  # there are some where affil not brought over as fed or not or
  # missing affil_id but can try to infer and fill in
  # authors_df %>% select(AF,federal) %>% distinct() %>% group_by(AF) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)
  authors_df %>% select(author_url,federal) %>% distinct() %>% group_by(author_url) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)


# final edits of agency and affils ---------------------------------------


papers_df<-papers_df %>% 
  mutate(source=case_when(
    is.na(source)~"scopus",
    .default = as.character(source)
  ))

affils_df<-affils_df %>% 
mutate(agency=case_when(
  agency=="nphs" & federal==TRUE~"usphs",
  agency=="usgs" & federal==TRUE~"interior",
  .default = as.character(agency)
)
) %>% 
mutate(agency_primary=agency) %>% 
mutate(agency_primary=case_when(
  agency_primary=="irs"~"treasury",
  agency_primary=="usphs"~"hhs",
  agency_primary=="nih"~"hhs",
  agency_primary=="cdc"~"hhs",
  agency_primary=="fda"~"hhs",
  agency_primary=="nist"~"commerce",
  agency_primary=="noaa"~"commerce",
  agency_primary=="usaid"~"state",
  agency_primary=="faa"~"dot",
  agency_primary=="dea"~"doj",
  agency_primary=="dha"~"dod",
  agency_primary=="fema"~"dhs",
  agency_primary=="usphs"~"hha",
  .default = as.character(agency_primary)
)
)


# remove useless columns

authors_df<-authors_df %>% 
  select(-`afid.@_fa`,-authorID)
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
  
authors_df_trim <-authors_df_trim  %>% 
left_join(affil_vector,by="affil_id") %>% 
  mutate(federal.x=if_else(is.na(federal.x)&federal.y==TRUE,federal.y,federal.x)) %>% 
  select(-federal.y) %>% 
  rename(federal=federal.x) %>% 
  relocate(c(federal, agency),.before=1)


write_rds(papers_df_trim,"./data_clean/papers_df_clean.rds")
write_rds(authors_df_trim,"./data_clean/authors_df_clean.rds")
write_rds(affils_df_trim,"./data_clean/affils_df_clean.rds")






