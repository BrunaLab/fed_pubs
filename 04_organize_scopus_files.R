library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)
# bind the subfiles for each category and check for dupes ------------------

folder<-c(
  "2019",
  "2020",
  "2021",
  "2022",
  "2023",
  "2024",
  "2025")

folder<-folder[1]

#import data
# df <- fread("C:\\Users\\Bob\\Desktop\\data.csv")


# Load files and bind
# Define folder paths
folder_path_affils <- paste("./data_raw/affils/",folder,sep="")
folder_path_papers <- paste("./data_raw/papers/",folder,sep="")
folder_path_authors <- paste("./data_raw/authors/",folder,sep="")



# BIND OF THE ANNUAL CSVS

data_dir<-"./data_raw"


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

all_papers_df_fed.csv

# # standardize the csv column names   --------------------------------------
# # names(affils_df)
source("./code/name_standardizer.R")
# 
# # ----- affils
#   
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
#   
# affils_df<-affils_df %>% 
#   mutate(source=gsub("_.csv",".csv",source))
# 
# # ----- papers
# 
papers_df<-names_standardizer(papers_df) %>%
  ungroup() %>%
  select(-"@_fa") %>%
  # group_by(scopus_article_id,SO,TI) %>%
  # tally() %>%
  # arrange(desc(n)) %>%
  distinct(scopus_article_id,SO,TI,.keep_all = TRUE) %>%
  mutate_all(tolower) %>%
  remove_empty(which = c("rows", "cols"))

# 
# # ----- authors 
# 
authors_df<-names_standardizer(authors_df) %>%
  remove_empty(which = c("rows", "cols")) %>%
  mutate_all(tolower)

# write_rds(authors_df,"./data_intermediate/authors_df_uni.rds")
# write_rds(papers_df,"./data_intermediate/papers_df_uni.rds")
# write_rds(affils_df,"./data_intermediate/affils_df_uni.rds")

# write_rds(affils_df,"./data_intermediate/affils_df.rds")  
# write_rds(papers_df,"./data_intermediate/papers_df.rds")
# write_rds(authors_df,"./data_intermediate/authors_df.rds")


# affils_df<-read_rds("./data_intermediate/affils_df.rds")  
# # add refID ---------------------------------------------------------------
# papers_df$source
# papers_df$entry_no
# 
# papers_df<-papers_df %>% 
#   mutate(refID = source) %>% 
#   relocate(refID,.before=1) %>% 
#   mutate(refID = str_replace(refID, "affil_", "")) %>%
#   mutate(refID = str_replace(refID, paste("_", folder,".csv","",sep=""),"")) %>% 
#   mutate(refID = paste(refID, "-",entry_no,sep="")) 

         
#  
# 
# papers_df$refID <- seq.int(nrow(papers_df))  
# papers_df<-papers_df %>% relocate(refID,.before=1)

# 
# 
# # bring over the refID from papers to authors ----------------------------
# paper_ID_nos<-papers_df %>% select(refID,source,entry_no)
# 
# authors_df<-left_join(authors_df,paper_ID_nos,by=c("source","entry_no")) %>% 
#   relocate(refID,.before=1)
# affils_df<-left_join(affils_df,paper_ID_nos,by=c("source","entry_no")) %>% 
#   relocate(refID,.before=1)
#    
# rm(paper_ID_nos)
#   
# cleanup -----------------------------------------------------------------
  

source("./code/papers_df_cleanup.R")
papers_df<-papers_df_cleanup(papers_df)

# names<-names(papers_df)
# 
# papers_df %>% group_by(DI,TI) %>% tally() %>%  arrange(desc(n)) %>% filter(n>1) %>% 
#   filter(!is.na(DI))
# 
# papers_df %>% select(refID,PY) %>% group_by(PY) %>% tally()
# IDENTIFY FEDERAL AFFILIATIONS -------------------------------------------

## NEED TO CONFIRM THE DOD CDC ESP. INTENRATIONAL

source("./code/ID_fed_affiliations.R")
affils_df<-ID_fed_affiliations(affils_df)


# foo<-fed_affils %>%
#   group_by(affil_id, affiliation, agency_short) %>%
#   tally() %>%
#   arrange(desc(n))
# summary <- affils_df_temp %>% 
# group_by(agency) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% 
#   drop_na()


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
  
  
  # there are some where affil not brought over as fed or not or 
  # missing affil_id but can try to infer and fill in 
  # authors_df %>% select(AF,federal) %>% distinct() %>% group_by(AF) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)
  authors_df %>% select(author_url,federal) %>% distinct() %>% group_by(author_url) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)

# separate pubs with & without DOI ----------------------------------------
# 
#   pubs_no_doi<-papers_df %>% 
#     filter(is.na(DI)) %>% 
#     distinct(source,SO,PY,TI,.keep_all=TRUE)
#   
#   pubs_no_doi_dupes<-pubs_no_doi %>% 
#     group_by(source,SO,PY,TI) %>% 
#     tally() %>% 
#     arrange(desc(n)) %>% 
#     filter(n>1)
#   
#   pubs_with_doi<-papers_df %>% 
#     filter(!is.na(DI)) %>% 
#     distinct(source,DI,SO,PY,PG,TI,.keep_all=TRUE)
#   
#   pubs_with_doi_dupes<-pubs_with_doi %>% 
#     group_by(source,DI,SO,PY,PG,TI) %>% 
#     filter(!is.na(DI)) %>% 
#     tally() %>% 
#     arrange(desc(n)) %>% 
#     filter(n>1)
#   # sum(pubs_with_doi_dupes$n)  
#   
#   
#   papers_df<-bind_rows(pubs_no_doi,pubs_with_doi)
#   
#   
#   rm(pubs_no_doi,
#      pubs_with_doi,
#      pubs_with_doi_dupes,
#      pubs_no_doi_dupes)
#   
#   authors_df %>% group_by(
#     source,
#     author_order,
#     author_url,
#     SID,
#     AF,
#     surname,
#     given_name,
#     first_middle_initials,
#     affil_id,
#     OI,
#     federal) %>%
#     tally() %>% 
#     arrange(desc(n))
# 
# #   
#   authors_df<-authors_df %>% distinct(
#                        source,
#                        author_order,
#                        author_url,
#                        SID,
#                        AF,
#                        surname,
#                        given_name,
#                        first_middle_initials,
#                        affil_id,
#                        OI,
#                        federal,
#                        .keep_all=TRUE)
#   
  
  
  
  # affils_df<-affils_df[!duplicated(affils_df), ]
  
  
  
  # STANDARDIZE AUTHOR NAMES & ADDRESSES ------------------------------------
  source("./code/author_name_cleaner.R")
  authors_df<-author_name_cleaner(authors_df)
  
  

  # ID PAPERS WITHOUT AUTHORS OR AUTHORS WITHOUT PAPERS ---------------------  
  
papers_df %>% summarize(n_distinct(refID))
authors_df %>% summarize(n_distinct(refID))
setdiff((papers_df$refID),(authors_df$refID))
setdiff((authors_df$refID),(papers_df$refID))


# 
# papers_df$refID<-as.character(papers_df$refID)
# authors_df$refID
# from_papers<-
#   anti_join(papers_df %>% select(refID),
#             unique(authors_df %>% select(refID)))
# 
# need_to_find_authors<- semi_join(papers_df,from_papers,by="refID")
# 
# from_authors<-
#   anti_join(unique(authors_df %>% select(refID)),
#             papers_df %>% select(refID))
# 
# need_to_find_papers<- semi_join(authors_df,from_authors,by="refID")
# names(need_to_find_papers)
# 
# papers_df<-anti_join(papers_df,from_papers,by="refID")
# authors_df<-anti_join(authors_df,from_authors,by="refID")
# 
# rm(from_papers,from_authors)
# 
# # papers_df$PY<-papers_df$PY2
# # papers_df$PY<-as.numeric(papers_df$PY)
# # papers_df$PY2<-NULL
# 
PY_binder<-papers_df %>% select(refID,PY,PM)

authors_df<-left_join(authors_df,PY_binder,by="refID")
affils_df$refID<-as.character(affils_df$refID)
affils_df<-left_join(affils_df,PY_binder)

names(PY_binder)
names(affils_df)
affil_binder<-affils_df %>% select(affil_id,federal,agency) %>% distinct()
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



# find and remove duplicates ----------------------------------------------

# 
# 
# papers_df  <- papers_df %>% 
#   remove_empty(c("rows","cols")) %>% 
#   filter(PY>2018)



papers_df$SO
papers_df$PY
papers_df$AF


papers_df %>% filter(is.na(DI)) %>% tally()

papers_df %>% filter(!is.na(DI)) %>% tally()

unique(papers_df$DT)

papers_df %>% filter(is.na(DT)) %>% tally()
papers_df %>% filter(!is.na(DT)) %>% tally()


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


authors_df_trim %>% distinct(refID) %>% tally()

papers_df %>% distinct(refID) %>% tally()
papers_df %>% distinct(DI) %>% tally()

papers_df_trim %>% distinct(refID) %>% tally()
papers_df_trim %>% distinct(DI) %>% tally()





# SAVE CLEAN FILES --------------------------------------------------------
authors_df_trim %>% summarize(n=n_distinct(refID))
papers_df_trim %>% summarize(n=n_distinct(refID))
affils_df_trim %>% summarize(n=n_distinct(refID))
# 
# authors_refid<-authors_df %>% select(refID) %>% distinct()
# papers_refid<-papers_df %>% select(refID) %>% distinct() %>% tibble()



authors_df_trim %>%  filter(federal==TRUE) %>% summarize(n=n_distinct(refID))

affils_df_trim %>%  filter(federal==TRUE) %>% summarize(n=n_distinct(affil_id))

authors_df_trim <-authors_df_trim %>% relocate(c(federal, agency),.before=1)

# binding the annuals
write_rds(papers_df_trim,"./data_clean/papers_df_clean.rds")
write_rds(authors_df_trim,"./data_clean/authors_df_clean.rds")
write_rds(affils_df_trim,"./data_clean/affils_df_clean.rds")




write_rds(papers_df_trim,paste("./data_clean/papers_df_clean_",folder,".rds",sep=""))
write_rds(authors_df_trim,paste("./data_clean/authors_df_clean_",folder,".rds",sep=""))
write_rds(affils_df_trim,paste("./data_clean/affils_df_clean_",folder,".rds",sep=""))





