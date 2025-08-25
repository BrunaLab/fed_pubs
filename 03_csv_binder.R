
# intro -------------------------------------------------------------------
# 
# this binds the csv files downloaded for a given year into a single
# csv file. They are then bound together with `csv_binder_cross_years.R` 
# The fed affiliations are NOT corrected here - they are added and corrected 
# in `merge_usgs_and_final_cleaner.R`

library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)

# 


# data_dir<-"./data_raw/scopus_api/unis_files"
data_dir<-"./data_raw/scopus_api/fed_files/redux"
# data_dir<-"./data_raw/scopus_api/2025-2"
# folder<-c("2025-2") # 8

folder<-c(
  "2019", # 1
  "2020", # 2
  "2021", # 3
  "2022", # 4
  "2023", # 5
  "2024", # 6
  "2025")# 7
  





folder_id <- seq_along(folder)

    for  (k in folder_id) {


# FILE CHECK AND VALIDATION -----------------------------------------------

# Define folder paths

folder_path_papers  <- file.path(data_dir, "papers",folder[k])
folder_path_authors <- file.path(data_dir, "authors",folder[k])
folder_path_affils  <- file.path(data_dir, "affils",folder[k])


# Validate structure
validate_csv_structure <- function(...) {
  folders <- list(...)
  file_counts <- map_int(folders, ~ length(list.files(.x, pattern = "\\.csv$", full.names = TRUE)))

  if (any(file_counts == 0) || length(unique(file_counts)) != 1) {
    
    
    
    papers_list<-list.files(folder_path_papers) %>% 
      tibble() %>% 
      rename(file=".") %>% 
      mutate(file=str_remove(file,"scopus_affil_")) %>% 
      mutate(file=str_remove(file,"_papers.csv")) %>% 
      mutate(file=str_remove(file,"_papers ")) %>% 
      mutate(folder="papers")
    
    
    authors_list<-list.files(folder_path_authors) %>% 
      tibble() %>% 
      rename(file=".") %>% 
      mutate(file=str_remove(file,"scopus_affil_")) %>% 
      mutate(file=str_remove(file,"_authors.csv")) %>% 
      mutate(file=str_remove(file,"_authors ")) %>% 
      mutate(folder="authors")
    
    affils_list<-list.files(folder_path_affils)%>% 
      tibble() %>% 
      rename(file=".") %>% 
      mutate(file=str_remove(file,"scopus_affil_")) %>% 
      mutate(file=str_remove(file,"_affils.csv")) %>% 
      mutate(file=str_remove(file,"_affils_.csv")) %>% 
      mutate(file=str_remove(file,"_affils_ ")) %>% 
      mutate(folder="affils")
    
    file_checker<-full_join(papers_list,authors_list,by="file") %>% 
      full_join(affils_list,by="file") %>% 
      filter(is.na(folder)|is.na(folder.x)|is.na(folder.y)) %>% 
      rename(papers=folder.x,
             authors=folder.y,
             affils=folder)
    
    print(file_checker)
    
    stop("ERROR: Either the file path is incorrect OR folders contain non-CSV or mismatched files. Here is the file mismatch.")
  }
  
  message("All files are csvs. If comparing multiple folders, all have the same number of files.")
}


validate_csv_structure(folder_path_papers)
validate_csv_structure(folder_path_authors)
validate_csv_structure(folder_path_affils)
validate_csv_structure(folder_path_papers, folder_path_authors, folder_path_affils)


# BIND THE CSVs FOR A GIVEN YEAR ------------------------------------------



# fs::dir_ls(data_dir)
# binder using fs package wrapper for purrr
# https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/


# BIND AFFILIATION CSVs ---------------------------------------------------

data_dir_affils<-folder_path_affils
csv_files_affils_all <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")

dataDir <- data_dir_affils  # Update this path
dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)

# Read and tag each file
dt_list <- lapply(dataFls, function(file) {
  dt <- fread(file, fill = TRUE)
  dt[, source_file := basename(file)]  # Add column with filename
  return(dt)
})

# Combine all tagged data tables
affils_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list)



# BIND AUTHOR CSVs --------------------------------------------------------

data_dir_authors<-folder_path_authors
csv_files_authors_all <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")


dataDir <- data_dir_authors  # Update this path
dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)

# Read and tag each file
dt_list <- lapply(dataFls, function(file) {
  dt <- fread(file, fill = TRUE)
  dt[, source_file := basename(file)]  # Add column with filename
  return(dt)
})

# Combine all tagged data tables
authors_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list)

# BIND PAPERS CSVs --------------------------------------------------------

  data_dir_papers<-folder_path_papers
  csv_files_papers_all <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")
  
  
  dataDir <- data_dir_papers  # Update this path
  dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)
  
  # Read and tag each file
  dt_list <- lapply(dataFls, function(file) {
    dt <- fread(file, fill = TRUE)
    dt[, source_file := basename(file)]  # Add column with filename
    return(dt)
  })
  
  # Combine all tagged data tables
  papers_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  rm(dt_list)
  
  

# STANDARDIZE THE COLUMN NAMES --------------------------------------------

  source("./code/name_standardizer.R")
  
  # ----- affils
  
  affils_df<-names_standardizer(affils_df) 
    affils_df<-affils_df %>% 
    select(-"@_fa") %>% 
    # distinct(affil_id,affiliation,city,country,.keep_all = TRUE) %>% 
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
  
    
  # ----- papers
  
  papers_df<-names_standardizer(papers_df) %>% 
    ungroup() %>%
    select(-"@_fa") %>% 
    mutate_all(tolower) %>% 
    remove_empty(which = c("rows", "cols"))
  
  
  # ----- authors 
  
  authors_df<-names_standardizer(authors_df) %>% 
  ungroup() %>%
    select(-"@_fa") %>% 
    remove_empty(which = c("rows", "cols")) %>% 
    mutate_all(tolower)
  
  
  
  

# EDIT THE 'SOURCE FILE' COULMN & ADD A REFID -----------------------------

  # ----- papers 
papers_df<-papers_df %>% 
    mutate(source_file = str_replace(source_file, "scopus_affil_", "")) %>% 
    separate(source_file,c("source_file","left_over"),"_papers",remove=TRUE,extra="warn") %>% 
    select(-left_over) %>% 
    mutate(refID = paste(source_file, "-",entry_no,sep="")) %>% 
    relocate(refID,.before=1) %>% 
    mutate_all(as.character) 
  
  # ----- authors 
  
authors_df<-authors_df %>% 
  mutate(source_file = str_replace(source_file, "scopus_affil_", "")) %>% 
    mutate(source_file = str_replace(source_file, "/scopus_affil_", "")) %>% 
    separate(source_file,c("source_file","left_over"),"_authors",remove=TRUE,extra="warn") %>% 
  select(-left_over) %>% 
  mutate(refID = paste(source_file, "-",entry_no,sep="")) %>% 
  relocate(refID,.before=1) %>% 
  mutate_all(as.character) 

# ----- affils 

  affils_df<-affils_df %>%   
  mutate(source_file = str_replace(source_file, "scopus_affil_", "")) %>% 
  separate(source_file,c("source_file","left_over"),"_affils",remove=TRUE,extra="warn") %>% 
    select(-left_over) %>% 
    mutate(refID = paste(source_file, "-",entry_no,sep="")) %>% 
    relocate(refID,.before=1) %>% 
    mutate_all(as.character) 
  
  # papers_df cleanup   --------------------------------------
  
  # extracts PM and PY from cover date, split page range into 
  # BP and EP, seletcs columns to match refsplitr in case you want
  # to do mapping
  
  source("./code/papers_df_cleanup.R")
  
  papers_df<-papers_df_cleanup(papers_df)
  
  
  
# check to see if any files failed to load  -----------------------------

  pb_chk<-papers_df %>% 
    select(source_file,entry_no) %>% 
    distinct() %>% 
    mutate(papers="yes")
  
  au_chk<-authors_df %>% 
    select(source_file,entry_no)%>% 
    distinct() %>% 
    mutate_all(as.character) %>% 
    mutate(authors="yes")
  
  af_chk<-affils_df %>% 
    select(source_file,entry_no)%>% 
    distinct() %>% 
    mutate("affils"="yes")
    
  file_check<-full_join(pb_chk,au_chk,by=c("source_file","entry_no"))
  file_check<-full_join(file_check,af_chk) 
    
  file_check<-file_check %>% 
    filter(is.na(papers) | is.na(affils) |is.na(authors))
  
  # remove those where info from one of the three csv files is missing  -----
  # this is usually editorials etc. with no authors listed  
  
  incompletes_to_remove<-papers_df %>% 
    filter(source_file%in% file_check$source_file)
  incompletes_to_remove<-incompletes_to_remove %>% 
    filter(entry_no%in%file_check$entry_no) %>% 
    select(refID)
  
# check for duplicate pubs and remove -------------------------------------

  papers_df<-papers_df %>% 
    distinct(scopus_article_id,.keep_all = TRUE) 
  
  # filter out trade journals
  
  papers_df<-papers_df %>% 
  filter(PT!="trade journal")
  
  dupe_pubs<-papers_df %>% group_by(DI,TI) %>% 
    tally() %>%  
    arrange(desc(n)) %>% 
    filter(n>1) %>% 
    filter(!is.na(DI))
  
  dupe_pubs<- papers_df %>% filter(DI%in%dupe_pubs$DI) %>% 
    relocate(SO,.before=1) %>% 
    relocate(TI,.before=1) %>% 
    relocate(DI,.before=1) %>% 
    arrange(DI,TI,SO) %>% 
    tibble()
  
  papers_df<-anti_join(papers_df,dupe_pubs)
  
  dupe_pubs_clean<-dupe_pubs %>% 
    group_by(DI,TI) %>% 
    arrange(AB,DE,BP) %>% 
    slice_head(n=1)
  
  papers_df<-bind_rows(papers_df,dupe_pubs_clean)
  
  pubs_to_keep<-papers_df %>% 
    select(refID) %>% 
    distinct() %>%  
    filter(!refID%in%incompletes_to_remove$refID)
  
  pubs_to_keep$refID
  
  # filter out incompletes
  
  papers_df<-papers_df %>% 
    filter(refID%in% pubs_to_keep$refID)
  
  
  authors_df<-authors_df %>% 
    filter(refID%in% pubs_to_keep$refID)
  
  affils_df<-affils_df %>% 
    filter(refID%in% pubs_to_keep$refID)
  
  
  
# save --------------------------------------------------------------------

  # # FOR WITHIN YEAR BINDING - FEDS

  write_csv(affils_df,paste("./data_raw/affils/year_files_fed/affils_df_",folder[k],".csv",sep=""))
  write_csv(authors_df,paste("./data_raw/authors/year_files_fed/authors_df_",folder[k],".csv",sep=""))
  write_csv(papers_df,paste("./data_raw/papers/year_files_fed/papers_df_",folder[k],".csv",sep=""))

  write_csv(incompletes_to_remove,paste("./data_raw/incomplete_records_removed/fed/incompletes_removed_",folder[k],".csv",sep=""))

  
  # # FOR WITHIN YEAR BINDING - UNIS
  # 
  # write_csv(affils_df,paste("./data_raw/affils/year_files_uni/affils_df_",folder[k],".csv",sep=""))
  # write_csv(authors_df,paste("./data_raw/authors/year_files_uni/authors_df_",folder[k],".csv",sep=""))
  # write_csv(papers_df,paste("./data_raw/papers/year_files_uni/papers_df_",folder[k],".csv",sep=""))
  # 
  # 
  # write_csv(incompletes_to_remove,paste("./data_raw/incomplete_records_removed/uni/incompletes_removed_",folder[k],".csv",sep=""))

  
    }
  
  
  