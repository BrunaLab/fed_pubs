
# intro -------------------------------------------------------------------

# this takes the consolidated CSVs for each year and binds into one single CSV

library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)

cat<-"fed"
# cat<-"uni"

data_dir<-"./data_raw"




folder_count<-paste("year_files_",cat,sep="")



# Define folder paths -----------------------------------------------------

folder_path_papers  <- file.path(data_dir, "papers",folder_count)
folder_path_authors <- file.path(data_dir, "authors",folder_count)
folder_path_affils  <- file.path(data_dir, "affils",folder_count)


# bind the csv's together -------------------------------------------------
# fs::dir_ls(data_dir)
# binder using fs package wrapper for purrr
# https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/

# affil binder -----------------------------------------------------------

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


# author binder -----------------------------------------------------------


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


  # papers binder -----------------------------------------------------------


  
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
  
  
  
# edit the `source_file` columns   ----------------------------------------------
  papers_df<-papers_df %>% 
    mutate(source_file = str_replace(source_file, ".csv", ""))
  
  authors_df<-authors_df %>% 
    mutate(source_file = str_replace(source_file, ".csv", ""))
  
  affils_df<-affils_df %>% 
    mutate(source_file = str_replace(source_file, ".csv", ""))
  
  
  

# id duplicate papers -----------------------------------------------------

  # (sometimes early version of paper posted 
  #  in one year but only in print in the next)
  ## also I searched for some files in all years but saved in 2025 folder. 
  # easier to filter here than move and risk copying incorrectly
  
  dupe_pub_scopusID<-papers_df %>% 
    group_by(scopus_article_id) %>% 
    tally() %>% 
    filter(n>1) %>% 
    arrange(desc(n))
  dupe_pub_scopusID
  
  
  papers_df<-papers_df %>% 
    distinct(scopus_article_id,.keep_all = TRUE) 
  
  
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
    distinct()
  
  pubs_to_keep$refID
  
  # filter out incompletes
  
  papers_df<-papers_df %>% 
    filter(refID%in% pubs_to_keep$refID)
  
  
  authors_df<-authors_df %>% 
    filter(refID%in% pubs_to_keep$refID)
  
  affils_df<-affils_df %>% 
    filter(refID%in% pubs_to_keep$refID)
  
  
  
# save --------------------------------------------------------------------

  # FOR CROSS YEAR BINDING
  
  write_csv(affils_df,paste("./data_raw/affils/all_affils_df_",cat,".csv",sep=""))
  write_csv(authors_df,paste("./data_raw/authors/all_authors_df_",cat,".csv",sep=""))
  write_csv(papers_df,paste("./data_raw/papers/all_papers_df_",cat,".csv",sep=""))
  