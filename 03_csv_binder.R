library(janitor)
library(tidyverse)
library(progress)
library(fs)
# #####################################
data_dir<-"./data_raw/scopus_api"
dir = TRUE
include_all=FALSE
# #####################################


# Define folder paths

folder_path_papers  <- file.path(data_dir, "papers")
folder_path_authors <- file.path(data_dir, "authors")
folder_path_affils  <- file.path(data_dir, "affils")

# Validate structure
validate_csv_structure <- function(...) {
  folders <- list(...)
  file_counts <- map_int(folders, ~ length(list.files(.x, pattern = "\\.csv$", full.names = TRUE)))
  
  if (any(file_counts == 0) || length(unique(file_counts)) != 1) {
    stop("ERROR: Either the file path is incorrect OR folders contain non-CSV or mismatched files.")
  }
  
  message("Now processing all references files")
}
validate_csv_structure(folder_path_papers)
validate_csv_structure(folder_path_authors)
validate_csv_structure(folder_path_affils)
validate_csv_structure(folder_path_papers, folder_path_authors, folder_path_affils)


# bind the csv's together -------------------------------------------------



# fs::dir_ls(data_dir)
# binder using fs package wrapper for purrr
# https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/

# affil binder -----------------------------------------------------------

data_dir_affils<-"./data_raw/scopus_api/affils"
csv_files_affils_all <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")


cvs_binder_affils <- function(csv_files_affils) {
  
  affils_df <- csv_files_affils %>%
    map_dfr(~ read_csv(.x),
            .id = "source")
  
}

affils_df_1<-cvs_binder_affils(csv_files_affils_all[1:5000])
write_csv(affils_df_1,"./data_raw/affils_df_1.csv")
affils_df_2<-cvs_binder_affils(csv_files_affils_all[5001:10000])
write_csv(affils_df_2,"./data_raw/affils_df_2.csv")
affils_df_3<-cvs_binder_affils(csv_files_affils_all[10001:13431])
write_csv(affils_df_3,"./data_raw/affils_df_3.csv")
affils_df_4<-cvs_binder_affils(csv_files_affils_all)
write_csv(affils_df_4,"./data_raw/affils_df_4.csv")


# author binder -----------------------------------------------------------

data_dir_authors<-"./data_raw/scopus_api/authors"
csv_files_authors_all <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")


cvs_binder_authors <- function(csv_files_authors) {
  
  authors_df <- csv_files_authors %>%
    map_dfr(~ read_csv(.x),
            .id = "source")
  
}

authors_df_1<-cvs_binder_authors(csv_files_authors_all[1:5000])
write_csv(authors_df_1,"./data_raw/authors_df_1.csv")
authors_df_2<-cvs_binder_authors(csv_files_authors_all[5001:10000])
write_csv(authors_df_2,"./data_raw/authors_df_2.csv")
authors_df_3<-cvs_binder_authors(csv_files_authors_all[10001:13431])
write_csv(authors_df_3,"./data_raw/authors_df_3.csv")

authors_df_4<-cvs_binder_authors(csv_files_authors_all)
write_csv(authors_df_4,"./data_raw/authors_df_4.csv")


# papers binder -----------------------------------------------------------

data_dir_papers<-"./data_raw/scopus_api/papers"
csv_files_papers_all <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")


cvs_binder_papers <- function(csv_files_papers) {
  
  papers_df <- csv_files_papers %>%
    map_dfr(~ read_csv(.x) %>% 
            mutate(
              `prism:eIssn` = as.character(`prism:eIssn`),
              `prism:issn` = as.character(`prism:issn`),
                   `prism:coverDate` = as.character(`prism:coverDate`)
                   ),
            .id = "source")
  
}





cvs_binder_papers <- function(csv_files_papers) {
  
  papers_df <- csv_files_papers %>%
    map_dfr(~ {
      df <- read_csv(.x) 
      
      # Ensure consistent column names across files
      required_columns <- c("prism:eIssn", "prism:issn", "prism:coverDate") # Add any other necessary columns
      missing_cols <- setdiff(required_columns, names(df))
      
      # Add missing columns as NA
      if (length(missing_cols) > 0) {
        df[missing_cols] <- NA
      }
      
      # Convert all columns to character type BEFORE returning
      df <- df %>% 
        mutate_all(as.character)
      
      return(df)  
    }, .id = "source")
  
  return(papers_df)
}





papers_df_1<-cvs_binder_papers(csv_files_papers_all[1:5000])
write_csv(papers_df_1,"./data_raw/papers_df_1.csv")
papers_df_2<-cvs_binder_papers(csv_files_papers_all[5001:10000])
write_csv(papers_df_2,"./data_raw/papers_df_2.csv")
papers_df_3<-cvs_binder_papers(csv_files_papers_all[10001:13431])
write_csv(papers_df_3,"./data_raw/papers_df_3.csv")

papers_df_4<-cvs_binder_papers(csv_files_papers_all)
write_csv(papers_df_4,"./data_raw/papers_df_4.csv")
