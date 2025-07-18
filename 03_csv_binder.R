library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)
# #####################################
# data_dir<-"./data_raw/scopus_api/unis"
# data_dir<-"./data_raw/scopus_api/unis_files"
data_dir<-"./data_raw/scopus_api/fed_files/redux"
dir = TRUE
include_all=FALSE
# #####################################



folder<-c(
  "2019",
  "2020",
  "2021",
  "2022",
  "2023",
  "2024",
  "2025",
  "misc")


folder_count<-folder[2]



# affils load -------------------------------------------------------------

# Define folder paths

folder_path_papers  <- file.path(data_dir, "papers",folder_count)
folder_path_authors <- file.path(data_dir, "authors",folder_count)
folder_path_affils  <- file.path(data_dir, "affils",folder_count)

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
# data_dir_affils<-"./data_raw/scopus_api/unis_files/affils"
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
# 
# cvs_binder_affils <- function(csv_files_affils) {
#   
#   affils_df <- csv_files_affils %>%
#     map_dfr(~ read_csv(.x),
#             .id = "source")
#   
# }
# 
# 
# 
# affils_df<-cvs_binder_affils(csv_files_affils_all)



# author binder -----------------------------------------------------------
# data_dir_authors<-"./data_raw/scopus_api/unis_files/authors"
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



# 
# cvs_binder_authors <- function(csv_files_authors) {
#   
#   authors_df <- csv_files_authors %>%
#     map_dfr(~ read_csv(.x),
#             .id = "source")
#   
# }
# 
# 
# authors_df<-cvs_binder_authors(csv_files_authors_all)

  
  # papers binder -----------------------------------------------------------
  # data_dir_papers<-"./data_raw/scopus_api/unis_files/papers"
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
  
  
  # 
  # cvs_binder_papers <- function(csv_files_papers) {
  #   
  #   papers_df <- csv_files_papers %>%
  #     map_dfr(~ read_csv(.x) %>% 
  #             mutate(
  #               `prism:eIssn` = as.character(`prism:eIssn`),
  #               `prism:issn` = as.character(`prism:issn`),
  #                    `prism:coverDate` = as.character(`prism:coverDate`)
  #                    ),
  #             .id = "source_file")
  #   
  # }
  
  
  
  # 
  # 
  # cvs_binder_papers <- function(csv_files_papers) {
  #   
  #   papers_df <- csv_files_papers %>%
  #     map_dfr(~ {
  #       df <- read_csv(.x) 
  #       
  #       # Ensure consistent column names across files
  #       required_columns <- c("prism:eIssn", "prism:issn", "prism:coverDate") # Add any other necessary columns
  #       missing_cols <- setdiff(required_columns, names(df))
  #       
  #       # Add missing columns as NA
  #       if (length(missing_cols) > 0) {
  #         df[missing_cols] <- NA
  #       }
  #       
  #       # Convert all columns to character type BEFORE returning
  #       df <- df %>% 
  #         mutate_all(as.character)
  #       
  #       return(df)  
  #     }, .id = "source_file")
  #   
  #   return(papers_df)
  # }
  # 
  # 
  # papers_df_1<-cvs_binder_papers(csv_files_papers_all)
  # write_csv(papers_df_1,"./data_raw/papers_df_uni1.csv")
  
  
  # 
  # papers_df<-cvs_binder_papers(csv_files_papers_all)
  # 
  # 
  
  
  # standardize the csv column names   --------------------------------------
  
  
  # names(affils_df)
  source("./code/name_standardizer.R")
  
  # ----- affils
  
  affils_df<-names_standardizer(affils_df) %>%
    ungroup() %>%
    # select(-"@_fa") %>% 
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
  # 
  # affils_df<-affils_df %>% 
  #   mutate(source_file=gsub("_.csv",".csv",source_file))
  
  # ----- papers
  
  papers_df<-names_standardizer(papers_df) %>% 
    ungroup() %>%
    # select(-"@_fa") %>% 
    # group_by(scopus_article_id,SO,TI) %>% 
    # tally() %>% 
    # arrange(desc(n)) %>% 
    # distinct(scopus_article_id,.keep_all = TRUE) %>% 
    mutate_all(tolower) %>% 
    remove_empty(which = c("rows", "cols"))
  
  
  # ----- authors 
  
  authors_df<-names_standardizer(authors_df) %>%
    ungroup() %>%
    select(-"@_fa",
           -"afid.@_fa") %>% 
    remove_empty(which = c("rows", "cols")) %>% 
    mutate_all(tolower)
  
  

# edit source_file and add refID ----------------------------------------------
papers_df<-papers_df %>% 
  # mutate(source_file = str_replace(source_file, folder_path_papers, "")) %>% 
    mutate(source_file = str_replace(source_file, "scopus_affil_", "")) %>% 
    # mutate(source_file = str_replace(source_file, "/scopus_affil_", "")) %>% 
    # mutate(source_file = str_replace(source_file, "_papers.csv", "")) %>% 
    separate(source_file,c("source_file","left_over"),"_papers",remove=TRUE,extra="warn") %>% 
    select(-left_over) %>% 
    mutate(refID = paste(source_file, "-",entry_no,sep="")) %>% 
    relocate(refID,.before=1) %>% 
    mutate_all(as.character) 
  
  # papers_df$refID
  
authors_df<-authors_df %>% 
  # mutate(source_file = str_replace(source_file, folder_path_authors, "")) %>% 
  mutate(source_file = str_replace(source_file, "scopus_affil_", "")) %>% 
    mutate(source_file = str_replace(source_file, "/scopus_affil_", "")) %>% 
    separate(source_file,c("source_file","left_over"),"_authors",remove=TRUE,extra="warn") %>% 
  select(-left_over) %>% 
  mutate(refID = paste(source_file, "-",entry_no,sep="")) %>% 
  relocate(refID,.before=1) %>% 
  mutate_all(as.character) 

  affils_df<-affils_df %>%   
  # mutate(source_file = str_replace(source_file, folder_path_affils, "")) %>% 
  mutate(source_file = str_replace(source_file, "scopus_affil_", "")) %>% 
  # mutate(source_file = str_replace(source_file, "scopus_affil_", "")) %>% 
  separate(source_file,c("source_file","left_over"),"_affils",remove=TRUE,extra="warn") %>% 
    select(-left_over) %>% 
    mutate(refID = paste(source_file, "-",entry_no,sep="")) %>% 
    relocate(refID,.before=1) %>% 
    mutate_all(as.character) 
  
# check to see if any records failed to load  -----------------------------

  pb_chk<-papers_df %>% 
    select(source_file,entry_no) %>% 
    distinct() %>% 
    mutate(papers="yes")
  
  
  au_chk<-authors_df %>% 
    select(source_file,entry_no)%>% 
    distinct() %>% 
    mutate_all(as.character) %>% 
    mutate(authors="yes")
  
    # 
    # 
    # mutate(source_file = str_replace(source_file, "_authors.csv", "")) 
  
  af_chk<-affils_df %>% 
    select(source_file,entry_no)%>% 
    distinct() %>% 
    mutate("affils"="yes")
    # mutate(source_file = str_replace(source_file, "_affils.csv", "")) 
  
  # setdiff(pb_chk,au_chk)

  file_check<-full_join(pb_chk,au_chk,by=c("source_file","entry_no"))
  file_check<-full_join(file_check,af_chk) 
    
  file_check<-file_check %>% 
    filter(is.na(papers) | is.na(affils) |is.na(authors))
  
  file_check
  
  
  
  # remove those where one of three csv files is missingwhere  --------------
  
  # this is usally editorials etc. with no authors listed  
  
  incompletes_to_remove<-papers_df %>% 
    filter(source_file%in% file_check$source_file)
  incompletes_to_remove<-incompletes_to_remove %>% 
    filter(entry_no%in%file_check$entry_no) %>% 
    select(refID)
  
# check for duplicate pubs and remove -------------------------------------

  # dupe_pubs<-papers_df %>% 
  #   group_by(scopus_article_id) %>% 
  #   tally() %>% 
  #   filter(n>1) %>% 
  #   arrange(desc(n))
  
  
  papers_df<-papers_df %>% 
    distinct(scopus_article_id,.keep_all = TRUE) 
  
  # fiklter out trade journals
  
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
    arrange(AB,DE,page_range) %>% 
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

  
  
  write_csv(affils_df,paste("./data_raw/affils/",folder_count,"/affils_df_",folder_count,".csv",sep=""))
  write_csv(authors_df,paste("./data_raw/authors/",folder_count,"/authors_df_",folder_count,".csv",sep=""))
  write_csv(papers_df,paste("./data_raw/papers/",folder_count,"/papers_df_",folder_count,".csv",sep=""))
  
  
  write_csv(incompletes_to_remove,paste("./data_raw/incompletes_removed_",folder_count,".csv",sep=""))
  
  
  
  # 
  # 
  # papers_df_1<-cvs_binder_papers(csv_files_papers_all[1:5000])
  # write_csv(papers_df_1,"./data_raw/papers_df_1.csv")
  # papers_df_2<-cvs_binder_papers(csv_files_papers_all[5001:10000])
  # write_csv(papers_df_2,"./data_raw/papers_df_2.csv")
  # papers_df_3<-cvs_binder_papers(csv_files_papers_all[10001:13431])
  # write_csv(papers_df_3,"./data_raw/papers_df_3.csv")
  # 
  # papers_df_4<-cvs_binder_papers(csv_files_papers_all)
  # write_csv(papers_df_4,"./data_raw/papers_df_4.csv")
  # papers_df_5<-cvs_binder_papers(csv_files_papers_all)
  # write_csv(papers_df_5,"./data_raw/papers/papers_df_5.csv")
  

  
  
  
  # authors_df_1<-cvs_binder_authors(csv_files_authors_all)
  # write_csv(authors_df_1,"./data_raw/authors_df_uni1.csv")
  # 
  # 
  # authors_df_1<-cvs_binder_authors(csv_files_authors_all[1:5000])
  # write_csv(authors_df_1,"./data_raw/authors_df_1.csv")
  # authors_df_2<-cvs_binder_authors(csv_files_authors_all[5001:10000])
  # write_csv(authors_df_2,"./data_raw/authors_df_2.csv")
  # authors_df_3<-cvs_binder_authors(csv_files_authors_all[10001:13431])
  # write_csv(authors_df_3,"./data_raw/authors_df_3.csv")
  # 
  # authors_df_4<-cvs_binder_authors(csv_files_authors_all)
  # write_csv(authors_df_4,"./data_raw/authors_df_4.csv")
  # authors_df_5<-cvs_binder_authors(csv_files_authors_all)
  # write_csv(authors_df_5,"./data_raw/authors/authors_df_5.csv")
  
  
  
  
  # affils_df_1<-cvs_binder_affils(csv_files_affils_all)
  # write_csv(affils_df_1,"./data_raw/affils_df_uni1.csv")
  # 
  # affils_df_1<-cvs_binder_affils(csv_files_affils_all[1:5000])
  # write_csv(affils_df_1,"./data_raw/affils_df_1.csv")
  # affils_df_2<-cvs_binder_affils(csv_files_affils_all[5001:10000])
  # write_csv(affils_df_2,"./data_raw/affils_df_2.csv")
  # affils_df_3<-cvs_binder_affils(csv_files_affils_all[10001:13431])
  # write_csv(affils_df_3,"./data_raw/affils_df_3.csv")
  # affils_df_4<-cvs_binder_affils(csv_files_affils_all)
  # write_csv(affils_df_4,"./data_raw/affils_df_4.csv")
  # affils_df_5<-cvs_binder_affils(csv_files_affils_all)
  # write_csv(affils_df_5,"./data_raw/affils/affils_df_5.csv")
  
  