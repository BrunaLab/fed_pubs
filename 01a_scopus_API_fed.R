# install.packages("rscopus")
# install.packages("tidyverse")


library(rscopus)
library(tidyverse)
library(data.table)
library(purrr)

# help --------------------------------------------------------------------

# rscopus: https://cran.r-project.org/web/packages/rscopus/vignettes/multi_author.html

# load packages -----------------------------------------------------------

api<-""
set_api_key(api)

# date_folder_for_files<-"fed_20251010"
# date_folder_for_files<-"fed_20251210"
date_folder_for_files<-"fed_20260101"

set_api_key(api)
# res<-get_api_key()
# print(res, reveal = TRUE)

# token is from Scopus dev
# hdr = inst_token_header(token)
# res = author_df(last_name = "Muschelli", first_name = "John", verbose = FALSE, general = FALSE, headers = hdr)

# source("./code/generate_fed_affils_to_search.R")
# affils_to_search<-generate_fed_affils_to_search()

# affils_to_search<-read_csv("./data_clean/api_fed_affils_searched_2025-09-01.csv")

# AFFILS ORIGINALLY SEARCHED
affils_to_search_original<-read_csv("data_clean/api_fed_affils_searched_2025-11-04.csv") %>% 
  select(affil_id,agency_primary) %>% 
  distinct()

# FINAL LIST OF AFFILIATIONS FOUND IN DEC AFTER ANALYSIS

affils_to_search_followup <- readRDS("data_clean/affils_df_clean_fed_20251210.rds") %>% 
  filter(federal==TRUE) %>% 
  select(affil_id, agency_primary) %>% 
  distinct() %>% 
  arrange(agency_primary)

affils_to_search_all<-full_join(affils_to_search_original,
                            affils_to_search_followup) %>% 
  distinct()
 
# VA and HHS
search_term_over5K <- as_tibble(c(60014232,
                                  60006577)) %>%  
  rename(affil_id=value)


# create the folders for download -----------------------------------------



yr1=2024
yr2=2025
date_range <- seq(yr1,yr2)



# setting up the main directory
main_dir <- paste("data_raw/scopus_downloads/",date_folder_for_files,sep="")
sub_dir <- paste(main_dir,"/papers/",yr1,sep="")
if (!dir.exists(sub_dir)){
  dir.create(main_dir, recursive = TRUE)
} 

date_range <- seq(yr1,yr2)

year <- seq_along(date_range)


for (j in year) {
  
  # setting up the sub directories
  
  # papers
  # check if sub directory exists 
  sub_dir <- paste(main_dir,"/papers/",date_range[j],sep="")
  # Check if subdirectory exists
  if (dir.exists(sub_dir)) {
    print("The folder exists!")
  } else {
    # Create a new subdirectory inside the main path
    dir.create(sub_dir, recursive = TRUE)
    print("Subdirectory created.")
  }
  
  # Authors
  sub_dir <- paste(main_dir,"/authors/",date_range[j],sep="")
  # Check if subdirectory exists
  if (dir.exists(sub_dir)) {
    print("The folder exists!")
  } else {
    # Create a new subdirectory inside the main path
    dir.create(sub_dir, recursive = TRUE)
    print("Subdirectory created.")
  }
  sub_dir <- paste(main_dir,"/affils/",date_range[j],sep="")
  # Check if subdirectory exists
  if (dir.exists(sub_dir)) {
    print("The folder exists!")
  } else {
    # Create a new subdirectory inside the main path
    dir.create(sub_dir, recursive = TRUE)
    print("Subdirectory created.")
  }
  
}



# STEP 1 search initial affiliation IDs by PY ------------------------------

search_term <- anti_join(affils_to_search_all,search_term_over5K) %>% 
  select(-agency_primary)
# term<-seq_along(search_term)
yr1=2024
yr2=2025
date_range <- seq(yr1,yr2)
# year <- seq_along(date_range)

search_df<-expand_grid(search_term,date_range) %>% 
  arrange(desc(date_range))
# search_df <-search_df[580:nrow(search_df),] 
run <- seq(nrow(search_df))


# for (j in year) {
#   for (h in term){
#     
#     
#     # Print progress every 250-th value of h
#     if (h %% 50 == 0) {
#       # Safely handle if term is indexed by h
#       cat("Progress — h:", h, "; term:", search_term[h], "\n")
#     }


for (j in run) {
  
  
      # Print progress every 250-th value of j
      if (j %% 10 == 0) {
        # Safely handle if term is indexed by h
        cat("Progress:", j, "; term:", search_df[j,1], "\n")
      }
  
  a <- paste("(AF-ID('", search_df[j,1], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
  
  b <- paste0(" AND PUBYEAR IS ", search_df[j,2],")")
  # b <- paste(" AND (PUBYEAR AFT 2018)", sep = "")
  
  query_string <- paste0(a, b)
  
  
    

    scopus_data <- rscopus::scopus_search(query_string,
                                          max_count=8000,
                                          view = "COMPLETE",
                                          verbose = FALSE,
                                          api_key = api)
    
    
  
    scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
    
    if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
      next
    }else{
      scopus_papers <- scopus_data_raw$df
      scopus_affiliations <- scopus_data_raw$affiliation
      scopus_authors <- scopus_data_raw$author
      
      
      
      folder_for_files<-paste0(date_folder_for_files,"/",search_df[j,2])
      
      term_for_file<-paste0("scopus_affil_",search_df[j,1], "_", search_df[j,2])
      
      # 
      # folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
      # term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
      # 
      
      papers <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/papers/",search_df[j,2],"/",term_for_file,"_papers", ".csv", sep = "")
      suppressMessages(readr::write_csv(scopus_papers, papers))
      
      affils <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/affils/",search_df[j,2],"/",term_for_file,"_affils", ".csv", sep = "")
      suppressMessages(readr::write_csv(scopus_affiliations, affils))
      
      authors <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/authors/",search_df[j,2],"/",term_for_file,"_author", ".csv", sep = "")
      suppressMessages(readr::write_csv(scopus_authors, authors))
    }
  }



# STEP 2: search original but >5K by upload month ---------------------------

# In these you need to gather by PY and upload month, starting 3 years BEFORE
pub_year<-2024

search_term <-search_term_over5K$affil_id

# term<-seq_along(search_term)
months <- sprintf("%02d", 1:12)
# month <- seq_along(months)

search_df <- expand_grid(search_term, months)



# yr1=2025
# yr2=2025
# date_range <- seq(yr1,yr2)


final_study_yr<-2025
date_range <- seq(pub_year-5,final_study_yr) # this is the date range searching for upload of pubs from that year (5 year prior to publication through end of 2025)
# year <- seq_along(date_range)

search_df <- expand_grid(search_df, date_range) %>% 
  rename(load_mo=months,
         load_yr=date_range) %>% 
  arrange(search_term,load_yr,load_mo)

# search_df <-search_df[580:nrow(search_df),] 
run <- seq(nrow(search_df))




for (j in run) {
  
  
  
  # Print progress every 250-th value of j
  if (j %% 12 == 0) {
    # Safely handle if term is indexed by h
    cat("Progress: term / month / year :", (paste(search_df[j,1],search_df[j,2],search_df[j,3],sep="-")), "\n")
  }
  
  
  
  
  start_date <- paste0(search_df[j,3],search_df[j,2], "01")
  if(search_df[j,2]!="12"){
    end_date <- paste0(search_df[j,3],search_df[j+1,2], "02")
  }
  if(search_df[j,2]=="12"){
    end_date <- paste0(search_df[j,3]+1,"01", "02")
  }
  
  
  
  a <- paste("(AF-ID('", search_df[j,1], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
  
  # a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar))", sep = "")
  b <- paste0(" AND PUBYEAR IS ", pub_year)
  # b <- paste(" AND (PUBYEAR AFT 2018)", sep = "")
  
  c<- paste(" AND ORIG-LOAD-DATE > ",start_date, " AND ORIG-LOAD-DATE < ", end_date,")",sep="")
  
  
  query_string <- paste0(a, b, c)
  # query_string <- paste0(a, c)
  
  scopus_data <- rscopus::scopus_search(query_string,
                                        max_count=8000,
                                        # start = 0,
                                        verbose = FALSE,
                                        view = "COMPLETE",
                                        api_key = api)
  
  
  
  scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
  
  if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
    next
  }else{
    scopus_papers <- scopus_data_raw$df
    
    
    scopus_papers <- scopus_data_raw$df
    scopus_affiliations <- scopus_data_raw$affiliation
    scopus_authors <- scopus_data_raw$author
    
    
    folder_for_files<-paste0(date_folder_for_files,"/",pub_year)
    
    term_for_file<-paste("scopus_affil_",search_df[j,1], "_", search_df[j,3],"_",search_df[j,2], sep = "")
    
    
    papers <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/papers/",pub_year,"/",term_for_file,"_papers", ".csv", sep = "")
    write_csv(scopus_papers, papers)
    
    
    
    affils <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/affils/",pub_year,"/",term_for_file,"_affils", ".csv", sep = "")
    write_csv(scopus_affiliations, affils)
    
    
    authors <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/authors/",pub_year,"/",term_for_file,"_author", ".csv", sep = "")
    write_csv(scopus_authors, authors)
    
  }
}








# parrallellizing #1  -----------------------------------------------------

library(furrr)
library(future)

# Set up parallel processing
plan(multisession, workers = 2)  # Adjust based on API limits and CPU cores

# Prepare search grid
search_term <- anti_join(affils_to_search_all, search_term_over5K) %>% 
  select(-agency_primary) 

yr1 <- 2024
yr2 <- 2025
date_range <- seq(yr1, yr2)

search_df <- expand_grid(search_term, date_range) %>% 
  arrange(desc(date_range)) %>%
  mutate(row_id = row_number())

# Define processing function
process_search <- function(row_data, api, date_folder_for_files) {
  
  j <- row_data$row_id
  
  # Progress reporting
  if (j %% 10 == 0) {
    cat("Progress:", j, "; term:", row_data$affil_id, "\n")
  }
  
  # Build query
  a <- paste0("(AF-ID('", row_data$affil_id, "') AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))")
  b <- paste0(" AND PUBYEAR IS ", row_data$date_range, ")")
  query_string <- paste0(a, b)
  
  # API call with error handling
  tryCatch({
    scopus_data <- rscopus::scopus_search(
      query_string,
      max_count = 5001,
      view = "COMPLETE",
      verbose = FALSE,
      api_key = api
    )
    
    scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
    
    # Check if valid data
    if (nrow(scopus_data_raw$df) == 1 & ncol(scopus_data_raw$df) == 3) {
      return(list(success = TRUE, skipped = TRUE, 
                  term = row_data$affil_id, year = row_data$date_range))
    }
    
    # Extract data
    scopus_papers <- scopus_data_raw$df
    scopus_affiliations <- scopus_data_raw$affiliation
    scopus_authors <- scopus_data_raw$author
    
    # Create file paths
    term_for_file <- paste0("scopus_affil_", row_data$affil_id, "_", row_data$date_range)
    
    papers_path <- paste0("data_raw/scopus_downloads/", date_folder_for_files, 
                          "/papers/", row_data$date_range, "/", term_for_file, "_papers.csv")
    affils_path <- paste0("data_raw/scopus_downloads/", date_folder_for_files, 
                          "/affils/", row_data$date_range, "/", term_for_file, "_affils.csv")
    authors_path <- paste0("data_raw/scopus_downloads/", date_folder_for_files, 
                           "/authors/", row_data$date_range, "/", term_for_file, "_author.csv")
    
    # Write files
    suppressMessages({
      write_csv(scopus_papers, papers_path)
      write_csv(scopus_affiliations, affils_path)
      write_csv(scopus_authors, authors_path)
    })
    
    return(list(success = TRUE, skipped = FALSE,
                term = row_data$affil_id, year = row_data$date_range))
    
  }, error = function(e) {
    return(list(success = FALSE, term = row_data$affil_id, 
                year = row_data$date_range, error = e$message))
  })
}

# Run in parallel
results <- future_map(
  split(search_df, 1:nrow(search_df)),
  ~ process_search(.x, api, date_folder_for_files),
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

# Summarize results
successful <- sum(sapply(results, function(x) x$success && !x$skipped))
skipped <- sum(sapply(results, function(x) x$success && x$skipped))
failed <- sum(sapply(results, function(x) !x$success))

cat("\nCompleted:", successful, "successful,", skipped, "skipped (empty),", failed, "failed\n")

# Get failed items
failed_items <- results %>%
  keep(~ !.x$success) %>%
  map_df(~ tibble(term = .x$term, year = .x$year, error = .x$error))

if (nrow(failed_items) > 0) {
  cat("\nFailed items:\n")
  print(failed_items)
}

# Clean up
plan(sequential)

# parraellizing # 2-----------------------------------------------------------

library(furrr)
library(future)

# Set up parallel processing (adjust workers based on your CPU cores)
plan(multisession, workers = 4)  # Use 4 cores, adjust as needed


pub_year <- 2024
search_term <- search_term_over5K$affil_id
months <- sprintf("%02d", 1:12)
final_study_yr <- 2025
date_range <- seq(pub_year - 5, final_study_yr)

search_df <- expand_grid(search_term, months, date_range) %>% 
  rename(load_mo = months,
         load_yr = date_range) %>% 
  arrange(search_term, load_yr, load_mo) %>%
  mutate(row_id = row_number())

# Define the processing function
process_search <- function(row_data, pub_year, api, date_folder_for_files) {
  
  j <- row_data$row_id
  
  # Progress reporting (will show in each worker)
  if (j %% 12 == 0) {
    cat("Progress: term / month / year :", 
        paste(row_data$search_term, row_data$load_mo, row_data$load_yr, sep = "-"), "\n")
  }
  
  # Calculate dates
  start_date <- paste0(row_data$load_yr, row_data$load_mo, "01")
  
  if (row_data$load_mo != "12") {
    next_month <- sprintf("%02d", as.numeric(row_data$load_mo) + 1)
    end_date <- paste0(row_data$load_yr, next_month, "02")
  } else {
    end_date <- paste0(row_data$load_yr + 1, "01", "02")
  }
  
  # Build query
  a <- paste0("(AF-ID('", row_data$search_term, "') AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))")
  b <- paste0(" AND PUBYEAR IS ", pub_year)
  c <- paste0(" AND ORIG-LOAD-DATE > ", start_date, " AND ORIG-LOAD-DATE < ", end_date, ")")
  query_string <- paste0(a, b, c)
  
  # API call with error handling
  tryCatch({
    scopus_data <- rscopus::scopus_search(
      query_string,
      max_count = 8000,
      verbose = FALSE,
      view = "COMPLETE",
      api_key = api
    )
    
    scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
    
    # Check if valid data
    if (nrow(scopus_data_raw$df) == 1 & ncol(scopus_data_raw$df) == 3) {
      return(NULL)  # Skip empty results
    }
    
    # Extract data
    scopus_papers <- scopus_data_raw$df
    scopus_affiliations <- scopus_data_raw$affiliation
    scopus_authors <- scopus_data_raw$author
    
    # Create file paths
    term_for_file <- paste0("scopus_affil_", row_data$search_term, "_", 
                            row_data$load_yr, "_", row_data$load_mo)
    
    papers_path <- paste0("data_raw/scopus_downloads/", date_folder_for_files, 
                          "/papers/", pub_year, "/", term_for_file, "_papers.csv")
    affils_path <- paste0("data_raw/scopus_downloads/", date_folder_for_files, 
                          "/affils/", pub_year, "/", term_for_file, "_affils.csv")
    authors_path <- paste0("data_raw/scopus_downloads/", date_folder_for_files, 
                           "/authors/", pub_year, "/", term_for_file, "_author.csv")
    
    # Write files
    write_csv(scopus_papers, papers_path)
    write_csv(scopus_affiliations, affils_path)
    write_csv(scopus_authors, authors_path)
    
    return(list(success = TRUE, term = row_data$search_term, 
                month = row_data$load_mo, year = row_data$load_yr))
    
  }, error = function(e) {
    return(list(success = FALSE, term = row_data$search_term, 
                month = row_data$load_mo, year = row_data$load_yr, 
                error = e$message))
  })
}

# Run in parallel
results <- future_map(
  split(search_df, 1:nrow(search_df)),
  ~ process_search(.x, pub_year, api, date_folder_for_files),
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

# Summarize results
successful <- sum(sapply(results, function(x) !is.null(x) && x$success))
failed <- sum(sapply(results, function(x) !is.null(x) && !x$success))

cat("\nCompleted:", successful, "successful,", failed, "failed\n")

# Clean up
plan(sequential)



# 
# # STEP 3 - SEARCH THE NEW ONES BY PY --------------------------------------
# 
# search_term<-anti_join(affils_to_search_followup,affils_to_search_original) 
# search_term<-search_term$affil_id
# term<-seq_along(search_term)
# 
# 
# yr1=2019
# yr2=2025
# date_range <- seq(yr1,yr2)
# year <- seq_along(date_range)
# 
# for (j in year) {
#   for (h in term){
#     
#     
#     # Print progress every 250-th value of h
#     if (h %% 50 == 0) {
#       # Safely handle if term is indexed by h
#       cat("Progress — h:", h, "; term:", search_term[h], "\n")
#     }
#     
#     a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
#     
#     
#     c <- " AND PUBYEAR IS "
#     
#     query_string <-paste0(a, c, date_range[j],")",sep = "")
#     
#     
#     scopus_data <- rscopus::scopus_search(query_string,
#                                           max_count=8000,
#                                           view = "COMPLETE",
#                                           verbose = FALSE,
#                                           api_key = api)
#     
#     
#     
#     scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
#     
#     if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
#       next
#     }else{
#       scopus_papers <- scopus_data_raw$df
#       scopus_affiliations <- scopus_data_raw$affiliation
#       scopus_authors <- scopus_data_raw$author
#       
#       folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
#       term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
#       
#       
#       papers <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
#       suppressMessages(readr::write_csv(scopus_papers, papers))
#       
#       affils <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
#       suppressMessages(readr::write_csv(scopus_affiliations, affils))
#       
#       authors <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
#       suppressMessages(readr::write_csv(scopus_authors, authors))
#     }
#   }
# }



# STEP 4: CHECK ANY UPLOADS OF ALL NOs AFTER INITIAL SEARCHES ----------------------

# after the initial three steps, only need to run this step to update. it checks all of them for uploads of every PY
# after the date of the initial three searches.


all_terms<-bind_rows(affils_to_search_all, search_term_over5K) %>% 
  select(affil_id) %>% 
  distinct() 
search_term<-all_terms$affil_id
# search_term<-all_terms$affil_id[580:nrow(all_terms)]
term<-seq_along(search_term)
months <- sprintf("%02d", 1:12)

# select months since most recent search and present month
# months<-months[1:2]
# search_df <- expand_grid(search_term, months)
# month <- seq_along(months)

# scan_year = seq(2017,2025,by=1)
# scan_year_seq = seq_along(scan_year)
yr1 = 2019
yr2 = 2025
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)

check_month = 12
check_year = 2025

for (h in term) {
  # for (j in year) {
    # for  (k in month) {
      
      
      
      # Print progress every 250-th value of h
      if (h %% 50 == 0) {
        # Safely handle if term is indexed by h
        cat("Progress — h:", h, "; term:", search_term[h], "\n")
      }
      
      
      
      
      start_date <- paste0(check_year,months[check_month], "01",sep="")
      if(check_month<12){
        end_date <- paste0(check_year,(months[check_month+1]), "02",sep="")
      }
      if(check_month==12){
        end_date <- paste0(check_year+1,months[1], "02",sep="")
      }
      
      
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
      
      b <- paste(" AND PUBYEAR AFT ", yr1-1, sep = "")
      
      c<- paste(" AND ORIG-LOAD-DATE > ",start_date,")",sep="")
      
      
      query_string <- paste0(a, b, c)
      # query_string <- paste0(a, c)
      
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=8000,
                                            # start = 0,
                                            verbose = FALSE,
                                            view = "COMPLETE",
                                            api_key = api)
      
      
      
      scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
      
      if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
        next
      }else{
        scopus_papers <- scopus_data_raw$df
        
        
        scopus_papers <- scopus_data_raw$df
        scopus_affiliations <- scopus_data_raw$affiliation
        scopus_authors <- scopus_data_raw$author
        
        
        folder_for_files<-paste(date_folder_for_files,"/",check_year,sep="")
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", start_date, sep = "")
        
        
        papers <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/papers/",check_year,"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        
        
        affils <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/affils/",check_year,"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        
        authors <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/authors/",check_year,"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
      }
    }
















###########################################################################
# original api loop -------------------------------------------------------



# this removes the two that are searched by month below
affils_to_search<-affils_to_search %>% 
  select(affil_id) %>% 
  filter(affil_id!=60006577) %>% 
  filter(affil_id!= 60014232) # BIT ONLY YEAR 2021 
search_term<-affils_to_search$affil_id[1:nrow(affils_to_search)]
# search_term<-affils_to_search$affil_id[3444:nrow(affils_to_search)]

# THIS IS TO DO THE "ADDITIONAL ONES DIUSCOVERED AFTER FIRST REVIEW
# affils_to_search_rd2<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") %>% 
#   anti_join(affils_to_search,by="affil_id")
# search_term<-affils_to_search_rd2$affil_id[248:nrow(affils_to_search_rd2)]
# affils_to_search<-60014232
# 2022,2023
yr1=2022
yr2=2022
date_range <- seq(yr1,yr2)


year <- seq_along(date_range)


# search_term<-affils_to_search$affil_id[1:nrow(affils_to_search)]
search_term<-affils_to_search$affil_id[5950:nrow(affils_to_search)]
# term <- seq(7745,nrow(affils_to_search),by=1)
term <- seq_along(search_term)
# term<-seq(3301,nrow(affils_to_search),by=1)

# date_folder_for_files<-"scopus_downloads/fed_20250901"
# date_folder_for_files<-"scopus_downloads/fed_20251010"
date_folder_for_files<-"scopus_downloads/fed_20251210"



# create folders  ---------------------------------------------------------




for (j in year) {


# setting up the main directory
main_dir <- paste("data_raw/",date_folder_for_files,sep="")
sub_dir <- paste(main_dir,"/papers/",date_range[j],sep="")
if (!dir.exists(sub_dir)){
  dir.create(main_dir, recursive = TRUE)
} 

# setting up the sub directories
# papers

sub_dir <- paste(main_dir,"/papers/",date_range[j],sep="")
# check if sub directory exists 

# Check if subdirectory exists
if (dir.exists(sub_dir)) {
  print("The folder exists!")
} else {
  # Create a new subdirectory inside the main path
  dir.create(sub_dir, recursive = TRUE)
  print("Subdirectory created.")
}

# Authors
sub_dir <- paste(main_dir,"/authors/",date_range[j],sep="")
# Check if subdirectory exists
if (dir.exists(sub_dir)) {
  print("The folder exists!")
} else {
  # Create a new subdirectory inside the main path
  dir.create(sub_dir, recursive = TRUE)
  print("Subdirectory created.")
}
sub_dir <- paste(main_dir,"/affils/",date_range[j],sep="")
# Check if subdirectory exists
if (dir.exists(sub_dir)) {
  print("The folder exists!")
} else {
  # Create a new subdirectory inside the main path
  dir.create(sub_dir, recursive = TRUE)
  print("Subdirectory created.")
}
}


#  initialize searches ----------------------------------------------------


library("tictoc")
tic("total time")
for (j in year) {
for (h in term){
  
  
    # Print progress every 250-th value of h
    if (h %% 50 == 0) {
    # Safely handle if term is indexed by h
    cat("Progress — h:", h, "; term:", search_term[h], "\n")
  }
        
        a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
        

        c <- " AND PUBYEAR IS "
        
        query_string <-paste0(a, c, date_range[j],")",sep = "")
        
        
        # query_string<-"AF-ID(60014232) AND TITLE(Multilevel Regulation of Protein Kinase) AND PUBYEAR = 2020"
        # query_string<-"AF-ID(60014232) AND PUBYEAR = 2022"
        scopus_data <- rscopus::scopus_search(query_string,
                                              max_count=8000,
                                              view = "COMPLETE",
                                              verbose = FALSE,
                                              api_key = api)
        
        
        # Suppress all messages (and optionally warnings) from scopus_search
        # scopus_data <- suppressWarnings(
        #   suppressMessages(
        #     rscopus::scopus_search(
        #       query_string,
        #       max_count = 8000,
        #       view      = "COMPLETE",
        #       api_key   = api
        #       )
        #     )
        #   )
        
        
        
        
        scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
        
        if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
          next
        }else{
        scopus_papers <- scopus_data_raw$df
        scopus_affiliations <- scopus_data_raw$affiliation
        scopus_authors <- scopus_data_raw$author
        
        folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        
        
        papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        suppressMessages(readr::write_csv(scopus_papers, papers))
        
        affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        suppressMessages(readr::write_csv(scopus_affiliations, affils))
        
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
        suppressMessages(readr::write_csv(scopus_authors, authors))
        }
      }
    }
toc(log = TRUE)
tic.log(format = TRUE) 
tic.clearlog()

    



# search by category ---------------------------------------------------------

search_term<-c(60014232,60006577) 
# months <- month.name

# Need to split:
# 60006577 (2019-2024), 60014232 (2019-2022)
yr1=2019
yr2=2021

date_range <- seq(yr1,yr2)
# str(search_term)

pubcat<-c("re","ch","ed","le","dp","no","ar")
term <- seq_along(search_term)

year <- seq_along(date_range)

cat <- seq_along(pubcat)


for (h in term) {
  for (j in year) {
    for  (k in cat) {
      
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND DOCTYPE(",pubcat[k],")", sep = "")
      
      b <- paste(" AND PUBYEAR IS ", date_range[j], ")", sep = "")
      
    
      
      
      # query_string <- paste0(a, b, c)
      query_string <- paste0(a, b)
      
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=5000,
                                            # start = 0,
                                            view = "COMPLETE",
                                            # wait_time = 0.2,
                                            api_key = api)
      
      
      
      # url <- paste0(
      #   "https://api.elsevier.com/content/search/scopus?query=AF-ID(60014232)%20AND%20pubyear%20%3D%202024%20AND%20(DOCTYPE(ar)%20OR%20DOCTYPE(re)%20OR%20DOCTYPE(ch)%20OR%20DOCTYPE(ed)%20OR%20DOCTYPE(le)%20OR%20DOCTYPE(dp)%20OR%20DOCTYPE(no))",
      #   "&cursor=", URLencode(next_cursor, reserved = TRUE),
      #   # "&count=200&apiKey=", Sys.getenv("SCOPUS_API_KEY")
      #   "&count=200&apiKey=c253aa47dd592442b1d5ad7ded7b0514")
      # 
      
      
      
      
      
      
      scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
      
      if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
        next
      }else{
        scopus_papers <- scopus_data_raw$df
        scopus_affiliations <- scopus_data_raw$affiliation
        scopus_authors <- scopus_data_raw$author
        
        
        folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", date_range[j], "_", pubcat[k], sep = "")
        
        
        papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        
        
        
        affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        
      
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
        
        
        
      }
    }
  }
}




# for >5K: search by cat and OA/Not ---------------------------------------


# search by category ---------------------------------------------------------

search_term<-c(60006577) 
# months <- month.name

# Need to split:
# 60006577 (2019-2024), 60014232 (2019-2022)
yr1=2021
yr2=2021

date_range <- seq(yr1,yr2)
# str(search_term)
pubcat<-c("ar")
oa<-c("AND OA(publisherfullgold))", "AND NOT OA(publisherfullgold))")
# oa<-c("AND OA(all))", "AND NOT OA(all))")
oa_for_filename<-c("oa","notoa")
# All Open Access - all
# Gold Open Access - publisherfullgold
# Hybrid Gold Open Access - publisherhybridgold
# Bronze Open Access - publisherfree2read
# All Green Open Access* - repository
# Green Final Open Access - repositoryvor
# Green Accepted Open Access - repositoryam
# (OA(publisherfullgold) OR OA(publisherhybridgold))


# SUBJAREA(MEDI OR NURS OR VETE OR DENT OR HEAL OR MULT) # Health Sciences
# SUBJAREA(AGRI OR BIOC OR IMMU OR NEUR OR PHAR). # all life sci
# SUBJAREA(CENG OR CHEM OR COMP OR EART OR ENER OR ENGI OR ENVI OR MATE OR MATH OR PHYS) #all phys sci
# SUBJAREA(ARTS OR BUSI OR DECI OR ECON OR PSYC OR SOCI) # All Social Sciences

term <- seq_along(search_term)
year <- seq_along(date_range)
cat <- seq_along(pubcat)
oa_cat<-seq_along(oa)


for (h in term) {
  for (j in year) {
    for  (k in cat) {
      for  (m in oa_cat) {  
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND DOCTYPE(",pubcat[k],")", sep = "")
      
      b <- paste(" AND PUBYEAR IS ", date_range[j], " ", sep = "")
      
      c <- oa[m]
      
      
      query_string <- paste0(a, b, c)
      # query_string <- paste0(a, b)
      
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=5000,
                                            # start = 0,
                                            view = "COMPLETE",
                                            # wait_time = 0.2,
                                            api_key = api)
      
      
      
      # url <- paste0(
      #   "https://api.elsevier.com/content/search/scopus?query=AF-ID(60014232)%20AND%20pubyear%20%3D%202024%20AND%20(DOCTYPE(ar)%20OR%20DOCTYPE(re)%20OR%20DOCTYPE(ch)%20OR%20DOCTYPE(ed)%20OR%20DOCTYPE(le)%20OR%20DOCTYPE(dp)%20OR%20DOCTYPE(no))",
      #   "&cursor=", URLencode(next_cursor, reserved = TRUE),
      #   # "&count=200&apiKey=", Sys.getenv("SCOPUS_API_KEY")
      #   "&count=200&apiKey=c253aa47dd592442b1d5ad7ded7b0514")
      # 
      
      
      
      
      
      
      scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
      
      if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
        next
      }else{
        scopus_papers <- scopus_data_raw$df
        scopus_affiliations <- scopus_data_raw$affiliation
        scopus_authors <- scopus_data_raw$author
        
        
        folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", date_range[j], "_", pubcat[k], "_", oa_for_filename[m], sep = "")
        
        
        papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        
        
        
        affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        
        
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
        
        
        
      }
    }
  }
  }
}

    

# search by month ---------------------------------------------------------

search_term<-c(60014232,60006577) 
# months <- month.name
months <- sprintf("%02d", 1:12)
  # Need to split:
  # 60006577 (2019-2024), 60014232 (2019-2022)
# yr1=2025
# yr2=2025

date_range <- seq(yr1,yr2)
# str(search_term)


term <- seq_along(search_term)

year <- seq_along(date_range)

month <- seq_along(months)

# 
# Multilevel regulation of protein kinase CdI alternative splicing by lithium chloride
# ((AF-ID("VA Medical Center" 60014232) AND PUBYEAR > 2018) AND (multilevel AND regulation AND of AND protein AND kinase AND cdi AND alternative AND splicing AND by AND lithium AND chloride))
# # (AF-ID("VA Medical Center" 60014232) AND PUBYEAR > 2000 AND (PUBDATETXT(JANUARY) OR PUBDATETXT(FEBRUARY) OR PUBDATETXT(march) OR PUBDATETXT(april) OR PUBDATETXT(may) OR PUBDATETXT(june) OR PUBDATETXT(july) OR PUBDATETXT(august) OR PUBDATETXT(september) OR PUBDATETXT(october) OR PUBDATETXT(november) OR PUBDATETXT(december)))
# returns NOTHING: (AF-ID(60014232) AND (multilevel AND regulation AND of AND protein AND kinase AND cdi AND alternative AND splicing AND by AND lithium AND chloride) AND ( PUBDATETXT ( JANUARY ) OR PUBDATETXT ( FEBRUARY ) OR PUBDATETXT ( march ) OR PUBDATETXT ( april ) OR PUBDATETXT ( may ) OR PUBDATETXT ( june ) OR PUBDATETXT ( july ) OR PUBDATETXT ( august ) OR PUBDATETXT ( september ) OR PUBDATETXT ( october ) OR PUBDATETXT ( november ) OR PUBDATETXT ( december )))
# returns 144881: (AF-ID("VA Medical Center" 60014232) AND (PUBDATETXT(JANUARY) OR PUBDATETXT(FEBRUARY) OR PUBDATETXT(march) OR PUBDATETXT(april) OR PUBDATETXT(may) OR PUBDATETXT(june) OR PUBDATETXT(july) OR PUBDATETXT(august) OR PUBDATETXT(september) OR PUBDATETXT(october) OR PUBDATETXT(november) OR PUBDATETXT(december)))
# returns 28446 ( AF-ID ( 60014232 ) AND PUBYEAR > 2018 AND ( PUBDATETXT ( JANUARY ) OR PUBDATETXT ( FEBRUARY ) OR PUBDATETXT ( march ) OR PUBDATETXT ( april ) OR PUBDATETXT ( may ) OR PUBDATETXT ( june ) OR PUBDATETXT ( july ) OR PUBDATETXT ( august ) OR PUBDATETXT ( september ) OR PUBDATETXT ( october ) OR PUBDATETXT ( november ) OR PUBDATETXT ( december ) ) )
# ( ( AF-ID ( 60014232 ) AND PUBYEAR = 2020 ) ) AND ( PUBDATETXT ( JANUARY ) OR PUBDATETXT ( FEBRUARY ) OR PUBDATETXT ( march ) OR PUBDATETXT ( april ) OR PUBDATETXT ( may ) OR PUBDATETXT ( june ) OR PUBDATETXT ( july ) OR PUBDATETXT ( august ) OR PUBDATETXT ( september ) OR PUBDATETXT ( october ) OR PUBDATETXT ( november ) OR PUBDATETXT ( december ) )
# USE THIS !!!! #   query_string<-'AUTH(bader,D) AND AF-ID(60014232) AND (ORIG-LOAD-DATE > 20191231 AND ORIG-LOAD-DATE < 20241231)'

# TEST ARTICLE: https://pubmed.ncbi.nlm.nih.gov/33288642/
# RETRUSN BADER ARTICVLE # ( ( AF-ID ( "VA Medical Center" 60014232 ) AND ( DOCTYPE ( ar ) OR DOCTYPE ( re ) OR DOCTYPE ( ch ) OR DOCTYPE ( ed ) OR DOCTYPE ( le ) OR DOCTYPE ( dp ) OR DOCTYPE ( no ) ) AND ORIG-LOAD-DATE > 20210301 ) ) AND ( bader D )

  
for (h in term) {
  for (j in year) {
    for  (k in month) {
      
      
      start_date <- paste0(date_range[j],months[k], "01",sep="")
      if(k<12){
      end_date <- paste0(date_range[j],months[k+1], "02",sep="")
      }
      if(k==12){
        end_date <- paste0(date_range[j]+1,months[1], "02",sep="")
      }
      
      
      # query_string<-'AUTH(bader,D) AND AF-ID(60014232) AND (PUBDATETXT(JANUARY) OR PUBDATETXT(FEBRUARY) OR PUBDATETXT(march) OR PUBDATETXT(april) OR PUBDATETXT(may) OR PUBDATETXT(june) OR PUBDATETXT(july) OR PUBDATETXT(august) OR PUBDATETXT(september) OR PUBDATETXT(october) OR PUBDATETXT(november) OR PUBDATETXT(december))'
      # query_string<-'AF-ID(60014232) AND COVERDATE(2020-01-01)'
      # AF-ID(60014232) AND ORIG-LOAD-DATE > 20201231 OR ORIG-LOAD-DATE = 20201231
      
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
      
      # b <- paste(" AND (PUBYEAR = ", date_range[j], ")", sep = "")
      
      c<- paste(" AND ORIG-LOAD-DATE > ",start_date, " AND ORIG-LOAD-DATE < ", end_date,")",sep="")
      
      
      # query_string <- paste0(a, b, c)
      query_string <- paste0(a, c)
      
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=8000,
                                            # start = 0,
                                            view = "COMPLETE",
                                            api_key = api)
      
      
      
      # url <- paste0(
      #   "https://api.elsevier.com/content/search/scopus?query=AF-ID(60014232)%20AND%20pubyear%20%3D%202024%20AND%20(DOCTYPE(ar)%20OR%20DOCTYPE(re)%20OR%20DOCTYPE(ch)%20OR%20DOCTYPE(ed)%20OR%20DOCTYPE(le)%20OR%20DOCTYPE(dp)%20OR%20DOCTYPE(no))",
      #   "&cursor=", URLencode(next_cursor, reserved = TRUE),
      #   # "&count=200&apiKey=", Sys.getenv("SCOPUS_API_KEY")
      #   "&count=200&apiKey=c253aa47dd592442b1d5ad7ded7b0514")
      # 
      
      
      
      

      scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
      
      if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
        next
      }else{
        scopus_papers <- scopus_data_raw$df
        
        
        
        
        folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", date_range[j], "_", months[k], sep = "")
        
        # papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"2_papers", ".csv", sep = "")
        papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j], "_", months[k], sep = "")
        affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        # affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"2_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j], "_", months[k], sep = "")
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
        # authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"2_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
        
        
        
      }
    }
  }
}





# trying to catch any with > 5000 -----------------------------------------



# 
# # Prepare a CSV to record AFIDs that exceed the threshold
# skipped_log <- file.path("./data_raw", date_folder_for_files, "skipped_afids.csv")
# if (!file.exists(skipped_log)) {
#   utils::write.csv(
#     data.frame(AFID = character(), PUBYEAR = character(), TOTAL = integer()),
#     skipped_log,
#     row.names = FALSE
#   )
# }
# 
# # Helper to safely grab total results from rscopus search object
# get_total_results_safe <- function(x) {
#   # Try the most common locations for the total results (varies by rscopus/response)
#   tryCatch({
#     if (!is.null(x$total_results)) {
#       as.integer(x$total_results)
#     } else if (!is.null(x$content) && !is.null(x$content[["opensearch:totalResults"]])) {
#       as.integer(x$content[["opensearch:totalResults"]])
#     } else if (!is.null(x$content) && !is.null(x$content[["totalResults"]])) {
#       as.integer(x$content[["totalResults"]])
#     } else if (!is.null(x$entries)) {
#       # Fallback: number of entries returned (may be truncated by max_count)
#       length(x$entries)
#     } else {
#       NA_integer_
#     }
#   }, error = function(e) NA_integer_)
# }
# 
# for (j in year) {
#   for (h in term) {
#     
#     # Print progress every 50th value of h
#     if (h %% 50 == 0) {
#       cat("Progress — h:", h, "; term:", search_term[h], "\n")
#     }
#     
#     a <- paste(
#       "(AF-ID('", search_term[h], "')",
#       " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",
#       sep = ""
#     )
#     
#     c <- " AND PUBYEAR IS "
#     query_string <- paste0(a, c, date_range[j], ")", sep = "")
#     
#     afid <- search_term[h]
#     yr <- date_range[j]
#     
#     # ---------- Pre-check: lightweight call to get total results ----------
#     head_check <- rscopus::scopus_search(
#       query_string,
#       max_count = 1,           # minimal pull just to read totals
#       view = "STANDARD",       # lighter view is fine for totals
#       verbose = FALSE,
#       api_key = api
#     )
#     
#     total_n <- get_total_results_safe(head_check)
#     
#     # If we can read a valid total and it exceeds 5000, record and skip
#     if (!is.na(total_n) && total_n > 5000L) {
#       cat("Skipping AFID:", afid, "for year:", yr, "— total results:", total_n, "\n")
#       
#       # Append to the log CSV
#       utils::write.table(
#         data.frame(AFID = afid, PUBYEAR = yr, TOTAL = total_n),
#         file = skipped_log,
#         sep = ",",
#         col.names = FALSE,
#         row.names = FALSE,
#         append = TRUE
#       )
#       next
#     }
#     
#     # ---------- Full retrieval ----------
#     scopus_data <- rscopus::scopus_search(
#       query_string,
#       max_count = 8000,
#       view = "COMPLETE",
#       verbose = FALSE,
#       api_key = api
#     )
#     
#     scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
#     
#     # Handle the "no-data" sentinel
#     if (nrow(scopus_data_raw$df) == 1 & ncol(scopus_data_raw$df) == 3) {
#       next
#     } else {
#       scopus_papers <- scopus_data_raw$df
#       scopus_affiliations <- scopus_data_raw$affiliation
#       scopus_authors <- scopus_data_raw$author
#       
#       folder_for_files <- paste(date_folder_for_files, "/", yr, sep = "")
#       term_for_file <- paste("scopus_affil_", afid, "_", yr, sep = "")
#       
#       papers <- paste("./data_raw/", date_folder_for_files, "/papers/", yr, "/", term_for_file, "_papers", ".csv", sep = "")
#       suppressMessages(readr::write_csv(scopus_papers, papers))
#       
#       affils <- paste("./data_raw/", date_folder_for_files, "/affils/", yr, "/", term_for_file, "_affils", ".csv", sep = "")
#       suppressMessages(readr::write_csv(scopus_affiliations, affils))
#       
#       authors <- paste("./data_raw/", date_folder_for_files, "/authors/", yr, "/", term_for_file, "_author", ".csv", sep = "")
#       suppressMessages(readr::write_csv(scopus_authors, authors))
#     }
#   }
#   
