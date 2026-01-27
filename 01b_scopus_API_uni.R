
# Universities ------------------------------------------------------------

library(rscopus)
library(tidyverse)
library(data.table)
library(purrr)


api<-"----"
set_api_key(api)

# date_folder_for_files<-"uni_20251010"
# date_folder_for_files<-"uni_20251210"
date_folder_for_files<-"uni_20260101"

# AFFILS ORIGINALLY SEARCHED
affils_to_search_original<-read_csv("./data_clean/api_uni_affils_searched_2025-10-18.csv") %>% 
  select(affil_id,uni) %>% 
  distinct()

focal_uni<-affils_to_search_original %>% select(uni) %>% distinct() %>% filter(uni!="mass_general")

# FINAL LIST OF AFFILIATIONS FOUND IN DEC AFTER ANALYSIS

affils_to_search_followup <- readRDS("data_clean/affils_df_clean_uni_20251210.rds") %>% 
  select(affil_id, uni) %>% 
  distinct() %>% 
  filter(uni%in%focal_uni$uni) %>% 
  arrange(uni)

affils_to_search_all<-full_join(affils_to_search_original,
                                affils_to_search_followup) %>% 
  distinct() %>% 
  select(affil_id)

# unis and med schools with > 5K per year
search_term_over5K <- tibble(affil_id= 
  c(
    60013959,
    60002746,
    60029929,
    60025778,
    60029445,
    60003500,
    60006297,
    60012708,
    60032838,
    60027550,
    60030612,
    60023691,
    60031970,
    60025111,
    60009982,
    60033182,
    60003711,
    60028548,
    60015481)) 
  


# create the folders for download -----------------------------------------



yr1=2019
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




# STEP 1 search affiliations with < 5000 pubs by PY ------------------------------
# 
search_term <- anti_join(affils_to_search_all,search_term_over5K)
search_term<-search_term$affil_id
# search_term<-search_term[447:length(search_term)]
term<-seq_along(search_term)
yr1=2024
yr2=2025
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)

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


    scopus_data <- rscopus::scopus_search(query_string,
                                          max_count=5000,
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

      folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
      term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")


      papers <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
      suppressMessages(readr::write_csv(scopus_papers, papers))

      affils <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
      suppressMessages(readr::write_csv(scopus_affiliations, affils))

      authors <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
      suppressMessages(readr::write_csv(scopus_authors, authors))
    }
  }
}





# STEP 2: search original but >5K by upload month ---------------------------

# In these you need to gather by PY and upload month, starting 2 years BEFORE


# search_term<-search_term_over5K$affil_id
search_term <-search_term_over5K$affil_id
# search_term <-search_term[13:length(search_term)]
term<-seq_along(search_term)
months <- sprintf("%02d", 1:12)
month <- seq_along(months)

# yr1=2025
# yr2=2025
# date_range <- seq(yr1,yr2)

pub_year<-2025
date_range <- seq(pub_year-2,pub_year) # this is the date range searching for upload of pubs from that year (2 year sliding windown
year <- seq_along(date_range)


  for (j in year) {
    
    
    # Print progress every 250-th value of j
    if (j %% 1 == 0) {
      # Safely handle if term is indexed byjh
      cat("Progress — j: ", j, "; year:", date_range[j], "\n")
    }
    
    
    for  (k in month) {
      
      
      # Print progress every 250-th value of j
      if (k %% 1 == 0) {
        # Safely handle if term is indexed byjh
        cat("Progress — k: ", k, "; month:", months[k], "\n")
      }
      
      for (h in term) {      
      
      
      # Print progress every 250-th value of h
      if (h %% 5 == 0) {
        # Safely handle if term is indexed by h
        cat("Progress — h:", h, "; search term:", search_term[h], "\n")
      }
      
      
      # This uses a sliding 2 year window
      
      start_date <- paste0(date_range[j],months[k], "01",sep="")
      if(k<12){
        end_date <- paste0(date_range[j],months[k+1], "02",sep="")
      }
      if(k==12){
        end_date <- paste0(date_range[j]+1,"01", "02",sep="")
      }
      
      
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
      
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
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", date_range[j],"_",months[k], sep = "")
        
        
        papers <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/papers/",pub_year,"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        
        
        affils <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/affils/",pub_year,"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        
        authors <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/authors/",pub_year,"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
      }
    }
  }
}



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



# STEP 4: CHECK UPLOAD OF ALL AND ALL YEARS AFTER INITIAL SEARCH DATE ----------------------

# after the initial three steps, only need to run this step to update. it checks all of them for uploads of every PY
# after the date of the initial three searches.


all_terms<-affils_to_search_all %>%  
  distinct()
search_term<-all_terms$affil_id
# search_term<-all_terms$affil_id[580:nrow(all_terms)]
term<-seq_along(search_term)
months <- sprintf("%02d", 1:12)
month <- seq_along(months)

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
    
    
    papers <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/papers/",check_year,"/",term_for_file,"_papers", ".csv", sep = "")
    write_csv(scopus_papers, papers)
    
    
    
    affils <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/affils/",check_year,"/",term_for_file,"_affils", ".csv", sep = "")
    write_csv(scopus_affiliations, affils)
    
    
    authors <- paste("data_raw/scopus_downloads/",date_folder_for_files,"/authors/",check_year,"/",term_for_file,"_author", ".csv", sep = "")
    write_csv(scopus_authors, authors)
    
  }
}
















###########################################################################
































# create folders  ---------------------------------------------------------


# setting up the main directory
main_dir <- paste("data_raw/scopus_downloads/",date_folder_for_files,sep="")
sub_dir <- paste(main_dir,"/papers/",yr1,sep="")
if (!dir.exists(sub_dir)){
  dir.create(main_dir, recursive = TRUE)
} 



# start scopus search here ------------------------------------------------


# affils_df<-read_csv("./data_clean/api_uni_affils_searched_2025-09-01.csv")
affils_df<-read_csv("./data_clean/api_uni_affils_searched_2025-10-18.csv")


search_by_month<- 
  c(
    60013959,
    60002746,
    60029929,
    60025778,
    60029445,
    60003500,
    60006297,
    60012708,
    60032838,
    60027550,
    60030612,
    60023691,
    60031970,
    60025111,
    60009982,
    60033182,
    60003711,
    60028548,
    60015481)


affils_to_search<-affils_df %>% select(affil_id) %>% filter(!affil_id%in%search_by_month)

search_term<-affils_to_search$affil_id[1:nrow(affils_to_search)]
# search_term<-affils_to_search$affil_id[437:nrow(affils_to_search)]

# 
# affil_ids<- read_csv("./data_raw/affiliations_to_search/uni_affils/follow_up/uni_affils_follow_up.csv") %>%
# select(affil_id)
# 
# search_term<-affil_ids$affil_id[22:nrow(affil_ids)]
#  search by year ----------------------------------------------------------
# yr1=2021
# yr2=2022
# search_term<-"129430709"
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)
term <- seq_along(search_term)

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
            
            for (h in term){    
        
              
              # Print progress every 250-th value of h
              if (h %% 50 == 0) {
                # Safely handle if term is indexed by h
                cat("Progress — h:", h, "; term:", search_term[h], "\n")
              }
                
        
        a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
              c <- " AND PUBYEAR IS "
        
        query_string <-paste0(a, c, date_range[j],")",sep = "")
      
        
        scopus_data <- rscopus::scopus_search(query_string,
                                              max_count=5000,
                                              view = "COMPLETE",
                                              verbose = TRUE,
                                              api_key = api)
        
        
        
        
        

        
        
        
        
        
        scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
        
        if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
          next
        }else{
        scopus_papers <- scopus_data_raw$df
  
        
        folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
        
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        
        papers <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        affils <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        authors <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_authors", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        }
    }
    }
  

    
    
    

# search by term (for the most productive) ----------------------------
# lost one done:
# list(query = "(AF-ID('60023691') AND DOCTYPE(ed) AND PUBYEAR IS 2024)", 
#      count = 25, start = 0, view = "COMPLETE")
yr1=2023
yr2=2024
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)

search_term<-search_by_month

pubcat<-c("re","ch","ed","le","dp","no")
search_term<-search_by_month
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
        
        
        papers <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        
        
        
        affils <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        
        
        authors <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
        
        
        
      }
    }
  }
}


# search for article cat by OA/NOT ----------------------------------------


pubcat<-c("ar")
oa<-c("AND OA(publisherfullgold))", "AND NOT OA(publisherfullgold))")


# oa<-c("AND OA(all))", "AND NOT OA(all))")
oa_for_filename<-c("oa","notoa")

oa<-c("NURS","VETE","DENT","HEAL","MULT","AGRI","BIOC","IMMU","NEUR","PHAR",
      "CENG","CHEM","COMP","EART","ENER","ENGI","ENVI","MATE","MATH","PHYS","ARTS","BUSI",
      "DECI","ECON","PSYC","SOCI","MEDI")

oa_for_filename<-c("NURS","VETE","DENT","HEAL","MULT","AGRI","BIOC","IMMU","NEUR","PHAR",
                   "CENG","CHEM","COMP","EART","ENER","ENGI","ENVI","MATE","MATH","PHYS","ARTS","BUSI",
                   "DECI","ECON","PSYC","SOCI","MEDI")
search_term<-search_by_month
term <- seq_along(search_term)
# MEDI >5000
# 60002746 need all
# 60029929 need non oa gold medi
# 60006297 need non oa gold medi
# 60023691 need non oa gold medi

search_by_month<-60023691
search_term<-search_by_month
term <- seq_along(search_term)

yr1=2025
yr2=2025
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)

cat <- seq_along(pubcat)
oa_cat<-seq_along(oa)


for  (m in oa_cat) {
for (h in term) {
  for (j in year) {
    for  (k in cat) {
        
        
        a <- paste("(AF-ID('", search_term[h], "')", " AND DOCTYPE(",pubcat[k],")", sep = "")
        
        b <- paste(" AND PUBYEAR IS ", date_range[j], " ", sep = "")
        
        c<- paste( " AND SUBJAREA(",oa[m],"))", sep = "")
        # c <- oa[m]
        
        
        query_string <- paste0(a, b, c)
        # query_string <- paste0(a, b)
        
        scopus_data <- rscopus::scopus_search(query_string,
                                              max_count=6000,
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
          
          
          papers <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
          write_csv(scopus_papers, papers)
          
          
          
          
          affils <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
          write_csv(scopus_affiliations, affils)
          
          
          
          authors <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
          write_csv(scopus_authors, authors)
          
          
          
          
        }
      }
    }
  }
}







#  LOOP BY ORIGINAL LOAD DATE ---------------------------------------------


# 60002746 need all
# 60029929 need non oa gold medi
# 60006297 need non oa gold medi
# 60023691 need non oa gold medi
# search_term<-c(60002746)
 # search_term<-c(60029929)
 # search_term<-c(60006297)
 search_term<-c(60023691)
term<-seq_along(search_term)
months <- sprintf("%02d", 1:12)
month <- seq_along(months)

yr1=2019
yr2=2025
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)

cat<-"ar_MEDI"
pubcat<-seq_along(cat)
for (z in pubcat){
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
      
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar) AND SUBJAREA(MEDI))", sep = "")
      
      b <- paste(" AND (PUBYEAR IS ", yr2, ")", sep = "")
      
      c<- paste(" AND ORIG-LOAD-DATE > ",start_date, " AND ORIG-LOAD-DATE < ", end_date,")",sep="")
      
      
      query_string <- paste0(a, b, c)
      # query_string <- paste0(a, c)
      
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
        
        
        scopus_papers <- scopus_data_raw$df
        scopus_affiliations <- scopus_data_raw$affiliation
        scopus_authors <- scopus_data_raw$author
        
        
        folder_for_files<-paste(date_folder_for_files,"/",yr2,sep="")
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", date_range[j],"_",months[k], "_", cat[z], sep = "")
        
        
        papers <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/papers/",yr2,"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        
        
        affils <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/affils/",yr2,"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        
        authors <- paste("./data_raw/scopus_downloads/",date_folder_for_files,"/authors/",yr2,"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
        
      }
    }
  }
}
}




