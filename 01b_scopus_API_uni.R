
# Universities ------------------------------------------------------------

library(rscopus)
library(tidyverse)
library(data.table)

# date_folder_for_files<-"uni_20250901"
date_folder_for_files<-"uni_20251010"
api<-"---"


yr1=2019
yr2=2022

# create folders  ---------------------------------------------------------


# setting up the main directory
main_dir <- paste("data_raw/scopus_downloads",date_folder_for_files,sep="")
sub_dir <- paste(main_dir,"/papers/",yr1,sep="")
if (!dir.exists(sub_dir)){
  dir.create(main_dir, recursive = TRUE)
} 



# start scopus search here ------------------------------------------------


affils_df<-read_csv("./data_clean/api_uni_affils_searched_2025-09-01.csv")


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
# search_term<-affils_to_search$affil_id[444:nrow(affils_to_search)]

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
        
        
        
        a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
              c <- " AND PUBYEAR = "
        
        query_string <-paste0(a, c, date_range[j],")",sep = "")
      
        
        scopus_data <- rscopus::scopus_search(query_string,
                                              max_count=8000,
                                              view = "COMPLETE",
                                              api_key = api)
        
        
        
        
        

        
        
        
        
        
        scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
        
        if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
          next
        }else{
        scopus_papers <- scopus_data_raw$df
  
        
        folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
        
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_authors", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        }
    }
    }
  

    
    
    

# search by month and year --------------------------------------------------
# START HERE WHEN CURRENT RUN ENDS



yr1=2022
yr2=2022
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)

search_term<-search_by_month
term <- seq_along(search_term)

month_name<- month.name
# month_name<-month_name[11:12]
month<-seq_along(month_name)

  for (j in year) {
    for (h in term) {
    for  (k in month) {
      
      
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
      b <- paste(" AND (PUBYEAR = ", date_range[j], ")", sep = "")
      c <- paste(" AND (PUBDATETXT(", month_name[k], ")))", sep = "")
      
      
      query_string <- paste0(a, b, c)
      
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=8000,
                                            # start = 0,
                                            view = "COMPLETE",
                                            api_key = api)
      
      
      
      
      
      scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
      
      if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
        next
      }else{
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", month_name[k],"_",date_range[j], sep = "")
        
        scopus_papers <- scopus_data_raw$df
        papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_authors", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
        
        
      }
    }
  }
}