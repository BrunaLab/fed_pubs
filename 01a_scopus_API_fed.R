# install.packages("rscopus")
# install.packages("tidyverse")
library(rscopus)
library(tidyverse)

# source("./code/generate_fed_affils_to_search.R")
# affils_to_search<-generate_fed_affils_to_search()

affils_to_search<-read_csv("./data_clean/api_fed_affils_searched_2025-09-01.csv")


# affils_to_search<-affils_to_search %>% select(affil_id)

# this removes the two that are searched by month below
affils_to_search<-affils_to_search %>% 
  select(affil_id) %>% 
  filter(affil_id!=60006577) %>% 
  filter(affil_id!=60014232) 
search_term<-affils_to_search$affil_id[1:nrow(affils_to_search)]
# search_term<-affils_to_search$affil_id[3444:nrow(affils_to_search)]

# THIS IS TO DO THE "ADDITIONAL ONES DIUSCOVERED AFTER FIRST REVIEW
# affils_to_search_rd2<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") %>% 
#   anti_join(affils_to_search,by="affil_id")
# search_term<-affils_to_search_rd2$affil_id[248:nrow(affils_to_search_rd2)]

yr1=2019
yr2=2025
date_range <- seq(yr1,yr2)


year <- seq_along(date_range)
term <- seq_along(search_term)
# term <- seq(3444,nrow(affils_to_search),by=1)
# search_term<-affils_to_search$affil_id[5997:nrow(affils_to_search)]




# date_folder_for_files<-"scopus_downloads/fed_20250901"
date_folder_for_files<-"scopus_downloads/fed_20251010"
api<-"---"



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


    
for (h in term){
      for (j in year) {
        
            
        
        
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
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        }
    }
    }
  

    
    
    

# search by month ---------------------------------------------------------

search_term<-c(60006577,60014232) 
months <- month.name
  # Need to split:
  # 60006577 (2019-2024), 60014232 (2019-2022)
# yr1=2025
# yr2=2025

# date_range <- seq(yr1,yr2)
# str(search_term)

term <- seq_along(search_term)

year <- seq_along(date_range)

month <- seq_along(months)

for (h in term) {
  for (j in year) {
    for  (k in month) {
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
      
      b <- paste(" AND (PUBYEAR = ", date_range[j], ")", sep = "")
      
      c <- paste(" AND (PUBDATETXT(", months[k], ")))", sep = "")
      
      
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
        scopus_papers <- scopus_data_raw$df
        
        
        
        
        folder_for_files<-paste(date_folder_for_files,"/",date_range[j],sep="")
        
        term_for_file<-paste("scopus_affil_",search_term[h], "_", date_range[j], "_", months[k], sep = "")
        
        papers <- paste("./data_raw/",date_folder_for_files,"/papers/",date_range[j],"/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j], "_", months[k], sep = "")
        affils <- paste("./data_raw/",date_folder_for_files,"/affils/",date_range[j],"/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j], "_", months[k], sep = "")
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        
        
        
        
      }
    }
  }
}
