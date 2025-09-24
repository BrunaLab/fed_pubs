# install.packages("rscopus")
# install.packages("tidyverse")
library(rscopus)
library(tidyverse)

# # source("./code/generate_fed_affils_to_search.R")
# # affils_to_search<-generate_fed_affils_to_search()
# affils_to_search<-read_csv("./data_clean/api_affils_searched_2025-09-01.csv")
affils_to_search<-affils_to_search %>% select(affil_id)
affils_to_search<-affils_to_search %>% select(affil_id) %>% filter(affil_id!=60006577)
search_term<-affils_to_search$affil_id[7176:nrow(affils_to_search)]
# search_term<-affils_to_search$affil_id[837:nrow(affils_to_search)]
search_term<-
# Need to split:
# 60006577 (2019-2024), 60014232 (2019-2022)

yr1=2019
yr2=2022

date_folder_for_files<-"feds_20250901"



# create folders  ---------------------------------------------------------


# setting up the main directory
main_dir <- paste("./data_raw/",date_folder_for_files,sep="")

# setting up the sub directory
sub_dir <- paste("papers/",yr1,sep="")
# check if sub directory exists 
if (file.exists(sub_dir)){
  
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, sub_dir))
  
}


sub_dir <- paste("affils/",yr1,sep="")
# check if sub directory exists 
if (file.exists(sub_dir)){
  
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, sub_dir))
  
}

sub_dir <- paste("authors/",yr1,sep="")
# check if sub directory exists 
if (file.exists(sub_dir)){
  
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, sub_dir))
  
}

# check if sub directory exists 
if (file.exists(sub_dir)){
  
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, sub_dir))
  
}


#  initialize searches ----------------------------------------------------


date_range <- seq(yr1,yr2)

year <- seq_along(date_range)
term <- seq_along(search_term)



    for (h in term){
      
      for (j in year) {
        
        
        
        
        a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
        

        c <- " AND PUBYEAR = "
        
        query_string <-paste0(a, c, date_range[j],")",sep = "")
      
        
        scopus_data <- rscopus::scopus_search(query_string,
                                              max_count=8000,
                                              view = "COMPLETE",
                                              api_key = "---")
        
        
        
        
        
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

search_term<-c(60006577)
  # Need to split:
  # 60006577 (2019-2024), 60014232 (2019-2022)
yr1=2019
yr2=2024
months <- month.name


date_range <- seq(yr1,yr2)
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
                                            api_key = "-----")
      
      
      

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
