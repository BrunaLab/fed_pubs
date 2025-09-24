

# Universities ------------------------------------------------------------

library(rscopus)
library(tidyverse)
library(data.table)

date_folder_for_files<-"uni_20250901"

# 
# dataDir <- "./data_raw/affiliations_to_search/uni_affils"
# 
# dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)
# dataFls<-dataFls[ !dataFls == "./data_raw/affiliations_to_search/uni_affils/scopus_info_uni_affils.csv"]
# 
# # Read and tag each file
# dt_list <- lapply(dataFls, function(file) {
#   dt <- fread(file, fill = TRUE)
#   dt[, source_file := basename(file)]  # Add column with filename
#   return(dt)
# })
# 
# 
# # Combine all tagged data tables
# affils_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
# affils_df<-affils_df %>% 
# pivot_longer(
#   cols = starts_with("V"),
#   names_to = "col",
#   values_to = "affil_id",
#   values_drop_na = TRUE) %>% 
#   separate_wider_delim(source_file,delim = ".", names = c("uni", "csv")) %>%
#   distinct() %>% 
#   select(-col,-csv) 
# 
# affil_info<-read_csv("./data_raw/affiliations_to_search/uni_affils/scopus_info_uni_affils.csv")
#   
# affils_df<-left_join(affils_df,affil_info,by="affil_id") %>% 
#   select(-document_count,-pub_count) %>% 
#   filter(!is.na(affiliation)) %>% 
#   distinct(affil_id,.keep_all = TRUE)

# affils_df<- read_csv("./data_raw/affiliations_to_search/uni_affils/follow_up/uni_affils_follow_up.csv")



# start scopus search here ------------------------------------------------



affils_df<-read_csv("./data_raw/affiliations_to_search/uni_affils/follow_up/all_uni_affils_searched.csv")

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
# affils_to_search<-affils_to_search %>% select(affil_id) %>% filter(affil_id!=60006577)
search_term<-affils_to_search$affil_id[1:nrow(affils_to_search)]


# 
# affil_ids<- read_csv("./data_raw/affiliations_to_search/uni_affils/follow_up/uni_affils_follow_up.csv") %>%
# select(affil_id)
# 
# search_term<-affil_ids$affil_id[22:nrow(affil_ids)]
#  search by year ----------------------------------------------------------

yr1=2019
yr2=2025

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
        authors <- paste("./data_raw/",date_folder_for_files,"/authors/",date_range[j],"/",term_for_file,"_authors", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        }
    }
    }
  

    
    
    

# search by month and year --------------------------------------------------
# START HERE WHEN CURRENT RUN ENDS
search_by_month<-
  c(
    60023691,
    60031970,
    60025111,
    60009982,
    60033182,
    60003711,
    60028548,
    60015481)

yr1=2019
yr2=2024

date_range <- seq(yr1,yr2)
year <- seq_along(date_range)
search_term<-search_by_month

term <- seq_along(search_term)

month_name<- month.name[1:5]
# month_name<-month_name[6:12]
month<-seq_along(month_name)

for (h in term) {
  for (j in year) {
    for  (k in month) {
      
      
      
      a <- paste("(AF-ID('", search_term[h], "')", " AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))", sep = "")
      b <- paste(" AND (PUBYEAR = ", date_range[j], ")", sep = "")
      c <- paste(" AND (PUBDATETXT(", month_name[k], ")))", sep = "")
      
      
      query_string <- paste0(a, b, c)
      
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=8000,
                                            # start = 0,
                                            view = "COMPLETE",
                                            api_key = "---")
      
      
      
      
      
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