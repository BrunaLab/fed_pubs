# install.packages("rscopus")
# install.packages("tidyverse")
library(rscopus)
library(tidyverse)


  search_term<-c(
                 '127295808',
                 '106720691'

                 )
    
    
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
                                              api_key = "")
        
        
        
        
        scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
        
        if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
          next
        }else{
        scopus_papers <- scopus_data_raw$df
  
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        papers <- paste("./data_raw/papers/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        affils <- paste("./data_raw/affils/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        authors <- paste("./data_raw/authors/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        }
    }
    }
  
