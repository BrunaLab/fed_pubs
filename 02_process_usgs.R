

library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)

api<-"-----"
# clean_usgs <- function(authors_df,papers_df) {
  
  # USGS Publications_Warehouse file:
  # https://pubs.usgs.gov/pubs-services/publication/?mimeType=csv&
  
  # KEEP ONLY ARTICLES on USGS LIST
  
# usgs_file <-"./data_raw/usgs_20250907.csv"
usgs_file <-"./data_raw/usgs_20251027.csv"
fed<-"fed_20251010"
# usgs %>% group_by(PT) %>% tally()

  usgs<-read_csv(usgs_file) %>% 
    mutate_all(tolower) %>% 
    select("usgs_refID"="Publication ID",
           DI=DOI,
           "AF"="Author(s)",
           "Country",
           "agency_3"="Contributing office(s)",
           "State",
           "City",
           "BP"="First page",
           "DI"="DOI",
           "EP"="Last page", 
           "IS"="Issue",
           "PT"="Publication type",
           "PY"="Year Published",
           "SN"="ISSN (print)",
           "EI"="ISSN (online)",
           # "SO"="CHORUS Journal Name",
           "SO"="Series title",
           "TI"="Title",
           "VL"="Volume",
           "URL"="CHORUS URL") %>% 
    mutate("agency"="interior",
           source="usgs",
           "agency_2"="usgs") %>% 
    filter(PY>2018) %>% 
    filter(PT=="article"|PT=="book chapter") %>% 
    mutate(PT=case_when(
      PT=="article"~"j",
      PT=="book chapter"~"j"
      )
    )
  

  # AUTHORS AT PLACES LIKE COOP UNITS AREN'T LISTED AS USGS IN SCOPUS,
  # NEED TO FIND THEM VIA EMAIL AND CHNAGE THEIR AFFILIATIONS 
  # authors from usgs

  usgs_authors<-usgs %>%
    select(usgs_refID,PY,source,agency,agency_2,agency_3,Country,State,City,AF) %>%
    separate_rows(AF, sep = "; ") %>%
    mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>%
    mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>%
    # mutate(AF = str_remove(AF, fixed(email))) %>%
    group_by(usgs_refID) %>%
    mutate(author_order=row_number(),
           affil_id=60011347,
           entry_no=cur_group_id()) %>%
    mutate(AF=gsub(", jr."," jr.",AF)) %>%
    mutate(Country=gsub("united states","usa",Country)) %>%
    separate_wider_delim(AF,
                         delim=", ",
                         cols_remove = FALSE,
                         names=c("surname","given_name"),
                         too_many = "debug",
                         too_few = "debug") %>%
    select(-AF_remainder,-AF_pieces) %>%
    separate_wider_delim(given_name,
                         delim=" ",
                         cols_remove = FALSE,
                         names=c("init1","init2"),
                         too_many = "debug",
                         too_few = "debug") %>%
    mutate(given_name_remainder=gsub("jr.","",given_name_remainder)) %>%
    mutate(given_name_remainder=gsub("[.]","",given_name_remainder)) %>%
    mutate(given_name_remainder=gsub("iii","",given_name_remainder)) %>%
    mutate(init1 = str_sub(init1, 1, 1)) %>%
    mutate(init2 = str_sub(init2, 1, 1)) %>%
    mutate(first_middle_initials=paste(init1,".",init2,".",sep="")) %>%
    mutate(first_middle_initials=gsub("NA.NA.","",first_middle_initials)) %>%
    select(-c(given_name_ok,
              given_name_pieces,
              given_name_remainder,
              init1,
              init2,
              AF_ok)
    ) %>%
    rename(country=Country,
           city=City,
           state=State,
    ) %>%
    mutate(AU=paste(surname,first_middle_initials, sep=",")) %>%
    mutate(AU=gsub("[.]","",AU)) %>%
    mutate(AU=ifelse(AU=="NA,",NA,AU)) %>%
    group_by(AF) %>%
    mutate(authorID=cur_group_id()) %>%
    mutate(authorID=paste(authorID,"usgs",sep="")) %>%
    mutate(AU=gsub("NA","",AU),
           AU=gsub(",",", ",AU),
           AU=gsub("[(]","",AU)) %>%
    mutate_all(trimws) %>% 
    mutate(federal=if_else(!is.na(email),TRUE,FALSE)) 
  
  
  usgs_authors$AF<-trimws(usgs_authors$AF)
  usgs_authors$AU<-trimws(usgs_authors$AU)

  


  # Define replacements for affiliation cleanup
  # affil_replacements <- c(
  #   "&amp;amp;" = "and",
  #   "u s " = "us ",
  #   "united states " = "us ",
  #   "americorps vista" = "americorps",
  #   "u\\.s\\. " = "us ",
  #   "u\\. s\\. " = "us ",
  #   "\\." = ""
  # )

  # Define replacements for given_name_remainder cleanup
  given_name_replacements <- c(
    " jr\\." = "",
    "\\." = "",
    " iii" = "",
    " ii" = ""
  )

  usgs_authors <- usgs_authors %>%
    mutate(
      surname=str_replace_all(surname, given_name_replacements),
      AF=str_replace_all(AF, given_name_replacements)
    ) %>% 
    mutate_all(trimws)
    
  # Extract ORCID ID FROM AF
  usgs_authors <- usgs_authors %>%
    select(usgs_refID, PY, source, agency, agency_2, agency_3, country, state, city, AF,federal) %>%
    separate_rows(AF, sep = "; ") %>%
    mutate(
      # email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"),
      # AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"),
      # federal = !is.na(email),
      country = str_replace_all(country, "united states", "usa")
    ) %>% 
    mutate_all(trimws)


  
  
  
  
      
# PIVOT AUTHOR NAMES TO LONG
  usgs_authors <- usgs_authors  %>%
    group_by(usgs_refID) %>%
    mutate(
      author_order = row_number(),
      affil_id = 60011347,
      entry_no = cur_group_id()
    ) %>%
    separate_wider_delim(AF, delim = ", ", cols_remove = FALSE,
                         names = c("surname", "given_name"),
                         too_many = "debug", too_few = "debug") %>%
    separate_wider_delim(given_name, delim = " ", cols_remove = FALSE,
                         names = c("init1", "init2"),
                         too_many = "debug", too_few = "debug") 
  
  
  # Extract and clean up Orcid IDs 
  usgs_authors <- usgs_authors %>%
    mutate(OI = str_extract(AF, "[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}"),
           AF = str_remove(AF, "[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}"),
           init2 = str_remove(init2, "[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}"),
           init2 = str_remove(init2, "[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}"),
           given_name = str_remove(given_name, "[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}[-]+[0-9a-z]{4}")
           )
  
  
  
  usgs_authors <- usgs_authors %>%
    mutate(
      given_name_remainder = given_name,
      given_name_remainder = str_replace_all(given_name_remainder, given_name_replacements),
      init1 = str_sub(init1, 1, 1),
      init2 = str_sub(init2, 1, 1),
      first_middle_initials = paste0(init1, ".", init2, "."),
      first_middle_initials = na_if(first_middle_initials, "NA.NA."),
      AU = paste(surname, first_middle_initials, sep = ","),
      AU = str_replace_all(AU, "\\.", ""),
      AU = if_else(AU == "NA,", NA_character_, AU)
    ) %>%
    group_by(AF) %>%
    mutate(
      authorID = paste0(cur_group_id(), "usgs"),
      AU = str_replace_all(AU, c("NA" = "", "," = ", ", "\\(" = ""))
    ) %>%
    ungroup() %>%
    mutate(across(everything(), trimws)) %>%
    # rename(country = Country, city = City, state = State) %>%
    select(-c(given_name_ok, given_name_pieces, given_name_remainder,
              init1, init2, AF_ok)) %>% 
    mutate(first_middle_initials=gsub("NA.","",first_middle_initials))
    

  # Final trim for AF and AU (if needed)
  usgs_authors<-usgs_authors %>% 
    mutate_all(trimws)
  

  write_rds(usgs_authors, paste("./data_clean/usgs_authors_clean_",Sys.Date(),".rds",sep=""))
  write_rds(usgs, paste("./data_clean/usgs_papers_clean_",Sys.Date(),".rds",sep=""))
  
  
  
  # usgs<-read_rds("./data_clean/usgs_papers_clean_2025-10-31.rds")
  # 
  
  
  
  usgs_no_doi<-usgs %>% filter(is.na(DI))%>% select(AF,BP,EP,TI,PY,SO,VL) %>% distinct()
  usgs_with_doi<-usgs %>% filter(!is.na(DI)) %>% select(DI) %>% distinct()
  
  
  

  
  


# search scopus for those with no DOI -------------------------------------

# these will be saved and bound with the ones from year searches
  
  library(rscopus)
  library(tidyverse)
  
  
  # remove
  usgs_no_doi<-usgs_no_doi %>% filter(!str_detect(TI, "foreward|foreword"))
  usgs_no_doi<-usgs_no_doi %>% filter(!str_detect(TI, "special issue"))
  usgs_no_doi<-usgs_no_doi %>% filter(!str_detect(TI, "book review"))
  usgs_no_doi<-usgs_no_doi %>% distinct(TI,.keep_all = TRUE)
  
  search_term<-usgs_no_doi %>% select(TI,PY,SO) %>% 
    mutate(TI=gsub("[?]","",TI)) %>% 
    mutate(id=row_number())
  
  
  # https://www.scopus.com/pages/organization/60024266
  
  # search_term <- seq_along(search_term)[184:219]
  term <- seq_along(1:nrow(search_term))
  # term <- seq(101,nrow(search_term),by=1)
  # h<-"377"
  # search_term <- search_term[377,]
  
  for (h in term){
    
      
      a<-paste0('TITLE("',search_term$TI[h],'")',sep="")
      b <- paste0(" AND PUBYEAR = ",search_term$PY[h],sep="")
      c<- paste0(' AND SRCTITLE("',search_term$SO[h],'")',sep="")
      
      
      if (is.na(search_term$SO[h])){
        query_string <-paste0(a,b,sep = "")
      }else{
      query_string <-paste0(a,b,c,sep = "")
      }
      
      
      
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
        
        term_for_file<-paste("usgs_TI_",search_term$id[h],sep="")
        
        papers <- paste("./data_raw/",fed,"/papers/usgs/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        
        
        if(is.null(scopus_affiliations)){
        scopus_affiliations<-TRUE
        scopus_affiliations<-as.data.frame(scopus_affiliations)
        scopus_affiliations<-scopus_affiliations %>% rename(check_affils=scopus_affiliations)
        }
        affils <- paste("./data_raw/",fed,"/affils/usgs/",term_for_file,"_affils_", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        authors <- paste("./data_raw/",fed,"/authors/usgs/",term_for_file,"_authors", ".csv",sep = "")
        write_csv(scopus_authors, authors)
      }
    
  }
  
  
  
  
  
  
