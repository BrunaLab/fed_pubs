merge_with_usgs <- function(authors_df,papers_df) {
  
  
  # KEEP ONLY ARTICLES on USGS LIST
  
  usgs<-read_csv("./data_raw/usgs_publications.csv") %>% 
    mutate_all(tolower) %>% 
    select("usgs_refID"="Publication ID",
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
           "SO"="CHORUS Journal Name",
           "TI"="Title",
           "VL"="Volume",
           "URL"="CHORUS URL") %>% 
    mutate("agency"="interior",
           source="usgs",
           "agency_2"="usgs") %>% 
    filter(PT=="article") %>% 
    mutate(PT="j")
  
  
  
  # authors from usgs
  
  usgs_authors<-usgs %>% 
    select(usgs_refID,PY,source,agency,agency_2,agency_3,Country,State,City,AF) %>% 
    separate_rows(AF, sep = "; ") %>% 
    mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
    mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
    # mutate(AF = str_remove(AF, fixed(email))) %>% 
    mutate(federal=if_else(!is.na(email),TRUE,FALSE)) %>% 
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
              email,
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
    mutate_all(trimws)
  usgs_authors$AF<-trimws(usgs_authors$AF)
  usgs_authors$AU<-trimws(usgs_authors$AU)
  
  
  
  
  write_rds(usgs_authors, "./data_intermediate/usgs_authors_clean.rds")
  write_rds(usgs, "./data_intermediate/usgs_papers_clean.rds")
  
  
  
  
  # get a df of papers already in papers_df from usgs
  # get the federal usgs authors of those papers (doi, author order) and 
  # change the affiliation in papers_df
  
  
  usgs_DI<-usgs %>% 
    select(DI,usgs_refID) %>% 
    drop_na() 
  
  papers_DI<-papers_df %>% 
    select(DI,refID) %>% 
    drop_na()
  
  
  usgs_papers_in_papers_df<-semi_join(papers_DI,usgs_DI,by="DI") %>% 
    select(DI)
  
  
  
  for_usgs_authors1<-semi_join(usgs,usgs_papers_in_papers_df) %>% select(usgs_refID,DI,PY)
  for_usgs_authors2<-semi_join(usgs_authors,for_usgs_authors1,by="usgs_refID")
  for_usgs_authors2<-left_join(for_usgs_authors2,for_usgs_authors1) %>% 
    filter(federal==TRUE) %>% 
    select(DI,AF,affil_id,author_order,PY,federal)
  
  refID_to_correct_authors<-semi_join(papers_df,for_usgs_authors2,by="DI") %>% 
    select(refID,DI)
  
  for_usgs_authors2<-left_join(for_usgs_authors2,refID_to_correct_authors) %>% 
    mutate(PY=as.numeric(PY))
  
  authors_df<-left_join(authors_df,for_usgs_authors2,by=c("refID","PY","author_order")) %>% 
    mutate(federal.x=as.character(federal.x)) %>% 
    mutate(federal.x=
             case_when(
               federal.y == "TRUE" ~ "TRUE",
               .default = as.character(federal.x)
             )
    ) %>% 
    mutate(affil_id.x=
             case_when(
               affil_id.y == "60011347" ~ "60011347",
               .default = as.character(affil_id.x)
             )
    ) %>% 
    select(-affil_id.y,
           -federal.y) %>% 
    rename(affil_id=affil_id.x,
           federal=federal.x) %>% 
    mutate(AF.y=if_else(AF.y==AF.x,NA,AF.y)) %>% 
    select(-AF.y) %>% 
    rename(AF=AF.x)
  
  
  
  # now the ones that aren't in papers_df
  
  
  usgs_papers_NOT_in_papers_df<-anti_join(usgs_DI,papers_DI,by="DI") %>% 
    select(DI)
  
  
  
  
  add_to_papers_df<-semi_join(usgs,usgs_papers_NOT_in_papers_df) %>% 
    select(-c(Country,
              agency_3,
              State,
              City,
              URL,
              agency,
              agency_2)) %>% 
    mutate(BP=as.numeric(BP),
           EP=as.numeric(EP),
           PY=as.numeric(PY))
  
  add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%add_to_papers_df$usgs_refID) %>% 
    mutate(from="usgs") %>% 
    rename(refID=usgs_refID) %>% 
    select(-c(agency_2,agency_3,country,city,from)) %>% 
    mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
    mutate(federal=as.logical(federal),
           PY=as.numeric(PY)) 
  
  # add to authors_df and papers_df -----------------------------------------
  authors_df<-authors_df %>% mutate(federal=as.logical(federal))
  authors_df<-bind_rows(authors_df,add_to_authors_df)
  
  add_to_papers_df<-add_to_papers_df %>% rename(refID=usgs_refID)
  papers_df<-bind_rows(papers_df,add_to_papers_df)
  
  return(list(papers=papers_df,authors=authors_df))
  
  
}
