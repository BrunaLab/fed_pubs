merge_with_usgs <- function(authors_df,papers_df) {
  
  usgs_authors<-read_rds("./data_intermediate/usgs_authors_clean.Rds")
  usgs_papers<-read_rds("./data_intermediate/usgs_papers_clean.rds")
  
  # get a df of papers already in papers_df from usgs
  # get the federal usgs authors of those papers (doi, author order) and 
  # change the affiliation in papers_df
  
  
  
  
  # See if any of the usgs pubs are aleready in the file and check author status 
  
  # by DOI
  
  # usgs<-read_csv("./data_raw/scopus_api/fed_files/usgs_with_doi.csv") 
  usgs_with_DI<-usgs_papers %>% filter(!is.na(DI))
  usgs_already_in_papers_df<-papers_df %>% filter(DI%in%usgs_with_DI$DI) %>% select(refID,DI)
  
  
  authors_df_usgs_check<-authors_df %>%
    filter(refID%in%usgs_already_in_papers_df$refID) %>% 
    select(AF,surname,given_name,federal,SID,affil_id) %>% 
    mutate(federal=as.character(federal)) 
  
  affils_for_usgs_check<-affils_df %>% filter(affil_id%in%authors_df_usgs_check$affil_id) %>% 
    select(affil_id,agency,federal) %>% 
    mutate(federal=as.character(federal)) %>% 
    distinct()
  
  authors_df_usgs_check<-left_join(authors_df_usgs_check,affils_for_usgs_check)
  
  library(stringr)
  
  
  
  authors_df_usgs_check<-authors_df_usgs_check %>% 
    mutate(AF=str_replace_all(AF,"[.]","")) %>% 
    separate(given_name,c("first","middle"),extra = "merge")
  
  
  usgs_authors_clean<-usgs_authors %>% 
    select(surname,AF, given_name, federal,email) %>% 
    arrange(desc(federal)) %>% 
    separate(given_name,c("first","middle"),extra = "merge") 
  
  fed_check_usgs<-full_join(authors_df_usgs_check, usgs_authors_clean,by=c("surname","first")) 
  
  
  fed_check_usgs_match<-fed_check_usgs %>% 
    filter((federal.x=="TRUE" & federal.y=="TRUE")|(federal.x=="FALSE" & federal.y=="FALSE")) %>% 
    distinct()
  
  fed_check_usgs_remain<-anti_join(fed_check_usgs,fed_check_usgs_match) %>% distinct(SID,federal.x,federal.y,.keep_all=TRUE)
  fed_check_usgs_remain<-fed_check_usgs_remain %>% 
    mutate(federal.x=if_else(federal.y=="TRUE","TRUE",federal.x)) %>% 
    arrange(desc(federal.y),federal.x) %>% 
    select(SID,federal.x) %>% 
    filter(federal.x==TRUE)
  
  
  authors_df<-authors_df %>%
    left_join(fed_check_usgs_remain,by="SID") %>% 
    mutate(federal=if_else(federal.x=="TRUE",TRUE,federal)) %>% 
    select(-federal.x)
  
  
  authors_df %>% distinct(surname,given_name,.keep_all=TRUE) %>% group_by(surname,given_name) %>% tally(n_distinct(federal))
  
  
  
  
  # <-read_csv("./data_raw/scopus_api/fed_files/usgs_no_doi.csv")
  usgs_papers_no_doi<-usgs_papers %>% filter(is.na(DI))
  match_title<-papers_df %>% filter(TI%in%usgs_papers_no_doi$TI) 
  
  usgs_already_in_papers_df<-papers_df %>% filter(DI%in%usgs_with_DI$DI) %>% select(refID,DI)
  
  # usgs_pubs<-read_rds("./data_intermediate/usgs_papers_clean.rds")
  # 
  
  usgs_pubs_to_add1<-anti_join(usgs_papers,usgs_already_in_papers_df,by="DI")
  usgs_pubs_to_add2<-semi_join(usgs_papers,match_title,by="TI") 
  
  usgs_papers_NOT_in_papers_df<-bind_rows(usgs_pubs_to_add1,usgs_pubs_to_add2) %>% 
    select(-c(Country,
              agency_3,
              State,
              City,
              URL,
              agency,
              agency_2)) 
  
  # 
  # 
  # usgs_authors<-read_rds("./data_intermediate/usgs_authors_clean.rds") 
  # 
  
  add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%usgs_papers_NOT_in_papers_df$usgs_refID) %>% 
    mutate(from="usgs") %>% 
    rename(refID=usgs_refID) %>% 
    select(-c(agency_2,agency_3,country,city,from)) %>% 
    mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
    mutate(federal=as.logical(federal))
  # ,
  #          PY=as.numeric(PY)) 
  # 
  # add to authors_df and papers_df -----------------------------------------
  authors_df<-authors_df %>% mutate(federal=as.logical(federal))
  add_to_authors_df$author_order<-as.integer(add_to_authors_df$author_order)
  authors_df$author_order<-as.integer(authors_df$author_order)
  
  authors_df$authorID<-as.character(authors_df$authorID)
  add_to_authors_df$authorID<-as.character(add_to_authors_df$authorID)
  
  add_to_authors_df$PY<-as.numeric(add_to_authors_df$PY)
  
  add_to_authors_df$entry_no<-as.integer(add_to_authors_df$entry_no)
  
  # add_to_authors_df: are any of these federal in the main df?
  
  
  
  
  add_to_authors_df_trim<-  add_to_authors_df %>% 
    select(AF,surname,given_name,federal,affil_id) %>% 
    mutate(federal=as.character(federal)) %>% 
    distinct() %>% 
    mutate(affil_id=as.integer(affil_id))
  
  
  
  
  authors_df_fed_check<-authors_df %>% 
    select(AF,surname,given_name,federal,SID,affil_id) %>% 
    mutate(federal=as.character(federal)) %>% 
    distinct() 
  
  # add_to_authors_df<-left_join(add_to_authors_df_trim,authors_df_fed_check,by="affil_id")
  
  names<-names(authors_df)
  names2<-names(add_to_authors_df)
  names<-intersect(names,names2)
  
  
  add_to_authors_df<-add_to_authors_df %>% mutate(affil_id=as.numeric(affil_id)) %>% select(all_of(names)) 
  
  add_to_papers_df<-usgs_papers_NOT_in_papers_df %>% rename(refID=usgs_refID)
  
  add_to_papers_df$PY<-as.numeric(add_to_papers_df$PY)
  
  papers_df$BP<-as.character(papers_df$BP)
  papers_df$EP<-as.character(papers_df$EP)
  
  papers_df<-bind_rows(papers_df,add_to_papers_df)
  
  return(list(papers=papers_df,authors=authors_df))
  
  
  
  
  
  
  
  
  
  
  
  
  
  usgs_DI<-usgs_pubs_to_add %>% 
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
    select(DI,AF,affil_id,author_order,PY,federal) %>% 
    distinct() %>% 
    mutate(PY=as.integer(PY))
  
  refID_to_correct_authors<-semi_join(papers_df,for_usgs_authors2,by="DI") %>% 
    select(refID,DI,PY)
  
  for_usgs_authors2<-left_join(for_usgs_authors2,refID_to_correct_authors)
  
  # %>% 
  #   mutate(PY=as.numeric(PY))
  
  
  for_usgs_authors2$author_order<-as.numeric(for_usgs_authors2$author_order)
  
  for_usgs_authors2$PY<-as.numeric(for_usgs_authors2$PY)
  
  
  authors_df<-left_join(authors_df,for_usgs_authors2,by=c("refID","author_order")) %>% 
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
           EP=as.numeric(EP))
  # ,
  # PY=as.numeric(PY))
  
  add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%add_to_papers_df$usgs_refID) %>% 
    mutate(from="usgs") %>% 
    rename(refID=usgs_refID) %>% 
    select(-c(agency_2,agency_3,country,city,from)) %>% 
    mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
    mutate(federal=as.logical(federal))
  # ,
  #          PY=as.numeric(PY)) 
  # 
  # add to authors_df and papers_df -----------------------------------------
  authors_df<-authors_df %>% mutate(federal=as.logical(federal))
  add_to_authors_df$author_order<-as.integer(add_to_authors_df$author_order)
  authors_df$author_order<-as.integer(authors_df$author_order)
  
  authors_df$authorID<-as.character(authors_df$authorID)
  add_to_authors_df$authorID<-as.character(add_to_authors_df$authorID)
  
  add_to_authors_df$PY<-as.numeric(add_to_authors_df$PY)
  
  add_to_authors_df$entry_no<-as.integer(add_to_authors_df$entry_no)
  
  authors_df<-bind_rows(authors_df,add_to_authors_df)
  
  add_to_papers_df<-add_to_papers_df %>% rename(refID=usgs_refID)
  
  add_to_papers_df$PY<-as.numeric(add_to_papers_df$PY)
  papers_df<-bind_rows(papers_df,add_to_papers_df)
  
  return(list(papers=papers_df,authors=authors_df))
  
  
  
}
