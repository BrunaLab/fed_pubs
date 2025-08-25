add_missing_usgs<-function(papers_df,authors_df){
  
  # 
  papers_df<-papers_df_trim
  authors_df<-authors_df_trim
  # See if any of the usgs pubs are aleready in the file and check author status 
  
  # by DOI
  
  usgs_papers<-read_rds("./data_intermediate/usgs_papers_clean.rds")
  usgs_with_DI<-usgs_papers %>% filter(!is.na(DI))
  usgs_already_in_papers_df<-papers_df %>% 
    filter(DI%in%usgs_with_DI$DI) %>% 
    select(refID,DI)
  
  # that means these usgs papers are NOT in papers_df and need to be added
  usgs_NOT_in_papers_df<-anti_join(usgs_papers,usgs_already_in_papers_df,by="DI") %>% 
    mutate(source="usgs_database")
  
  # and these are the authors of those papers
  usgs_authors<-read_rds("./data_intermediate/usgs_authors_clean.rds") 
  
  usgs_authors_for_papers_NOT<- usgs_authors %>% 
    filter(usgs_refID %in%usgs_NOT_in_papers_df$usgs_refID)
  
  names(usgs_NOT_in_papers_df)
  names(papers_df)
  
  
  
  usgs_NOT_in_papers_df<-
    usgs_NOT_in_papers_df %>% select(
      refID=usgs_refID,
      source_file=source,
      DI,
      scopus_article_id=usgs_refID,
      AF,
      ,BP,EP,IS,PT,PY,SN,EI,SO,TI,VL) %>% 
    mutate(entry_no=row_number())
  
  
  
  
  usgs_authors_for_papers_NOT
  
  
  
  usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
    mutate(AF=str_replace_all(AF,"[.]","")) %>% 
    separate(given_name,c("first","middle"),extra = "merge") 
  
  usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
    mutate(first_middle_initials=str_replace_all(first_middle_initials,".NA.","")) %>% 
    mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]","")) 
  
  usgs_authors_for_papers_NOT<-
    usgs_authors_for_papers_NOT %>% 
    select(
      refID=usgs_refID,
      source_file=source,
      SID=usgs_refID,
      PY,
      # agency_primary=agency,
      # agency_short=agency_2,
      # affiliation=agency_3,
      country,
      state,
      city,
      surname,
      given_name=first,
      first_middle_initials,
      AF,
      federal,
      author_order,
      affil_id,
      entry_no,
      authorID,
      federal,
    ) %>% 
    mutate(entry_no=as.character(entry_no)) %>% 
    mutate(author_order=as.integer(author_order)) %>% 
    mutate(SID=as.character(SID)) %>% 
    mutate(federal=as.logical(federal))
  
  
  usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
    mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>%
    mutate(federal=as.logical(federal))
  
  authors_df$SID<-as.character(authors_df$SID)
  
  # add these: usgs_NOT_in_papers_df
  # to these: papers_df
  
  names(usgs_NOT_in_papers_df)
  names(papers_df)
  
  usgs_NOT_in_papers_df$scopus_article_id<-as.integer(usgs_NOT_in_papers_df$scopus_article_id)
  usgs_NOT_in_papers_df$BP<-as.double(usgs_NOT_in_papers_df$BP)
  usgs_NOT_in_papers_df$EP<-as.double(usgs_NOT_in_papers_df$EP)
  usgs_NOT_in_papers_df$PY<-as.integer(usgs_NOT_in_papers_df$PY)
  papers_df$BP<-as.numeric(papers_df$BP)
  usgs_NOT_in_papers_df$BP<-as.numeric(usgs_NOT_in_papers_df$BP)
  papers_df<-bind_rows(papers_df,usgs_NOT_in_papers_df)
  
  papers_df<-papers_df %>% distinct()
  
  
  # add these: usgs_NOT_in_papers_df
  # to these: papers_df
  
  names(usgs_authors_for_papers_NOT)
  names(authors_df)
  usgs_authors_for_papers_NOT$SID<-as.character(usgs_authors_for_papers_NOT$SID)
  authors_df<-bind_rows(authors_df,usgs_authors_for_papers_NOT)
  
  authors_df<-authors_df %>% distinct()

  return(list(papers=papers_df,authors=authors_df))
}
  
  