ID_univ_authors <- function(authors_df, affils_df) {
  
  authors_df<-authors_df %>% 
    mutate(refID=as.character(refID)) %>% 
    mutate(entry_no=as.character(entry_no)) %>% 
    mutate(affil_id=as.character(affil_id)) 
  
  # affil_ids$affil_id<-as.character(affil_id$affil_id)
  
  univ_bind<-affils_df %>% select(univ,affil_id)
  
  univ_bind$affil_id<-as.character(univ_bind$affil_id)
  
  authors_df<- left_join(authors_df,univ_bind) 
  
  return(authors_df)
}
