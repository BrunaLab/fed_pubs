ID_fed_authors <- function(authors_df, affils_df) {
  
  authors_df<-authors_df %>% 
    mutate(refID=as.character(refID)) %>% 
    mutate(entry_no=as.character(entry_no)) %>% 
    mutate(affil_id=as.character(affil_id)) 
  
  fed_bind<-affils_df %>% select(agency,agency_primary, federal,affil_id) %>% distinct()
  fed_bind<-fed_bind %>% filter(federal==TRUE)
  fed_bind$affil_id<-as.character(fed_bind$affil_id)
  
  authors_df<- left_join(authors_df,fed_bind,by="affil_id") 
  
  return(authors_df)
  
}
