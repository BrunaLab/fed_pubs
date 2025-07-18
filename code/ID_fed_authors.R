ID_fed_authors <- function(authors_df, affils_df) {
  
  authors_df<-authors_df %>% 
    mutate(refID=as.character(refID)) %>% 
    mutate(entry_no=as.character(entry_no)) %>% 
    mutate(affil_id=as.character(affil_id)) 
  
  # affil_ids$affil_id<-as.character(affil_id$affil_id)
  
  fed_bind<-affils_df %>% select(federal,affil_id) %>% distinct()
  
  fed_bind$affil_id<-as.character(fed_bind$affil_id)
  
  # authors_df<- left_join(authors_df,fed_bind,by="affil_id") 
  authors_df<- left_join(authors_df,fed_bind,by="affil_id") 
  
  return(authors_df)
  
}

# optimized with ftable
# 
# library(data.table)
# 
# ID_fed_authors <- function(authors_df, affils_df) {
#   # Convert to data.table
#   setDT(authors_df)
#   setDT(affils_df)
#   
#   # Ensure affil_id is character in both
#   authors_df[, `:=`(
#     refID = as.character(refID),
#     entry_no = as.character(entry_no),
#     affil_id = as.character(affil_id)
#   )]
#   affils_df[, affil_id := as.character(affil_id)]
#   
#   # Select only needed columns from affils_df
#   fed_bind <- affils_df[, .(affil_id, federal)]
#   
#   # Perform fast join
#   authors_df <- merge(authors_df, fed_bind, by = "affil_id", all.x = TRUE)
#   
#   return(authors_df)
# }
