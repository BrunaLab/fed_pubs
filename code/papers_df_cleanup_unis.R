papers_df_cleanup_unis <- function(original_papers_df) {
  
  #remove text from scopus id
  original_papers_df$scopus_article_id <- gsub("scopus_id:", "", original_papers_df$scopus_article_id)
  
  
  
  # VALIDATE Publication year (PY) ------------------------------------------
  
  original_papers_df <-original_papers_df %>% 
    separate(cover_date, c("PY2","PM","PD"), remove=FALSE,extra="merge")
  
  original_papers_df$PY<-as.numeric(original_papers_df$PY2)
  original_papers_df$PD<-as.numeric(original_papers_df$PD)
  original_papers_df$PM<-as.numeric(original_papers_df$PM)
  
  
  
  # ADD PAGE NUMBERS --------------------------------------------------------
  
  
  
  # Split the 'page_range' 
  split_pages <- strsplit(original_papers_df$page_range, "-")
  
  # Convert the list of split pages to data frame
  split_pages_df <- do.call(rbind, lapply(split_pages, function(x) {
    data.frame(BP = x[1], EP = x[2], stringsAsFactors = FALSE)
  }))
  
  original_papers_df <- cbind(original_papers_df, split_pages_df)
  original_papers_df$BP<-as.numeric(original_papers_df$BP) 
  original_papers_df$EP<-as.numeric(original_papers_df$EP) 
  original_papers_df$PG<-original_papers_df$EP-original_papers_df$BP
  
  
  
  
  # REORDER COLUMNS ---------------------------------------------------------
  
  
  
  original_papers_df$AF<-NA
  original_papers_df$AU<-NA
  original_papers_df$CA<-NA
  original_papers_df$AF<-NA
  original_papers_df$CA<-NA
  original_papers_df$C1<-NA
  original_papers_df$C3<-NA
  original_papers_df$CR<-NA
  original_papers_df$EM<-NA
  original_papers_df$ID<-NA
  original_papers_df$JI<-NA
  original_papers_df$NR<-NA
  original_papers_df$PD<-NA
  original_papers_df$PU<-NA
  original_papers_df$RI<-NA
  original_papers_df$OI<-NA
  # original_papers_df$PM<-NA
  original_papers_df$RP<-NA
  original_papers_df$SC<-NA
  original_papers_df$WC<-NA
  original_papers_df$AR<-NA
  original_papers_df$Z9<-NA
  original_papers_df$WE<-NA
  # "FN", 
  original_papers_df <- 
    original_papers_df[, c(
      "refID","source","AB", "AF", "AU",
      "CA", "BP", "C1", "C3", "CR", 
      "DE", "DI", "DT", "EM", "EP",
      "FU", "ID", "IS", "JI",
      "NR", "PD", "PG", "PT", "PU",
      "PY", "PY2","RI", "OI", "PM", "RP",
      "SC", "SN", "EI", "SO", "TC",
      "TI", "UT", "VL", "WC", "Z9",
      "AR", "WE"
    )]
  
  
  
  original_papers_df$DE<-gsub(" [|] ", "; ",original_papers_df$DE)
  original_papers_df$PT[original_papers_df$PT == "journal"] <- "j"
  
  return(original_papers_df)
}