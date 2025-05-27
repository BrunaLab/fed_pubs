papers_df_cleanup <- function(papers_df) {
  
  #remove text from scopus id
  papers_df$scopus_article_id <- gsub("scopus_id:", "", papers_df$scopus_article_id)
  
  
  
  # VALIDATE Publication year (PY) ------------------------------------------
  
  papers_df$cover_date
  papers_df <-papers_df %>% 
    separate(cover_date, c("PY2","PM","PD"), remove=FALSE,extra="merge")
  
  papers_df$PY<-as.numeric(papers_df$PY)
  papers_df$PD<-as.numeric(papers_df$PD)
  papers_df$PM<-as.numeric(papers_df$PM)
  
  
  
  # ADD PAGE NUMBERS --------------------------------------------------------
  
  
  
  # Split the 'page_range' 
  split_pages <- strsplit(papers_df$page_range, "-")
  
  # Convert the list of split pages to data frame
  split_pages_df <- do.call(rbind, lapply(split_pages, function(x) {
    data.frame(BP = x[1], EP = x[2], stringsAsFactors = FALSE)
  }))
  
  papers_df <- cbind(papers_df, split_pages_df)
  papers_df$BP<-as.numeric(papers_df$BP) 
  papers_df$EP<-as.numeric(papers_df$EP) 
  papers_df$PG<-papers_df$EP-papers_df$BP
  
  
  
  
  # REORDER COLUMNS ---------------------------------------------------------
  
  
  
  papers_df$AF<-NA
  papers_df$AU<-NA
  papers_df$CA<-NA
  papers_df$AF<-NA
  papers_df$CA<-NA
  papers_df$C1<-NA
  papers_df$C3<-NA
  papers_df$CR<-NA
  papers_df$EM<-NA
  papers_df$ID<-NA
  papers_df$JI<-NA
  papers_df$NR<-NA
  papers_df$PD<-NA
  papers_df$PU<-NA
  papers_df$RI<-NA
  papers_df$OI<-NA
  # papers_df$PM<-NA
  papers_df$RP<-NA
  papers_df$SC<-NA
  papers_df$WC<-NA
  papers_df$AR<-NA
  papers_df$Z9<-NA
  papers_df$WE<-NA
  # "FN", 
  papers_df <- 
    papers_df[, c(
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
  
  
  
  papers_df$DE<-gsub(" [|] ", "; ",papers_df$DE)
  papers_df$PT[papers_df$PT == "journal"] <- "j"
  
  return(papers_df)
}