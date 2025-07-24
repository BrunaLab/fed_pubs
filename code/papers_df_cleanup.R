papers_df_cleanup <- function(papers_df) {
  
  # Remove text from scopus id
  papers_df$scopus_article_id <- str_remove(papers_df$scopus_article_id, "scopus_id:")
  
  # Separate cover_date into year, month, day
  papers_df <- papers_df %>%
    separate(cover_date, into = c("PY2", "PM", "PD"), remove = FALSE, extra = "merge", fill = "right") %>%
    mutate(
      PY = as.numeric(PY),
      PD = as.numeric(PD),
      PM = as.numeric(PM)
    )
  
  # Split page_range into BP and EP, then calculate PG
  split_pages <- str_split_fixed(papers_df$page_range, "-", 2)
  papers_df <- papers_df %>%
    mutate(
      BP = as.numeric(split_pages[, 1]),
      EP = as.numeric(split_pages[, 2]),
      PG = EP - BP
    )
  
  
  
  
  
  
  # Add missing columns with NA (only if they don't exist)
  missing_cols <- c("AF", "AU", "CA", "C1", "C3", "CR", "EM", "ID", "JI", "NR", 
                    "PU", "RI", "OI", "RP", "SC", "WC", "AR", "Z9", "WE")
  for (col in missing_cols) {
    if (!col %in% names(papers_df)) {
      papers_df[[col]] <- NA
    }
  }
  
  # Reorder columns
  desired_order <- c(
    "refID", "source_file","entry_no","scopus_article_id", "AB", "AF", "AU", "CA", "BP", "C1", "C3", "CR", 
    "DE", "DI", "DT", "EM", "EP", "FU", "ID", "IS", "JI", "NR", "PD", "PG", 
    "PT", "PU", "PY", "PY2", "RI", "OI", "PM", "RP", "SC", "SN", "EI", "SO", 
    "TC", "TI", "UT", "VL", "WC", "Z9", "AR", "WE"
  )
  papers_df <- papers_df[, intersect(desired_order, names(papers_df))]
  
  # Final cleanup
  papers_df <- papers_df %>%
    mutate(
      PY = PY2,
      DE = str_replace_all(DE, " [|] ", "; "),
      PT = if_else(PT == "journal", "j", PT)
    ) %>%
    select(-PY2)
  
  return(papers_df)
  
  
  
  
  
  
}
# 
# 
# library("microbenchmark")
# 
# # Define your original and optimized functions here
# # (Paste both versions into your R script)
# 
# # Benchmark both functions
# benchmark_results <- microbenchmark(
#   original = papers_df_cleanup(papers_df),
#   optimized = papers_df_cleanup_new(papers_df),
#   times = 10
# )
# 
# print(benchmark_results)

# papers_df_cleanup <- function(papers_df) {
#   
#   #remove text from scopus id
#   papers_df$scopus_article_id <- gsub("scopus_id:", "", papers_df$scopus_article_id)
#   
#   
#   
#   # VALIDATE Publication year (PY) ------------------------------------------
#   
#   papers_df$cover_date
#   papers_df <-papers_df %>% 
#     separate(cover_date, c("PY2","PM","PD"), remove=FALSE,extra="merge")
#   
#   papers_df$PY<-as.numeric(papers_df$PY)
#   papers_df$PD<-as.numeric(papers_df$PD)
#   papers_df$PM<-as.numeric(papers_df$PM)
#   
#   
#   
#   # ADD PAGE NUMBERS --------------------------------------------------------
#   
#   
#   
#   # Split the 'page_range' 
#   split_pages <- strsplit(papers_df$page_range, "-")
#   
#   # Convert the list of split pages to data frame
#   split_pages_df <- do.call(rbind, lapply(split_pages, function(x) {
#     data.frame(BP = x[1], EP = x[2], stringsAsFactors = FALSE)
#   }))
#   
#   papers_df <- cbind(papers_df, split_pages_df)
#   papers_df$BP<-as.numeric(papers_df$BP) 
#   papers_df$EP<-as.numeric(papers_df$EP) 
#   papers_df$PG<-papers_df$EP-papers_df$BP
#   
#   
#   
#   
#   # REORDER COLUMNS ---------------------------------------------------------
#   
#   
#   
#   papers_df$AF<-NA
#   papers_df$AU<-NA
#   papers_df$CA<-NA
#   papers_df$AF<-NA
#   papers_df$CA<-NA
#   papers_df$C1<-NA
#   papers_df$C3<-NA
#   papers_df$CR<-NA
#   papers_df$EM<-NA
#   papers_df$ID<-NA
#   papers_df$JI<-NA
#   papers_df$NR<-NA
#   papers_df$PD<-NA
#   papers_df$PU<-NA
#   papers_df$RI<-NA
#   papers_df$OI<-NA
#   # papers_df$PM<-NA
#   papers_df$RP<-NA
#   papers_df$SC<-NA
#   papers_df$WC<-NA
#   papers_df$AR<-NA
#   papers_df$Z9<-NA
#   papers_df$WE<-NA
#   # "FN", 
#   papers_df <- 
#     papers_df[, c(
#       "refID","source_file","AB", "AF", "AU",
#       "CA", "BP", "C1", "C3", "CR", 
#       "DE", "DI", "DT", "EM", "EP",
#       "FU", "ID", "IS", "JI",
#       "NR", "PD", "PG", "PT", "PU",
#       "PY", "PY2","RI", "OI", "PM", "RP",
#       "SC", "SN", "EI", "SO", "TC",
#       "TI", "UT", "VL", "WC", "Z9",
#       "AR", "WE"
#     )]
#   
#   papers_df <-papers_df %>% 
#     mutate(PY=PY2) %>% 
#     select(-PY2)
#   
#   papers_df$DE<-gsub(" [|] ", "; ",papers_df$DE)
#   papers_df$PT[papers_df$PT == "journal"] <- "j"
#   
#   return(papers_df)
# }
# 
# 


