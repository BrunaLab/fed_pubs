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
