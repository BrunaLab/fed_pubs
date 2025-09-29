fix_usgs_affils <- function(authors_to_fix, papers_to_fix) {
  # TO CORRECT *ALL* THE USGS AUTHORS in authors_to_fix, not just the ones that
  # were in the added papers
  #
  # authors_to_fix<-authors_df_trim
  # papers_to_fix<-papers_df_trim

  usgs_papers <- read_rds("./data_clean/usgs_papers_clean.rds")

  usgs_authors <- read_rds("./data_clean/usgs_authors_clean.rds")
  usgs_authors <- usgs_authors %>%
    filter(federal == TRUE)

  usgs_papers_key <- usgs_papers %>%
    select(usgs_refID, DI)

  papers_to_fix_to_fix_au <- usgs_papers %>%
    filter(usgs_refID %in% usgs_authors$usgs_refID) %>%
    select(DI, TI, PY)

  papers_to_fix <- papers_to_fix
  papers_to_fix_to_fix_au$PY <- as.integer(papers_to_fix_to_fix_au$PY)
  papers_to_fix_cut <- inner_join(papers_to_fix, papers_to_fix_to_fix_au, by = c("DI", "TI", "PY")) %>%
    select(refID)

  authors_fixed <- authors_to_fix %>% filter(refID %in% papers_to_fix_cut$refID)

  authors_fixed <- authors_fixed %>%
    mutate(AU = AF) %>%
    mutate(AU = str_replace_all(AU, "[.]", "")) %>%
    mutate(first_middle_initials = str_replace_all(first_middle_initials, "[.]", "")) %>%
    mutate(first = str_sub(first_middle_initials, start = 1, end = 1)) %>%
    filter(is.na(federal))

  usgs_authors$author_order <- as.integer(usgs_authors$author_order)
  usgs_authors$federal <- as.logical(usgs_authors$federal)
  usgs_authors <- usgs_authors %>%
    rename(
      agency_primary = agency,
      agency_short = agency_2,
      affiliation = agency_3,
    ) %>%
    mutate(first_middle_initials = str_replace_all(first_middle_initials, "[.]", "")) %>%
    mutate(AU = str_replace_all(AU, "[,]", "")) %>%
    mutate(AF = str_replace_all(AF, "[.]", "")) %>%
    mutate(first = str_sub(first_middle_initials, start = 1, end = 1))

  # DO IN ORDER FROM GREATEST RIGOR DOWN

  update_df <- full_join(authors_fixed, usgs_authors, by = c("AU"))

  update_df <- update_df %>%
    relocate(federal.y, .before = 1) %>%
    relocate(federal.x, .before = 1) %>%
    arrange(federal.x, federal.y) %>%
    select(
      -federal.x,
      -agency,
      -agency_primary.x
    ) %>%
    # filter(is.na(federal.x)) %>%
    distinct() %>%
    filter(federal.y == TRUE) %>%
    select(federal.y, AF.x, SID, refID, agency_short, agency_primary.y) %>%
    distinct() %>%
    mutate(AF = AF.x) %>%
    select(-AF.x) %>%
    rename(
      agency_primary = agency_primary.y,
      federal = federal.y
    )

  # names(update_df)
  authors_to_fix <- authors_to_fix %>%
    mutate(AF = str_replace_all(AF, "[.]", ""))

  # authors_to_fix<-left_join(authors_to_fix,update_df,by=c("SID","refID","AF"))
  authors_to_fix <- left_join(authors_to_fix, update_df, by = c("SID", "refID"))
  authors_to_fix <- authors_to_fix %>%
    mutate(name_check = (AF.x == AF.y))

  # names(authors_to_fix)

  authors_to_fix <- authors_to_fix %>%
    mutate(federal.x = if_else(name_check == TRUE & (is.na(federal.x) & !is.na(federal.y)), federal.y, federal.x)) %>%
    mutate(agency = if_else(name_check == TRUE & (is.na(agency) & !is.na(agency_short)), agency_short, agency)) %>%
    mutate(agency_primary.x = if_else(name_check == TRUE & (is.na(agency_primary.x) & !is.na(agency_primary.y)), agency_primary.y, agency_primary.x)) %>%
    select(
      -federal.y,
      -agency_short,
      -agency_primary.y,
      -AF.y
    ) %>%
    rename(
      federal = federal.x,
      agency_primary = agency_primary.x,
      AF = AF.x
    )





  # step 2 ------------------------------------------------------------------




  authors_fixed <- authors_to_fix %>% filter(refID %in% papers_to_fix_cut$refID)

  authors_fixed <- authors_fixed %>%
    mutate(AU = AF) %>%
    mutate(AU = str_replace_all(AU, "[.]", "")) %>%
    mutate(first_middle_initials = str_replace_all(first_middle_initials, "[.]", "")) %>%
    mutate(first = str_sub(first_middle_initials, start = 1, end = 1)) %>%
    filter(is.na(federal))
  update_df <- left_join(authors_fixed, usgs_authors, by = c("surname", "first"))

  update_df <- update_df %>%
    relocate(federal.y, .before = 1) %>%
    relocate(federal.x, .before = 1) %>%
    arrange(federal.x, federal.y) %>%
    select(
      -federal.x,
      -agency,
      -agency_primary.x
    ) %>%
    # filter(is.na(federal.x)) %>%
    distinct() %>%
    filter(federal.y == TRUE) %>%
    select(federal.y, AF.x, SID, refID, agency_short, agency_primary.y) %>%
    distinct() %>%
    mutate(AF = AF.x) %>%
    select(-AF.x) %>%
    rename(
      agency_primary = agency_primary.y,
      federal = federal.y
    )

  names(update_df)
  authors_to_fix <- authors_to_fix %>%
    mutate(AF = str_replace_all(AF, "[.]", ""))

  # authors_to_fix<-left_join(authors_to_fix,update_df,by=c("SID","refID","AF"))
  authors_to_fix <- left_join(authors_to_fix, update_df, by = c("SID", "refID"))
  authors_to_fix <- authors_to_fix %>%
    mutate(name_check = (AF.x == AF.y))

  # names(authors_to_fix)

  authors_to_fix <- authors_to_fix %>%
    mutate(federal.x = if_else(name_check == TRUE & 
                                 (is.na(federal.x) & !is.na(federal.y)), 
                               federal.y, 
                               federal.x)) %>%
    mutate(agency = if_else(name_check == TRUE & 
                              (is.na(agency) & !is.na(agency_short)), 
                            agency_short, 
                            agency)) %>%
    mutate(agency_primary.x = if_else((is.na(agency_primary.x) & 
                                         !is.na(agency_primary.y)), 
                                      agency_primary.y, 
                                      agency_primary.x)) %>%
    select(
      -federal.y,
      -agency_short,
      -agency_primary.y,
      -AF.y
    ) %>%
    rename(
      federal = federal.x,
      agency_primary = agency_primary.x,
      AF = AF.x
    ) %>%
    select(-name_check)


  authors_to_fix <- authors_to_fix %>%
    mutate(agency = if_else(agency == "usgs", "interior", agency))

  return(authors_to_fix)
}
