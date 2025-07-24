clean_usgs <- function(authors_df,papers_df) {
  
  # USGS Publications_Warehouse file:
  # https://pubs.usgs.gov/pubs-services/publication/?mimeType=csv&
  
  # KEEP ONLY ARTICLES on USGS LIST
  
  usgs<-read_csv("./data_raw/usgs_publications.csv") %>% 
    mutate_all(tolower) %>% 
    select("usgs_refID"="Publication ID",
           DI=DOI,
           "AF"="Author(s)",
           "Country",
           "agency_3"="Contributing office(s)",
           "State",
           "City",
           "BP"="First page",
           "DI"="DOI",
           "EP"="Last page", 
           "IS"="Issue",
           "PT"="Publication type",
           "PY"="Year Published",
           "SN"="ISSN (print)",
           "EI"="ISSN (online)",
           "SO"="CHORUS Journal Name",
           "TI"="Title",
           "VL"="Volume",
           "URL"="CHORUS URL") %>% 
    mutate("agency"="interior",
           source="usgs",
           "agency_2"="usgs") %>% 
    filter(PT=="article") %>% 
    mutate(PT="j")
  
  usgs<-usgs %>% filter(PY>2018)
  
  usgs_no_doi<-usgs %>% filter(is.na(DI))%>% select(AF,BP,EP,TI,PY,SO,VL)
  usgs_with_doi<-usgs %>% filter(!is.na(DI)) %>% select(DI)
  
  write_csv(usgs_no_doi,"./data_raw/scopus_api/fed_files/usgs_no_doi.csv")
  write_csv(usgs_with_doi,"./data_raw/scopus_api/fed_files/usgs_with_doi.csv")
  
  
  
  # authors from usgs
  
  # usgs_authors<-usgs %>% 
  #   select(usgs_refID,PY,source,agency,agency_2,agency_3,Country,State,City,AF) %>% 
  #   separate_rows(AF, sep = "; ") %>% 
  #   mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
  #   mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
  #   # mutate(AF = str_remove(AF, fixed(email))) %>% 
  #   mutate(federal=if_else(!is.na(email),TRUE,FALSE)) %>% 
  #   group_by(usgs_refID) %>% 
  #   mutate(author_order=row_number(),
  #          affil_id=60011347,
  #          entry_no=cur_group_id()) %>% 
  #   mutate(AF=gsub(", jr."," jr.",AF)) %>% 
  #   mutate(Country=gsub("united states","usa",Country)) %>% 
  #   separate_wider_delim(AF,
  #                        delim=", ",
  #                        cols_remove = FALSE,
  #                        names=c("surname","given_name"),
  #                        too_many = "debug",
  #                        too_few = "debug") %>% 
  #   select(-AF_remainder,-AF_pieces) %>% 
  #   separate_wider_delim(given_name,
  #                        delim=" ",
  #                        cols_remove = FALSE,
  #                        names=c("init1","init2"),
  #                        too_many = "debug",
  #                        too_few = "debug") %>% 
  #   mutate(given_name_remainder=gsub("jr.","",given_name_remainder)) %>% 
  #   mutate(given_name_remainder=gsub("[.]","",given_name_remainder)) %>% 
  #   mutate(given_name_remainder=gsub("iii","",given_name_remainder)) %>% 
  #   mutate(init1 = str_sub(init1, 1, 1)) %>% 
  #   mutate(init2 = str_sub(init2, 1, 1)) %>% 
  #   mutate(first_middle_initials=paste(init1,".",init2,".",sep="")) %>% 
  #   mutate(first_middle_initials=gsub("NA.NA.","",first_middle_initials)) %>% 
  #   select(-c(given_name_ok,
  #             given_name_pieces, 
  #             given_name_remainder,
  #             init1,
  #             init2,
  #             email,
  #             AF_ok)
  #   ) %>% 
  #   rename(country=Country,
  #          city=City,
  #          state=State,
  #   ) %>% 
  #   mutate(AU=paste(surname,first_middle_initials, sep=",")) %>% 
  #   mutate(AU=gsub("[.]","",AU)) %>% 
  #   mutate(AU=ifelse(AU=="NA,",NA,AU)) %>%
  #   group_by(AF) %>% 
  #   mutate(authorID=cur_group_id()) %>% 
  #   mutate(authorID=paste(authorID,"usgs",sep="")) %>% 
  #   mutate(AU=gsub("NA","",AU),
  #          AU=gsub(",",", ",AU),
  #          AU=gsub("[(]","",AU)) %>% 
  #   mutate_all(trimws)
  # usgs_authors$AF<-trimws(usgs_authors$AF)
  # usgs_authors$AU<-trimws(usgs_authors$AU)
  # 
  
  
  
  # Define replacements for affiliation cleanup
  affil_replacements <- c(
    "&amp;amp;" = "and",
    "u s " = "us ",
    "united states " = "us ",
    "americorps vista" = "americorps",
    "u\\.s\\. " = "us ",
    "u\\. s\\. " = "us ",
    "\\." = ""
  )
  
  # Define replacements for given_name_remainder cleanup
  given_name_replacements <- c(
    "jr\\." = "",
    "\\." = "",
    "iii" = ""
  )
  
  usgs_authors <- usgs %>%
    select(usgs_refID, PY, source, agency, agency_2, agency_3, Country, State, City, AF) %>%
    separate_rows(AF, sep = "; ") %>%
    mutate(
      email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"),
      AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"),
      federal = !is.na(email),
      AF = str_replace_all(AF, affil_replacements),
      Country = str_replace_all(Country, "united states", "usa")
    ) 
  
  
  
  
  usgs_authors <- usgs_authors  %>%
    group_by(usgs_refID) %>%
    mutate(
      author_order = row_number(),
      affil_id = 60011347,
      entry_no = cur_group_id()
    ) %>%
    separate_wider_delim(AF, delim = ", ", cols_remove = FALSE,
                         names = c("surname", "given_name"),
                         too_many = "debug", too_few = "debug") %>%
    separate_wider_delim(given_name, delim = " ", cols_remove = FALSE,
                         names = c("init1", "init2"),
                         too_many = "debug", too_few = "debug") %>%
    mutate(
      given_name_remainder = given_name,
      given_name_remainder = str_replace_all(given_name_remainder, given_name_replacements),
      init1 = str_sub(init1, 1, 1),
      init2 = str_sub(init2, 1, 1),
      first_middle_initials = paste0(init1, ".", init2, "."),
      first_middle_initials = na_if(first_middle_initials, "NA.NA."),
      AU = paste(surname, first_middle_initials, sep = ","),
      AU = str_replace_all(AU, "\\.", ""),
      AU = if_else(AU == "NA,", NA_character_, AU)
    ) %>%
    group_by(AF) %>%
    mutate(
      authorID = paste0(cur_group_id(), "usgs"),
      AU = str_replace_all(AU, c("NA" = "", "," = ", ", "\\(" = ""))
    ) %>%
    ungroup() %>%
    mutate(across(everything(), trimws)) %>%
    rename(country = Country, city = City, state = State) %>%
    select(-c(given_name_ok, given_name_pieces, given_name_remainder,
              init1, init2, AF_ok))
  
  # Final trim for AF and AU (if needed)
  usgs_authors$AF <- trimws(usgs_authors$AF)
  usgs_authors$AU <- trimws(usgs_authors$AU)
  
  
  write_rds(usgs_authors, "./data_intermediate/usgs_authors_clean.rds")
  write_rds(usgs, "./data_intermediate/usgs_papers_clean.rds")
  
  
  
}
