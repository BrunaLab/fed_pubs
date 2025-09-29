library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)



# LOAD FILES -----------------------------------------------------------


# folder_path_affils <- "./data_raw/affils/year_files_fed"
folder_path_papers <- "./data_raw/papers/year_files_fed"





# affil binder -----------------------------------------------------------

data_dir_affils <- folder_path_affils
csv_files_affils_all <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")



dataDir <- data_dir_affils # Update this path
dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)

# Read and tag each file
dt_list <- lapply(dataFls, function(file) {
  dt <- fread(file, fill = TRUE)
  dt[, source_file_combined := basename(file)] # Add column with filename
  return(dt)
})

# Combine all tagged data tables
affils_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list)



# papers binder -----------------------------------------------------------
# data_dir_papers<-"./data_raw/scopus_api/unis_files/papers"
data_dir_papers <- folder_path_papers
csv_files_papers_all <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")


dataDir <- data_dir_papers # Update this path
dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)

# Read and tag each file
dt_list <- lapply(dataFls, function(file) {
  dt <- fread(file, fill = TRUE)
  dt[, source_file_combined := basename(file)] # Add column with filename
  return(dt)
})

# Combine all tagged data tables
papers_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list)

# clean_usgs <- function(authors_df,papers_df) {

# USGS Publications_Warehouse file:
# https://pubs.usgs.gov/pubs-services/publication/?mimeType=csv&

# KEEP ONLY ARTICLES on USGS LIST

usgs <- read_csv("./data_raw/usgs_publications.csv") %>%
  mutate_all(tolower) %>%
  select(
    "usgs_refID" = "Publication ID",
    DI = DOI,
    "AF" = "Author(s)",
    "Country",
    "agency_3" = "Contributing office(s)",
    "State",
    "City",
    "BP" = "First page",
    "DI" = "DOI",
    "EP" = "Last page",
    "IS" = "Issue",
    "PT" = "Publication type",
    "PY" = "Year Published",
    "SN" = "ISSN (print)",
    "EI" = "ISSN (online)",
    "SO" = "CHORUS Journal Name",
    "TI" = "Title",
    "VL" = "Volume",
    "URL" = "CHORUS URL"
  ) %>%
  mutate(
    "agency" = "interior",
    source = "usgs",
    "agency_2" = "usgs"
  ) %>%
  filter(PT == "article") %>%
  mutate(PT = "j")

usgs <- usgs %>% filter(PY > 2018)

# AUTHORS AT PLACES LIKE COOP UNITS ARE NOT T LISTED AS USGS IN SCOPUS
# NEED TO FIND THEM VIA EMAIL AND CHNAGE THEIR AFFILIATIONS IN OTHER DFs

# authors from usgs

usgs_authors <- usgs %>%
  select(usgs_refID, PY, source, agency, agency_2, agency_3, Country, State, City, AF) %>%
  separate_rows(AF, sep = "; ") %>%
  mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>%
  mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>%
  # mutate(AF = str_remove(AF, fixed(email))) %>%
  mutate(federal = if_else(!is.na(email), TRUE, FALSE)) %>%
  group_by(usgs_refID) %>%
  mutate(
    author_order = row_number(),
    affil_id = 60011347,
    entry_no = cur_group_id()
  ) %>%
  mutate(AF = gsub(", jr.", " jr.", AF)) %>%
  mutate(Country = gsub("united states", "usa", Country)) %>%
  separate_wider_delim(AF,
    delim = ", ",
    cols_remove = FALSE,
    names = c("surname", "given_name"),
    too_many = "debug",
    too_few = "debug"
  ) %>%
  select(-AF_remainder, -AF_pieces) %>%
  separate_wider_delim(given_name,
    delim = " ",
    cols_remove = FALSE,
    names = c("init1", "init2"),
    too_many = "debug",
    too_few = "debug"
  ) %>%
  mutate(given_name_remainder = gsub("jr.", "", given_name_remainder)) %>%
  mutate(given_name_remainder = gsub("[.]", "", given_name_remainder)) %>%
  mutate(given_name_remainder = gsub("iii", "", given_name_remainder)) %>%
  mutate(init1 = str_sub(init1, 1, 1)) %>%
  mutate(init2 = str_sub(init2, 1, 1)) %>%
  mutate(first_middle_initials = paste(init1, ".", init2, ".", sep = "")) %>%
  mutate(first_middle_initials = gsub("NA.NA.", "", first_middle_initials)) %>%
  select(-c(
    given_name_ok,
    given_name_pieces,
    given_name_remainder,
    init1,
    init2,
    email,
    AF_ok
  )) %>%
  rename(
    country = Country,
    city = City,
    state = State,
  ) %>%
  mutate(AU = paste(surname, first_middle_initials, sep = ",")) %>%
  mutate(AU = gsub("[.]", "", AU)) %>%
  mutate(AU = ifelse(AU == "NA,", NA, AU)) %>%
  group_by(AF) %>%
  mutate(authorID = cur_group_id()) %>%
  mutate(authorID = paste(authorID, "usgs", sep = "")) %>%
  mutate(
    AU = gsub("NA", "", AU),
    AU = gsub(",", ", ", AU),
    AU = gsub("[(]", "", AU)
  ) %>%
  mutate_all(trimws)
usgs_authors$AF <- trimws(usgs_authors$AF)
usgs_authors$AU <- trimws(usgs_authors$AU)

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


# ASSIGN USGS AFFILIATION ID
usgs_authors <- usgs_authors %>%
  group_by(usgs_refID) %>%
  mutate(
    author_order = row_number(),
    affil_id = 60011347,
    entry_no = cur_group_id()
  ) %>%
  separate_wider_delim(AF,
    delim = ", ", cols_remove = FALSE,
    names = c("surname", "given_name"),
    too_many = "debug", too_few = "debug"
  ) %>%
  separate_wider_delim(given_name,
    delim = " ", cols_remove = FALSE,
    names = c("init1", "init2"),
    too_many = "debug", too_few = "debug"
  ) %>%
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
  select(-c(
    given_name_ok, given_name_pieces, given_name_remainder,
    init1, init2, AF_ok
  ))

# Final trim for AF and AU (if needed)
usgs_authors$AF <- trimws(usgs_authors$AF)
usgs_authors$AU <- trimws(usgs_authors$AU)


# SAVE RDS OF USGS PAPERS AND AUTHORS

write_rds(usgs_authors, "./data_intermediate/usgs_authors_clean.rds")
write_rds(usgs, "./data_intermediate/usgs_papers_clean.rds")








usgs_no_doi <- usgs %>%
  filter(is.na(DI)) %>%
  select(AF, BP, EP, TI, PY, SO, VL) %>%
  distinct()
usgs_with_doi <- usgs %>%
  filter(!is.na(DI)) %>%
  select(DI) %>%
  distinct()



# usgs<-read_csv("./data_raw/scopus_api/fed_files/usgs_with_doi.csv")
usgs_to_scopus_DI <- usgs_with_doi %>%
  anti_join(papers_df, by = "DI")

usgs_to_scopus_TI <- usgs_no_doi %>%
  anti_join(papers_df, by = "TI")




# search scopus


library(rscopus)
library(tidyverse)

# long loop ---------------------------------------------------------------

# for all loaded after a specific date
# AF-ID("Lawrence Berkeley National Laboratory" 60007174) AND LOAD-DATE > 20250501

# new_2025_search<-affils_all %>%
#   select(affil_id) %>%
#   distinct()
#
# search_term<-new_2025_search
#



search_term <- usgs_to_scopus_TI$TI






# 2021       (ONE(60014232, 60021658 60077228)

# search_term<-scopus_id_total$affil_id[101:nrow(scopus_id_total)]

# search_term<-"60006577"

# https://www.scopus.com/pages/organization/60024266

term <- seq_along(search_term)
term <- seq_along(search_term)[184:219]

for (h in term) {
  # a<-paste("(AF-ID('",search_term[h],"') AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
  # a<-paste("(DOI('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no)) AND LOAD-DATE < ",LOAD,sep="")
  # a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
  # a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar))",sep="")
  # a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) AND LOAD-DATE > ",LOAD,")",sep="")
  # a<-paste("((AFFIL(",search_term[h],")"," AND AFFILCOUNTRY('united states')) AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")


  ###  DONT FORGET TO CHANGE THE FILE NAME BELOW!!!!!!! ###


  # c <- " AND PUBYEAR > 2018"
  # query_string <-paste0(a, c,")",sep = "")


  # c <- " AND PUBYEAR = "
  query_string <- paste0('TITLE("', search_term[h], '")', sep = "")



  # api1: 38c1ea28aed25f40f11034d20557ccde
  # 8d8d7b628fae6e1a5a04db969b0bca93
  # api2:

  # 8e204bc721cb41c0251c8846351342b0

  # "c253aa47dd592442b1d5ad7ded7b0514" throttled 7/15
  # "8d8d7b628fae6e1a5a04db969b0bca93" throttled 7/16


  # # Example for downloading 25 at a time
  #
  # start_vals <- seq(300, 7999, by = 25)
  #
  #
  #
  # results_list <- lapply(start_vals, function(s) {
  #   rscopus::scopus_search(query_string,
  #                          max_count = 25,
  #                          start = s,
  #                          view = "COMPLETE",
  #                          api_key = "8e204bc721cb41c0251c8846351342b0")
  # })

  scopus_data <- rscopus::scopus_search(query_string,
    max_count = 8000,
    # start = 0,
    view = "COMPLETE",
    api_key = "8d8d7b628fae6e1a5a04db969b0bca93"
  )



  # query_string <- paste0("eid(2-s2.0-0024266051)")
  scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
  # nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3
  if (nrow(scopus_data_raw$df) == 1 & ncol(scopus_data_raw$df) == 3) {
    next
  } else {
    scopus_papers <- scopus_data_raw$df
    # jae_papers$`prism:publicationName`
    term_for_file <- paste("usgs_TI_", h, sep = "")

    papers <- paste("./data_raw/scopus_api/usgs/papers/", term_for_file, "_papers", ".csv", sep = "")
    write_csv(scopus_papers, papers)

    scopus_affiliations <- scopus_data_raw$affiliation

    affils <- paste("./data_raw/scopus_api/usgs/affils/", term_for_file, "_affils_", ".csv", sep = "")
    write_csv(scopus_affiliations, affils)

    scopus_authors <- scopus_data_raw$author
    authors <- paste("./data_raw/scopus_api/usgs/authors/", term_for_file, "_authors", ".csv", sep = "")
    write_csv(scopus_authors, authors)
  }
}






#
#
# # that means these usgs papers arent in df and need to be added
# usgs_NOT_in_papers_df<-anti_join(usgs_papers,usgs_already_in_papers_df,by="DI") %>%
#   mutate(source="usgs_database")
#
#
#
#
#
#
# # authors from usgs
#
# # usgs_authors<-usgs %>%
# #   select(usgs_refID,PY,source,agency,agency_2,agency_3,Country,State,City,AF) %>%
# #   separate_rows(AF, sep = "; ") %>%
# #   mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>%
# #   mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>%
# #   # mutate(AF = str_remove(AF, fixed(email))) %>%
# #   mutate(federal=if_else(!is.na(email),TRUE,FALSE)) %>%
# #   group_by(usgs_refID) %>%
# #   mutate(author_order=row_number(),
# #          affil_id=60011347,
# #          entry_no=cur_group_id()) %>%
# #   mutate(AF=gsub(", jr."," jr.",AF)) %>%
# #   mutate(Country=gsub("united states","usa",Country)) %>%
# #   separate_wider_delim(AF,
# #                        delim=", ",
# #                        cols_remove = FALSE,
# #                        names=c("surname","given_name"),
# #                        too_many = "debug",
# #                        too_few = "debug") %>%
# #   select(-AF_remainder,-AF_pieces) %>%
# #   separate_wider_delim(given_name,
# #                        delim=" ",
# #                        cols_remove = FALSE,
# #                        names=c("init1","init2"),
# #                        too_many = "debug",
# #                        too_few = "debug") %>%
# #   mutate(given_name_remainder=gsub("jr.","",given_name_remainder)) %>%
# #   mutate(given_name_remainder=gsub("[.]","",given_name_remainder)) %>%
# #   mutate(given_name_remainder=gsub("iii","",given_name_remainder)) %>%
# #   mutate(init1 = str_sub(init1, 1, 1)) %>%
# #   mutate(init2 = str_sub(init2, 1, 1)) %>%
# #   mutate(first_middle_initials=paste(init1,".",init2,".",sep="")) %>%
# #   mutate(first_middle_initials=gsub("NA.NA.","",first_middle_initials)) %>%
# #   select(-c(given_name_ok,
# #             given_name_pieces,
# #             given_name_remainder,
# #             init1,
# #             init2,
# #             email,
# #             AF_ok)
# #   ) %>%
# #   rename(country=Country,
# #          city=City,
# #          state=State,
# #   ) %>%
# #   mutate(AU=paste(surname,first_middle_initials, sep=",")) %>%
# #   mutate(AU=gsub("[.]","",AU)) %>%
# #   mutate(AU=ifelse(AU=="NA,",NA,AU)) %>%
# #   group_by(AF) %>%
# #   mutate(authorID=cur_group_id()) %>%
# #   mutate(authorID=paste(authorID,"usgs",sep="")) %>%
# #   mutate(AU=gsub("NA","",AU),
# #          AU=gsub(",",", ",AU),
# #          AU=gsub("[(]","",AU)) %>%
# #   mutate_all(trimws)
# # usgs_authors$AF<-trimws(usgs_authors$AF)
# # usgs_authors$AU<-trimws(usgs_authors$AU)
# #
#
#
#
# # Define replacements for affiliation cleanup
# affil_replacements <- c(
#   "&amp;amp;" = "and",
#   "u s " = "us ",
#   "united states " = "us ",
#   "americorps vista" = "americorps",
#   "u\\.s\\. " = "us ",
#   "u\\. s\\. " = "us ",
#   "\\." = ""
# )
#
# # Define replacements for given_name_remainder cleanup
# given_name_replacements <- c(
#   "jr\\." = "",
#   "\\." = "",
#   "iii" = ""
# )
#
# usgs_authors <- usgs %>%
#   select(usgs_refID, PY, source, agency, agency_2, agency_3, Country, State, City, AF) %>%
#   separate_rows(AF, sep = "; ") %>%
#   mutate(
#     email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"),
#     AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"),
#     federal = !is.na(email),
#     AF = str_replace_all(AF, affil_replacements),
#     Country = str_replace_all(Country, "united states", "usa")
#   )
#
#
#
#
# usgs_authors <- usgs_authors  %>%
#   group_by(usgs_refID) %>%
#   mutate(
#     author_order = row_number(),
#     affil_id = 60011347,
#     entry_no = cur_group_id()
#   ) %>%
#   separate_wider_delim(AF, delim = ", ", cols_remove = FALSE,
#                        names = c("surname", "given_name"),
#                        too_many = "debug", too_few = "debug") %>%
#   separate_wider_delim(given_name, delim = " ", cols_remove = FALSE,
#                        names = c("init1", "init2"),
#                        too_many = "debug", too_few = "debug") %>%
#   mutate(
#     given_name_remainder = given_name,
#     given_name_remainder = str_replace_all(given_name_remainder, given_name_replacements),
#     init1 = str_sub(init1, 1, 1),
#     init2 = str_sub(init2, 1, 1),
#     first_middle_initials = paste0(init1, ".", init2, "."),
#     first_middle_initials = na_if(first_middle_initials, "NA.NA."),
#     AU = paste(surname, first_middle_initials, sep = ","),
#     AU = str_replace_all(AU, "\\.", ""),
#     AU = if_else(AU == "NA,", NA_character_, AU)
#   ) %>%
#   group_by(AF) %>%
#   mutate(
#     authorID = paste0(cur_group_id(), "usgs"),
#     AU = str_replace_all(AU, c("NA" = "", "," = ", ", "\\(" = ""))
#   ) %>%
#   ungroup() %>%
#   mutate(across(everything(), trimws)) %>%
#   rename(country = Country, city = City, state = State) %>%
#   select(-c(given_name_ok, given_name_pieces, given_name_remainder,
#             init1, init2, AF_ok))
#
# # Final trim for AF and AU (if needed)
# usgs_authors$AF <- trimws(usgs_authors$AF)
# usgs_authors$AU <- trimws(usgs_authors$AU)
#
#
# write_rds(usgs_authors, "./data_intermediate/usgs_authors_clean.rds")
# write_rds(usgs, "./data_intermediate/usgs_papers_clean.rds")
