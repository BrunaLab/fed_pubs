library(janitor)
library(tidyverse)
library(progress)
library(fs)
# #####################################
data_dir<-"./data_raw/scopus_api"
dir = TRUE
include_all=FALSE
# #####################################


# Define folder paths

folder_path_papers  <- file.path(data, "papers")
folder_path_authors <- file.path(data, "authors")
folder_path_affils  <- file.path(data, "affils")

# Validate structure
validate_csv_structure <- function(...) {
  folders <- list(...)
  file_counts <- map_int(folders, ~ length(list.files(.x, pattern = "\\.csv$", full.names = TRUE)))
  
  if (any(file_counts == 0) || length(unique(file_counts)) != 1) {
    stop("ERROR: Either the file path is incorrect OR folders contain non-CSV or mismatched files.")
  }
  
  message("Now processing all references files")
}
validate_csv_structure(folder_path_papers)
validate_csv_structure(folder_path_authors)
validate_csv_structure(folder_path_affils)
validate_csv_structure(folder_path_papers, folder_path_authors, folder_path_affils)


# bind the csv's together -------------------------------------------------



# fs::dir_ls(data_dir)
# binder using fs package wrapper for purrr
# https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/

# affil binder -----------------------------------------------------------

data_dir_affils<-"./data_raw/scopus_api/affils"
csv_files_affils_all <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")


cvs_binder_affils <- function(csv_files_affils) {
  
  affils_df <- csv_files_affils %>%
    map_dfr(~ read_csv(.x),
            .id = "source")

}

affils_df_1<-cvs_binder_affils(csv_files_affils_all[1:5000])
write_csv(affils_df_1,"./data_raw/affils_df_1.csv")
affils_df_2<-cvs_binder_affils(csv_files_affils_all[5001:10000])
write_csv(affils_df_2,"./data_raw/affils_df_2.csv")
affils_df_3<-cvs_binder_affils(csv_files_affils_all[10001:13431])
write_csv(affils_df_3,"./data_raw/affils_df_3.csv")




# author binder -----------------------------------------------------------

data_dir_authors<-"./data_raw/scopus_api/authors"
csv_files_authors_all <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")


cvs_binder_authors <- function(csv_files_authors) {
  
  authors_df <- csv_files_authors %>%
    map_dfr(~ read_csv(.x),
            .id = "source")
  
}

authors_df_1<-cvs_binder_authors(csv_files_authors_all[1:5000])
write_csv(authors_df_1,"./data_raw/authors_df_1.csv")
authors_df_2<-cvs_binder_authors(csv_files_authors_all[5001:10000])
write_csv(authors_df_2,"./data_raw/authors_df_2.csv")
authors_df_3<-cvs_binder_authors(csv_files_authors_all[10001:13431])
write_csv(authors_df_3,"./data_raw/authors_df_3.csv")



# papers binder -----------------------------------------------------------

data_dir_papers<-"./data_raw/scopus_api/papers"
csv_files_papers_all <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")


cvs_binder_papers <- function(csv_files_papers) {
  
  papers_df <- csv_files_papers %>%
    map_dfr(~ read_csv(.x),
            .id = "source")
  
}

papers_df_1<-cvs_binder_papers(csv_files_papers_all[1:5000])
write_csv(papers_df_1,"./data_raw/papers_df_1.csv")
papers_df_2<-cvs_binder_papers(csv_files_papers_all[5001:10000])
write_csv(papers_df_2,"./data_raw/papers_df_2.csv")
papers_df_3<-cvs_binder_papers(csv_files_papers_all[10001:13431])
write_csv(papers_df_3,"./data_raw/papers_df_3.csv")


# bind the subfiles for each category and check for dupes ------------------

data_dir_affils<-"./data_raw/affils"
csv_files_affils <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")
affils_df <- csv_files_affils %>%
  map_dfr(~ read_csv(.x))


data_dir_papers<-"./data_raw/papers"
csv_files_papers <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")
papers_df <- csv_files_papers %>%
  map_dfr(~ read_csv(.x))

data_dir_authors<-"./data_raw/authors"
csv_files_authors <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")
authors_df <- csv_files_authors %>%
  map_dfr(~ read_csv(.x))


# names(affils_df)

names_standardizer <- function(combined_data) {
  names(combined_data)[names(combined_data) == "prism:url"] <- "URL"
  names(combined_data)[names(combined_data) == "dc:identifier"] <- "scopus_article_id"
  names(combined_data)[names(combined_data) == "eid"] <- "eid"
  names(combined_data)[names(combined_data) == "dc:title"] <- "TI"
  names(combined_data)[names(combined_data) == "prism:publicationName"] <- "SO"
  names(combined_data)[names(combined_data) == "prism:issn"] <- "SN"
  names(combined_data)[names(combined_data) == "prism:volume"] <- "VL"
  names(combined_data)[names(combined_data) == "prism:issueIdentifier"] <- "IS"
  names(combined_data)[names(combined_data) == "prism:pageRange"] <- "page_range"
  names(combined_data)[names(combined_data) == "prism:coverDisplayDate"] <- "PY"
  names(combined_data)[names(combined_data) == "prism:doi"] <- "DI"
  names(combined_data)[names(combined_data) == "dc:description"] <- "AB"
  names(combined_data)[names(combined_data) == "citedby-count"] <- "TC"
  names(combined_data)[names(combined_data) == "pubmed-id"] <- "PI"
  names(combined_data)[names(combined_data) == "prism:aggregationType"] <- "PT"
  names(combined_data)[names(combined_data) == "subtypeDescription"] <- "DT"
  names(combined_data)[names(combined_data) == "authkeywords"] <- "DE"
  names(combined_data)[names(combined_data) == "source-id"] <- "scopus_source_id"
  names(combined_data)[names(combined_data) == "fund-no"] <- "funder_id"
  names(combined_data)[names(combined_data) == "openaccess"] <- "OA"
  names(combined_data)[names(combined_data) == "entry_number"] <- "entry_no"
  names(combined_data)[names(combined_data) == "article-number"] <- "article_number"
  names(combined_data)[names(combined_data) == "prism:eIssn"] <- "EI"
  names(combined_data)[names(combined_data) == "fund-sponsor"] <- "FU"
  names(combined_data)[names(combined_data) == "pii"] <- "UT" #elsevier_pub_identifier
  
  # from data_authors
  names(combined_data)[names(combined_data) == "@seq"] <- "author_order"
  names(combined_data)[names(combined_data) == "authid"] <- "SID"
  names(combined_data)[names(combined_data) == "authname"] <- "AF"
  names(combined_data)[names(combined_data) == "surname"] <- "surname"
  names(combined_data)[names(combined_data) == "given-name"] <- "given_name"
  names(combined_data)[names(combined_data) == "initials"] <- "first_middle_initials"
  names(combined_data)[names(combined_data) == "afid.$"] <- "affil_id"
  names(combined_data)[names(combined_data) == "orcid"] <- "OI"
  
  # from affiliations
  names(combined_data)[names(combined_data) == "afid"] <- "affil_id"
  names(combined_data)[names(combined_data) == "affilname"] <- "affiliation"
  names(combined_data)[names(combined_data) == "affiliation-city"] <- "city"
  names(combined_data)[names(combined_data) == "affiliation-country"] <- "country"
  
  
  
  
  # to be deleted
  names(combined_data)[names(combined_data) == "dc:creator"] <- "creator"
  names(combined_data)[names(combined_data) == "prism:coverDate"] <- "cover_date"
  names(combined_data)[names(combined_data) == "subtype"] <- "document_type_abbrev"
  names(combined_data)[names(combined_data) == "author-count.@limit"] <- "author_count_limit"
  names(combined_data)[names(combined_data) == "author-count.@total"] <- "author_count_total"
  names(combined_data)[names(combined_data) == "author-count.$"] <- "author_count"
  names(combined_data)[names(combined_data) == "openaccessFlag"] <- "open_access_tf"
  
  names(combined_data)[names(combined_data) == "freetoread.value.$"] <- "free_to_read"
  names(combined_data)[names(combined_data) == "freetoreadLabel.value.$"] <- "free_to_read_label"
  
  
  names(combined_data)[names(combined_data) == "fund-acr"] <- "funder_acronym"
  names(combined_data)[names(combined_data) == "author-url"] <- "author_url"
  names(combined_data)[names(combined_data) == "affiliation-url"] <- "affil_url"
  
  combined_data<-replace(combined_data, combined_data =="", NA)  
  # 
  combined_data<-combined_data %>%
    mutate(source=str_replace(source, "./data_raw/scopus_api/papers/scopus_", "")) %>%
    mutate(source=str_replace(source, "_papers", "")) %>%
    mutate(source=str_replace(source, "./data_raw/scopus_api/authors/scopus_", "")) %>%
    mutate(source=str_replace(source, "_authors", "")) %>%
    mutate(source=str_replace(source, "./data_raw/scopus_api/affils/scopus_", "")) %>%
     mutate(source=str_replace(source, "_affils", "")) %>%
    
    relocate(entry_no,.before = 1)
  
  return(combined_data)
}  

# ----- affils
  
affils_df<-names_standardizer(affils_df) %>%
  ungroup() %>%
  select(-"@_fa") %>% 
  distinct(affil_id,affiliation,city,country,.keep_all = TRUE) %>% 
  mutate_all(tolower) %>% 
  mutate(country=as.factor(country),
         city=as.factor(city)) %>% 
  mutate(country=
           case_when(
             country == "united states" ~ "usa",
             .default = as.character(country)
             )
         ) %>% 
  remove_empty(c("rows", "cols"))
  
write_rds(affils_df,"./data_clean/affils_df.rds")

# ----- papers

papers_df<-names_standardizer(papers_df) %>% 
  ungroup() %>%
  select(-"@_fa") %>% 
  # group_by(scopus_article_id,SO,TI) %>% 
  # tally() %>% 
  # arrange(desc(n)) %>% 
  distinct(scopus_article_id,SO,TI,.keep_all = TRUE) %>% 
  mutate_all(tolower) %>% 
  remove_empty(which = c("rows", "cols"))
        
write_rds(papers_df,"./data_clean/papers_df.rds")

# ----- authors 

authors_df<-names_standardizer(authors_df) %>%
  ungroup() %>%
  select(-"@_fa",
         -"afid.@_fa") %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  mutate_all(tolower)

write_rds(authors_df,"./data_clean/authors_df.rds")

# 
#   
# # load rds files
# authors_df<-read_rds("./data_clean/authors_df.rds")
# papers_df<-read_rds("./data_clean/papers_df.rds")
# affils_df<-read_rds("./data_clean/affils_df.rds")
# 

# add refID ---------------------------------------------------------------
  
papers_df$refID <- seq.int(nrow(papers_df))  
papers_df<-papers_df %>% relocate(refID,.before=1)


# bring over the refiID from papers to authors ----------------------------
paper_ID_nos<-papers_df %>% select(refID,source,entry_no)

authors_df<-left_join(authors_df,paper_ID_nos) %>% 
  relocate(refID,.before=1)
affils_df<-left_join(affils_df,paper_ID_nos) %>% 
  relocate(refID,.before=1)
   
rm(paper_ID_nos)
  
# cleanup -----------------------------------------------------------------
  
#remove text from scopus id
papers_df$scopus_article_id <- gsub("scopus_id:", "", papers_df$scopus_article_id)
  
# STANDARDIZING PAPER INFO ------------------------------------------------
  
  
# publication yr ----------------------------------------------------------
papers_df$cover_date
  papers_df <-papers_df %>% 
    separate(cover_date, c("PY2","PM","PD"), remove=FALSE,extra="merge")
  
  # %>% 
  #   mutate(PY=substr(filename, nchar(filename) - 7, nchar(filename) - 4))
  # 
  papers_df$PY<-as.numeric(papers_df$PY)
  papers_df$PD<-as.numeric(papers_df$PD)
  papers_df$PM<-as.numeric(papers_df$PM)
  
  
  # page numbers ------------------------------------------------------------
  
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
  
  
  
  # reorder the columns -----------------------------------------------------
  
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
  
  
  

# identify affiliation as federal -----------------------------------------


affil_list<-read_csv("./data_clean/affil_code_name/complete_affil_list.csv") %>% 
    rename(affil_id=scopus_affil_id) %>% select(-source.x,-source.y) %>% 
    mutate(federal=TRUE) %>% 
    select(-agency_full) %>% 
    distinct(affil_id,agency,city,.keep_all=TRUE) 
  affils_df$affil_id<-as.numeric(affils_df$affil_id)
  
affils_df<-left_join(affils_df,affil_list,by="affil_id") %>% 
  replace_na(list(federal=FALSE)) %>% 
  mutate(across(!federal, tolower)) %>% 
  mutate(affiliation.x=gsub("united states ","us ",affiliation.x)) %>% 
  mutate(affiliation.y=gsub("united states ","us ",affiliation.y)) %>% 
  mutate(country.x=gsub("united states ","us ",country.y)) %>% 
  mutate(affiliation.x=gsub("[.]","",affiliation.x)) %>% 
  mutate(affiliation.y=gsub("[.]","",affiliation.y)) %>% 
  mutate(affiliation.x=if_else(federal==TRUE, gsub("u s ","us ",affiliation.x),affiliation.x)) %>%
  mutate(affiliation.y=if_else(federal==TRUE, gsub("u s ","us ",affiliation.y),affiliation.y)) %>% 
  mutate(city.y=if_else(city.y==city.x,NA,city.y)) %>% 
  mutate(affil_match=(affiliation.x==affiliation.y)) %>% 
  mutate(affiliation.y=if_else(affil_match==TRUE,NA,affiliation.y)) %>% 
  mutate(country_match=(country.x==country.y)) %>% 
  mutate(country.y=if_else(country_match==TRUE,NA,country.y)) %>% 
  mutate(country.x=if_else(is.na(country.x),country.y,country.x)) %>% 
  rename(country=country.x,
         city=city.x,
         alt_affiliation_name=affiliation.y) %>% 
  mutate(affil_name_scopus=affiliation.x) %>% 
  select(-affiliation.x,
         -country_match,
         -country.y,
         -city.y,
         -affil_match) 


affils_df <-affils_df %>% 
  group_by(affil_id) %>% 
  slice_head(n=1) 
rm(affil_list)

           
# cava de' tirreni aou s giovanni di dio e ruggiero d'arago
# u s president’s malaria initiative evolve project nigeria

 
  
  # merging the affiliation and author data dfs -----------------------------
  # combined_data_authors<-merge(x = authors_df, y = affils_df, by.x = c("affil_id","refID"), by.y = c("affil_id","refID"), all.x = TRUE)
  # authors_df<-authors_df %>% 
  #   rename(entry_no_authors=entry_no.x,entry_no_affils=entry_no.y,
  #          filename=filename.x)
  # 
  # 
  # 
  # 
  # # TODO: make sure the affiliations all made it over correctly 
  # # clean after merging
  # rm(affils_df)
  # 
  # 
  # authors_df$FN.y<-NULL
  # authors_df$filename.y<-NULL
  # authors_df$file_no.y<-NULL
  # authors_df$file_no.x<-NULL
  # 
  # names(authors_df)[names(authors_df) == "FN.x"] <- "FN"
  # names(authors_df)[names(authors_df) == "filename.x"] <- "filename"
  # 
  # # BE SURE TO LET USERS KNOW THAT SOME WILL COME BACK WITH AFFILS AS NA BECVAUSE OF ISSUES WITH THEW ORTIGIUNAL DATA
  # 
  # 
  
  

names(affils_df)
authors_df<-authors_df %>% 
  mutate(refID=as.character(refID)) %>% 
  mutate(entry_no=as.character(entry_no)) %>% 
  mutate(affil_id=as.character(affil_id)) 
  

fed_bind<-affils_df %>% select(federal,affil_id)
authors_df<- left_join(authors_df,fed_bind) 



  pubs_no_doi<-papers_df %>% 
    filter(is.na(DI)) %>% 
    distinct(source,SO,PY,TI,.keep_all=TRUE)
  
  pubs_no_doi_dupes<-pubs_no_doi %>% 
    group_by(source,SO,PY,TI) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  
  
  
  pubs_with_doi<-papers_df %>% 
    filter(!is.na(DI)) %>% 
    distinct(source,DI,SO,PY,PG,TI,.keep_all=TRUE)
  
  pubs_with_doi_dupes<-pubs_with_doi %>% 
    group_by(source,DI,SO,PY,PG,TI) %>% 
    filter(!is.na(DI)) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  # sum(pubs_with_doi_dupes$n)  
  
  
  papers_df<-bind_rows(pubs_no_doi,pubs_with_doi)
  
  
  rm(pubs_no_doi,
     pubs_with_doi,
     pubs_with_doi_dupes,
     pubs_no_doi_dupes)
  
  
  authors_df<-distinct(authors_df,
                       source,
                       author_order,
                       author_url,
                       SID,
                       AF,
                       surname,
                       given_name,
                       first_middle_initials,
                       affil_id,
                       OI,
                       federal,
                       .keep_all=TRUE)
  
  # affils_df<-affils_df[!duplicated(affils_df), ]
  
  
  
  # standardizing AUTHOR INFO -----------------------------------------------
  
  # AU
  authors_df$AU<-paste(authors_df$surname, authors_df$first_middle_initials, sep=", ")
  authors_df$AU<-gsub("[.]", "",authors_df$AU)
  
  # AF
  authors_df$AF<-paste(authors_df$surname, authors_df$given_name, sep=", ")
  
  
  

  
  # standardize some countries ----------------------------------------------
  levels(as.factor(affils_df$country))
  
  
  affils_df$country[affils_df$country == "united states"] <- "usa"
  affils_df$country[affils_df$country == "virgin islands (u.s.)"] <- "us virgin islands"
  affils_df$country[affils_df$country == "viet nam"] <- "vietnam"
  affils_df$country[affils_df$country == "cote d'ivoire"] <- "ivory coast"
  
  
  
  
  
  # adresss column for affils_df to be able to use authors georef
  names(affils_df)
  authors_df$authorID<-authors_df$SID
  authors_df$state<-NA
  authors_df$postal_code<-NA
   

library(tidyverse)
papers_df %>% summarize(n_distinct(refID))
authors_df %>% summarize(n_distinct(refID))
setdiff((papers_df$refID),(authors_df$refID))
setdiff((authors_df$refID),(papers_df$refID))

# CREATE FILE OF MISSING FROM ONE OR THE OTHER
papers_df$refID<-as.character(papers_df$refID)
authors_df$refID
from_papers<-
  anti_join(papers_df %>% select(refID),
            unique(authors_df %>% select(refID)))

need_to_find_authors<- semi_join(papers_df,from_papers,by="refID")

from_authors<-
  anti_join(unique(authors_df %>% select(refID)),
            papers_df %>% select(refID))

need_to_find_papers<- semi_join(authors_df,from_authors,by="refID")
names(need_to_find_papers)

papers_df<-anti_join(papers_df,from_papers,by="refID")
authors_df<-anti_join(authors_df,from_authors,by="refID")

rm(from_papers,from_authors)

# papers_df %>% 
# separate_wider_delim(source,
#                      delim = "_",
#                      names = c("agency"),
#                      too_many = "drop"
# ) %>% 
#   mutate_all(tolower) %>% 
#   remove_empty(c("rows", "cols")) %>% 
#   mutate(refID=as.numeric(refID))
papers_df$PY<-papers_df$PY2
papers_df$PY<-as.numeric(papers_df$PY)
papers_df$PY2<-NULL

PY_binder<-papers_df %>% select(refID,PY,PM)

authors_df<-left_join(authors_df,PY_binder)
affils_df<-left_join(affils_df,PY_binder)

affil_binder<-affils_df %>% select(affil_id,federal,agency)

authors_df<-left_join(authors_df,affil_binder)
# 2x to make sure all federal have agency





write_rds(papers_df,"./data_clean/papers_df_preusgs.rds")
write_rds(authors_df,"./data_clean/authors_df_preusgs.rds")
write_rds(affils_df,"./data_clean/affils_df_preusgs.rds")




# load and add usgs  ------------------------------------------------------





# KEEP ONLY ARTICLES on USGS LIST

usgs<-read_csv("./usgs_publications.csv") %>% 
  mutate_all(tolower) %>% 
  select("usgs_refID"="Publication ID",
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



# authors from usgs

usgs_authors<-usgs %>% 
  select(usgs_refID,PY,source,agency,agency_2,agency_3,Country,State,City,AF) %>% 
  separate_rows(AF, sep = "; ") %>% 
  mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
  mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
  # mutate(AF = str_remove(AF, fixed(email))) %>% 
  mutate(federal=if_else(!is.na(email),TRUE,FALSE)) %>% 
  group_by(usgs_refID) %>% 
  mutate(author_order=row_number(),
         affil_id=60011347,
         entry_no=cur_group_id()) %>% 
  mutate(AF=gsub(", jr."," jr.",AF)) %>% 
  mutate(Country=gsub("united states","usa",Country)) %>% 
  separate_wider_delim(AF,
                       delim=", ",
                       cols_remove = FALSE,
                       names=c("surname","given_name"),
                       too_many = "debug",
                       too_few = "debug") %>% 
  select(-AF_remainder,-AF_pieces) %>% 
  separate_wider_delim(given_name,
                       delim=" ",
                       cols_remove = FALSE,
                       names=c("init1","init2"),
                       too_many = "debug",
                       too_few = "debug") %>% 
  mutate(given_name_remainder=gsub("jr.","",given_name_remainder)) %>% 
  mutate(given_name_remainder=gsub("[.]","",given_name_remainder)) %>% 
  mutate(given_name_remainder=gsub("iii","",given_name_remainder)) %>% 
  mutate(init1 = str_sub(init1, 1, 1)) %>% 
  mutate(init2 = str_sub(init2, 1, 1)) %>% 
  mutate(first_middle_initials=paste(init1,".",init2,".",sep="")) %>% 
  mutate(first_middle_initials=gsub("NA.NA.","",first_middle_initials)) %>% 
  select(-c(given_name_ok,
            given_name_pieces, 
            given_name_remainder,
            init1,
            init2,
            email,
            AF_ok)
  ) %>% 
  rename(country=Country,
         city=City,
         state=State,
  ) %>% 
  mutate(AU=paste(surname,first_middle_initials, sep=",")) %>% 
  mutate(AU=gsub("[.]","",AU)) %>% 
  mutate(AU=ifelse(AU=="NA,",NA,AU)) %>%
  group_by(AF) %>% 
  mutate(authorID=cur_group_id()) %>% 
  mutate(authorID=paste(authorID,"usgs",sep="")) %>% 
  mutate(AU=gsub("NA","",AU),
         AU=gsub(",",", ",AU),
         AU=gsub("[(]","",AU)) %>% 
  mutate_all(trimws)
usgs_authors$AF<-trimws(usgs_authors$AF)
usgs_authors$AU<-trimws(usgs_authors$AU)




write_rds(usgs_authors, "./data_clean/usgs_authors_clean.rds")
write_rds(usgs, "./data_clean/usgs_papers_clean.rds")




# get a df of papers already in papers_df from usgs
# get the federal usgs authors of those papers (doi, author order) and 
# change the affiliation in papers_df


usgs_DI<-usgs %>% 
  select(DI,usgs_refID) %>% 
  drop_na() 

papers_DI<-papers_df %>% 
  select(DI,refID) %>% 
  drop_na()


usgs_papers_in_papers_df<-semi_join(papers_DI,usgs_DI,by="DI") %>% 
  select(DI)



for_usgs__authors1<-semi_join(usgs,usgs_papers_in_papers_df) %>% select(usgs_refID,DI,PY)
for_usgs__authors2<-semi_join(usgs_authors,for_usgs__authors1,by="usgs_refID")
for_usgs__authors2<-left_join(for_usgs__authors2,for_usgs__authors1) %>% 
  filter(federal==TRUE) %>% 
  select(DI,AF,affil_id,author_order,PY,federal)

refID_to_correct_authors<-semi_join(papers_df,for_usgs__authors2,by="DI") %>% 
  select(refID,DI)

for_usgs__authors2<-left_join(for_usgs__authors2,refID_to_correct_authors) %>% 
  mutate(PY=as.numeric(PY))

authors_df<-left_join(authors_df,for_usgs__authors2,by=c("refID","PY","author_order")) %>% 
  mutate(federal.x=as.character(federal.x)) %>% 
  mutate(federal.x=
        case_when(
             federal.y == "TRUE" ~ "TRUE",
             .default = as.character(federal.x)
           )
  ) %>% 
  mutate(affil_id.x=
           case_when(
             affil_id.y == "60011347" ~ "60011347",
             .default = as.character(affil_id.y)
           )
  ) %>% 
    select(-affil_id.y,
         -federal.y) %>% 
  rename(affil_id=affil_id.x,
         federal=federal.x) %>% 
  mutate(AF.y=if_else(AF.y==AF.x,NA,AF.y)) %>% 
  select(-AF.y) %>% 
  rename(AF=AF.x)


         
# now the ones that aren't in papers_df


usgs_papers_NOT_in_papers_df<-anti_join(usgs_DI,papers_DI,by="DI") %>% 
  select(DI)




add_to_papers_df<-semi_join(usgs,usgs_papers_NOT_in_papers_df) %>% 
  select(-c(Country,
  agency_3,
  State,
  City,
  URL,
  agency,
  agency_2)) %>% 
  mutate(BP=as.numeric(BP),
         EP=as.numeric(EP),
         PY=as.numeric(PY))
  
add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%add_to_papers_df$usgs_refID) %>% 
  mutate(from="usgs") %>% 
  rename(refID=usgs_refID) %>% 
  select(-c(agency_2,agency_3,country,city,from)) %>% 
  mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
  mutate(federal=as.logical(federal),
         PY=as.numeric(PY)) 

# add to authors_df and papers_df -----------------------------------------
authors_df<-authors_df %>% mutate(federal=as.logical(federal))
authors_df<-bind_rows(authors_df,add_to_authors_df)

add_to_papers_df<-add_to_papers_df %>% rename(refID=usgs_refID)
papers_df<-bind_rows(papers_df,add_to_papers_df)






# bind and save -----------------------------------------------------------






# save all ----------------------------------------------------------------




write_rds(papers_df,"./data_clean/papers_df_clean.rds")
write_rds(authors_df,"./data_clean/authors_df_clean.rds")
write_rds(affils_df,"./data_clean/affils_df_clean.rds")



write_rds(need_to_find_authors,"./data_clean/need_to_find_authors.rds")
write_rds(need_to_find_papers,"./data_clean/need_to_find_papers.rds")

