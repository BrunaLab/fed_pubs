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
          group_by(scopus_article_id,SO,TI) %>% tally() %>% arrange(desc(n)) %>% 
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



# load and add usgs  ------------------------------------------------------



names(papers_df)

# KEEP ONLY ARTICLES on USGS LIST

usgs<-read_csv("./usgs_publications.csv") %>% 
  mutate_all(tolower) %>% 
  select("refID"="Publication ID",
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


# find the doi on the usgs list that aren't on the main papers_df list

usgs_DI<-as.data.frame(usgs$DI)|>
  rename(DI=`usgs$DI`)

papers_DI<-as.data.frame(papers_df$DI)|>
  rename(DI=`papers_df$DI`)



IN_papersdf<-semi_join(usgs_DI,papers_DI)
NOT_in_papersdf<-anti_join(usgs_DI,papers_DI)


# nrow(usgs_DI)
# 
# 
# names_usgs<-as.data.frame(names(usgs))|>
#   rename(names=`names(usgs)`)
# names_all<-as.data.frame(names(papers_df_clean))|>
#   rename(names=`names(papers_df_clean)`)
# 
# anti_join(names_usgs,names_all)
# anti_join(names_all,names_usgs)
# 


write_rds(usgs, "./data_clean/usgs_papers_clean.rds")

# usgs authors ------------------------------------------------------------



usgs_authors<-usgs %>% 
  select(refID,PY,source,agency,agency_2,agency_3,Country,State,City,AF) %>% 
  separate_rows(AF, sep = "; ") %>% 
  mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
  mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
  # mutate(AF = str_remove(AF, fixed(email))) %>% 
  mutate(federal=if_else(!is.na(email),TRUE,FALSE)) %>% 
  group_by(refID) %>% 
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
  mutate(authorID=paste(authorID,"usgs",sep=""))



names_usgs<-as.data.frame(names(usgs_authors))|>
  rename(names=`names(usgs_authors)`)

names_all<-as.data.frame(names(authors_df_clean))|>
  rename(names=`names(authors_df_clean)`)

anti_join(names_usgs,names_all)
anti_join(names_all,names_usgs)


write_rds(usgs_authors, "./data_clean/usgs_authors_clean.rds")



  
  
  
  
  
  
  
# load rds files
authors_df<-read_rds("./data_clean/authors_df.rds")
papers_df<-read_rds("./data_clean/papers_df.rds")
affils_df<-read_rds("./data_clean/affils_df.rds")


  # add refID ---------------------------------------------------------------
  
papers_df$refID <- seq.int(nrow(papers_df))  
papers_df<-papers_df %>% relocate(refID,.before=1)

  
  # create an entry number
  # 1.1 means article 1 in csv 1, 2.1 is article 1 in csv2, etc. 
  # papers_df$entry_no<-paste(papers_df$file_no,papers_df$entry_no,sep=".")
  # combined_data_authors$entry_no<-paste(combined_data_authors$file_no,combined_data_authors$entry_no,sep=".")
  # combined_data_affils$entry_no<-paste(combined_data_affils$file_no,combined_data_affils$entry_no,sep=".")
  
  # bring over the refiID from papers to authors ----------------------------
paper_ID_nos<-papers_df %>% select(refID,source,entry_no)

authors_df<-left_join(authors_df,paper_ID_nos) %>% 
  relocate(refID,.before=1)
affils_df<-left_join(affils_df,paper_ID_nos) %>% 
  relocate(refID,.before=1)
# 
# paper_ID_nos<-data.frame(entry_no=papers_df$entry_no, refID=papers_df$refID)
#   combined_data_authors<-merge(x = combined_data_authors, y = paper_ID_nos, by.x = c("entry_no"), by.y = c("entry_no"), all.x = TRUE)
#   combined_data_affils<-merge(x = combined_data_affils, y = paper_ID_nos, by.x = c("entry_no"), by.y = c("entry_no"), all.x = TRUE)
#   
#   
  rm(paper_ID_nos)
  
  # cleanup -----------------------------------------------------------------
  
  #remove text from scopus id
  papers_df$scopus_article_id <- gsub("SCOPUS_ID:", "", papers_df$scopus_article_id)
  
  
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
  papers_df$PT[papers_df$PT == "Journal"] <- "J"
  
  
  

# identify affiliation as federal -----------------------------------------


affil_list<-read_csv("./data_clean/affil_code_name/complete_affil_list.csv") %>% 
    rename(affil_id=scopus_affil_id) %>% select(-source.x,-source.y) %>% 
    mutate(federal=TRUE) %>% 
    select(-agency_full) %>% 
    distinct(affil_id,agency,city,.keep_all=TRUE) 
  
  
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
  # These are the duplicates
  # duplicated_papers<-papers_df[duplicated(papers_df), ]
  # duplicated_authors<-authors_df[duplicated(authors_df), ]
  # duplicated_affils<-affils_df[duplicated(affils_df), ]
  # This removes them
  # papers_df<-papers_df[!duplicated(papers_df), ]
  # authors_df<-authors_df[!duplicated(authors_df), ]
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
  
  
  affils_df$country[affils_df$country == "United States"] <- "usa"
  affils_df$country[affils_df$country == "virgin islands (u.s.)"] <- "us virgin islands"
  affils_df$country[affils_df$country == "Viet Nam"] <- "vietnam"
  
  
  
  
  # adresss column for affils_df to be able to use authors georef
  names(affils_df)
  authors_df$authorID<-authors_df$SID
  authors_df$state<-NA
  authors_df$postal_code<-NA
  # authors_df$authorID
  # 
  # affils_df$city
  # affils_df$country
  # # IN PROGRESS
  # # need to combine author names from authors_df to get AF and AU
  # 
  # max_authors<-max(as.numeric(authors_df$author_order), na.rm=TRUE)
  # 
  # # library(tidyverse)
  # # au_af_df<-authors_df %>% 
  # #   select(refID, author_order, AF, AU) %>% 
  # #   mutate(refID=as.numeric(refID)) %>% 
  # #   mutate(author_order=as.numeric(author_order)) %>%
  # #   arrange(refID,author_order) 
  # 
  # # Select the relevant columns
  # au_af_df <- authors_df[, c("refID", "author_order", "AF", "AU")]
  # 
  # # Convert refID and author_order columns to numeric
  # au_af_df$refID <- as.numeric(au_af_df$refID)
  # au_af_df$author_order <- as.numeric(au_af_df$author_order)
  # 
  # # Arrange by refID and author_order
  # au_af_df <- au_af_df[order(au_af_df$refID, au_af_df$author_order), ]
  # 
  # 
  # 
  # au_af_df$AF<-gsub("[.] ",".",au_af_df$AF)
  # 
  # # au_af_df<-au_af_df %>% 
  # #   pivot_wider(values_from=AF:AU, names_from = author_order) %>% 
  # #   unite("AF", starts_with("AF_"),na.rm=TRUE, sep=" ", remove=TRUE) %>% 
  # #   unite("AU", starts_with("AU_"),na.rm=TRUE, sep=" ", remove=TRUE) 
  # # 
  # 
  # # Pivot the data wider: Create columns for each author_order
  # au_af_df_wide <- reshape(au_af_df, 
  #                          timevar = "author_order", 
  #                          idvar = "refID", 
  #                          direction = "wide")
  # 
  # # Unite columns starting with "AF_" into a single "AF" column
  # AF_columns <- grep("^AF.", names(au_af_df_wide))
  # au_af_df_wide$AF <- apply(au_af_df_wide[, AF_columns], 1, function(x) paste(na.omit(x), collapse = " "))
  # au_af_df_wide <- au_af_df_wide[, !names(au_af_df_wide) %in% names(au_af_df_wide)[AF_columns]]
  # 
  # # Unite columns starting with "AU_" into a single "AU" column
  # AU_columns <- grep("^AU.", names(au_af_df_wide))
  # au_af_df_wide$AU <- apply(au_af_df_wide[, AU_columns], 1, function(x) paste(na.omit(x), collapse = " "))
  # au_af_df_wide <- au_af_df_wide[, !names(au_af_df_wide) %in% names(au_af_df_wide)[AU_columns]]
  # 
  # # Result is stored in `au_af_df_wide`
  # au_af_df<-au_af_df_wide
  # 
  # au_af_df$AU<-gsub("[.]","",au_af_df$AU)
  # 
  # # Need to combine SID from all authors to get combined SID (equal to RID) in 
  # 
  # 
  # # Select relevant columns
  # ri_df <- authors_df[, c("refID", "author_order", "AF", "SID")]
  # 
  # # Convert refID and author_order to numeric
  # ri_df$refID <- as.numeric(ri_df$refID)
  # ri_df$author_order <- as.numeric(ri_df$author_order)
  # 
  # # Arrange by refID and author_order
  # ri_df <- ri_df[order(ri_df$refID, ri_df$author_order), ]
  # 
  # # Create the RI column by pasting AF and SID with a "/"
  # ri_df$RI <- paste(ri_df$AF, ri_df$SID, sep = "/")
  # 
  # # Remove AF and SID columns
  # ri_df <- ri_df[, !(names(ri_df) %in% c("AF", "SID"))]
  # 
  # # Pivot wider: Create columns for each author_order
  # ri_df_wide <- reshape(ri_df,
  #                       timevar = "author_order",
  #                       idvar = "refID",
  #                       direction = "wide",
  #                       sep = "_")
  # 
  # # Unite columns starting with "order_" into a single "RI" column
  # order_columns <- grep("^RI_", names(ri_df_wide))
  # ri_df_wide$RI <- apply(ri_df_wide[, order_columns], 1, function(x) paste(na.omit(x), collapse = ";"))
  # ri_df_wide <- ri_df_wide[, !names(ri_df_wide) %in% names(ri_df_wide)[order_columns]]
  # 
  # # Result is stored in `ri_df_wide`
  # ri_df <- ri_df_wide 
  # 
  # 
  # 
  # 
  # # Perform a left join to combine au_af_df and ri_df by "refID"
  # au_af_df <- merge(au_af_df, ri_df, by = "refID", all.x = TRUE)
  # 
  # 
  # str(papers_df)
  # papers_df$RI<-NULL
  # papers_df$AF<-NULL
  # papers_df$AU<-NULL
  # 
  # # papers_df<-left_join(papers_df,au_af_df,by="refID")
  # # Perform a left join
  # # names(papers_df)
  # # names(au_af_df)
  # papers_df <- merge(papers_df,au_af_df, by = "refID", all.x = TRUE)
  # # 
  # # papers_df$AF
  # # papers_df$AU
  # # papers_df$RI
  # # 
  # 
  # rm(au_af_df,
  #    au_af_df_wide,
  #    duplicated_authors,
  #    duplicated_papers,
  #    ri_df,
  #    split_pages,
  #    split_pages_df,
  #    ri_df_wide)
  # 

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
library(janitor)
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


# 
# # usgs --------------------------------------------------------------------
# 
#  names(papers_df)
# 
# usgs<-read_csv("./usgs_publications.csv") %>% 
#   mutate_all(tolower) %>% 
#   select("refID"="Publication ID",
#          "AF"="Author(s)",
#          "Country",
#          "agency_3"="Contributing office(s)",
#          "State",
#          "City",
#          "BP"="First page",
#          "DI"="DOI",
#          "EP"="Last page", 
#          "IS"="Issue",
#          "PT"="Publication type",
#          "PY"="Year Published",
#          "SN"="ISSN (print)",
#          "EI"="ISSN (online)",
#          "SO"="CHORUS Journal Name",
#          "TI"="Title",
#          "VL"="Volume",
#          "URL"="CHORUS URL") %>% 
#   mutate("agency"="interior",
#          source="usgs",
#          "agency_2"="usgs") %>% 
#   filter(PT=="article") %>% 
#   mutate(PT="j")
# 
# 
# names(usgs)
# 
# 
# 
# names_usgs<-as.data.frame(names(usgs))|>
#   rename(names=`names(usgs)`)
# names_all<-as.data.frame(names(papers_df_clean))|>
#   rename(names=`names(papers_df_clean)`)
# 
# anti_join(names_usgs,names_all)
# anti_join(names_all,names_usgs)
# 
# 
# 
# write_rds(usgs, "./data_clean/usgs_papers_clean.rds")
# 
# # usgs authors ------------------------------------------------------------
# 
# 
# 
# usgs_authors<-usgs %>% 
#   select(refID,PY,source,agency,agency_2,agency_3,Country,State,City,AF) %>% 
#   separate_rows(AF, sep = "; ") %>% 
#   mutate(email = str_extract(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
#   mutate(AF = str_remove(AF, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")) %>% 
#   # mutate(AF = str_remove(AF, fixed(email))) %>% 
#   mutate(federal=if_else(!is.na(email),TRUE,FALSE)) %>% 
#   group_by(refID) %>% 
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
#          ) %>% 
#   rename(country=Country,
#         city=City,
#         state=State,
#         ) %>% 
#   mutate(AU=paste(surname,first_middle_initials, sep=",")) %>% 
#   mutate(AU=gsub("[.]","",AU)) %>% 
#   mutate(AU=ifelse(AU=="NA,",NA,AU)) %>%
#            group_by(AF) %>% 
#            mutate(authorID=cur_group_id()) %>% 
#            mutate(authorID=paste(authorID,"usgs",sep=""))
#   
#          
# 
# names_usgs<-as.data.frame(names(usgs_authors))|>
#   rename(names=`names(usgs_authors)`)
# 
# names_all<-as.data.frame(names(authors_df_clean))|>
#   rename(names=`names(authors_df_clean)`)
#   
# anti_join(names_usgs,names_all)
# anti_join(names_all,names_usgs)
# 
# 
# write_rds(usgs_authors, "./data_clean/usgs_authors_clean.rds")
# 
# 


# bind and save -----------------------------------------------------------




papers_usgs  <- papers_usgs %>% 
  mutate(BP=as.numeric(BP),
         EP=as.numeric(EP),
         PY=as.numeric(PY))


papers_df<-bind_rows(papers_df,papers_usgs) %>% 
  mutate_all(tolower) %>% mutate_all(trimws)



authors_usgs <- authors_usgs %>%
  mutate(PY=as.numeric(PY),
         entry_no=as.character(entry_no),
         affil_id=as.character(affil_id),
         author_order=as.numeric(author_order),
         federal=as.logical(federal),
         author_order=as.character(author_order)) 

authors_df<-authors_df %>% 
  mutate(federal=as.logical(federal),
         PY=as.numeric(PY))
authors_df<-bind_rows(authors_df,authors_usgs) %>% 
  mutate_all(tolower) %>% mutate_all(trimws)


# save all ----------------------------------------------------------------




write_rds(papers_df,"./data_clean/papers_df_clean.rds")
write_rds(authors_df,"./data_clean/authors_df_clean.rds")
write_rds(affils_df,"./data_clean/affils_df_clean.rds")



write_rds(need_to_find_authors,"./data_clean/need_to_find_authors.rds")
write_rds(need_to_find_papers,"./data_clean/need_to_find_papers.rds")

