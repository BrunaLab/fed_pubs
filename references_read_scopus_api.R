#' Reads SCOPUS API Output 
##' Thomson Reuters Web of Knowledge/Science and ISI reference export files (both .txt or .ciw format accepted)
#'
#' \code{references_read_scopus_api} This function reads Scopus 
#' reference data files downloaded via API into an R-friendly data format. The resulting dataframe
#' is the argument for the refsplitr function `authors_clean()`.    
#'
#' @param data the location of the file or files to be imported. This can be either the absolute or 
#' relative name of the file (for a single file) or folder (for multiple files stored in the same folder; 
#' used in conjunction with `dir = TRUE``). If left blank it is assumed the location is the working directory.
#' @param dir if FALSE it is assumed a single file is to be imported. 
#' Set to TRUE if importing multiple files (the path to the folder in which files are stored is set with `data=``; 
#' all files in the folder will be imported). Defaults to FALSE. 
#' @param include_all if FALSE only a subset of commonly used fields from references records are imported. 
#' If TRUE then all fields from the reference records are imported. Defaults to FALSE.  
#' The additional data fields included if `include_all=TRUE`: CC, CH, CL, CT, CY, DT, FX, GA, GE, ID, IS, J9, JI, 
#' LA, LT, MC, MI, NR, PA, PI, PN, PS, RID, SU, TA, VR.
#' @export references_read_scopus_api
#' 
#' @examples 
#' ## If a single files is being imported from a folder called "data" located in an RStudio Project: 
#' ## imported_refs<-references_read_scopus_api(data = './data/refs.txt', dir = FALSE, include_all=FALSE)
#' 
#' ## If multiple files are being imported from a folder named "heliconia" nested within a folder
#' ## called "data" located in an RStudio Project: 
#' ## heliconia_refs<-references_read_scopus_api(data = './data/heliconia', dir = TRUE, include_all=FALSE)
#' 
#' ## To load the Scopus API records used in the examples in the documentation  
#' scopus_api_data <- system.file('extdata', 'BITR_test.txt', package = 'refsplitr')
#' scopus_api_example <- references_read_scopus_api(scopus_api_data)
#' 
#' 

# SEE IMPORTANT NOTES ON ORIGINAL 265 and 295 IN TROPICAL SCIENTOMETRIX


library(tidyverse)
library(janitor)
# read & standardize: SCOPUS papers ---------------------------------------

#####################################
data<-"./data_raw/scopus_api"
dir = TRUE
include_all=FALSE
#####################################

# references_read_scopus_api <- function(data = ".", dir = TRUE, include_all=FALSE) {
  ## ", "NOTE: The fields stored in our output table are a combination of the
  ## 	"Thomson Reuters Web of Knowledge" FN format and the "ISI Export
  ## 	Format" both of which are version 1.0:
  
  
  folder_path_papers<-paste(data,"/papers/",sep="")
  folder_path_authors<-paste(data,"/authors/",sep="")
  folder_path_affils<-paste(data,"/affils/",sep="")
  
  
  
  if (dir) {
    
    file_list_papers <- dir(folder_path_papers)
    file_list_authors <- dir(folder_path_authors)
    file_list_affils <- dir(folder_path_affils)
    
  } else {
    stop("ERROR: SCOPUS api files must be organized in three subfolders: `papers`, `authors`, and `affils`")
  }
  
  
  ## 	makes sure all files ars .csv and same number of files
  file_list_papers_count <- length(file_list_papers[ grep(".csv", file_list_papers) ])
  file_list_authors_count <- length(file_list_authors[ grep(".csv", file_list_authors) ])
  file_list_affils_count <- length(file_list_affils[ grep(".csv", file_list_affils) ])
  #  foo1<-as_tibble(file_list_papers)
  #  foo2<-as_tibble(file_list_authors)
  #  foo3<-as_tibble(file_list_affils)
  #  foo1<-foo1 %>% mutate(value=gsub("papers","",value))
  #  foo2<-foo2 %>% mutate(value=gsub("authors","",value))
  #  foo3<-foo3 %>% mutate(value=gsub("affils","",value))
  #  anti_join(foo1,foo2)
  #  anti_join(foo1,foo3)
  #  anti_join(foo2,foo3)
  #
# nih_scopus__2019.csv                  
#  nih_scopus__2020.csv                  
#  nih_scopus__2021.csv                  
#  nih_scopus__2022.csv                  
#  nih_scopus__2023.csv                  
#  nih_scopus__2024.csv                  
#  nih_scopus__2025.csv
#    us-army-health-clinic_scopus__2019.csv
#   us-southern-command_scopus__2019.csv  
#    us-southern-command_scopus__2025.csv 
#   
   if (
    (file_list_papers_count == 0 | file_list_authors_count==0|file_list_affils_count==0)|
    (file_list_papers_count != file_list_authors_count|
     file_list_authors_count!=file_list_affils_count|
     file_list_papers_count != file_list_affils_count)
  ) {
    stop("ERROR:  Either the file path provided is incorrect OR the specified 
    file / directory contains Scopus files that are NOT in .csv format!")
  }
  message("Now processing all references files")
  
  
  
  combine_csv_files <- function(folder_path) {
    
    # List all CSV files in the folder
    csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
    # 
    # 
    # ############################### Clock######################################
    # total <- length(csv_files)
    # pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    # utils::setTxtProgressBar(pb, counter)
    # utils::flush.console()
    # ###########################################################################
    
    
    
    # Read all CSV files into a list of data frames, convert target column to integer, and add a column for the file name
    data_list <- lapply(csv_files, function(file) {
      df <- read.csv(file, stringsAsFactors = FALSE)
      if ("prism.coverDisplayDate" %in% names(df)) {
        df[["prism.coverDisplayDate"]] <- as.character(df[["prism.coverDisplayDate"]])
      }
      if ("X._fa" %in% names(df)) {
        df[["X._fa"]] <- NULL
      }
      if ("afid.._fa" %in% names(df)) {
        df[["afid.._fa"]] <- NULL
      }
      
      
      
      df$filename <- basename(file)
      df$FN<-"scopus_api"
      return(df)
    })
    
    # Find all unique column names across all data frames
    all_columns <- unique(unlist(lapply(data_list, names)))
    
    # Function to add missing columns with NA values
    add_missing_columns <- function(df, all_columns) {
      missing_cols <- setdiff(all_columns, names(df))
      df[missing_cols] <- NA
      return(df)
    }
    
    # Apply the function to each data frame in the list
    data_list <- lapply(data_list, add_missing_columns, all_columns)
    
    
    
    
    # Combine all data frames into one
    # combined_data <- do.call(rbind, data_list)
    
    # Combine all data frames into one and add an index number for each file. 
    # this is useful because each file has articles numbered as 1...n.
    # with this you can create 1.1,2.1,3.1 etc to distinguishe the 1st article
    # in csv 1 from 1st in csv 2 from 1st in csv 3 etc. 
    combined_data <- do.call(rbind, lapply(seq_along(data_list), function(x) {
      data_list[[x]]$file_no <- x
      data_list[[x]]
    }))
    
    return(combined_data)
    
  }
  
  
  combined_data_papers <- combine_csv_files(folder_path_papers) 
  combined_data_authors <- combine_csv_files(folder_path_authors) 
  combined_data_affils <- combine_csv_files(folder_path_affils) 
  
  
  
  names_standardizer <- function(combined_data) {
    names(combined_data)[names(combined_data) == "prism.url"] <- "URL"
    names(combined_data)[names(combined_data) == "dc.identifier"] <- "scopus_article_id"
    names(combined_data)[names(combined_data) == "eid"] <- "eid"
    names(combined_data)[names(combined_data) == "dc.title"] <- "TI"
    names(combined_data)[names(combined_data) == "prism.publicationName"] <- "SO"
    names(combined_data)[names(combined_data) == "prism.issn"] <- "SN"
    names(combined_data)[names(combined_data) == "prism.volume"] <- "VL"
    names(combined_data)[names(combined_data) == "prism.issueIdentifier"] <- "IS"
    names(combined_data)[names(combined_data) == "prism.pageRange"] <- "page_range"
    names(combined_data)[names(combined_data) == "prism.coverDisplayDate"] <- "PY"
    names(combined_data)[names(combined_data) == "prism.doi"] <- "DI"
    names(combined_data)[names(combined_data) == "dc.description"] <- "AB"
    names(combined_data)[names(combined_data) == "citedby.count"] <- "TC"
    names(combined_data)[names(combined_data) == "pubmed.id"] <- "PI"
    names(combined_data)[names(combined_data) == "prism.aggregationType"] <- "PT"
    names(combined_data)[names(combined_data) == "subtypeDescription"] <- "DT"
    names(combined_data)[names(combined_data) == "authkeywords"] <- "DE"
    names(combined_data)[names(combined_data) == "source.id"] <- "scopus_source_id"
    names(combined_data)[names(combined_data) == "fund.no"] <- "funder_id"
    names(combined_data)[names(combined_data) == "openaccess"] <- "OA"
    names(combined_data)[names(combined_data) == "entry_number"] <- "entry_no"
    names(combined_data)[names(combined_data) == "prism.eIssn"] <- "EI"
    names(combined_data)[names(combined_data) == "fund.sponsor"] <- "FU"
    names(combined_data)[names(combined_data) == "pii"] <- "UT" #elsevier_pub_identifier
    
    # from data_authors
    names(combined_data)[names(combined_data) == "X.seq"] <- "author_order"
    names(combined_data)[names(combined_data) == "authid"] <- "SID"
    names(combined_data)[names(combined_data) == "authname"] <- "AF"
    names(combined_data)[names(combined_data) == "surname"] <- "surname"
    names(combined_data)[names(combined_data) == "given.name"] <- "given_name"
    names(combined_data)[names(combined_data) == "initials"] <- "first_middle_initials"
    names(combined_data)[names(combined_data) == "afid.."] <- "affil_id"
    names(combined_data)[names(combined_data) == "orcid"] <- "OI"
    
    # from affiliations
    names(combined_data)[names(combined_data) == "afid"] <- "affil_id"
    names(combined_data)[names(combined_data) == "affilname"] <- "affiliation"
    names(combined_data)[names(combined_data) == "affiliation.city"] <- "city"
    names(combined_data)[names(combined_data) == "affiliation.country"] <- "country"
    
    
    
    
    # to be deleted
    names(combined_data)[names(combined_data) == "dc.creator"] <- "creator"
    names(combined_data)[names(combined_data) == "prism.coverDate"] <- "cover_date"
    names(combined_data)[names(combined_data) == "subtype"] <- "document_type_abbrev"
    names(combined_data)[names(combined_data) == "author.count..limit"] <- "author_count_limit"
    names(combined_data)[names(combined_data) == "author.count..total"] <- "author_count_total"
    names(combined_data)[names(combined_data) == "author.count.."] <- "author_count"
    names(combined_data)[names(combined_data) == "openaccessFlag"] <- "open_access_tf"
    names(combined_data)[names(combined_data) == "freetoread.value.."] <- "free_to_read"
    names(combined_data)[names(combined_data) == "freetoreadLabel.value.."] <- "free_to_read_label"
    names(combined_data)[names(combined_data) == "fund.acr"] <- "funder_acronym"
    names(combined_data)[names(combined_data) == "author.url"] <- "author_url"
    names(combined_data)[names(combined_data) == "affiliation.url"] <- "affil_url"
    
    combined_data<-replace(combined_data, combined_data =="", NA)
    
    # Convert all "" to NA
    # combined_data_papers<-replace(combined_data_papers, combined_data_papers =="", NA)
    # combined_data_authors<-replace(combined_data_authors, combined_data_authors =="", NA)
    # combined_data_affils<-replace(combined_data_affils, combined_data_affils =="", NA)
    
    return(combined_data)
  }  
  
  
  combined_data_papers<-names_standardizer(combined_data_papers)
  combined_data_authors<-names_standardizer(combined_data_authors)
  combined_data_affils<-names_standardizer(combined_data_affils)
  
  ######### DELETE THIS _ FOR CODE DEV ONLY ###########################
  names(combined_data_papers)
  names(combined_data_authors)
  names(combined_data_affils)
  #####################################################################
  

  # add refID ---------------------------------------------------------------
  
  combined_data_papers$refID <- seq.int(nrow(combined_data_papers))  
  
  names(combined_data_papers)
  
  # create an entry number
  # 1.1 means article 1 in csv 1, 2.1 is article 1 in csv2, etc. 
  combined_data_papers$entry_no<-paste(combined_data_papers$file_no,combined_data_papers$entry_no,sep=".")
  combined_data_authors$entry_no<-paste(combined_data_authors$file_no,combined_data_authors$entry_no,sep=".")
  combined_data_affils$entry_no<-paste(combined_data_affils$file_no,combined_data_affils$entry_no,sep=".")
  # combined_data_papers$entry_no<-paste(combined_data_papers$file_no,combined_data_papers$entry_no,sep=".")
  # combined_data_authors$entry_no<-paste(combined_data_authors$file_no,combined_data_authors$entry_no,sep=".")
  # combined_data_affils$entry_no<-paste(combined_data_affils$file_no,combined_data_affils$entry_no,sep=".")
  # bring over the refiID from papers to authors ----------------------------
  paper_ID_nos<-data.frame(entry_no=combined_data_papers$entry_no, refID=combined_data_papers$refID)
  combined_data_authors<-merge(x = combined_data_authors, y = paper_ID_nos, by.x = c("entry_no"), by.y = c("entry_no"), all.x = TRUE)
  combined_data_affils<-merge(x = combined_data_affils, y = paper_ID_nos, by.x = c("entry_no"), by.y = c("entry_no"), all.x = TRUE)
  # combined_data_authors$entry_no<-NULL
  # combined_data_papers$entry_no<-NULL
  # combined_data_affils$entry_no<-NULL
  rm(paper_ID_nos)
  
  # cleanup -----------------------------------------------------------------
  
  #remove text from scopus id
  combined_data_papers$scopus_article_id <- gsub("SCOPUS_ID:", "", combined_data_papers$scopus_article_id)
  
  
  # STANDARDIZING PAPER INFO ------------------------------------------------
  
  
  # publication yr ----------------------------------------------------------
  
  combined_data_papers <-combined_data_papers %>% 
    separate(cover_date, c("PY2","PM","PD"), remove=FALSE,extra="merge") %>% 
    mutate(PY=substr(filename, nchar(filename) - 7, nchar(filename) - 4))
  
  combined_data_papers$PY<-as.numeric(combined_data_papers$PY)
  combined_data_papers$PD<-as.numeric(combined_data_papers$PD)
  combined_data_papers$PM<-as.numeric(combined_data_papers$PM)
  
  # combined_data_papers$PM <- gsub("[0-9]", "", combined_data_papers$PM) #PM
  # combined_data_papers$PM <- trimws(combined_data_papers$PM,which=c("both"))
  # combined_data_papers$PY <- gsub("[a-zA-z]", "", combined_data_papers$PY)
  # combined_data_papers$PY <- trimws(combined_data_papers$PY,which=c("both"))
  # combined_data_papers$PY<-as.numeric(combined_data_papers$PY)
  
  # page numbers ------------------------------------------------------------
  
  # Split the 'page_range' 
  split_pages <- strsplit(combined_data_papers$page_range, "-")
  
  # Convert the list of split pages to data frame
  split_pages_df <- do.call(rbind, lapply(split_pages, function(x) {
    data.frame(BP = x[1], EP = x[2], stringsAsFactors = FALSE)
  }))
  
  combined_data_papers <- cbind(combined_data_papers, split_pages_df)
  combined_data_papers$BP<-as.numeric(combined_data_papers$BP) 
  combined_data_papers$EP<-as.numeric(combined_data_papers$EP) 
  combined_data_papers$PG<-combined_data_papers$EP-combined_data_papers$BP
  
  
  
  # reorder the columns -----------------------------------------------------
  
  combined_data_papers$AF<-NA
  combined_data_papers$AU<-NA
  combined_data_papers$CA<-NA
  combined_data_papers$AF<-NA
  combined_data_papers$CA<-NA
  combined_data_papers$C1<-NA
  combined_data_papers$C3<-NA
  combined_data_papers$CR<-NA
  combined_data_papers$EM<-NA
  combined_data_papers$ID<-NA
  combined_data_papers$JI<-NA
  combined_data_papers$NR<-NA
  combined_data_papers$PD<-NA
  combined_data_papers$PU<-NA
  combined_data_papers$RI<-NA
  combined_data_papers$OI<-NA
  # combined_data_papers$PM<-NA
  combined_data_papers$RP<-NA
  combined_data_papers$SC<-NA
  combined_data_papers$WC<-NA
  combined_data_papers$AR<-NA
  combined_data_papers$Z9<-NA
  combined_data_papers$WE<-NA
  
  combined_data_papers <- combined_data_papers[, c("filename","entry_no","refID","AB", "AF", "AU",
                                                   "CA", "BP", "C1", "C3", "CR", 
                                                   "DE", "DI", "DT", "EM", "EP",
                                                   "FN", "FU", "ID", "IS", "JI",
                                                   "NR", "PD", "PG", "PT", "PU",
                                                   "PY", "PY2","RI", "OI", "PM", "RP",
                                                   "SC", "SN", "EI", "SO", "TC",
                                                   "TI", "UT", "VL", "WC", "Z9",
                                                   "AR", "WE")]
  
  
  
  combined_data_papers$DE<-gsub(" [|] ", "; ",combined_data_papers$DE)
  combined_data_papers$PT[combined_data_papers$PT == "Journal"] <- "J"
  
  
  # merging the affiliation and author data dfs -----------------------------
  combined_data_authors<-merge(x = combined_data_authors, y = combined_data_affils, by.x = c("affil_id","refID"), by.y = c("affil_id","refID"), all.x = TRUE)
  combined_data_authors<-combined_data_authors %>% 
    rename(entry_no_authors=entry_no.x,entry_no_affils=entry_no.y,
           filename=filename.x)
  # TODO: make sure the affiliations all made it over correctly 
  # clean after merging
  rm(combined_data_affils)
  
  # names(combined_data_authors)
  # summary(combined_data_authors$FN.x==combined_data_authors$FN.y)
  # summary(combined_data_authors$filename.x==combined_data_authors$filename.y)
  # summary(combined_data_authors$file_no.x==combined_data_authors$file_no.y)
  
  # combined_data_authors<-combined_data_authors %>% relocate(entry_no.y,.after=entry_no.x) %>% filter(is.na(entry_no.y))
  
  combined_data_authors$FN.y<-NULL
  combined_data_authors$filename.y<-NULL
  combined_data_authors$file_no.y<-NULL
  combined_data_authors$file_no.x<-NULL
  
  names(combined_data_authors)[names(combined_data_authors) == "FN.x"] <- "FN"
  names(combined_data_authors)[names(combined_data_authors) == "filename.x"] <- "filename"
  
  # BE SURE TO LET USERS KNOW THAT SOME WILL COME BACK WITH AFFILS AS NA BECVAUSE OF ISSUES WITH THEW ORTIGIUNAL DATA
  
  
  
  
  
  
  
  
  # check for duplicates ----------------------------------------------------
  
  
  ######### DELETE THIS _ FOR CODE DEV ONLY ###########################
  # summary(duplicated(combined_data_papers))
  # summary(duplicated(combined_data_authors))
  # summary(duplicated(combined_data_affils))
  #####################################################################
  
  
  
  pubs_no_doi<-combined_data_papers %>% 
    filter(is.na(DI)) %>% 
    distinct(filename,SO,PY,TI,.keep_all=TRUE)
  
  pubs_no_doi_dupes<-pubs_no_doi %>% 
    group_by(filename,SO,PY,TI) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  
  
  
  pubs_with_doi<-combined_data_papers %>% 
    filter(!is.na(DI)) %>% 
    distinct(filename,DI,SO,PY,PG,TI,.keep_all=TRUE)
  
  pubs_with_doi_dupes<-pubs_with_doi %>% 
    group_by(filename,DI,SO,PY,PG,TI) %>% 
    filter(!is.na(DI)) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  # sum(pubs_with_doi_dupes$n)  
  
  
  combined_data_papers<-bind_rows(pubs_no_doi,pubs_with_doi)
  
  
  rm(pubs_no_doi,
     pubs_with_doi,
     pubs_with_doi_dupes,
     pubs_no_doi_dupes)
  # These are the duplicates
  duplicated_papers<-combined_data_papers[duplicated(combined_data_papers), ]
  duplicated_authors<-combined_data_authors[duplicated(combined_data_authors), ]
  # duplicated_affils<-combined_data_affils[duplicated(combined_data_affils), ]
  # This removes them
  combined_data_papers<-combined_data_papers[!duplicated(combined_data_papers), ]
  combined_data_authors<-combined_data_authors[!duplicated(combined_data_authors), ]
  # combined_data_affils<-combined_data_affils[!duplicated(combined_data_affils), ]
  
  
  
  # standardizing AUTHOR INFO -----------------------------------------------
  
  # AU
  combined_data_authors$AU<-paste(combined_data_authors$surname, combined_data_authors$first_middle_initials, sep=", ")
  combined_data_authors$AU<-gsub("[.]", "",combined_data_authors$AU)
  
  # AF
  combined_data_authors$AF<-paste(combined_data_authors$surname, combined_data_authors$given_name, sep=", ")
  
  levels(as.factor(combined_data_authors$country))
  
  
  # convert to lower case ---------------------------------------------------
  
  # 
  # combined_data_authors <- data.frame(lapply(combined_data_authors, function(x) tolower(as.character(x))))
  # combined_data_papers <- data.frame(lapply(combined_data_papers, function(x) tolower(as.character(x))))
  
  
  # standardize some countries ----------------------------------------------
  
  levels(as.factor(combined_data_authors$country))
  combined_data_authors$country[combined_data_authors$country == "United States"] <- "USA"
  combined_data_authors$country[combined_data_authors$country == "Viet Nam"] <- "Vietnam"
  
  
  
  
  # adresss column for combined_data_authors to be able to use authors georef
  names(combined_data_authors)
  combined_data_authors$authorID<-combined_data_authors$SID
  combined_data_authors$state<-NA
  combined_data_authors$postal_code<-NA
  # combined_data_authors$authorID
  
  
  # IN PROGRESS
  # need to combine author names from combined_data_authors to get AF and AU
  
  max_authors<-max(as.numeric(combined_data_authors$author_order), na.rm=TRUE)
  
  # library(tidyverse)
  # au_af_df<-combined_data_authors %>% 
  #   select(refID, author_order, AF, AU) %>% 
  #   mutate(refID=as.numeric(refID)) %>% 
  #   mutate(author_order=as.numeric(author_order)) %>%
  #   arrange(refID,author_order) 
  
  # Select the relevant columns
  au_af_df <- combined_data_authors[, c("refID", "author_order", "AF", "AU")]
  
  # Convert refID and author_order columns to numeric
  au_af_df$refID <- as.numeric(au_af_df$refID)
  au_af_df$author_order <- as.numeric(au_af_df$author_order)
  
  # Arrange by refID and author_order
  au_af_df <- au_af_df[order(au_af_df$refID, au_af_df$author_order), ]
  
  
  
  au_af_df$AF<-gsub("[.] ",".",au_af_df$AF)
  
  # au_af_df<-au_af_df %>% 
  #   pivot_wider(values_from=AF:AU, names_from = author_order) %>% 
  #   unite("AF", starts_with("AF_"),na.rm=TRUE, sep=" ", remove=TRUE) %>% 
  #   unite("AU", starts_with("AU_"),na.rm=TRUE, sep=" ", remove=TRUE) 
  # 
  
  # Pivot the data wider: Create columns for each author_order
  au_af_df_wide <- reshape(au_af_df, 
                           timevar = "author_order", 
                           idvar = "refID", 
                           direction = "wide")
  
  # Unite columns starting with "AF_" into a single "AF" column
  AF_columns <- grep("^AF.", names(au_af_df_wide))
  au_af_df_wide$AF <- apply(au_af_df_wide[, AF_columns], 1, function(x) paste(na.omit(x), collapse = " "))
  au_af_df_wide <- au_af_df_wide[, !names(au_af_df_wide) %in% names(au_af_df_wide)[AF_columns]]
  
  # Unite columns starting with "AU_" into a single "AU" column
  AU_columns <- grep("^AU.", names(au_af_df_wide))
  au_af_df_wide$AU <- apply(au_af_df_wide[, AU_columns], 1, function(x) paste(na.omit(x), collapse = " "))
  au_af_df_wide <- au_af_df_wide[, !names(au_af_df_wide) %in% names(au_af_df_wide)[AU_columns]]
  
  # Result is stored in `au_af_df_wide`
  au_af_df<-au_af_df_wide
  
  au_af_df$AU<-gsub("[.]","",au_af_df$AU)
  
  # Need to combine SID from all authors to get combined SID (equal to RID) in 
  
  
  
  # ri_df<-combined_data_authors %>% 
  #   select(refID, author_order, AF, SID) %>% 
  #   mutate(refID=as.numeric(refID)) %>% 
  #   mutate(author_order=as.numeric(author_order)) %>%
  #   arrange(refID,author_order) %>%
  #   mutate(RI=paste(AF,SID,sep="/")) %>% 
  #   select(-AF,-SID) %>% 
  #   pivot_wider(values_from=RI, names_from = author_order,names_prefix="order_") %>% 
  #   unite("RI", starts_with("order"),na.rm=TRUE, sep=";", remove=TRUE) 
  
  
  # Select relevant columns
  ri_df <- combined_data_authors[, c("refID", "author_order", "AF", "SID")]
  
  # Convert refID and author_order to numeric
  ri_df$refID <- as.numeric(ri_df$refID)
  ri_df$author_order <- as.numeric(ri_df$author_order)
  
  # Arrange by refID and author_order
  ri_df <- ri_df[order(ri_df$refID, ri_df$author_order), ]
  
  # Create the RI column by pasting AF and SID with a "/"
  ri_df$RI <- paste(ri_df$AF, ri_df$SID, sep = "/")
  
  # Remove AF and SID columns
  ri_df <- ri_df[, !(names(ri_df) %in% c("AF", "SID"))]
  
  # Pivot wider: Create columns for each author_order
  ri_df_wide <- reshape(ri_df,
                        timevar = "author_order",
                        idvar = "refID",
                        direction = "wide",
                        sep = "_")
  
  # Unite columns starting with "order_" into a single "RI" column
  order_columns <- grep("^RI_", names(ri_df_wide))
  ri_df_wide$RI <- apply(ri_df_wide[, order_columns], 1, function(x) paste(na.omit(x), collapse = ";"))
  ri_df_wide <- ri_df_wide[, !names(ri_df_wide) %in% names(ri_df_wide)[order_columns]]
  
  # Result is stored in `ri_df_wide`
  ri_df <- ri_df_wide 
  
  
  # au_af_df<-left_join(au_af_df,ri_df,by="refID")
  
  # Perform a left join to combine au_af_df and ri_df by "refID"
  au_af_df <- merge(au_af_df, ri_df, by = "refID", all.x = TRUE)
  
  
  str(combined_data_papers)
  combined_data_papers$RI<-NULL
  combined_data_papers$AF<-NULL
  combined_data_papers$AU<-NULL
  
  # combined_data_papers<-left_join(combined_data_papers,au_af_df,by="refID")
  # Perform a left join
  # names(combined_data_papers)
  # names(au_af_df)
  combined_data_papers <- merge(combined_data_papers,au_af_df, by = "refID", all.x = TRUE)
  # 
  # combined_data_papers$AF
  # combined_data_papers$AU
  # combined_data_papers$RI
  # 
  
  rm(au_af_df,
     au_af_df_wide,
     duplicated_authors,
     duplicated_papers,
     ri_df,
     split_pages,
     split_pages_df,
     ri_df_wide)
  

library(tidyverse)
combined_data_papers %>% summarize(n_distinct(refID))
combined_data_authors %>% summarize(n_distinct(refID))
setdiff((combined_data_papers$refID),(combined_data_authors$refID))
setdiff((combined_data_authors$refID),(combined_data_papers$refID))

# CREATE FILE OF MISSING FROM ONE OR THE OTHER
from_papers<-
  anti_join(combined_data_papers %>% select(refID),
            unique(combined_data_authors %>% select(refID)))

need_to_find_authors<- semi_join(combined_data_papers,from_papers,by="refID")

from_authors<-
  anti_join(unique(combined_data_authors %>% select(refID)),
            combined_data_papers %>% select(refID))

need_to_find_papers<- semi_join(combined_data_authors,from_authors,by="refID")
names(need_to_find_papers)

combined_data_papers<-anti_join(combined_data_papers,from_papers,by="refID")
combined_data_authors<-anti_join(combined_data_authors,from_authors,by="refID")

rm(from_papers,from_authors)

combined_data_papers %>% 
separate_wider_delim(filename,
                     delim = "_",
                     names = c("agency"),
                     too_many = "drop"
) %>% 
  mutate_all(tolower) %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(refID=as.numeric(refID))

write_rds(combined_data_papers,"./data_clean/scopus_papers.rds")
write_rds(combined_data_authors,"./data_clean/scopus_authors.rds")

write_rds(need_to_find_authors,"./data_clean/need_to_find_authors.rds")
write_rds(need_to_find_papers,"./data_clean/need_to_find_papers.rds")

  # output<-list(combined_data_papers=combined_data_papers, combined_data_authors=combined_data_authors)
  # write_rds(output,"./data_clean/nih_scopus_api.rds")
  # write_rds(output,"./data_clean/all_scopus_api.rds")
  
  # return and end ----------------------------------------------------------
  
  
#   
#   
#   return(output)
# }

