
library(tidyverse)
library(janitor)
# read & standardize: SCOPUS papers ---------------------------------------

#####################################
data<-"./data_raw/scopus_api/unis_files"
dir = TRUE
include_all=FALSE
#####################################


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

file_list_papers_count
file_list_authors_count
file_list_affils_count


paper_files<-as_tibble(file_list_papers)
author_files<-as_tibble(file_list_authors)
affil_files<-as_tibble(file_list_affils)
affil_files %>% group_by(value) %>% tally() %>% arrange(desc(n))
 paper_files<-paper_files %>% 
   mutate(value=gsub("scopus_affil_","",value)) %>% 
   mutate(value=gsub("_papers","",value)) %>% 
   mutate(paper=TRUE)
 author_files<-author_files %>% 
   mutate(value=gsub("scopus_affil_","",value)) %>% 
   mutate(value=gsub("_authors","",value)) %>% 
   mutate(author=TRUE)
 affil_files<-affil_files %>% 
   mutate(value=gsub("scopus_affil_","",value)) %>% 
   mutate(value=gsub("_affils_","",value)) %>% 
   mutate(value=gsub("_affils","",value)) %>% 
   mutate(affil=TRUE)
all_files<-full_join(paper_files,author_files,by="value") 
all_files<-full_join(all_files,affil_files,by="value")
 
 missing_files <-all_files %>% 
   filter(is.na(paper)|is.na(author)|is.na(affil))
 missing_files