library(tidyverse)
library(readr)



# read & standardize: SCOPUS papers ---------------------------------------

scopus_papers <- list.files(path='./bibliometrics/data_raw/scopus/papers',
                            full.names = TRUE)

scopus_papers2 <- data.frame(filename=scopus_papers,
                              PY = scopus_papers, 
                              # SO = scopus_papers, 
                              csv_id = as.character(1:length(scopus_papers))) 
scopus_papers2 <- scopus_papers2 %>% # next line is id for joining
  mutate(PY=str_sub(PY, start=-8L, end=-5L)) %>% 
  # mutate(SO=str_sub(SO, start=-21L, end=-17L)) %>% 
  # mutate(SO = case_when(
  #   SO ==  "/bitr" ~ "bitr",
  #   SO ==  "ology" ~ "ecology",
  #   SO ==  "/evol" ~ "evol",
  #   SO ==  "s/jae" ~ "jae",
  #   # SO ==  "s/jecol" ~ "jecol",
  #   SO ==  "s/jte" ~ "jte",
  #   # SO ==  "rs/amna" ~ "amnat",
  #   TRUE ~ as.character(SO))) %>% 
  mutate(filename=str_sub(filename, start=26L)) 

scopus_papers <- scopus_papers %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
  bind_rows(.id = "csv_id") %>% # bind all tables into one object, and give id for each
  # left_join(scopus_authors2)
  # lapply(read_csv,col_types = cols(.default = "c")) %>% 
  # bind_rows(.filename = scopus_papers) %>% 
  rename_all(~str_replace_all(.,"prism","")) %>% 
  rename_all(~str_replace_all(.,"dc","")) %>% 
  rename_all(~str_replace_all(.,"\\:","")) %>% 
  mutate(PY=str_sub(coverDate, 1, 4)) %>% 
  rename("SO"="publicationName",
        "AB"="description",
        "DI"="doi",
         "DT"="subtype",
        "VL"="volume",
         "article_type_long"="subtypeDescription",
         "author_count"='author-count.@total',
        "IS"='issueIdentifier',
        "SN"="issn",
        "EI"="eIssn",
        "DE"="authkeywords",
        "PM"="pubmed-id",
        "TC"="citedby-count",
        "TI"="title",
        # "UT"="url",
        "fund_acr"="fund-acr",
        "fund_no"="fund-no",
        "fund_sp"="fund-sponsor",
        "UT"="identifier",
        ) %>% 
  mutate(SO=tolower(SO)) %>% 
  mutate(SO = case_when(
    SO ==  "biotropica" ~ "bitr",
    SO ==  "evolution" ~ "evol",
    SO ==  "journal of applied ecology" ~ "jae",
    SO ==  "journal of ecology" ~ "jecol",
    SO ==  "j. ecol." ~ "jecol",
    SO ==  "american naturalist" ~ "amnat",
    SO ==  "the american naturalist" ~ "amnat",
    SO ==  "journal of tropical ecology" ~ "jte",
    SO ==  "journal of animal ecology" ~ "jae",
    SO ==  "the journal of animal ecology" ~ "jae",
    TRUE ~ as.character(SO))) %>% 
  distinct() %>% 
  filter(if_any(everything(), is.na)) %>% 
  filter(!is.na(url)) %>% 
  mutate(pub_number = row_number()) %>% 
  separate(pageRange,c("BP","EP"),remove=FALSE) %>% 
  mutate(FU=paste(fund_acr,fund_sp,fund_no, sep="-")) %>% 
  mutate(refID=paste("scopus",pub_number,sep="_")) %>% 
  select(-"@_fa",
         -"coverDisplayDate",
         -"aggregationType",
         -"author-count.@limit",
         -"openaccess",
         -"freetoread.value.$",
         -"freetoreadLabel.value.$",
         -"pii",
         -'author-count.$',
         -"coverDate",
         # -"error",
         -"eid",
         -"url",
         -"pageRange",
         -article_type_long,
  ) 
rm(scopus_papers2)
scopus_papers
# names(scopus_papers)
head(scopus_papers)
# head(scopus_papers)
# unique(scopus_papers$journal)
unique(scopus_papers$SO)



# read & standardize: SCOPUS authors --------------------------------------

scopus_authors <- list.files(path='./bibliometrics/data_raw/scopus/authors',
                        full.names = TRUE)

scopus_authors2 <- data.frame(PY = scopus_authors, 
                         SO = scopus_authors, 
                         csv_id = as.character(1:length(scopus_authors))) 
scopus_authors2 <- scopus_authors2 %>% # next line is id for joining
  mutate(PY=str_sub(PY, start=-8L, end=-5L)) %>% 
  mutate(SO=str_sub(SO, start=-24L, end=-18L)) %>% 
  mutate(SO = case_when(
    SO ==  "rs/bitr" ~ "bitr",
    SO ==  "rs/evol" ~ "evol",
    SO ==  "ors/jae" ~ "jae",  
    SO ==  "s/jecol" ~ "jecol",  
    SO ==  "ors/jte" ~ "jte",
    SO ==  "s/amnat" ~ "amnat",
    TRUE ~ as.character(SO)))


scopus_authors <- scopus_authors %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
  bind_rows(.id = "csv_id") %>% # bind all tables into one object, and give id for each
  mutate(author_key = dplyr::row_number()) %>% 
  left_join(scopus_authors2) %>% # join month column created earlier
  select(-"@_fa",
         -"afid.@_fa",
         -"csv_id") %>% 
  rename("author_order"="@seq",
         "afid"="afid.$")
rm(scopus_authors2)



# scopus_authors %>% group_by(author_key) %>% 
#   tally() %>% filter(n>1)
# 
# 
# scopus_authors %>% group_by(author_key) %>% 
#   tally() %>% filter(n>1)

# PY
# SO
# entry_number



# authors wide ------------------------------------------------------------


scopus_papers_id<-scopus_papers %>% select(entry_number,refID,PY,SO) 
 
glimpse(scopus_papers_id)
glimpse(scopus_authors)

scopus_authors_refID<-scopus_authors %>% left_join(scopus_papers_id)


authors<-scopus_authors_refID %>% 
  select(SO,PY,refID,authid, author_order,authname, surname, initials) %>% 
  distinct() %>% 
  mutate(authname=toupper(authname)) %>% 
  mutate(surname=toupper(surname)) %>% 
  mutate(initials=toupper(initials)) %>% 
  mutate(initials = str_replace_all(initials, "\\.", "")) %>% 
  mutate(authname = str_replace_all(authname, "\\.", "")) %>% 
  mutate(name = case_when(
    author_order ==  1 ~ authname,
    TRUE ~ paste(initials, surname, sep=" ")))



authors_wide<-authors %>% 
  select(-c(authname,surname,initials,SO,PY,authid)) %>% 
  pivot_wider(names_from = author_order, 
                        values_from = name,
                        names_prefix = "AU_") %>%
  unite("AU", AU_1:AU_37, remove = TRUE,na.rm=TRUE, sep=", ") %>% 
  mutate(AF=AU)


scopus_papers<-scopus_papers %>% left_join(authors_wide)%>% 
  relocate(c(AU,AF), .after = AB)


scopus_papers<-scopus_papers %>%  mutate(across(everything(), as.character))
allrefs<-allrefs %>%  mutate(across(everything(), as.character))






# read & standardize: SCOPUS affiliations ---------------------------------

scopus_affils <- list.files(path='./bibliometrics/data_raw/scopus/affils',
                            full.names = TRUE) %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% 
  bind_rows %>% 
  select(-"@_fa") %>% 
  mutate(affilname =str_replace_all(affilname, "\\,", "")) %>% 
  select(-'affiliation-url',-"entry_number") %>% 
  distinct() %>%
  mutate(C1=paste(affilname,`affiliation-city`,`affiliation-country`,sep=", ")) %>% 
  mutate(C1=paste("[", C1,"]",sep="")) %>% 
  relocate(C1, .after = afid)
  
authors_slim<-scopus_authors %>% 
  left_join(scopus_affils) %>% 
  select(-`author-url`,
         -authname,
         -`given-name`,
         -affilname, 
         -`affiliation-city`,
         -`affiliation-country`) %>% 
  mutate(C1_name = paste(surname, initials,sep= ", ")) %>% 
  mutate(C1 = paste(C1_name, C1 ,sep= " ")) %>% 
  rename("author_count"="author_order") %>% 
  select(-C1_name)
  



authors_slim2 <- authors_slim %>% 
  select(SO,PY,entry_number,author_count, C1) %>% 
  distinct() %>% 
  pivot_wider(names_from = author_count, 
              values_from = C1,
              names_prefix = "C") %>% 
  unite("C1", C1:C37, remove = TRUE,na.rm=TRUE, sep="./") 

joined_scopus<- scopus_papers %>% 
  left_join(authors_slim2) %>% 
  relocate(C1, .after = AF) 
  
  
  


  

# putting it all together -------------------------------------------------


joined_scopus3 <-joined_scopus %>% filter(author_count==3) %>% 
  separate(AU,letters[seq( from = 1, to = 6 )],sep=",",remove=TRUE) %>% 
  mutate(AU=paste(a,b,c,sep = " ")) %>% 
  select(-a,-b,-c,-d,-e,-f) %>% 
  relocate(AU, .before = AF) 

joined_scopus_not3 <-joined_scopus %>% filter(author_count!=3) 

joined_scopus_1 <-joined_scopus %>% filter(author_count==1) %>% filter(!is.na(AF)) 
joined_scopus_1 <- allrefs %>% bind_rows(joined_scopus_1) 





















all_authors1<-authors_clean(joined_scopus_1)
all_authors1<-authors_clean(allrefs)


joined_scopus1<-joined_scopus %>% 
  # filter(author_count==1) %>% 
  filter(!is.na(AF)) %>% 
  select_if(names(.) %in% c(names(allrefs))) %>% 
  mutate(EM=NA) %>% 
  mutate(OI=NA) %>% 
  mutate(RI=NA) %>% 
  mutate(RP=NA) %>% 
  mutate(PU=NA) %>% 
  mutate(PT="J") %>% 
  mutate(ref=NA)

joined_scopus1[] <- lapply(joined_scopus1, gsub, pattern="\\;", replacement="")

joined_scopus1 <- joined_scopus1 %>% mutate(across(, ~as.character(.)))
allrefs <- allrefs %>% mutate(across(, ~as.character(.)))
joined_scopus1<-bind_rows(allrefs,joined_scopus1)

allrefs<-authors_clean(joined_scopus1)
write_csv(allrefs,"./bibliometrics/data_clean/joinedscopus1_authors.csv")

joined_scopus_1 <-joined_scopus %>% filter(author_count==1) %>% filter(!is.na(AF))
joined_scopus_2 <-joined_scopus %>% filter(author_count==2) %>% filter(!is.na(AF))
joined_scopus_3 <-joined_scopus %>% filter(author_count==3) %>% filter(!is.na(AF))
joined_scopus_4 <-joined_scopus %>% filter(author_count==4) %>% filter(!is.na(AF))
joined_scopus_5 <-joined_scopus %>% filter(author_count==5) %>% filter(!is.na(AF))
joined_scopus_6 <-joined_scopus %>% filter(author_count>5) %>% filter(!is.na(AF))

joined_scopus<-bind_rows(joined_scopus3,joined_scopus_not3)

joined_scopus<-joined_scopus %>% 
  mutate(AF=str_replace_all(AF, "\\.", "")) %>% 
  mutate(AU=str_replace_all(AU, "\\.", "")) %>% 
  mutate(AF=str_replace_all(AF, "\\,", "")) %>% 
  mutate(AU=str_replace_all(AU, "\\,", "")) 

# 
# foo<-joined_scopus %>% filter(author_count==5) %>% 
#   separate(AU,letters[seq( from = 1, to = 6 )],sep=",")
# 
# 
# foo<-joined_scopus %>% filter(author_count==3) %>% 
#   separate(AU,letters[seq( from = 1, to = 6 )],sep=",")


bigtest<-allrefs %>% bind_rows(joined_scopus) 


missingAU<-bigtest %>% 
  filter(is.na(AF))

# %>% 
#   filter(refID!=scopus_19713)

bigtest_clean<-anti_join(bigtest,missingAU)




# foo<-bigtest_clean %>% filter(author_count==3) %>% 
#   separate(AU,letters[seq( from = 1, to = 6 )],sep=",",remove=TRUE)
# 



all_authors<-authors_clean(bigtest_clean)
