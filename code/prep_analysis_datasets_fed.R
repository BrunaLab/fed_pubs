prep_analysis_datasets_fed <- function(date,PM_max,PY_max) {

# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(gghighlight)
library(kableExtra)
library(ggrepel)
library(progress)
library(fs)
library(data.table)
library(forcats)





cat<-"fed"

# create folders for output -----------------------------------------------


# setting up the main directory
main_dir1 <- "./docs"

# setting up the sub directory
sub_dir1 <- "summary_info"

# check if sub directory exists 
if (file.exists(sub_dir1)){
  
  # specifying the working directory
  setwd(file.path(main_dir1, sub_dir1))
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir1, sub_dir1))
  
}




# setting up the main directory
main_dir2 <- paste(main_dir1,"/",sub_dir1,sep="")

# setting up the sub directory
sub_dir2 <- paste(cat,date,sep="_")

# check if sub directory exists 
if (file.exists(sub_dir2)){
  
  # specifying the working directory
  setwd(file.path(main_dir2, sub_dir2))
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir2, sub_dir2))
  
}

save_dir<-paste(main_dir2,sub_dir2,sep="/")


message("(sub)directories created")


# select max month to analyze & plot --------------------------------------

# PM_max<-6 # june
# PM_max<-7 # july
# PM_max<-8 # aug


# load data  --------------------------------------------------------------

# Affiliations
affils_df  <- setDT(read_rds(paste("./data_clean/affils_df_clean_",cat,"_",date,".rds",sep="")))

# unique(affils_df_complete$agency_primary)

# Publications
# papers_df  <- setDT(read_rds("./data_clean/papers_df_clean.rds")) 
papers_df  <- setDT(read_rds(paste("./data_clean/papers_df_clean_",cat,"_",date,".rds",sep=""))) %>% 
  filter(PY<=PY_max) %>% 
  filter(if_else(PY==2025, PM<=PM_max,PM<=12)) 
  
# Authors
# authors_df <- setDT(read_rds("./data_clean/authors_df_clean.rds")) 
authors_df  <- setDT(read_rds(paste("./data_clean/authors_df_clean_",cat,"_",date,".rds",sep="")))


message("data loaded")

# chose publication types or titles to remove -----------------------------

# unique(papers_df$DT)
# ARTICLE TYPES - KEEP
# "book chapter"
# "article"
# "review"
# "note"
# "data paper"

# ARTICLE TYPES - REMOVE
# "letter" # excluded below
# "editorial" # excluded below" 

# papers_df %>% filter(DT=="editorial") %>% select(TI)
# authors_df %>% filter(is.na(agency)) %>% group_by(federal) %>% tally()
# 


papers_cat<-c("article",
              "book chapter",
              "data paper",
              "note",
              "review")

papers_title<-c("editorial comment",
                "editorial commentary",
                "a quick glance at selected topics in this issue",
                "reply by authors",
                "conclusion",
                "preface",
                "a word from olaw and usda",
                "introduction",
                "editorial comment",
                "editorial commentary",
                "a quick glance at selected topics in this issue",
                "conclusion",
                "reply by authors",
                "preface",
                "a word from olaw and usda",
                "author reply",
                "a word from olaw",
                "comment",
                "commentary",
                "conclusions",
                "a word from usda and olaw",
                "overview",
                "introduction")
#  
papers_df <- papers_df %>%
   filter(DT%in%papers_cat) %>% # KEEP the categories
   filter(!TI%in%papers_title) # EXCLUDE the titles

authors_df<-authors_df %>%
  filter(refID%in%papers_df$refID)


rm(papers_cat,papers_title)


# all downloads together --------------------------------------------------
# 
# p1<-read_rds("./data_clean/papers_df_clean_fed_20250901.rds")
# p2<-read_rds("./data_clean/papers_df_clean_fed_20251010.rds")
# papers_df<-bind_rows(p1,p2) %>% 
#   distinct(scopus_article_id,.keep_all = TRUE)
# 
# au1<-read_rds("./data_clean/authors_df_clean_fed_20250901.rds")
# au2<-read_rds("./data_clean/authors_df_clean_fed_20251010.rds")
# authors_df<-bind_rows(au1,au2) %>% 
#   distinct(SID,.keep_all = TRUE)
# 
# 
# af1<-read_rds("./data_clean/affils_df_clean_fed_20250901.rds")
# af2<-read_rds("./data_clean/affils_df_clean_fed_20251010.rds")
# affils_df<-bind_rows(af1,af2) %>% 
#   distinct(affil_id,.keep_all = TRUE)

# chose publication types or titles to remove -----------------------------

# remove with these TITLE WORDS
# "editor's"
# "preface"
# "foreward"
#  "- reply"


# can 2x they were removed here: 
# papers_by_agency <- authors_df %>% 
#   select(refID,agency_primary,author_order) %>% 
#   group_by(refID) %>% 
#   count(agency_primary) %>% 
#   pivot_wider(names_from = agency_primary, values_from = n) %>% 
#   replace(is.na(.), 0)

# 
# single_agency_pub_filter<-papers_by_agency %>% 
#   mutate(
#     sum = sum(c_across(where(is.integer)))
#   ) %>% 
#   relocate(sum,.after=1)
# 
# single_agency_counter <- authors_df %>%
#   count(refID, agency_primary) %>%
#   pivot_wider(names_from = agency_primary, values_from = n, values_fill = 0) %>%
#   mutate(sum = rowSums(select(., where(is.integer)))) %>%
#   relocate(sum, .after = refID)
# 
# single_agency_publications<-single_agency_counter %>% 
#   mutate(agency=case_when(
#          sum==hhs~"hhs",
#          sum==dod~"dod",
#          sum==commerce~"commerce",
#          sum==epa~"epa",
#          sum==other~"other",
#          sum==doe~"doe",
#          sum==labor~"labor",
#          sum==nsf~"nsf",
#          sum==va~"va",
#          sum==usda~"usda",
#          sum==interior~"interior",
#          sum==smithsonian~"smithsonian",
#          sum==nasa~"nasa",
#          sum==education~"education",
#          sum==doj~"doj",
#          sum==state~"state",
#          sum==dhs~"dhs",
#          sum==dot~"dot",
#          # sum==`federal reserve system`~"federal reserve system",
#          sum==treasury~"treasury",
#          sum==hud~"hud",
#          .default = NA))%>% 
#   relocate(agency,.after=1) %>% 
#   filter(!is.na(agency)) %>% 
# select(refID,agency)
# 
# # single_agency_publications        
# # papers by agency --------------------------------------------------------
# 
# 
# # NA_only_pubs2<-papers_by_agency2 %>%
# #   ungroup() %>%
# #   rowwise() %>%
# #   mutate(flag = `NA` > 0 && all(c_across(-c(refID, `NA`)) == 0)) %>%
# #   ungroup() %>%
# #   filter(flag==TRUE)
# 
# # can now count how many papers by agency (note - no fractional authorship)
# counts<-single_agency_counter %>% 
#   ungroup() %>% 
#   select(-refID,-sum) %>% 
#   summarise(across(everything(), ~ sum(. > 0))) %>% 
#   pivot_longer(everything(),names_to="agency_primary", values_to = "total_papers") %>% 
#   arrange(desc(total_papers))
# 
# # write_csv(counts,"./docs/summary_info/total_papers_by_agency.csv")
# write_csv(counts,paste(save_dir,"/","total_papers_by_agency.csv",sep=""))


# and also count how many papers by article category
papers_by_cat_agency<-
  papers_df %>% 
  group_by(DT) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n))

# write_csv(papers_by_cat_agency,"./docs/summary_info/papers_by_cat_agency.csv")
write_csv(papers_by_cat_agency,paste(save_dir,"/","papers_by_cat_agency.csv",sep=""))

message("papers by category calculated")

# data summaries ----------------------------------------------------------

# N and % of articles in each category after cleanup
pub_cat_summary<-papers_df %>% 
  group_by(DT) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(n)

# authors (fed, non, total) per publication
# removes any with no fed authors that might have 
# managed to sneak through 
auth_per_pub <-authors_df %>%   
  group_by(refID) %>% 
  summarize(fed=sum(federal==TRUE),
            nonFed=sum(federal==FALSE),
            total=sum(fed+nonFed)) %>% 
  filter(fed!=0) %>% 
  mutate(perc_fed=fed/total*100) %>% 
  arrange(desc(perc_fed)) %>% 
  mutate(author_no_cat = cut(total,
                                  breaks = c(0,5, 10, 15, 20, max(total)),
                                  include.lowest = T,
                                 right = T))

# include.lowest = T and right = F creates bins of the 
# form [40, 50) (40 to < 50)


p <- auth_per_pub %>%
  ggplot( aes(x=perc_fed)) +
  geom_histogram( binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Percent of Authors Federal") +
  theme_bw() +
  theme(
    plot.title = element_text(size=15)
  )+
  facet_grid(rows = vars(author_no_cat))
p
# dfs based on author number  ---------------------------------------------




# summaries ---------------------------------------------------------------



 # mean and SD of authors per publication (fed, nonfed, total)
auth_per_pub_means<-auth_per_pub %>% 
  ungroup() %>% 
  drop_na() %>% 
  summarize(
    avg_Fed=mean(fed),
    sd_Fed=sd(fed),
    avg_NonFed=mean(nonFed),
    sd_NonFed=sd(nonFed),
    avg_Total=mean(total),
    sd_Total=sd(total)
  ) %>%  
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "author_category",
    names_prefix = "avg_",
    values_to = "mean_per_pub",
    values_drop_na = TRUE
  ) %>% 
  mutate(sd=if_else(author_category=="Total",sd_Total,NA)) %>% 
  mutate(sd=if_else(author_category=="NonFed",sd_NonFed,sd)) %>% 
  mutate(sd=if_else(author_category=="Fed",sd_Fed,sd)) %>% 
  select(-sd_Fed,-sd_NonFed,-sd_Total) 




auth_per_pub_means


# remove any papers with no feds that snuck through -----------------------


authors_df<-authors_df %>% 
  filter(refID%in%auth_per_pub$refID)
papers_df<-papers_df %>% 
  filter(refID%in%auth_per_pub$refID)
affils_df<-affils_df %>% 
  filter(refID%in%auth_per_pub$refID)

# total number of scopus IDs in the initial search
scopus_id_1<-read_csv("./data_raw/affiliations_to_search/fed_affils/agencies_redux_clean.csv")
scopus_id_2<-read_csv("./data_raw/affiliations_to_search/fed_affils/agencies_orig_clean.csv")
scopus_id_initial_search<-bind_rows(scopus_id_2,scopus_id_1) %>% select(affil_id) %>% distinct() 
rm(scopus_id_1,scopus_id_2)

scopus_id_initial<- scopus_id_initial_search %>% 
  summarize(n=n_distinct(affil_id))


# scopus_id_initial
# total number of scopus IDs in the follow-up search

if (date=="20250901"){
  
  scopus_id_followup<-read_csv("./data_clean/api_fed_affils_searched_2025-09-01.csv") %>%
    distinct(affil_id) %>% 
    anti_join(scopus_id_initial_search) %>% 
    tally() }
  
if (date=="20251010"){
scopus_id_followup<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") %>%
  group_by(search_cat) %>% 
  tally() %>% 
  filter(search_cat=="returned_affil") %>% 
  select(n)
}



if (date=="20251210"){
  scopus_id_followup<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") %>%
    group_by(search_cat) %>% 
    tally() %>% 
    filter(search_cat=="returned_affil") %>% 
    select(n)
}
# note search_cat==original_affil is how many of the ones origianlly 
# searched actually pinged a paper back socketAccept(it is less than number searched)

# Total number of federal affiliations in the final data set 
# no_fed_affils<-authors_df %>% 
#   filter(federal==TRUE) %>% 
#   summarize(n=n_distinct(affil_id))
# no_fed_affils

no_fed_affils<-affils_df %>%
  filter(federal==TRUE) %>% 
  summarize(n=n_distinct(affil_id))
# Total number of publications in the final data set 
# # internal check to see if same when using different df to calclulate
# total_pubs_au<-authors_df %>% 
#   summarize(n=n_distinct(refID))
# 
# total_pubs_pa<-papers_df %>% 
#   summarize(n=n_distinct(refID))
# total_pubs_au==total_pubs_pa

total_pubs<-papers_df %>% 
  summarize(n=n_distinct(refID))


# Total_authors (fed+non)
total_authors<-authors_df %>% 
  select(SID) %>%
  tally()
# 
# there might be some errors, a few where 
# authors_df[1:1000,] %>% 
#   group_by(SID,agency_primary) %>% 
#   tally(n_distinct(surname)) %>% 
#   filter(n>1)
  

# total unique authors (fed+non)
total_unique_authors<-authors_df %>% 
  select(SID) %>% 
  distinct() %>% 
  tally()
total_unique_authors

# total unique federal authors
total_federals<-authors_df %>% 
  filter(federal==TRUE) %>% 
  select(SID) %>% 
  distinct() %>% 
  tally()
total_federals

total_NOTfederals<-authors_df %>% 
  filter(federal==FALSE) %>% 
  select(SID) %>% 
  distinct() %>% 
  tally() 
total_NOTfederals


first_authors <- authors_df %>%
  filter(federal == TRUE) %>%
  filter(author_order == 1) 


prop_papers_fed_1st<-nrow(first_authors)/ total_pubs*100

last_authors <- authors_df %>%
  group_by(refID) %>%
  slice_tail() %>%
  filter(author_order != 1) %>%
  filter(federal == TRUE) 

prop_papers_fed_last<-nrow(last_authors)/ total_pubs*100


all_author_positions <- authors_df %>%
  filter(federal == TRUE) %>%
  distinct(refID,agency,.keep_all=TRUE) %>% 
  arrange(refID)

# write_csv(auth_per_pub_means,"./docs/summary_info/auth_per_pub_means.csv")
write_csv(auth_per_pub_means,paste(save_dir,"/","auth_per_pub_means.csv",sep=""))


# make df of papers with only fed_authors --------------------------------

all_feds<-auth_per_pub %>% 
  filter(perc_fed==100) 


all_fed_authors<-authors_df %>%
  remove_empty(c("rows","cols")) %>% 
  filter(federal == TRUE) %>%
  distinct()

papers_with_only_feds<-papers_df %>% 
  filter(refID%in%all_feds$refID) %>% 
  distinct(scopus_article_id,.keep_all=TRUE) 


papers_w_allfed_authors<-papers_with_only_feds %>% 
  summarize(n=n_distinct(refID))



PY_for_all_fed_authors_df<-papers_with_only_feds %>% 
  select(refID,PY,PM) 
all_fed_authors <- all_fed_authors %>% 
  left_join(PY_for_all_fed_authors_df)




# write_csv(all_fed_authors,"./data_clean/for_pub/all_fed_authors.csv")




summary_data<-data.frame(value=c("scopus_id_initial",
                                 "scopus_id_followup",
                                 "no_fed_affils",
                                 "total_pubs",
                                 "total_authors",
                                 "total_unique_authors",
                                 "total_federals",
                                 "total_NOTfederals",
                                 "papers_w_allfed_authors",
                                 "prop_papers_fed_1st",
                                 "prop_papers_fed_last"),
                         n=c(scopus_id_initial$n,
                             scopus_id_followup$n,
                             no_fed_affils$n,
                             total_pubs$n,
                             total_authors$n,
                             total_unique_authors$n,
                             total_federals$n,
                             total_NOTfederals$n,
                             papers_w_allfed_authors$n,
                             prop_papers_fed_1st$n,
                             prop_papers_fed_last$n))

# write_csv(summary_data,"./docs/summary_info/summary_data.csv")
write_csv(summary_data,paste(save_dir,"/","summary_data.csv",sep=""))
summary_data



rm(summary_data, 
   scopus_id_initial,
   scopus_id_followup,
   auth_per_pub_means,
   all_author_positions,
   # auth_per_pub,
   no_fed_affils,
   prop_papers_fed_1st,
   prop_papers_fed_last,
   total_pubs,
   total_authors,
   papers_w_allfed_authors,
   total_unique_authors,
   total_federals,
   total_NOTfederals)


message("summaries calculated & saved")





# 
# 
# 
# # pubs only hhs authors ---------------------------------------------------
# 
# 
# hhs_only<-single_agency_publications %>% 
#   filter(agency=="hhs")
# 
# 
# papers_with_all_hhs<-papers_df %>% 
#   filter(refID%in%hhs_only$refID) %>% 
#   distinct(scopus_article_id,.keep_all=TRUE) 
# 
# all_hhs_authors<-authors_df %>%
#   remove_empty(c("rows","cols")) %>% 
#   filter(refID%in%hhs_only$refID) %>% 
#   distinct()
# 
# 
# 
# single_agency_only<-single_agency_publications %>% 
#   filter(agency=="interior")
# 
# 
# 
# papers_with_all_interior<-papers_df %>% 
#   filter(refID%in%single_agency_only$refID) %>% 
#   distinct(scopus_article_id,.keep_all=TRUE) 
# 
# all_interior_authors<-authors_df %>%
#   remove_empty(c("rows","cols")) %>% 
#   filter(refID%in%single_agency_only$refID) %>% 
#   distinct()

# make df of papers with fed 1st author ----------------------------------


first_authors <- authors_df %>%
  remove_empty(c("rows","cols")) %>% 
  filter(federal == TRUE) %>%
  filter(author_order == 1) %>% 
  distinct()

papers_with_fed_first<-papers_df %>% 
  filter(refID%in%first_authors$refID) %>% 
  distinct(scopus_article_id,.keep_all=TRUE) 


message("df of papers with fed 1st authors prepared")


# df last author fed 

# last_authors <- authors_df %>%
#   group_by(refID) %>%
#   slice_tail() %>%
#   filter(author_order != 1) %>%
#   filter(federal == TRUE) 
#  
# papers_with_fed_last<-papers_df %>% 
#   filter(refID%in%last_authors$refID) %>% 
#   distinct(scopus_article_id,.keep_all=TRUE) 

# 
# papers_with_fed_first %>%
#   group_by(PY) %>%
#   tally()
# 
# 
# papers_with_fed_last %>%
#   group_by(PY) %>%
#   tally()
# 
# agencies<-authors_df %>%
#   select(agency,agency_primary) %>%
#   distinct() %>%
#   drop_na() %>%
#   arrange(agency_primary)


all_authors_df_for_fed_1st_papers<-authors_df %>% 
  filter(refID%in%first_authors$refID) 


# 
# all_authors_df_for_fed_last_papers<-authors_df %>%
#   filter(refID%in%last_authors$refID)





PY_for_authors_df<-papers_with_fed_first %>% 
  select(refID,PY,PM) 
first_authors <- first_authors %>% 
  left_join(PY_for_authors_df)
# 
# 
# PY_for_last_authors_df<-papers_with_fed_last %>%
#   select(refID,PY,PM)
# last_authors <- last_authors %>%
#   left_join(PY_for_last_authors_df)



all_authors_df_for_fed_1st_papers<-authors_df %>% 
  filter(refID%in%first_authors$refID) %>% 
  left_join(PY_for_authors_df)

message("df of authors of fed 1st papers prepared")

# 
# 
# all_authors_df_for_fed_last_papers<-authors_df %>%
#   filter(refID%in%last_authors$refID) %>%
#   left_join(PY_for_last_authors_df)

# rm(PY_for_authors_df,PY_for_last_authors_df)
rm(PY_for_authors_df)
# first and last authors
# papers_first_last<-bind_rows(papers_with_fed_first,papers_with_fed_last)
# fed_first_last_authors<-bind_rows(first_authors,last_authors)

# all authors

PY_for_all<-papers_df %>% 
  select(refID,PY,PM) 

all_authors_df <- authors_df %>% 
  left_join(PY_for_all)

rm(PY_for_all)

# pubs per agency ---------------------------------------------------------

# paper_type<-papers_df %>% select(refID,DT,PT)
# first_authors2<-first_authors %>% left_join(paper_type,by="refID")

total_pubs_per_agency_first <- first_authors %>% 
  # mutate(agency=if_else(agency=="us department of the interior", "interior",agency)) %>% 
  # mutate(agency=if_else(agency=="federal reserve system", "frs",agency)) %>% 
  # mutate(agency=if_else(agency=="us department of defense", "dod",agency)) %>% 
  # select(refID,DT,agency,agency_primary) %>% 
  select(refID,agency,agency_primary) %>% 
  distinct() %>% 
  drop_na() %>% 
  # group_by(agency,agency_primary,DT) %>% 
  group_by(agency,agency_primary) %>% 
  summarize(n=n_distinct(refID)) %>% 
  ungroup() %>% 
  mutate(total=sum(n)) %>% 
  mutate(perc=n/total*100) %>% 
  arrange(desc(n)) %>% 
  select(-total)

# 
# write_csv(total_pubs_per_agency_first,"./docs/summary_info/total_pubs_per_agency_first.csv")
write_csv(total_pubs_per_agency_first,paste(save_dir,"/","total_pubs_per_agency_first.csv",sep=""))

message("'total_pubs_per_agency_first' prepared")

# most common affil_id ----------------------------------------------------

# go back and change affils_to_search so that original reduc are as common as original
affils_to_search<-read_csv("./data_clean/api_fed_affils_searched_2025-09-01.csv") %>% 
  select(search,affil_id) %>% 
  mutate(affil_id=as.character(affil_id))

pubs_by_affil<-first_authors %>% 
  group_by(affil_id,agency,agency_primary) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  left_join(affils_to_search,by="affil_id") %>% 
  replace_na(list(search = "final_cleanup"))

pubs_by_affil_all<-
all_authors_df %>% 
  filter(federal==TRUE) %>% 
  group_by(affil_id,agency,agency_primary) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  left_join(affils_to_search,by="affil_id") %>% 
  replace_na(list(search = "final_cleanup"))

# pubs_by_affil_all %>% 
#   group_by(search) %>% 
#   summarize(n=sum(n)) %>% 
#   mutate(perc=n/sum(n)*100) 
# 
# pubs_by_affil %>% 
#   group_by(search) %>% 
#   summarize(n=sum(n)) %>% 
#   mutate(perc=n/sum(n)*100) 

message("saving prepped focal datasets")


# SAVE FOCAL DATASETS -----------------------------------------------------

write_csv(papers_df,"./data_clean/for_pub/papers_df_fed_anywhere.csv")
write_csv(authors_df,"./data_clean/for_pub/authors_df_fed_anywhere.csv")


write_csv(papers_with_fed_first,"./data_clean/for_pub/papers_df_fed_first.csv")
write_csv(first_authors,"./data_clean/for_pub/authors_df_fed_first.csv")

write_csv(papers_with_only_feds,"./data_clean/for_pub/papers_df_only_fed_authors.csv")

}