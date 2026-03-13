prep_analysis_datasets_uni <- function(date,PM_max,PY_min,PY_max) {

  library(tidyverse)
library(janitor)
library(data.table)

  
  cat<-"uni"
  

  
  
  if(date=="20250901"){
    scopus_ids_searched<-read_csv("./data_clean/api_uni_affils_searched_2025-09-01.csv")
  }
  
  
  if(date=="20251010"){
    scopus_ids_searched<-read_csv("./data_clean/api_uni_affils_searched_2025-10-18.csv")
  }
  
  
  
  if(date=="20251210"){
    scopus_ids_searched<-read_csv("./data_clean/api_uni_affils_searched_2025-10-18.csv")
  }

  
  
  
  if(date=="20260101"){
    scopus_ids_searched<-read_csv("./data_clean/api_uni_affils_searched.csv")
  }
  
# 


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



# max month to plot -------------------------------------------------------


# load data  --------------------------------------------------------------

# Affiliations
affils_df  <- setDT(read_rds(paste("./data_clean/affils_df_clean_",cat,"_",date,".rds",sep="")))

# unique(affils_df_complete$agency_primary)

# Publications

papers_df  <- setDT(read_rds(paste("./data_clean/papers_df_clean_",cat,"_",date,".rds",sep=""))) %>% 
  filter(PY<=PY_max) %>% 
  filter(PY>=PY_min) %>% 
  filter(if_else(PY==2025, PM<=PM_max,PM<=12)) 

# Authors

authors_df  <- setDT(read_rds(paste("./data_clean/authors_df_clean_",cat,"_",date,".rds",sep="")))


message("data loaded")



papers_cat<-c("article",
              # "book chapter",
              "editorial",
              "letter",
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
  # filter(!TI%in%papers_title) %>%  # to EXCLUDE the titles with the flag words above
  filter(DT%in%papers_cat) # KEEP the pubs in the categories above

  authors_df<-authors_df %>%
    filter(refID%in%papers_df$refID)
  
  
  
  # %>% 
  # mutate(uni=case_when(
  #   # uni == "unc_ch"~"other",
  #   # uni == "ohio_state"~"other",
  #   uni == "mass_general"~"other",
  #   uni == "uscd"~"ucsd",
  #   uni == "minnesota"~"minn",
  #   # uni=="beth israel deaconess medical center"~"harvard",
  #   # uni=="boston children’s hospital"~"harvard",
  #   # uni=="brigham and women’s hospital"~"harvard",
  #   # uni=="cambridge health alliance"~"harvard",
  #   # uni=="dana-farber cancer institute"~"harvard",
  #   # uni=="harvard pilgrim health care institute"~"harvard",
  #   # uni=="hebrew senior life"~"harvard",
  #   # uni=="joslin diabetes center"~"harvard",
  #   # uni=="judge baker children’s center"~"harvard",
  #   # uni=="massachusetts eye and ear | schepens eye research institute"~"harvard",
  #   # uni=="massachusetts general hospital"~"harvard",
  #   # uni=="mclean hospital"~"harvard",
  #   # uni=="mount auburn hospital"~"harvard",
  #   # uni=="spaulding rehabilitation hospital"~"harvard",
  #   # uni=="va boston healthcare system"~"harvard",
  #   is.na(uni) ~ "other",
  #   .default = as.character(uni)
  # )) 

# 
# # number of articles per category BEFORE we remove letters and editorials
# papers_df_complete %>% 
#   group_by(DT) %>% 
#   tally() %>% 
#   mutate(perc=n/sum(n)*100)
# 
# # remove letters and editorials
# papers_df <- papers_df %>% 
#   filter(DT!="editorial") %>% 
#   filter(DT!="letter") 

  
# removes the letters and editorials from authors_df
# authors_df<-authors_df %>% 
#   filter(refID%in%papers_df$refID)

# unique(authors_df_trim$uni)

# this calclulates how many papers have authors from each institution
# papers_by_uni<-authors_df %>% 
#   select(refID,uni,author_order) %>% 
#   group_by(refID) %>% 
#   count(uni) %>% 
#   pivot_wider(names_from = uni, values_from = n) %>% 
# replace(is.na(.), 0)
# 
# 
# # can now count how many papers by university (note - no fractional authorship)
# counts<-papers_by_uni %>% 
#   ungroup() %>% 
#   select(-refID) %>% 
#   summarise(across(everything(), ~ sum(. > 0))) %>% 
#   pivot_longer(everything(),names_to="uni", values_to = "total_papers")
# 
# # write_csv(counts,"./docs/summary_info/total_papers_by_uni.csv")
# write_csv(counts,paste(save_dir,"/","total_papers_by_uni.csv",sep=""))





# START HERE IF REMAKING FIGURES ------------------------------------------
# 
# papers_df<-read_rds("./data_clean/papers_df_analysis_uni.rds")
# authors_df<-read_rds("./data_clean/authors_df_analysis_uni.rds")


# PM_max<-6 # june
# PM_max<-8 # july
# PM_max<-8 # aug



# data summaries ----------------------------------------------------------
  

# removes any with no focal uni authors that might have 
# managed to sneak through 


auth_per_pub <-authors_df %>% 
  group_by(refID) %>% 
  summarize(focal=sum(uni!="other"),
            nonFocal=sum(uni=="other"),
            total=sum(focal+nonFocal)) %>% 
  filter(focal!=0) 

# NOTE - THIS ALLOWS YOU TO CHECK. MMost of these are because the
# person has the focal university as a SECONDAY Affiliation.
# eg doi=10.1093/cid/ciz409  and doi=10.1016/j.jth.2020.100880 
# UW is secondary for some authors

pub_no_focal <-authors_df %>% 
  group_by(refID) %>% 
  summarize(focal=sum(uni!="other"),
            nonFocal=sum(uni=="other"),
            total=sum(focal+nonFocal)) %>% 
  anti_join(auth_per_pub,by="refID")



# trim down datasets to only include primary address = focal unis ---------



papers_df <- papers_df %>%
  # filter(!TI%in%papers_title) %>%  # to EXCLUDE the titles with the flag words above
  filter(refID%in%auth_per_pub$refID) # KEEP the pubs in the categories above

authors_df<-authors_df %>%
  filter(refID%in%papers_df$refID)

affils_df<-affils_df %>% 
  filter(affil_id%in%authors_df$affil_id)



write_rds(papers_df,"./data_clean/for_pub/papers_df_analysis_uni.rds")
write_rds(authors_df,"./data_clean/for_pub/authors_df_analysis_uni.rds")
write_rds(affils_df,"./data_clean/for_pub/affils_df_analysis_uni.rds")



# papers by article category
papers_by_cat_uni<-
  papers_df %>% 
  group_by(DT) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n))

# write_csv(papers_by_cat_uni,"./docs/summary_info/papers_by_cat_uni.csv")
write_csv(papers_by_cat_uni,paste(save_dir,"/","papers_by_cat_uni.csv",sep=""))



# mean and SD of authors per publication (fed, nonFocal, total)
auth_per_pub_means<-auth_per_pub %>% 
  ungroup() %>% 
  drop_na() %>% 
  summarize(
    avg_focal=mean(focal),
    sd_focal=sd(focal),
    avg_nonFocal=mean(nonFocal),
    sd_nonFocal=sd(nonFocal),
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
  mutate(sd=if_else(author_category=="nonFocal",sd_nonFocal,sd)) %>% 
  mutate(sd=if_else(author_category=="focal",sd_focal,sd)) %>% 
  select(-sd_focal,-sd_nonFocal,-sd_Total) 

auth_per_pub_means
# 
# 
# remove any papers with no uni that snuck through -----------------------
# 
# dataDir <- "./data_raw/affiliations_to_search/uni_affils/original_search"
# 
# dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)
# dataFls<-dataFls[ !dataFls == "./data_raw/affiliations_to_search/uni_affils/scopus_info_uni_affils.csv"]
# 
# # Read and tag each file
# dt_list <- lapply(dataFls, function(file) {
#   dt <- fread(file, fill = TRUE)
#   dt[, source_file := basename(file)]  # Add column with filename
#   return(dt)
# })
# 
# 
# # Combine all tagged data tables
# affils_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
# affils_df<-affils_df %>%
# pivot_longer(
#   cols = starts_with("V"),
#   names_to = "col",
#   values_to = "affil_id",
#   values_drop_na = TRUE) %>%
#   separate_wider_delim(source_file,delim = ".", names = c("uni", "csv")) %>%
#   distinct() %>%
#   select(-col,-csv)
# 
# affil_info<-read_csv("./data_raw/affiliations_to_search/uni_affils/scopus_info_uni_affils.csv")
# 
# scopus_id_initial<-left_join(affils_df,affil_info,by="affil_id") %>%
#   select(-document_count,-pub_count) %>%
#   filter(!is.na(affiliation)) %>%
#   distinct(affil_id,.keep_all = TRUE) %>%
#   select(affil_id) %>%
#   distinct() %>%
#   summarize(n=n_distinct(affil_id))

scopus_id_initial<-scopus_ids_searched %>% 
  filter(cat=="original") %>% 
    distinct(affil_id,.keep_all = TRUE) %>%
    select(affil_id) %>%
    distinct() %>%
    summarize(n=n_distinct(affil_id))


# total number of scopus IDs in the follow-up search
scopus_id_followup<-scopus_ids_searched %>% 
  filter(cat=="followup") %>% 
  distinct(affil_id,.keep_all = TRUE) %>%
  select(affil_id) %>%
  distinct() %>%
  summarize(n=n_distinct(affil_id))


affils_df<-full_join(affils_df, affils_df,by="affil_id") %>% 
  mutate(uni=coalesce(uni.x,uni.y)) %>% 
  select(-uni.x,-uni.y) %>% 
  distinct(affil_id,uni,.keep_all = TRUE) 


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
total_focal<-authors_df %>% 
  filter(uni!="other") %>% 
  select(SID) %>% 
  distinct() %>% 
  tally()
total_focal

total_NOTfocal<-authors_df %>% 
  filter(uni=="other") %>% 
  select(SID) %>% 
  distinct() %>% 
  tally() 
total_NOTfocal


first_authors <- authors_df %>%
  filter(uni != "other") %>%
  filter(author_order == 1) 


prop_papers_focal_1st<-nrow(first_authors)/ total_pubs*100

last_authors <- authors_df %>%
  group_by(refID) %>%
  slice_tail() %>%
  filter(author_order != 1) %>%
  filter(uni != "other") 

prop_papers_focal_last<-nrow(last_authors)/ total_pubs*100


all_author_positions <- authors_df %>%
  filter(uni != "other") %>%
  distinct(refID,uni,.keep_all=TRUE) %>% 
  arrange(refID)






# write_csv(auth_per_pub_means,"./docs/summary_info/auth_per_pub_means_uni.csv")
write_csv(auth_per_pub_means,paste(save_dir,"/","auth_per_pub_means_uni.csv",sep=""))


summary_data<-data.frame(value=c("scopus_id_initial",
                                 "scopus_id_followup",
                                 # "no_fed_affils",
                                 "total_pubs",
                                 "total_authors",
                                 "total_unique_authors",
                                 "total_focal",
                                 "total_NOTfocal",
                                 "prop_papers_focal_1st",
                                 "prop_papers_focal_last"),
                         n=c(scopus_id_initial$n,
                             scopus_id_followup$n,
                             # no_focal_affils$n,
                             total_pubs$n,
                             total_authors$n,
                             total_unique_authors$n,
                             total_focal$n,
                             total_NOTfocal$n,
                             prop_papers_focal_1st$n,
                             prop_papers_focal_last$n))

# write_csv(summary_data,"./docs/summary_info/summary_data_uni.csv")
write_csv(summary_data,paste(save_dir,"/","summary_data_uni.csv",sep=""))
summary_data



rm(summary_data, 
   scopus_id_initial,
   scopus_id_followup,
   auth_per_pub_means,
   all_author_positions,
   auth_per_pub,
   no_focal_affils,
   prop_papers_focal_1st,
   prop_papers_focal_last,
   total_pubs,
   total_authors,
   total_unique_authors,
   total_focal,
   total_NOTfocal)

# 
# # total pubs by uni (all positions, no fractional) ------------------------
# 
# 
# 
focal_first_authors <- authors_df %>%
  remove_empty(c("rows","cols")) %>%
  filter(uni != "other") %>%
  # # filter(uni != "unc_ch") %>%
  # # filter(uni != "ohio_state") %>%
  # filter(uni != "mass_general") %>%
  # filter(!is.na(uni)) %>%
  filter(author_order == 1)

unique(focal_first_authors$uni)
# 
papers_with_focaluni_first<-papers_df %>%
  filter(refID%in%focal_first_authors$refID) %>%
  distinct(scopus_article_id,.keep_all=TRUE)
# 
# 
PY_for_authors_df<-papers_with_focaluni_first %>%
  select(refID,PY,PM)
focal_first_authors <- focal_first_authors %>%
  left_join(PY_for_authors_df)
# 
# 
# # 
# # uni_first_authors %>% 
# #   tally()
# 
# papers_with_focaluni_first %>% 
#   group_by(PY) %>% 
#   tally()
# 
# 
all_authors_df_for_focaluni_1st_papers<-authors_df %>%
  filter(refID%in%focal_first_authors$refID) %>%
  left_join(PY_for_authors_df)
# 
rm(PY_for_authors_df)
#   



# set datasets for figures ------------------------------------------------


papers_dataset<-papers_with_focaluni_first

authors_dataset<-focal_first_authors


# publications per year ---------------------------------------------------

# 
# papers_dataset %>% group_by(SO) %>% tally() %>% arrange(desc(n)) %>% slice_head(n=20)
# 
# papers_dataset %>% group_by(PY) %>% tally() 
# 
# focal_first_authors %>% 
#   distinct(refID,.keep_all=TRUE) %>% 
#   group_by(uni) %>% 
#   tally() %>% 
#   arrange(desc(n))
# 
# papers_dataset %>% filter(is.na(DI)) %>% tally()
# 
# papers_dataset %>% filter(!is.na(DI)) %>% tally()

# unique(papers_dataset$DT)
# 
# papers_dataset %>% filter(is.na(DT)) %>% tally()
# 
# papers_dataset %>%
#   group_by(SO,PY,DI,TI) %>% 
#   tally() %>% 
#   filter(n>1) %>% 
#   arrange(desc(n))


pubs_yr <- papers_dataset %>% 
  distinct(refID,.keep_all=TRUE) %>%
  group_by(PY) %>% 
  tally()
pubs_yr


pubs_uni <- focal_first_authors %>% 
  distinct(refID,.keep_all=TRUE) %>%
  group_by(uni) %>% 
  tally() %>% 
  arrange(desc(n))
pubs_uni


# write_csv(pubs_uni,"./docs/summary_info/uni_total_pubs_per_uni_first.csv")
write_csv(pubs_uni,paste(save_dir,"/","uni_total_pubs_per_uni_first.csv",sep=""))



}