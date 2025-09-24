

# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(gghighlight)
library(kableExtra)
library(ggrepel)
library(progress)
library(fs)
library(data.table)

# select max month to analyze & plot --------------------------------------

# PM_max<-6 # june
PM_max<-7 # july
# PM_max<-8 # aug

# load data  --------------------------------------------------------------

# Affiliations
affils_df  <- setDT(read_rds("./data_clean/affils_df_clean.rds"))

# unique(affils_df_complete$agency_primary)

# Publications
papers_df  <- setDT(read_rds("./data_clean/papers_df_clean.rds")) 
  
  
# Authors
authors_df <- setDT(read_rds("./data_clean/authors_df_clean.rds")) 


# chose publication types or titles to remove -----------------------------

# remove with these TITLE WORDS
# "editor's"
# "preface"
# "foreward"
#  "- reply"


# can 2x they were removed here: 
papers_by_agency <- authors_df %>% 
  select(refID,agency_primary,author_order) %>% 
  group_by(refID) %>% 
  count(agency_primary) %>% 
  pivot_wider(names_from = agency_primary, values_from = n) %>% 
  replace(is.na(.), 0)

# papers by agency --------------------------------------------------------


# NA_only_pubs2<-papers_by_agency2 %>%
#   ungroup() %>%
#   rowwise() %>%
#   mutate(flag = `NA` > 0 && all(c_across(-c(refID, `NA`)) == 0)) %>%
#   ungroup() %>%
#   filter(flag==TRUE)

# can now count how many papers by university (note - no fractional authorship)
counts<-papers_by_agency %>% 
  ungroup() %>% 
  select(-refID) %>% 
  summarise(across(everything(), ~ sum(. > 0))) %>% 
  pivot_longer(everything(),names_to="agency_primary", values_to = "total_papers") %>% 
  arrange(desc(total_papers))

write_csv(counts,"./docs/summary_info/total_papers_by_agency.csv")


# and also count how many papers by article category
papers_by_cat_agency<-
  papers_df %>% 
  group_by(DT) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n))

write_csv(papers_by_cat_agency,"./docs/summary_info/papers_by_cat_agency.csv")

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
filter(fed!=0) 



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
scopus_id_initial<-bind_rows(scopus_id_2,scopus_id_1) %>% select(affil_id) %>% distinct() %>% summarize(n=n_distinct(affil_id))
rm(scopus_id_1,scopus_id_2)
scopus_id_initial
# total number of scopus IDs in the follow-up search
scopus_id_followup<-read_csv("./data_clean/api_affils_searched_2025-09-01.csv") %>% 
select(affil_id) %>% 
  distinct() %>% 
  summarize(n=n_distinct(affil_id))
scopus_id_followup


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






write_csv(auth_per_pub_means,"./docs/summary_info/auth_per_pub_means.csv")

summary_data<-data.frame(value=c("scopus_id_initial",
                                 "scopus_id_followup",
                                 "no_fed_affils",
                                 "total_pubs",
                                 "total_authors",
                                 "total_unique_authors",
                                 "total_federals",
                                 "total_NOTfederals",
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
                             prop_papers_fed_1st$n,
                             prop_papers_fed_last$n))

write_csv(summary_data,"./docs/summary_info/summary_data.csv")
summary_data



rm(summary_data, 
   scopus_id_initial,
   scopus_id_followup,
   auth_per_pub_means,
   all_author_positions,
   auth_per_pub,
   no_fed_affils,
   prop_papers_fed_1st,
   prop_papers_fed_last,
   total_pubs,
   total_authors,
   total_unique_authors,
   total_federals,
   total_NOTfederals)











# make rds of papers with fed 1st author ----------------------------------


first_authors <- authors_df %>%
  remove_empty(c("rows","cols")) %>% 
  filter(federal == TRUE) %>%
  filter(author_order == 1) %>% 
  distinct()

papers_with_fed_first<-papers_df %>% 
  filter(refID%in%first_authors$refID) %>% 
  distinct(scopus_article_id,.keep_all=TRUE) 


# 
# last_authors <- authors_df %>%
#   group_by(refID) %>%
#   slice_tail() %>%
#   filter(author_order != 1) %>%
#   filter(federal == TRUE) 
# 
# 
# 
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

agencies<-authors_df %>% 
  select(agency,agency_primary) %>% 
  distinct() %>%
  drop_na() %>% 
  arrange(agency_primary) 


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

#  journals info ----------------------------------------------------------




journals_first <- papers_with_fed_first %>%
  select(refID,SO, PY) %>%
  distinct() %>% 
  mutate_all(tolower) %>% 
  drop_na() %>% 
  select(SO) %>% 
  distinct() %>% 
  arrange(SO)
# 
write_csv(journals_first,"./docs/summary_info/journals_first.csv")

jrnls_overall_first <- 
  papers_with_fed_first %>%
  select(refID,SO, PY) %>%
  distinct() %>% 
  mutate_all(tolower) %>% 
  drop_na() %>% 
  group_by(SO) %>%
  tally() %>%
  mutate(total=sum(n)) %>% 
  mutate(perc=n/total*100) %>% 
  arrange(desc(n))%>%
  select(-total)
write_csv(jrnls_overall_first,"./docs/summary_info/jrnls_overall_first.csv")


# 
jrnls_yr_first <- 
  papers_with_fed_first %>%
  select(refID,SO, PY) %>%
  distinct() %>% 
  mutate_all(tolower) %>% 
  drop_na() %>% 
  group_by(SO,PY) %>%
  tally() %>%
  group_by(PY) %>% 
  mutate(total=sum(n)) %>% 
  mutate(perc=n/total*100) %>% 
  select(-total) %>% 
  arrange(desc(n)) %>% 
  mutate(perc=round(perc,3))
  
# 
# 
journals_n_perc_annual_first <- jrnls_yr_first %>% 
  pivot_wider(names_from=PY,
              names_prefix = "yr_",
              values_from = c(n,perc)
  ) %>% 
  select(SO,
         n_yr_2025,
         perc_yr_2025,
         n_yr_2024,
         perc_yr_2024,
         n_yr_2023,
         perc_yr_2023,
         n_yr_2022,
         perc_yr_2022,
         n_yr_2021,
         perc_yr_2021,
         n_yr_2020,
         perc_yr_2020,
         n_yr_2019,
         perc_yr_2019)     
         
         
         
write_csv(journals_n_perc_annual_first,"./docs/summary_info/journals_n_perc_annual_first.csv")
         



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


write_csv(total_pubs_per_agency_first,"./docs/summary_info/total_pubs_per_agency_first.csv")


# choose focal datasets ---------------------------------------------------

# fed first authors 
papers_dataset<-papers_with_fed_first
authors_data_set<-first_authors

# fed authors anywhere on author list
# papers_dataset<-papers_df
# authors_data_set<-all_authors_df

# publications per year ---------------------------------------------------


pubs_yr <- papers_dataset %>% 
  distinct(refID,PY) %>% 
  group_by(PY) %>% 
  tally()
  


source("code/figs/total_pubs_per_year.R")
pubs_yr_fig<-total_pubs_per_year(pubs_yr,2024)

# publications per month --------------------------------------------------

library(forcats)

# month<-data.frame(month_name=month.name,PM=seq(1:12)) %>% 

month<-data.frame(month_name=month.abb,PM=seq(1:12)) %>% 
  mutate(month_name=as.factor(month_name)) 

pubs_mo <-
  papers_dataset %>%
  group_by(PM, PY) %>%
  tally() %>%
  mutate(PM=as.numeric(PM),
         PY=as.numeric(PY)) %>% 
  arrange(PY, PM) %>% 
  ungroup() %>% 
  mutate(month=row_number()) %>% 
  left_join(month) %>% 
  mutate(month_name=reorder(month_name,PM))


pubs_mo %>% filter(PM<(PM_max+1)) %>% arrange(PM,desc(PY))

source("code/figs/pubs_per_month.R")
pubs_mo_fig<-pubs_per_month(pubs_mo,2024)


# pubs per month_cumulative -----------------------------------------------


pubs_mo_cumulative <-
  papers_dataset %>%
  group_by(PM, PY) %>%
  tally() %>%
  mutate(PM=as.numeric(PM),
         PY=as.numeric(PY)) %>%
  arrange(PY, PM) %>%
  ungroup() %>%
  mutate(month=row_number()) %>%
  group_by(PY) %>%
  mutate(cumul_pubs=cumsum(n))

# last number is max month of focal year (ie 2025)


pubs_mo_cumulative %>% filter(PM<(PM_max+1)) %>% arrange(PM,desc(PY))




source("code/figs/pubs_per_month_cumulative.R")
pubs_mo_fig_cumulative<-pubs_per_month_cumulative(pubs_mo,2025,PM_max)



# publications per quarter ------------------------------------------------


source("code/figs/pubs_per_quarter.R")
pubs_per_quarter_fig<-pubs_per_quarter(pubs_mo,2024)



# pubs january to month X -------------------------------------------------

source("code/figs/pubs_jan_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

monthly_pubs_1<-pubs_jan_to_month_x(pubs_mo, PM_max)



# total pubs to month X  --------------------------------------------------


source("code/figs/total_pubs_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

total_pubs_to_month_x_fig<-total_pubs_to_month_x(pubs_mo, PM_max)


# total pubs per agency ---------------------------------------------------


# names(authors_data_set)
total_pubs_per_agency <- authors_data_set %>% 
  filter(federal==TRUE) %>% 
  mutate(agency=if_else(agency=="us department of the interior", "interior",agency)) %>% 
  mutate(agency=if_else(agency=="federal reserve system", "frs",agency)) %>% 
  mutate(agency=if_else(agency=="us department of defense", "dod",agency)) %>% 
  select(refID,agency_primary) %>% 
  distinct() %>% 
  drop_na() %>% 
  group_by(agency_primary) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(desc(n))

threshold<-4000

agencies_over_thresh<-total_pubs_per_agency %>% 
  filter(n>=threshold) %>% 
  mutate(agency_primary=toupper(agency_primary)) 

agencies_under_thresh<-total_pubs_per_agency %>% 
  filter(n<threshold) %>% 
  mutate(agency_primary=toupper(agency_primary)) 

# N and % change - agency level -------------------------------------------------------

agency_subset<-agencies_over_thresh$agency_primary


agency_n_decline_first <-
  authors_data_set %>%
  filter(agency_primary %in% tolower(agency_subset)) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM<(PM_max+1)) %>%
  group_by(agency_primary, PY) %>%
  tally() %>%
  group_by(agency_primary) %>%
  mutate(decline_n = (n - lag(n))) %>%
  mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
  mutate(author_position="first")




agency_n_decline<-agency_n_decline_first %>% 
  mutate(PY=as.numeric(PY))


agency_n_decline_sum<- agency_n_decline %>%
  # filter(author_position=="any") %>%
  select(agency_primary,PY,n) %>%
  arrange(PY) %>% 
  group_by(PY) %>%
  summarize(n=sum(n)) %>%
  mutate(n_diff=n-lag(n)) %>%
  mutate(perc_previous_yr=n_diff/lag(n)*100)
# 
# ggsave("./images/agency_n_decline_sum.png", width = 4, height = 6, units = "in")


# bar chart of decline for top agencies -----------------------------------
source("code/figs/agency_n_decline_bar.R")
agency_n_decline_bar_fig <- agency_n_decline_bar(agency_n_decline_sum, PY_max) 


# per agency comparison to previous year ----------------------------------

compare_agency_2425 <-
agency_n_decline %>%
drop_na() %>%
filter(PY > 2023)

# source("code/figs/agency_n_decline_2.R")
# agency_n_decline_2_fig <- agency_n_decline_2(agency_n_decline)

source("code/figs/pubs_per_month_cumulative_agency.R")
pubs_per_month_cumulative_agency<-pubs_per_month_cumulative_agency(papers_dataset,authors_data_set,2025,PM_max)
  


# non_feds<-affils_df_complete %>% 
#   filter(federal==FALSE) %>% 
#   distinct(affil_id,.keep_all=TRUE) %>% 
#   filter(country=="usa"|is.na(country))
#   


# table for ms ------------------------------------------------------------


agency_n_decline_first <-
  authors_data_set %>%
  # filter(agency_primary %in% tolower(agency_subset)) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM<(PM_max+1)) %>%
  group_by(agency_primary, PY) %>%
  tally() %>%
  group_by(agency_primary) %>%
  mutate(decline_n = (n - lag(n))) %>%
  mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
  mutate(author_position="first") 
write_csv(agency_n_decline_first,"./docs/summary_info/agency_n_decline_first.csv")


total_pubs_per_agency_first<-read_csv("./docs/summary_info/total_pubs_per_agency_first.csv") %>% 
  mutate(agency_primary=if_else(agency=='federal reserve system',"federal reserve system",agency_primary)) %>% 
  group_by(agency_primary) %>%
  mutate(total=sum(n)) %>%
  select(agency_primary,total) %>% 
  group_by(agency_primary) %>%
  slice_head(n=1) %>% 
  ungroup() %>% 
  mutate(perc_first=total/sum(total)*100) %>% 
  arrange(desc(total)) %>% 
  mutate(perc_first=round(perc_first,2))

perc_data<-read_csv("./docs/summary_info/agency_n_decline_first.csv") %>% 
  filter(PY==2025) %>% select(agency_primary,n,perc_decline=perc_previous) %>% 
  rename(Unit=agency_primary,
         n24=n) 
  



agency_total<-authors_df %>% 
  filter(federal==TRUE) %>% 
  group_by(agency_primary) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  drop_na() %>% 
  mutate(agency_primary= case_when(
    agency_primary=="OTHER"~"Misc.",
    .default = as.character(agency_primary))) %>% 
  mutate(agency_primary=if_else(nchar(agency_primary)<5,str_to_upper(agency_primary),str_to_title(agency_primary))) %>%
  mutate(agency_primary=gsub(" THE "," the ",agency_primary)) %>% 
  mutate(agency_primary=gsub(" OF "," of ",agency_primary)) %>% 
  mutate(agency_primary=gsub(" THE "," the ",agency_primary)) %>% 
  mutate(agency_primary=gsub("Us ","US ",agency_primary)) %>% 
  mutate(agency_primary=gsub(" AND "," and ",agency_primary)) %>% 
  rename(Unit=agency_primary) %>% 
  mutate(perc=n/sum(n)*100) 


agency_table<-  total_pubs_per_agency_first %>%
  rename(Unit=agency_primary) %>% 
  left_join(perc_data,by="Unit") %>% 
  mutate(Unit=if_else(nchar(Unit)<5,str_to_upper(Unit),str_to_title(Unit))) %>%
  left_join(agency_total) %>% 
  relocate(perc,.after=1) %>% 
  relocate(n,.after=1) %>% 

  # filter(agency_primary!="other") %>%
  # filter(agency_primary!="state") %>%
  # mutate(total=comma(total)) %>% 
  # mutate(perc=as.character(perc)) %>% 
  # mutate(total=as.character(total)) %>% 
  # mutate(n=as.character(n)) %>% 
  mutate(perc_decline=round(perc_decline,2)) %>%
  mutate(perc=round(perc,2)) %>%
  mutate(perc_first=round(perc_first,2)) 
  
  write_csv(agency_table,"./docs/summary_info/agency_table.csv") 

other_affils<-authors_data_set %>% filter(federal==TRUE) %>%
  filter(agency_primary=="other") %>% 
  select(agency) %>% 
  distinct() %>% 
  arrange(agency) %>% 
  mutate(agency=if_else(nchar(agency)<6,str_to_upper(agency),str_to_title(agency))) 
  
write_csv(other_affils,"./docs/summary_info/other_affils.csv") 
  
