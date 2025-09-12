
library(tidyverse)
library(janitor)
library(data.table)




# max month to plot -------------------------------------------------------

# PM_max<-6 # june
# PM_max<-7 # july
PM_max<-8 # aug

PY_max<-2025
# load data  --------------------------------------------------------------


papers_df_complete  <- setDT(read_rds("./data_clean/papers_df_uni_clean.rds")) %>% 
  mutate(PM=
           case_when(
             is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
             .default = as.numeric(PM)
           )
  ) %>% 
  mutate(PT=if_else(PT=="j","journal",PT))

# papers_df %>% filter(is.na(PM))

authors_df_complete <- setDT(read_rds("./data_clean/authors_df_uni_clean.rds")) %>% 
  mutate(uni=case_when(
    # uni == "unc_ch"~"other",
    # uni == "ohio_state"~"other",
    uni == "mass_general"~"harvard",
    is.na(uni) ~ "other",
    .default = as.character(uni)
  )) 


# chose any publication types or titles to remove -------------------------
# TITLE WORDS
# editor's
# preface
# foreward
#  - reply

# ARTICLE TYPE
# unique(papers_df$DT)
# "book chapter"
# "article"
# "letter"
# "review"
# "note"
# "data paper"
# "editorial" 

# papers_df %>% filter(DT=="editorial") %>% select(TI)


# make rds of papers with fed 1st author ----------------------------------



# make rds of papers with fed 1st author ----------------------------------



# papers_df %>% filter(is.na(PM))
# 
# authors_df_complete <- setDT(read_rds("./data_clean/authors_df_uni_clean.rds")) 




# chose any publication types or titles to remove -------------------------
# TITLE WORDS
# editor's
# preface
# foreward
#  - reply

# ARTICLE TYPE
# unique(papers_df$DT)
# "book chapter"
# "article"
# "letter"
# "review"
# "note"
# "data paper"
# "editorial" 

# papers_df %>% filter(DT=="editorial") %>% select(TI)

# authors_df %>% filter(is.na(agency)) %>% group_by(federal) %>% tally()

papers_df_complete %>% 
  group_by(DT) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100)

papers_df <- papers_df_complete %>% 
  filter(DT!="editorial") %>% 
  filter(DT!="letter") 

authors_df<-authors_df_complete %>% 
  filter(refID%in%papers_df$refID)



unique(authors_df$uni)

focal_first_authors <- authors_df %>%
  remove_empty(c("rows","cols")) %>% 
  filter(uni != "other") %>%
  # # filter(uni != "unc_ch") %>%  
  # # filter(uni != "ohio_state") %>%
  # filter(uni != "mass_general") %>%
  # filter(!is.na(uni)) %>%
  filter(author_order == 1) 
  
unique(focal_first_authors$uni)

papers_with_focaluni_first<-papers_df %>% 
  filter(refID%in%focal_first_authors$refID) %>% 
  distinct(scopus_article_id,.keep_all=TRUE) 


PY_for_authors_df<-papers_with_focaluni_first %>% 
  select(refID,PY,PM) 
focal_first_authors <- focal_first_authors %>% 
  left_join(PY_for_authors_df)


# 
# uni_first_authors %>% 
#   tally()

papers_with_focaluni_first %>% 
  group_by(PY) %>% 
  tally()


all_authors_df_for_focaluni_1st_papers<-authors_df %>% 
  filter(refID%in%focal_first_authors$refID) %>% 
  left_join(PY_for_authors_df)

rm(PY_for_authors_df)
  



# set datasets for figures ------------------------------------------------


papers_dataset<-papers_with_focaluni_first

authors_data_set<-focal_first_authors

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


write_csv(pubs_uni,"./docs/summary_info/uni_total_pubs_per_uni_first.csv")



source("code/figs_uni/total_pubs_per_year.R")
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

source("code/figs_uni/pubs_per_month.R")
pubs_mo_fig<-pubs_per_month(pubs_mo,2025)


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
  mutate(cumul_pubs=cumsum(n)) %>% 
  left_join(month) %>% 
  mutate(month_name=reorder(month_name,PM))


# last number is max month of focal year (ie 2025)


pubs_mo_cumulative %>% filter(PM<(PM_max+1)) %>% arrange(PM,desc(PY))

source("code/figs_uni/pubs_per_month_cumulative.R")
pubs_mo_fig_cumulative_all_uni<-pubs_per_month_cumulative(pubs_mo,PY_max,PM_max)




source("code/figs_uni/pubs_per_month_cumulative_by_uni.R")
pubs_mo_fig_cumulative_by_uni<-pubs_per_month_cumulative_by_uni(papers_dataset,authors_data_set,2025,PM_max)


# publications per quarter ------------------------------------------------


source("code/figs_uni/pubs_per_quarter.R")
pubs_per_quarter_fig<-pubs_per_quarter(pubs_mo,2024)



# pubs january to month X -------------------------------------------------

source("code/figs_uni/pubs_jan_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

monthly_pubs_1<-pubs_jan_to_month_x(pubs_mo, PM_max)



# total pubs to month X  --------------------------------------------------


source("code/figs_uni/total_pubs_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

total_pubs_to_month_x_fig<-total_pubs_to_month_x(pubs_mo, PM_max)


# total pubs per uni ---------------------------------------------------


authors_data_set %>% filter(author_order==1) %>%  group_by(uni) %>% tally() %>% arrange(desc(n))



names(authors_data_set)
total_pubs_per_uni <- authors_data_set %>% 
  select(refID,uni) %>% 
  distinct() %>% 
  drop_na() %>% 
  group_by(uni) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(desc(n))


# uni change fig -------------------------------------------------------



uni_n_decline_first <-
  authors_data_set %>%
  # filter(uni %in% uni_subset) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM<(PM_max+1)) %>%
  group_by(uni, PY) %>%
  tally() %>%
  group_by(uni) %>%
  mutate(decline_n = (n - lag(n))) %>%
  mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
  mutate(author_position="first")
# 
# uni_n_decline_last <-
#   last_authors %>%
#   filter(uni %in% uni_subset) %>%
#   # mutate(PM=if_else(PY==2025,5,PM)) %>%
#   filter(PM<PM_max+1)) %>%
#   group_by(uni, PY) %>%
#   tally() %>%
#   group_by(uni) %>%
#   mutate(decline_n = (n - lag(n))) %>%
#   mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
#   mutate(author_position="last")

# 
# uni_n_decline_any <-
#   all_author_positions %>%
#   filter(uni %in% uni_subset) %>%
#   # mutate(PM=if_else(PY==2025,5,PM)) %>%
#   filter(PM<PM_max+1)) %>%
#   group_by(uni, PY) %>%
#   tally() %>%
#   group_by(uni) %>%
#   mutate(decline_n = (n - lag(n))) %>%
#   mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
#   mutate(author_position="any")

# uni_n_decline<-bind_rows(uni_n_decline_first,uni_n_decline_last,uni_n_decline_any) %>% mutate(PY=as.numeric(PY))
uni_n_decline<-uni_n_decline_first %>% mutate(PY=as.numeric(PY))


uni_n_decline_sum<- uni_n_decline %>%
  # filter(author_position=="any") %>%
  select(uni,PY,n) %>%
  arrange(PY) %>% 
  group_by(PY) %>%
  summarize(n=sum(n)) %>%
  mutate(n_diff=n-lag(n)) %>%
  mutate(perc_previous_yr=n_diff/lag(n)*100)
# 
# ggsave("./docs/images/uni_n_decline_sum.png", width = 4, height = 6, units = "in")



# bar chart of decline for top unis -----------------------------------
source("code/figs/uni_n_decline_bar.R")
uni_n_decline_bar_fig <- uni_n_decline_bar(uni_n_decline_sum, PY_max) 



# per uni comparison to previous year ----------------------------------

  
# uni_n_decline %>%
#   # filter(author_position=="any") %>%
#   filter(PY>2022) %>%
#   drop_na() %>%
#   ggplot(aes(x = PY, y = n, group = uni, color = uni)) +
#   # ggplot(aes(x = PY, y = perc_previous, group = uni, color = uni)) +
#   # ggplot(aes(x=PY,y=n, group=uni, color=uni)) +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   facet_wrap(vars(uni), scales = "free")
# +
# scale_y_continuous(expand = c(0, 0), breaks = c(2019, 2025))


compare_uni_2425 <-
uni_n_decline %>%
drop_na() %>%
filter(PY > 2023)


source("code/figs/uni_n_decline_2.R")
uni_n_decline_2_fig <- uni_n_decline_2(uni_n_decline)



#  journals info ----------------------------------------------------------




journals_first <- papers_with_focaluni_first %>%
  select(refID,SO, PY) %>%
  distinct() %>% 
  mutate_all(tolower) %>% 
  drop_na() %>% 
  select(SO) %>% 
  distinct() %>% 
  arrange(SO)
# 
write_csv(journals_first,"./docs/summary_info/uni_journals_first.csv")

jrnls_overall_first <- 
  papers_with_focaluni_first %>%
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
write_csv(jrnls_overall_first,"./docs/summary_info/uni_jrnls_overall_first.csv")


# 
jrnls_yr_first <- 
  papers_with_focaluni_first %>%
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



write_csv(journals_n_perc_annual_first,"./docs/summary_info/uni_journals_n_perc_annual_first.csv")
