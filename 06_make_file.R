
library(tidyverse)
library(janitor)
library(gghighlight)
library(kableExtra)
library(ggrepel)
# library(knitr)
library(progress)
library(fs)
library(data.table)


papers_df_complete  <- setDT(read_rds("./data_clean/papers_df_clean.rds")) %>% 
  mutate(PM=
           case_when(
             is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
             .default = as.numeric(PM)
           )
  ) %>% 
  mutate(PT=if_else(PT=="j","journal",PT))
  
  
# make rds of papers with fed 1st author ----------------------------------

  

# papers_df %>% filter(is.na(PM))

authors_df_complete <- setDT(readRDS("./data_clean/authors_df_clean.rds")) 




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

# summary calculations ----------------------------------------------------

auth_per_pub<-authors_df %>% 
  group_by(refID) %>% 
  summarize(fed=sum(federal==TRUE),
            nonFed=sum(federal==FALSE),
            total=sum(fed+nonFed))

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

# number of scopus IDs searched 

scopus_id_1<-read_csv("./data_raw/agencies/agencies_redux_clean.csv")
scopus_id_2<-read_csv("./data_raw/agencies/agencies_orig_clean.csv")
scopus_id_total<-bind_rows(scopus_id_2,scopus_id_1) %>% select(affil_id) %>% distinct() %>% summarize(n=n_distinct(affil_id))
rm(scopus_id_1,scopus_id_2)
# 
# no_fed_affils_srch<-authors_df %>% select(source) %>% mutate(source=gsub("affil_","",source)) %>% mutate(source=gsub(".csv","",source)) %>% mutate(source=gsub("_2019","",source)) %>% 
# mutate(source=gsub("_2020","",source)) %>% 
# mutate(source=gsub("_2021","",source)) %>% 
# mutate(source=gsub("_2022","",source)) %>% 
# mutate(source=gsub("_2023","",source)) %>% 
# mutate(source=gsub("_2024","",source)) %>% 
# mutate(source=gsub("_2025","",source)) %>% 
# distinct() %>% 
# summarize(n=n_distinct(source))

# Number of federal affiliations returned

no_fed_affils<-authors_df %>% filter(federal==TRUE) %>% summarize(n=n_distinct(affil_id))


# %>% 
#   rename(SD=sd,
#          `Mean per Paper`=mean_per_pub,
#          `Author Category`=author_category)

# 
# auth_per_pub_means %>%
# kable(digits = 2,
#   format = "latex",
#   align = "lcc",
#   caption = "Avg No of Federal and NonFederal Authors Per Publication",
#   escape = FALSE,
#   row.names = FALSE,
#   booktabs = T,
#   linesep = ""
# ) %>%
#   kable_styling(
#     bootstrap_options = c("hover"),
#     # full_width = F,
#     latex_options = c("scale_down","hold_position"),
#     font_size = 12,
#     position = "center"
#   )





# total_pubs<-papers_df %>% 
#   summarize(n=n_distinct(refID))


total_pubs<-authors_df %>% 
  summarize(n=n_distinct(refID))

# Total_authors (fed+non)

total_authors<-authors_df %>% 
  select(SID) %>%
  tally()

total_unique_authors<-authors_df %>% 
  select(SID) %>% 
  distinct() %>% 
  tally()


total_federals<-authors_df %>% 
  filter(federal==TRUE) %>% 
  select(SID) %>% 
  distinct() %>% 
  tally()

total_NOTfederals<-authors_df %>% 
  filter(federal==FALSE) %>% 
  select(SID) %>% 
  distinct() %>% 
  tally() 


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
summary_data<-data.frame(value=c("scopus_id_total",
                                 "no_fed_affils",
                                 "total_pubs",
                                 "total_authors",
                                 "total_unique_authors",
                                 "total_federals",
                                 "total_NOTfederals",
                                 "prop_papers_fed_1st",
                                 "prop_papers_fed_last"),
                         n=c(scopus_id_total$n,
                             no_fed_affils$n,
                             total_pubs$n,
                             total_authors$n,
                             total_unique_authors$n,
                             total_federals$n,
                             total_NOTfederals$n,
                             prop_papers_fed_1st$n,
                             prop_papers_fed_last$n))

write_csv(summary_data,"./docs/summary_info/summary_data.csv")

rm(summary_data, 
   auth_per_pub_means,
   all_author_positions,
   auth_per_pub,
   scopus_id_total,
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



last_authors <- authors_df %>%
  group_by(refID) %>%
  slice_tail() %>%
  filter(author_order != 1) %>%
  filter(federal == TRUE) 




papers_with_fed_last<-papers_df %>% 
  filter(refID%in%last_authors$refID) %>% 
  distinct(scopus_article_id,.keep_all=TRUE) 




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

all_authors_df_for_fed_last_papers<-authors_df %>% 
  filter(refID%in%last_authors$refID) 





PY_for_authors_df<-papers_with_fed_first %>% 
  select(refID,PY,PM) 
first_authors <- first_authors %>% 
  left_join(PY_for_authors_df)


PY_for_last_authors_df<-papers_with_fed_last %>% 
  select(refID,PY,PM) 
last_authors <- last_authors %>% 
  left_join(PY_for_last_authors_df)



all_authors_df_for_fed_1st_papers<-authors_df %>% 
  filter(refID%in%first_authors$refID) %>% 
  left_join(PY_for_authors_df)



all_authors_df_for_fed_last_papers<-authors_df %>% 
  filter(refID%in%last_authors$refID) %>% 
  left_join(PY_for_last_authors_df)

rm(PY_for_authors_df,PY_for_last_authors_df)

# first and last authors
papers_first_last<-bind_rows(papers_with_fed_first,papers_with_fed_last)
fed_first_last_authors<-bind_rows(first_authors,last_authors)

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





# affils_df %>% 
# filter(federal==TRUE) %>% 
# group_by(agency,PY) %>% 
# tally() %>% 
# ungroup() %>% 
# group_by(agency) %>% 
# tally() %>% 
# arrange(n)


# set focal datasets ------------------------------------------------------

papers_dataset<-papers_with_fed_first
authors_data_set<-first_authors

# first and last
# 
# papers_dataset<-papers_first_last
# authors_data_set<-fed_first_last_authors

# any author position
# papers_dataset<-papers_df
# authors_data_set<-authors_df


# publications per year ---------------------------------------------------

# 
# 
# papers_dataset$SO
# papers_dataset$PY



papers_dataset %>% filter(is.na(DI)) %>% tally()

papers_dataset %>% filter(!is.na(DI)) %>% tally()

unique(papers_dataset$DT)

papers_dataset %>% filter(is.na(DT)) %>% tally()

papers_dataset %>%
  group_by(SO,PY,DI,TI) %>% 
  tally() %>% 
  filter(n>1) %>% 
  arrange(desc(n))


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


pubs_mo %>% filter(PM<4) %>% arrange(PM,desc(PY))

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


pubs_mo_cumulative %>% filter(PM<4) %>% arrange(PM,desc(PY))

source("code/figs/pubs_per_month_cumulative.R")
pubs_mo_fig_cumulative<-pubs_per_month_cumulative(pubs_mo,2025,6)



# publications per quarter ------------------------------------------------


source("code/figs/pubs_per_quarter.R")
pubs_per_quarter_fig<-pubs_per_quarter(pubs_mo,2024)



# pubs january to month X -------------------------------------------------

source("code/figs/pubs_jan_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

monthly_pubs_1<-pubs_jan_to_month_x(pubs_mo, 6)



# total pubs to month X  --------------------------------------------------


source("code/figs/total_pubs_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

total_pubs_to_month_x_fig<-total_pubs_to_month_x(pubs_mo, 6)


# total pubs per agency ---------------------------------------------------


authors_data_set %>% filter(author_order==1) %>%  group_by(agency) %>% tally() %>% arrange(desc(n))



names(authors_data_set)
total_pubs_per_agency <- authors_data_set %>% 
  filter(federal==TRUE) %>% 
  mutate(agency=if_else(agency=="us department of the interior", "interior",agency)) %>% 
  mutate(agency=if_else(agency=="federal reserve system", "frs",agency)) %>% 
  mutate(agency=if_else(agency=="us department of defense", "dod",agency)) %>% 
  select(refID,agency) %>% 
  distinct() %>% 
  drop_na() %>% 
  group_by(agency) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(desc(n))

agencies_past_20<-total_pubs_per_agency %>% 
  select(agency) %>% slice(21:nrow(total_pubs_per_agency)) %>% 
  mutate(agency=toupper(agency)) %>%  
  mutate(agency=if_else((agency=="STATE"|
                           agency=="EDUCATION"|
                           agency=="CONGRESS"|
                           agency=="TREASURY"|
                           agency=="LABOR"|
                           agency=="OTHER"),
                        str_to_title(agency),
                        agency)
  )

# agencies_past_20[1,]<-paste("Other agencies: ",agencies_past_20[1,],sep="")
# agencies_past_20[5,]<-paste(agencies_past_20[5,],"\n",sep="")
# agencies_past_20[10,]<-paste(agencies_past_20[10,],"\n",sep="")
# agencies_past_20[15,]<-paste(agencies_past_20[15,],"\n",sep="")
# agencies_past_20[20,]<-paste(agencies_past_20[20,],"\n",sep="")

agencies_past_20<-agencies_past_20 %>% mutate_all(tolower)
agencies_over_20_1<-paste(agencies_past_20$agency[1:10],collapse=",") 
agencies_over_20_2<-paste(agencies_past_20$agency[11:20],collapse=",") 
agencies_over_20_3<-paste(agencies_past_20$agency[21:30],collapse=",") 
agencies_over_20_4<-paste(agencies_past_20$agency[31:40],collapse=",") 
agencies_over_20_5<-paste(agencies_past_20$agency[41:50],collapse=",") 
# agencies_over_20_6<-paste(agencies_past_20$agency[51:nrow(agencies_past_20)],collapse=",") 

agencies_over_20<-paste(
  agencies_over_20_1,
  agencies_over_20_2,
  agencies_over_20_3,
  agencies_over_20_4,
  agencies_over_20_5
)



# agency change fig -------------------------------------------------------





agency_subset_over <- total_pubs_per_agency %>%
  # filter(n > 10000) %>%
  filter(n > 5000) %>%
  select(agency,n) %>%
  arrange(desc(n))

agency_subset_under <- total_pubs_per_agency %>%
  # filter(n < 10000) %>%
  filter(n < 5000) %>%
  select(agency,n) %>%
  arrange(desc(n))

agency_subset<-agency_subset_over$agency
# 
# agency_subset<-agency_subset_less10K$agency


agency_n_decline_first <-
  authors_data_set %>%
  filter(agency %in% agency_subset) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM<7) %>%
  group_by(agency, PY) %>%
  tally() %>%
  group_by(agency) %>%
  mutate(decline_n = (n - lag(n))) %>%
  mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
  mutate(author_position="first")
# 
# agency_n_decline_last <-
#   last_authors %>%
#   filter(agency %in% agency_subset) %>%
#   # mutate(PM=if_else(PY==2025,5,PM)) %>%
#   filter(PM<7) %>%
#   group_by(agency, PY) %>%
#   tally() %>%
#   group_by(agency) %>%
#   mutate(decline_n = (n - lag(n))) %>%
#   mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
#   mutate(author_position="last")

# 
# agency_n_decline_any <-
#   all_author_positions %>%
#   filter(agency %in% agency_subset) %>%
#   # mutate(PM=if_else(PY==2025,5,PM)) %>%
#   filter(PM<7) %>%
#   group_by(agency, PY) %>%
#   tally() %>%
#   group_by(agency) %>%
#   mutate(decline_n = (n - lag(n))) %>%
#   mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
#   mutate(author_position="any")

# agency_n_decline<-bind_rows(agency_n_decline_first,agency_n_decline_last,agency_n_decline_any) %>% mutate(PY=as.numeric(PY))
agency_n_decline<-agency_n_decline_first %>% mutate(PY=as.numeric(PY))


agency_n_decline_sum<- agency_n_decline %>%
  # filter(author_position=="any") %>%
  select(agency,PY,n) %>%
  arrange(PY) %>% 
  group_by(PY) %>%
  summarize(n=sum(n)) %>%
  mutate(n_diff=n-lag(n)) %>%
  mutate(perc_previous_yr=n_diff/lag(n)*100)
# 
# ggsave("./images/agency_n_decline_sum.png", width = 4, height = 6, units = "in")

agency_n_decline_sum_fig<-agency_n_decline_sum %>%
  ggplot(aes(x=PY, y=n)) +
  labs(x = "Year", size=5)+
  labs(y = "No. of Publications  (Jan-May)", size=5)+
  geom_bar(stat="identity")+
  expand_limits(y = 0)+
  theme_classic()+
  geom_hline(yintercept = 0)+
  annotate(geom="text", x=2024, y=(max(agency_n_decline_sum$n)-.02*max(agency_n_decline_sum$n)), label=(agency_n_decline_sum %>% filter(PY==2024) %>% select(n)),
           color="navyblue", size=4)+
  annotate(geom="text", x=2025, y=(max(agency_n_decline_sum$n)-.02*max(agency_n_decline_sum$n)), label=(agency_n_decline_sum %>% filter(PY==2025) %>% select(n)),
           color="navyblue", size=4)+
  annotate(geom="text", x=2025, y=(max(agency_n_decline_sum$n)-.17*max(agency_n_decline_sum$n)), label=paste("(", round((agency_n_decline_sum %>% filter(PY>2024) %>% select(perc_previous_yr)),2),"%)",sep=""),
           color="red", size=4)+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0, max(agency_n_decline_sum %>% select(n))+5000,by=2500))+
  scale_x_continuous( breaks=seq(2019,2025,by=1))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x =element_text(size = 12))+
  gghighlight(PY == 2025)

ggsave("./docs/images/agency_n_decline_sum.png", width = 6, height = 4, units = "in")



# per agency comparison to previous year ----------------------------------

  
agency_n_decline %>%
  # filter(author_position=="any") %>%
  filter(PY>2022) %>%
  drop_na() %>%
  ggplot(aes(x = PY, y = n, group = agency, color = agency)) +
  # ggplot(aes(x = PY, y = perc_previous, group = agency, color = agency)) +
  # ggplot(aes(x=PY,y=n, group=agency, color=agency)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(vars(agency), scales = "free")
# +
# scale_y_continuous(expand = c(0, 0), breaks = c(2019, 2025))


compare_agency_2425 <-
agency_n_decline %>%
drop_na() %>%
filter(PY > 2023)
#  -->
# agency_n_decline %>% 
#   drop_na() %>% 
#   filter(PY > 2023) %>% 
#   ggplot(aes(x = PY, y = perc_previous, group = agency, color = agency)) + 
#   # ggplot(aes(x=PY,y=n, group=agency, color=agency)) +
#   geom_point() + 
#   geom_line() + 
#   theme_classic() + 
#   facet_wrap(vars(author_position))+ 
#   scale_y_continuous(expand = c(0, 0), breaks = c(2019, 2025))  
# + 
# scale_y_continuous(expand = c(0, 0), limits = c(0, 2000))+ 
# gghighlight((perc_previous < -.5)) 

agency_n_decline_2<-
  agency_n_decline %>%
  drop_na() %>%
  # filter(author_position=="any") %>%
  filter(PY > 2021) %>%
  ggplot(aes(x = PY, y = perc_previous, fill = PY)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2024, 2025))+
  labs(x = "Year", size=5)+
  labs(y = "Relative Change in Productivity (%)", size=5)+
  theme_classic() +
  theme(legend.position="none")+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x =element_text(size = 12))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x =element_text(size = 14))+
  geom_hline(yintercept = 0) +
  facet_wrap(vars(agency),ncol = 3)+
  theme(strip.text = element_text(
    size = 10, color = "navy"))+
  gghighlight(PY==2025)

ggsave("./docs/images/agency_n_decline_2.png", width = 6, height = 9, units = "in")





# agency_primary ----------------------------------------------------------



# total pubs per agency primary ---------------------------------------------------


authors_data_set %>% filter(author_order==1) %>%  group_by(agency_primary) %>% tally() %>% arrange(desc(n))



names(authors_data_set)
total_pubs_per_agency_primary <- authors_data_set %>% 
  filter(federal==TRUE) %>% 
  mutate(agency_primary=if_else(agency=="us department of the interior", "interior",agency_primary)) %>% 
  mutate(agency_primary=if_else(agency=="federal reserve system", "frs",agency_primary)) %>% 
  mutate(agency_primary=if_else(agency=="us department of defense", "dod",agency_primary)) %>% 
  select(refID,agency_primary) %>% 
  distinct() %>% 
  drop_na() %>% 
  group_by(agency_primary) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(desc(n))

agencies_past_20<-total_pubs_per_agency_primary %>% 
  select(agency_primary) %>% slice(21:nrow(total_pubs_per_agency_primary)) %>% 
  mutate(agency_primary=toupper(agency_primary)) %>%  
  mutate(agency_primary=if_else((agency_primary=="STATE"|
                           agency_primary=="EDUCATION"|
                           agency_primary=="CONGRESS"|
                           agency_primary=="TREASURY"|
                           agency_primary=="LABOR"|
                           agency_primary=="OTHER"),
                        str_to_title(agency_primary),
                        agency_primary)
  )

# agencies_past_20[1,]<-paste("Other agencies: ",agencies_past_20[1,],sep="")
# agencies_past_20[5,]<-paste(agencies_past_20[5,],"\n",sep="")
# agencies_past_20[10,]<-paste(agencies_past_20[10,],"\n",sep="")
# agencies_past_20[15,]<-paste(agencies_past_20[15,],"\n",sep="")
# agencies_past_20[20,]<-paste(agencies_past_20[20,],"\n",sep="")

agencies_past_20<-agencies_past_20 %>% mutate_all(tolower)
agencies_over_20_1<-paste(agencies_past_20$agency_primary[1:10],collapse=",") 
agencies_over_20_2<-paste(agencies_past_20$agency_primary[11:20],collapse=",") 
agencies_over_20_3<-paste(agencies_past_20$agency_primary[21:30],collapse=",") 
agencies_over_20_4<-paste(agencies_past_20$agency_primary[31:40],collapse=",") 
agencies_over_20_5<-paste(agencies_past_20$agency_primary[41:50],collapse=",") 
# agencies_over_20_6<-paste(agencies_past_20$agency_primary[51:nrow(agencies_past_20)],collapse=",") 

agencies_over_20<-paste(
  agencies_over_20_1,
  agencies_over_20_2,
  agencies_over_20_3,
  agencies_over_20_4,
  agencies_over_20_5
)



# agency_primary change fig -------------------------------------------------------





agency_primary_subset_over <- total_pubs_per_agency_primary %>%
  # filter(n > 10000) %>%
  filter(n > 5000) %>%
  select(agency_primary,n) %>%
  arrange(desc(n))

agency_primary_subset_under <- total_pubs_per_agency_primary %>%
  # filter(n < 10000) %>%
  filter(n < 5000) %>%
  select(agency_primary,n) %>%
  arrange(desc(n))

agency_primary_subset<-agency_primary_subset_over$agency_primary
# 
# agency_primary_subset<-agency_primary_subset_less10K$agency_primary


agency_primary_n_decline_first <-
  authors_data_set %>%
  filter(agency_primary %in% agency_primary_subset) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM<7) %>%
  group_by(agency_primary, PY) %>%
  tally() %>%
  group_by(agency_primary) %>%
  mutate(decline_n = (n - lag(n))) %>%
  mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
  mutate(author_position="first")
# 
# agency_primary_n_decline_last <-
#   last_authors %>%
#   filter(agency_primary %in% agency_primary_subset) %>%
#   # mutate(PM=if_else(PY==2025,5,PM)) %>%
#   filter(PM<7) %>%
#   group_by(agency_primary, PY) %>%
#   tally() %>%
#   group_by(agency_primary) %>%
#   mutate(decline_n = (n - lag(n))) %>%
#   mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
#   mutate(author_position="last")

# 
# agency_primary_n_decline_any <-
#   all_author_positions %>%
#   filter(agency_primary %in% agency_primary_subset) %>%
#   # mutate(PM=if_else(PY==2025,5,PM)) %>%
#   filter(PM<7) %>%
#   group_by(agency_primary, PY) %>%
#   tally() %>%
#   group_by(agency_primary) %>%
#   mutate(decline_n = (n - lag(n))) %>%
#   mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
#   mutate(author_position="any")

# agency_primary_n_decline<-bind_rows(agency_primary_n_decline_first,agency_primary_n_decline_last,agency_primary_n_decline_any) %>% mutate(PY=as.numeric(PY))
agency_primary_n_decline<-agency_primary_n_decline_first %>% mutate(PY=as.numeric(PY))


agency_primary_n_decline_sum<- agency_primary_n_decline %>%
  # filter(author_position=="any") %>%
  select(agency_primary,PY,n) %>%
  arrange(PY) %>% 
  group_by(PY) %>%
  summarize(n=sum(n)) %>%
  mutate(n_diff=n-lag(n)) %>%
  mutate(perc_previous_yr=n_diff/lag(n)*100)
# 
# ggsave("./images/agency_primary_n_decline_sum.png", width = 4, height = 6, units = "in")

agency_primary_n_decline_sum_fig<-agency_primary_n_decline_sum %>%
  ggplot(aes(x=PY, y=n)) +
  labs(x = "Year", size=5)+
  labs(y = "No. of Publications  (Jan-May)", size=5)+
  geom_bar(stat="identity")+
  expand_limits(y = 0)+
  theme_classic()+
  geom_hline(yintercept = 0)+
  annotate(geom="text", x=2024, y=(max(agency_primary_n_decline_sum$n)-.02*max(agency_primary_n_decline_sum$n)), label=(agency_primary_n_decline_sum %>% filter(PY==2024) %>% select(n)),
           color="navyblue", size=4)+
  annotate(geom="text", x=2025, y=(max(agency_primary_n_decline_sum$n)-.02*max(agency_primary_n_decline_sum$n)), label=(agency_primary_n_decline_sum %>% filter(PY==2025) %>% select(n)),
           color="navyblue", size=4)+
  annotate(geom="text", x=2025, y=(max(agency_primary_n_decline_sum$n)-.17*max(agency_primary_n_decline_sum$n)), label=paste("(", round((agency_primary_n_decline_sum %>% filter(PY>2024) %>% select(perc_previous_yr)),2),"%)",sep=""),
           color="red", size=4)+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0, max(agency_primary_n_decline_sum %>% select(n))+5000,by=2500))+
  scale_x_continuous( breaks=seq(2019,2025,by=1))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x =element_text(size = 12))+
  gghighlight(PY == 2025)

ggsave("./docs/images/agency_primary_n_decline_sum.png", width = 6, height = 4, units = "in")



# per agency_primary comparison to previous year ----------------------------------


agency_primary_n_decline %>%
  # filter(author_position=="any") %>%
  filter(PY>2022) %>%
  drop_na() %>%
  ggplot(aes(x = PY, y = n, group = agency_primary, color = agency_primary)) +
  # ggplot(aes(x = PY, y = perc_previous, group = agency_primary, color = agency_primary)) +
  # ggplot(aes(x=PY,y=n, group=agency_primary, color=agency_primary)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(vars(agency_primary), scales = "free")
# +
# scale_y_continuous(expand = c(0, 0), breaks = c(2019, 2025))


compare_agency_primary_2425 <-
  agency_primary_n_decline %>%
  drop_na() %>%
  filter(PY > 2023)
#  -->
# agency_primary_n_decline %>% 
#   drop_na() %>% 
#   filter(PY > 2023) %>% 
#   ggplot(aes(x = PY, y = perc_previous, group = agency_primary, color = agency_primary)) + 
#   # ggplot(aes(x=PY,y=n, group=agency_primary, color=agency_primary)) +
#   geom_point() + 
#   geom_line() + 
#   theme_classic() + 
#   facet_wrap(vars(author_position))+ 
#   scale_y_continuous(expand = c(0, 0), breaks = c(2019, 2025))  
# + 
# scale_y_continuous(expand = c(0, 0), limits = c(0, 2000))+ 
# gghighlight((perc_previous < -.5)) 

agency_primary_n_decline_2<-
  agency_primary_n_decline %>%
  drop_na() %>%
  # filter(author_position=="any") %>%
  filter(PY > 2021) %>%
  ggplot(aes(x = PY, y = perc_previous, fill = PY)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2024, 2025))+
  labs(x = "Year", size=5)+
  labs(y = "Relative Change in Productivity (%)", size=5)+
  theme_classic() +
  theme(legend.position="none")+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x =element_text(size = 12))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x =element_text(size = 14))+
  geom_hline(yintercept = 0) +
  facet_wrap(vars(agency_primary),ncol = 3)+
  theme(strip.text = element_text(
    size = 10, color = "navy"))+
  gghighlight(PY==2025)

ggsave("./docs/images/agency_primary_n_decline_2.png", width = 6, height = 9, units = "in")
