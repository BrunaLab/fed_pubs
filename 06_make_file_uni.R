
library(tidyverse)
library(janitor)
library(data.table)


papers_df  <- setDT(read_rds("./data_clean/papers_df_uni_clean.rds")) 
papers_df <- papers_df %>% 
  mutate(PM=
           case_when(
             is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
             .default = as.numeric(PM)
           )
  ) %>% 
  remove_empty(c("rows","cols")) %>% 
  tibble()
  

papers_df %>% filter(is.na(PM))

authors_df <- setDT(readRDS("./data_clean/authors_df_uni_clean.rds"))




# make rds of papers with fed 1st author ----------------------------------

unique(authors_df$uni)

focal_first_authors <- authors_df %>%
  remove_empty(c("rows","cols")) %>% 
  filter(uni != "other") %>%
  filter(uni != "minn") %>% 
  filter(uni != "unc_ch") %>% 
  filter(uni != "ohio_state") %>% 
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


papers_dataset %>% group_by(SO) %>% tally() %>% arrange(desc(n)) %>% slice_head(n=20)

papers_dataset %>% group_by(PY) %>% tally() 

focal_first_authors %>% 
  distinct(refID,.keep_all=TRUE) %>% 
  group_by(uni) %>% 
  tally() %>% 
  arrange(desc(n))

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


pubs_mo %>% filter(PM<4) %>% arrange(PM,desc(PY))

source("code/figs_uni/pubs_per_month.R")
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
  mutate(cumul_pubs=cumsum(n)) %>% 
  left_join(month) %>% 
  mutate(month_name=reorder(month_name,PM))


# last number is max month of focal year (ie 2025)


pubs_mo_cumulative %>% filter(PM<4) %>% arrange(PM,desc(PY))

source("code/figs_uni/pubs_per_month_cumulative.R")
pubs_mo_fig_cumulative<-pubs_per_month_cumulative(pubs_mo,2025,6)



# publications per quarter ------------------------------------------------


source("code/figs_uni/pubs_per_quarter.R")
pubs_per_quarter_fig<-pubs_per_quarter(pubs_mo,2024)



# pubs january to month X -------------------------------------------------

source("code/figs_uni/pubs_jan_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

monthly_pubs_1<-pubs_jan_to_month_x(pubs_mo, 6)



# total pubs to month X  --------------------------------------------------


source("code/figs_uni/total_pubs_to_month_x.R")

# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,8)

total_pubs_to_month_x_fig<-total_pubs_to_month_x(pubs_mo, 6)


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
# 
# agencies_past_20<-total_pubs_per_uni %>% 
#   select(uni) %>% slice(21:nrow(total_pubs_per_uni)) %>% 
#   mutate(uni=toupper(uni)) %>%  
#   mutate(uni=if_else((uni=="STATE"|
#                            uni=="EDUCATION"|
#                            uni=="CONGRESS"|
#                            uni=="TREASURY"|
#                            uni=="LABOR"|
#                            uni=="OTHER"),
#                         str_to_title(uni),
#                         uni)
#   )

# agencies_past_20[1,]<-paste("Other agencies: ",agencies_past_20[1,],sep="")
# agencies_past_20[5,]<-paste(agencies_past_20[5,],"\n",sep="")
# agencies_past_20[10,]<-paste(agencies_past_20[10,],"\n",sep="")
# agencies_past_20[15,]<-paste(agencies_past_20[15,],"\n",sep="")
# agencies_past_20[20,]<-paste(agencies_past_20[20,],"\n",sep="")
# 
# agencies_past_20<-agencies_past_20 %>% mutate_all(tolower)
# agencies_over_20_1<-paste(agencies_past_20$uni[1:10],collapse=",") 
# agencies_over_20_2<-paste(agencies_past_20$uni[11:20],collapse=",") 
# agencies_over_20_3<-paste(agencies_past_20$uni[21:30],collapse=",") 
# agencies_over_20_4<-paste(agencies_past_20$uni[31:40],collapse=",") 
# agencies_over_20_5<-paste(agencies_past_20$uni[41:50],collapse=",") 
# # agencies_over_20_6<-paste(agencies_past_20$uni[51:nrow(agencies_past_20)],collapse=",") 
# 
# agencies_over_20<-paste(
#   agencies_over_20_1,
#   agencies_over_20_2,
#   agencies_over_20_3,
#   agencies_over_20_4,
#   agencies_over_20_5
# )
# 


# uni change fig -------------------------------------------------------





# uni_subset_over <- total_pubs_per_uni %>%
#   # filter(n > 10000) %>%
#   filter(n > 5000) %>%
#   select(uni,n) %>%
#   arrange(desc(n))
# 
# uni_subset_under <- total_pubs_per_uni %>%
#   # filter(n < 10000) %>%
#   filter(n < 5000) %>%
#   select(uni,n) %>%
#   arrange(desc(n))
# 
# uni_subset<-uni_subset_over$uni
# # 
# # uni_subset<-uni_subset_less10K$uni


uni_n_decline_first <-
  authors_data_set %>%
  # filter(uni %in% uni_subset) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM<7) %>%
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
#   filter(PM<7) %>%
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
#   filter(PM<7) %>%
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
# ggsave("./images/uni_n_decline_sum.png", width = 4, height = 6, units = "in")

uni_n_decline_sum_fig<-uni_n_decline_sum %>%
  ggplot(aes(x=PY, y=n)) +
  labs(x = "Year", size=5)+
  labs(y = "No. of Publications  (Jan-May)", size=5)+
  geom_bar(stat="identity")+
  expand_limits(y = 0)+
  theme_classic()+
  geom_hline(yintercept = 0)+
  annotate(geom="text", x=2024, y=(max(uni_n_decline_sum$n)-.02*max(uni_n_decline_sum$n)), label=(uni_n_decline_sum %>% filter(PY==2024) %>% select(n)),
           color="navyblue", size=4)+
  annotate(geom="text", x=2025, y=(max(uni_n_decline_sum$n)-.02*max(uni_n_decline_sum$n)), label=(uni_n_decline_sum %>% filter(PY==2025) %>% select(n)),
           color="navyblue", size=4)+
  annotate(geom="text", x=2025, y=(max(uni_n_decline_sum$n)-.17*max(uni_n_decline_sum$n)), label=paste("(", round((uni_n_decline_sum %>% filter(PY>2024) %>% select(perc_previous_yr)),2),"%)",sep=""),
           color="red", size=4)+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0, max(uni_n_decline_sum %>% select(n))+5000,by=2500))+
  scale_x_continuous( breaks=seq(2019,2025,by=1))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x =element_text(size = 12))+
  gghighlight(PY == 2025)

ggsave("./images/uni_n_decline_sum.png", width = 6, height = 4, units = "in")



# per uni comparison to previous year ----------------------------------

  
uni_n_decline %>%
  # filter(author_position=="any") %>%
  filter(PY>2022) %>%
  drop_na() %>%
  ggplot(aes(x = PY, y = n, group = uni, color = uni)) +
  # ggplot(aes(x = PY, y = perc_previous, group = uni, color = uni)) +
  # ggplot(aes(x=PY,y=n, group=uni, color=uni)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(vars(uni), scales = "free")
# +
# scale_y_continuous(expand = c(0, 0), breaks = c(2019, 2025))


compare_uni_2425 <-
uni_n_decline %>%
drop_na() %>%
filter(PY > 2023)
#  -->
# uni_n_decline %>% 
#   drop_na() %>% 
#   filter(PY > 2023) %>% 
#   ggplot(aes(x = PY, y = perc_previous, group = uni, color = uni)) + 
#   # ggplot(aes(x=PY,y=n, group=uni, color=uni)) +
#   geom_point() + 
#   geom_line() + 
#   theme_classic() + 
#   facet_wrap(vars(author_position))+ 
#   scale_y_continuous(expand = c(0, 0), breaks = c(2019, 2025))  
# + 
# scale_y_continuous(expand = c(0, 0), limits = c(0, 2000))+ 
# gghighlight((perc_previous < -.5)) 

uni_n_decline_2<-
  uni_n_decline %>%
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
  facet_wrap(vars(uni),ncol = 3)+
  theme(strip.text = element_text(
    size = 10, color = "navy"))+
  gghighlight(PY==2025)

ggsave("./images/uni_n_decline_2.png", width = 6, height = 9, units = "in")

