make_figs_uni <- function(date,PM_max,PY_min_analyses,PY_max,author_position) {

  library(tidyverse)
library(janitor)
library(data.table)

  cat<-"uni"
  

  
  if(date=="20250901"){
    scopus_ids_searched<-read_csv("./data_clean/api_uni_affils_searched_2025-09-01.csv")
  }
  
  
  if(date=="20251010"){
    scopus_ids_searched<-read_csv("./data_clean/api_uni_affils_searched.csv")
  }
  
  
  if(date=="20260101"){
    scopus_ids_searched<-read_csv("data_clean/api_uni_affils_searched.csv")
  }
  

# create folders for output -----------------------------------------------


# setting up the main directory
main_dir1 <- "./docs"

# setting up the sub directory
sub_dir1 <- "summary_info"


# setting up the main directory
main_dir2 <- paste(main_dir1,"/",sub_dir1,sep="")

# setting up the sub directory
sub_dir2 <- paste(cat,date,sep="_")
# 
# # check if sub directory exists 
# if (file.exists(sub_dir2)){
#   
#   # specifying the working directory
#   setwd(file.path(main_dir2, sub_dir2))
# } else {
#   
#   # create a new sub directory inside
#   # the main path
#   dir.create(file.path(main_dir2, sub_dir2))
#   
# }

save_dir<-paste(main_dir2,sub_dir2,sep="/")




# check if sub directory exists 
if (author_position=="anywhere"){
  
  # specifying the working directory
  
  papers_dataset<-read_csv("./data_clean/for_pub/papers_df_uni_anywhere.csv") 
  papers_dataset<-as.data.frame(papers_dataset)
  authors_dataset<-read_csv("./data_clean/for_pub/authors_df_uni_anywhere.csv")
  authors_dataset<-as.data.frame(authors_dataset)
  
  
}else if(author_position=="first"){ 
  
  papers_dataset<-read_csv("./data_clean/for_pub/papers_df_uni_first.csv") 
  papers_dataset<-as.data.frame(papers_dataset) %>% 
    filter(PY<=PY_max) %>% 
    filter(PY>=PY_min_analyses) 
  
  authors_dataset<-read_csv("./data_clean/for_pub/authors_df_uni_first.csv") 
  authors_dataset<-as.data.frame(authors_dataset) %>% 
    filter(PY<=PY_max) %>% 
    filter(PY>=PY_min_analyses) 
  # papers_with_all_feds<-read_csv("./data_clean/for_pub/papers_df_uni_first.csv") 
  # papers_with_all_feds<-as.data.frame(papers_with_all_feds)
}else{
  print("you chose a dataset that doesn't exist")
}




pubs_yr <- papers_dataset %>%
  distinct(refID,.keep_all=TRUE) %>%
  group_by(PY) %>%
  tally()


# 
# pubs_yr <- authors_dataset %>%
#   distinct(refID,.keep_all=TRUE) %>%
#   group_by(PY) %>%
#   tally()

source("code/figs_uni/total_pubs_per_year.R")
pubs_yr_fig<-total_pubs_per_year(pubs_yr,2025)

ggsave(paste(save_dir,"/","total_pubs_per_yr_uni.png",sep=""),
       width = 6, height = 8, units = "in",
       device='png', dpi=700)  


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

ggsave(paste(save_dir,"/","pubs_per_month_uni.png",sep=""),
       width = 6, height = 4, units = "in",
       device='png', dpi=700)  



# pubs per month_cumulative -----------------------------------------------

# 
# pubs_mo_cumulative <-
#   papers_dataset %>%
#   group_by(PM, PY) %>%
#   tally() %>%
#   mutate(PM=as.numeric(PM),
#          PY=as.numeric(PY)) %>%
#   arrange(PY, PM) %>%
#   ungroup() %>%
#   mutate(month=row_number()) %>%
#   group_by(PY) %>%
#   mutate(cumul_pubs=cumsum(n)) %>%
#   left_join(month) %>%
#   mutate(month_name=reorder(month_name,PM))
# 
# 
# # last number is max month of focal year (ie 2025)
# 
# 
# pubs_mo_cumulative %>% filter(PM<(PM_max+1)) %>% arrange(PM,desc(PY))
# 


source("code/figs_uni/pubs_per_month_cumulative.R")
pubs_mo_fig_cumulative_all_uni<-pubs_per_month_cumulative(pubs_mo,PY_max,PM_max)
write_csv(as.data.frame(pubs_mo_fig_cumulative_all_uni[1]),paste(save_dir,"/","cumulative_pubs_monthly_uni.csv",sep=""))
write_csv(as.data.frame(pubs_mo_fig_cumulative_all_uni[2]),paste(save_dir,"/","perc_change_uni.csv",sep=""))
pubs_mo_cum_fig_uni<-pubs_mo_fig_cumulative_all_uni[3]

ggsave(paste(save_dir,"/","pubs_mo_cum_fig_uni.png",sep=""),
       width = 6, height = 5, units = "in",
       device='png', dpi=700)  





source("code/figs_uni/pubs_per_month_cumulative_by_uni.R")
pubs_mo_fig_cumulative_by_uni<-pubs_per_month_cumulative_by_uni(papers_dataset,authors_dataset,2025,PM_max)

write_csv(as.data.frame(pubs_mo_fig_cumulative_by_uni[1]),paste(save_dir,"/","perc_change_uni_uni.csv",sep=""))
pubs_mo_cum_uni_lines<-pubs_mo_fig_cumulative_by_uni[2]

ggsave(paste(save_dir,"/","pubs_mo_cum_uni_lines.png",sep=""),
       width = 12, height = 8, units = "in",
       device='png', dpi=700)  

# publications per quarter ------------------------------------------------
# 
# 
# source("code/figs_uni/pubs_per_quarter.R")
# pubs_per_quarter_fig<-pubs_per_quarter(pubs_mo,2024)



# pubs january to month X -------------------------------------------------
# 
# source("code/figs_uni/pubs_jan_to_month_x.R")
# 
# # number is max month you want to visualize (i.e., 6 = june, 7 = july)
# # pubs_per_quarter(pubs_mo,8)
# 
# monthly_pubs_1<-pubs_jan_to_month_x(pubs_mo, PM_max)
# 
# 
# 
# # total pubs to month X  --------------------------------------------------
# 
# 
# source("code/figs_uni/total_pubs_to_month_x.R")
# 
# # number is max month you want to visualize (i.e., 6 = june, 7 = july)
# # pubs_per_quarter(pubs_mo,8)
# 
# total_pubs_to_month_x_fig<-total_pubs_to_month_x(pubs_mo, PM_max)


# total pubs per uni ---------------------------------------------------


authors_dataset %>% filter(author_order==1) %>%  group_by(uni) %>% tally() %>% arrange(desc(n))



names(authors_dataset)
total_pubs_per_uni <- authors_dataset %>% 
  select(refID,uni) %>% 
  distinct() %>% 
  drop_na() %>% 
  group_by(uni) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(desc(n))


# uni change fig -------------------------------------------------------



uni_n_decline_first <-
  authors_dataset %>%
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
# source("code/figs_uni/uni_n_decline_bar.R")
# uni_n_decline_bar_fig <- uni_n_decline_bar(uni_n_decline_sum, PY_max) 



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


source("code/figs_uni/uni_n_decline_2.R")
uni_n_decline_2_fig <- uni_n_decline_2(uni_n_decline)


ggsave(paste(save_dir,"/","uni_n_decline_2.png",sep=""),
       width = 6, height = 8, units = "in",
       device='png', dpi=700)  



#  journals info ----------------------------------------------------------




journals_first <- papers_dataset %>%
  select(refID,SO, PY) %>%
  distinct() %>% 
  mutate_all(tolower) %>% 
  drop_na() %>% 
  select(SO) %>% 
  distinct() %>% 
  arrange(SO)
# 
# write_csv(journals_first,"./docs/summary_info/uni_journals_first.csv")
write_csv(journals_first,paste(save_dir,"/","uni_journals_first.csv",sep=""))

jrnls_overall_first <- 
  papers_dataset %>%
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
# write_csv(jrnls_overall_first,"./docs/summary_info/uni_jrnls_overall_first.csv")
write_csv(jrnls_overall_first,paste(save_dir,"/","uni_jrnls_overall_first.csv",sep=""))

# 
jrnls_yr_first <- 
  papers_dataset %>%
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
         starts_with("n_yr_"),
         starts_with("perc_yr_")
  )
  # select(SO,
  #        n_yr_2025,
  #        perc_yr_2025,
  #        n_yr_2024,
  #        perc_yr_2024,
  #        n_yr_2023,
  #        perc_yr_2023,
  #        n_yr_2022,
  #        perc_yr_2022,
  #        n_yr_2021,
  #        perc_yr_2021,
  #        # n_yr_2019,
  #        # perc_yr_2019,
  #        n_yr_2020,
  #        perc_yr_2020
  #        )     



# write_csv(journals_n_perc_annual_first,"./docs/summary_info/uni_journals_n_perc_annual_first.csv")
write_csv(journals_n_perc_annual_first,paste(save_dir,"/","uni_journals_n_perc_annual_first.csv",sep=""))
message("journals_n_perc_annual_first saved")
}