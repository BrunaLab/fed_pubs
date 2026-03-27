make_figs_fed <- function(date,PM_max, PY_min_analyses,PY_max, author_position) {

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

# 
# # create folders for output -----------------------------------------------
# 
# 
# # setting up the main directory
main_dir1 <- "./docs"
# 
# # setting up the sub directory
sub_dir1 <- "summary_info"
# 

# 
# # setting up the main directory
main_dir2 <- paste(main_dir1,"/",sub_dir1,sep="")
# 
# # setting up the sub directory
sub_dir2 <- paste(cat,date,sep="_")
# 
# 
save_dir<-paste(main_dir2,sub_dir2,sep="/")
# 
# 
# CHOOSE FOCAL DATASETS ---------------------------------------------------
  
  
  
  
  # check if sub directory exists 
  if (author_position=="anywhere"){
    
    # specifying the working directory
    
    papers_dataset<-read_csv("./data_clean/for_pub/papers_df_fed_anywhere.csv") 
    papers_dataset<-as.data.frame(papers_dataset)
    authors_dataset<-read_csv("./data_clean/for_pub/authors_df_fed_anywhere.csv")
    authors_dataset<-as.data.frame(authors_dataset)
    
    
  }else if(author_position=="first"){ 
    # FED FIRST AUTHORS 
    papers_dataset<-read_csv("./data_clean/for_pub/papers_df_fed_first.csv") 
    papers_dataset<-as.data.frame(papers_dataset) %>% 
      filter(PY<=PY_max) %>% 
      filter(PY>=PY_min_analyses) 
      
    authors_dataset<-read_csv("./data_clean/for_pub/authors_df_fed_first.csv") 
    authors_dataset<-as.data.frame(authors_dataset) %>% 
      filter(PY<=PY_max) %>% 
      filter(PY>=PY_min_analyses) 
    # ALL AUTHORS FEDS
    papers_with_only_feds<-read_csv("./data_clean/for_pub/papers_df_only_fed_authors.csv")
    papers_with_only_feds<-as.data.frame(papers_with_only_feds) %>% 
      filter(PY<=PY_max) %>% 
      filter(PY>=PY_min_analyses) 
    
    # ANY AUTHOR FED
    papers_df_fed_anywhere<-read_csv("./data_clean/for_pub/papers_df_fed_anywhere.csv") 
    papers_df_fed_anywhere<-as.data.frame(papers_df_fed_anywhere) %>% 
      filter(PY<=PY_max) %>% 
      filter(PY>=PY_min_analyses) 
    
    
    authors_df_fed_anywhere<-read_csv("./data_clean/for_pub/authors_df_fed_anywhere.csv") 
    authors_df_fed_anywhere<-as.data.frame(authors_df_fed_anywhere)
    
  }else{
    print("you chose a dataset that doesn't exist")
  }
  





# message("focal dataset chosen")


# publications per year ---------------------------------------------------

# 
# pubs_by_affil<-authors_dataset %>% 
#   filter(federal==TRUE) %>% 
#   group_by(affil_id,agency_primary,agency) %>% 
#   tally() %>% 
#   arrange(desc(n)) 
# 
# pubs_by_agency<-authors_dataset %>% 
#   filter(federal==TRUE) %>% 
#   group_by(agency_primary,
#            agency) %>% 
#   tally() %>% 
# arrange(desc(n))
#   arrange(agency_primary,desc(n)) 
# 
# pubs_by_affil

# message("calculating pubs_per_yr of focal dataset")

#   
pubs_yr <- papers_dataset %>%
  distinct(refID,PY) %>%
  group_by(PY) %>%
  tally()


source("code/figs_fed/total_pubs_per_year.R")
pubs_yr_fig<-total_pubs_per_year(papers_dataset,PY_max)
pubs_yr_fig



# SAVE FIGURE


ggsave(paste(save_dir,"/","total_pubs_per_yr.png",sep=""),
       width = 6, height = 4, units = "in",
       device='png', dpi=700)  

# publications per month --------------------------------------------------

# message("error check 1")

month<-data.frame(month_name=month.abb,PM=seq(1:12)) %>% 
  mutate(month_name=as.factor(month_name)) 



# count_pubs_per_mo <- function(papers_dataset) {
# 
#   pubs_mo <-
#     papers_dataset %>%
#     group_by(PM, PY) %>%
#     tally() %>%
#     mutate(PM=as.numeric(PM),
#            PY=as.numeric(PY)) %>%
#     arrange(PY, PM) %>%
#     ungroup() %>%
#     mutate(month=row_number()) %>%
#     left_join(month) %>%
#     mutate(month_name=reorder(month_name,PM))
# 
#   return(pubs_mo)
# }

# message("error check 2")

source("code/count_pubs_per_mo.R")
# message("error check 3")

pubs_mo<-papers_dataset %>%
  group_by(PM, PY) %>%
  tally() %>%
  mutate(PM=as.numeric(PM),
         PY=as.numeric(PY)) %>% 
  arrange(PY, PM) %>% 
  ungroup() %>% 
  mutate(month=row_number()) %>% 
  left_join(month) %>% 
  mutate(month_name=reorder(month_name,PM))
  
  
  # count_pubs_per_mo(papers_dataset)

# pubs_mo %>% filter(PM<(PM_max+1)) %>% arrange(PM,desc(PY))
# message("error check 4")

source("code/figs_fed/pubs_per_month.R")
# message("error check 5")

pubs_mo_fig<-pubs_per_month(pubs_mo,2025)
pubs_mo_fig
# SAVE IMAGE
# message("error check 6")

ggsave(paste(save_dir,"/","pubs_per_month.png",sep=""),
       width = 6, height = 4, units = "in",
       device='png', dpi=700)  

# message("pubs per month fig saved")
# pubs per month_cumulative -----------------------------------------------

# 
# count_cumul_pubs_per_month <- function(papers_dataset) {
#   
#   pubs_mo_cumulative <-
#     papers_dataset %>%
#     group_by(PM, PY) %>%
#     tally() %>%
#     mutate(PM=as.numeric(PM),
#            PY=as.numeric(PY)) %>%
#     arrange(PY, PM) %>%
#     ungroup() %>%
#     mutate(month=row_number()) %>%
#     group_by(PY) %>%
#     mutate(cumul_pubs=cumsum(n))
# 
# # pubs_mo_cumulative<-count_cumul_pubs_per_month(papers_dataset)
# # last number is max month of focal year (ie 2025)
# 
# # pubs_mo_cumulative %>% filter(PM<(PM_max+1)) %>% arrange(PM,desc(PY))
# 
# pubs_mo<-count_pubs_per_mo(papers_dataset)
# 
# pubs_mo_cum<-pubs_mo %>% 
#   group_by(PY) %>% 
#   mutate(cumul_pubs=cumsum(n)) 
# 
# final_yr<-pubs_mo_cum %>% 
#   filter(PY==PY_max) %>% 
#   filter(PM<PM_max+1) 
# 
# 
# prior_yrs<-pubs_mo_cum %>% 
#   filter(PY<PY_max) 
# 
# prior_yrs %>% 
#   filter(PM==12) %>% 
#   ungroup() %>% 
#   mutate(perc=(cumul_pubs-lag(cumul_pubs))/lag(cumul_pubs)*100)
# 
# counter<-pubs_mo_cum %>% 
#   ungroup() %>% 
#   select(PM,month_name) %>% 
#   distinct()
# 
# prior_yrs_avg<-pubs_mo_cum %>% 
#   filter(PY<PY_max) %>% 
#   group_by(PM) %>% 
#   summarize(n=mean(n)) %>% 
#   mutate(cumul_pubs=cumsum(n)) %>% 
#   mutate(PY="Avg. (all yrs)") %>% 
#   left_join(counter)
# 
# plot_data<-bind_rows(final_yr,prior_yrs) %>% 
#   mutate(PY=as.character(PY)) %>% 
#   bind_rows(prior_yrs_avg)
# 
# # percent change from previous years
# 
# perc_change_avg<-final_yr %>% 
#   mutate(PY=as.character(PY)) %>% 
#   bind_rows(prior_yrs_avg) %>% 
#   filter(PM==PM_max) %>% 
#   ungroup() %>% 
#   mutate(change_n = (cumul_pubs - lead(cumul_pubs))) %>%
#   mutate(perc_previous = ((change_n) / lead(cumul_pubs)) * 100) %>% 
#   mutate(perc_previous=round(perc_previous,2)) %>% 
#   filter(PY==PY_max) %>% 
#   rename(perc_mean=perc_previous)
# 
# perc_change<-pubs_mo_cum %>% 
#   filter(PM==PM_max)%>% 
#   ungroup() %>% 
#   mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
#   mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
#   mutate(perc_previous=round(perc_previous,2))
# 
# 
# 
# 
# return(list(plot_data,perc_change,pubs_mo_cumulative))
# 
# }
# 
# foo<-count_cumul_pubs_per_month(papers_dataset)
# count_cumul_pubs_per_month
# write_csv(plot_data[[1]],"./data_clean/cumulative_pubs_monthly_fed_first.csv")
# write_csv(plot_data[[2]],"./docs/summary_info/perc_change_fed_first.csv")


# write_csv(plot_data[[1]],"./data_clean/cumulative_pubs_monthly_fed_only.csv")
# write_csv(plot_data[[2]],"./docs/summary_info/perc_change_fed_only.csv")

# message("error check 7")
source("code/figs_fed/pubs_per_month_cumulative_multipanel.R")
# message("error check 8")
pubs_mo_fig_cumulative<-pubs_per_month_cumulative_multipanel(papers_dataset,
                                                             papers_with_only_feds,
                                                             papers_df_fed_anywhere,
                                                             PM_max,
                                                             PY_max)
# perc_chg_panel1<-pubs_mo_fig_cumulative[[2]]
# perc_chg_panel2<-pubs_mo_fig_cumulative[[4]]
fig<-pubs_mo_fig_cumulative[[7]]
# message("error check 9")
fig
ggsave(paste(save_dir,"/","pubs_mo_cum_fig_multipanel.png",sep=""),
          width = 6, height = 8, units = "in",
          device='png', dpi=700)  

perc_change_cumul_only_feds<-as.data.frame(pubs_mo_fig_cumulative[4])
perc_change_cumul_first_fed<-as.data.frame(pubs_mo_fig_cumulative[2])
perc_change_cumul_any_fed<-as.data.frame(pubs_mo_fig_cumulative[6])
write_csv(perc_change_cumul_only_feds,paste(save_dir,"/","perc_change_cumul_only_feds.csv",sep="")) 

write_csv(perc_change_cumul_first_fed,paste(save_dir,"/","perc_change_cumul_first_fed.csv",sep="")) 

write_csv(perc_change_cumul_any_fed,paste(save_dir,"/","perc_change_cumul_any_fed.csv",sep="")) 


#TODO: convert the three csvs saved above to just one below, chanmge in Rmd
perc_change_cumul_only_feds<-perc_change_cumul_only_feds %>% mutate(cat="only_feds")
perc_change_cumul_first_fed<-perc_change_cumul_first_fed %>% mutate(cat="first_fed")
perc_change_cumul_any_fed<-perc_change_cumul_any_fed %>% mutate(cat="any_fed")

agency_n_decline_sum<-bind_rows(perc_change_cumul_only_feds,perc_change_cumul_first_fed,perc_change_cumul_any_fed)

# message("error check 10")
# message("pubs_mo_cum_fig_multipanel.png saved")
# message("error check 11")
# ggsave(file="./docs/images/pubs_mo_cum_fig_multipanel.png", pubs_mo_fig_cumulative,
#        width = 6, height = 8, units = "in",
#        device='png', dpi=700)  


# ggsave("./docs/images/pubs_mo_cum_fig.png",
#        width = 13, height = 10, units = "in",
#        device='png', dpi=700)



# publications per quarter ------------------------------------------------


# source("code/figs_fed/pubs_per_quarter.R")
# pubs_per_quarter_fig<-pubs_per_quarter(pubs_mo,2024)
# pubs_per_quarter_fig
# 

# SAVE FIGURE
# ggsave("./docs/images/pubs_per_quarter.png", 
#        width = 6, height = 4, units = "in", 
#        device='png', dpi=700)



# pubs january to month X -------------------------------------------------

# source("code/figs_fed/pubs_jan_to_month_x.R")
# 
# # number is max month you want to visualize (i.e., 6 = june, 7 = july)
# # pubs_per_quarter(pubs_mo,8)
# 
# monthly_pubs_1<-pubs_jan_to_month_x(pubs_mo, PM_max)
# 
# ggsave("./docs/images/monthly_pubs_to_date.png", 
#        width = 6, height = 4, units = "in", 
#        device='png', dpi=700)



# total pubs to month X (bar chart) --------------------------------------------------


# source("code/figs_fed/total_pubs_to_month_x.R")
# number is max month you want to visualize (i.e., 6 = june, 7 = july)
# pubs_per_quarter(pubs_mo,12)
# total_pubs_to_month_x_fig<-total_pubs_to_month_x(pubs_mo, PM_max)
# ggsave("./docs/images/total_pubs_to_month_x.png", 
#        width = 6, height = 4, units = "in", 
#        device='png', dpi=700)

# total pubs per agency ---------------------------------------------------

# message("error check 12")
# names(authors_dataset)


total_pubs_per_agency <- authors_df_fed_anywhere %>% 
  # filter(federal==TRUE) %>% 
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
  authors_dataset %>%
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

# 
# agency_n_decline_sum<- agency_n_decline %>%
#   # filter(author_position=="any") %>%
#   select(agency_primary,PY,n) %>%
#   arrange(PY) %>% 
#   group_by(PY) %>%
#   summarize(n=sum(n)) %>%
#   mutate(n_diff=n-lag(n)) %>%
#   mutate(perc_previous_yr=n_diff/lag(n)*100)

# bar chart of % decline  -----------------------------------
source("code/figs_fed/agency_n_decline_bar.R")
agency_n_decline_bar_fig <- agency_n_decline_bar(agency_n_decline_sum, PY_max) 

ggsave(paste(save_dir,"/","perc_change_bar_multipanel.png",sep=""),
       width = 6, height = 8, units = "in",
       device='png', dpi=700)  

# ggsave("./docs/images/agency_n_decline_sum.png", 
#        width = 7, height = 10, units = "in", 
#        device='png', dpi=700)


# per agency comparison to previous year ----------------------------------

# compare_agency_2425 <-
# agency_n_decline %>%
# drop_na() %>%
# filter(PY > 2023)
# message("error check 13")
source("code/figs_fed/pubs_per_month_cumulative_agency.R")
pubs_per_month_cumulative_agency<-pubs_per_month_cumulative_agency(papers_dataset,authors_dataset,PY_max,PM_max)

perc_change<-as.data.frame( pubs_per_month_cumulative_agency[[1]])
write_csv(perc_change,paste(save_dir,"/","perc_change_agency_fed.csv",sep="")) 
# ggsave("./docs/images/pubs_mo_cum_agency_lines.png",
#        width = 11, height = 8, units = "in",
#        device='png', dpi=700)

pubs_mo_cum_agency_lines<-pubs_per_month_cumulative_agency[[2]]
pubs_mo_cum_agency_lines
ggsave(paste(save_dir,"/","pubs_mo_cum_agency_lines.png",sep=""),
       width = 11, height = 8, units = "in",
       device='png', dpi=700)


# message("pubs_mo_cum_agency_lines.png saved")
# non_feds<-affils_df_complete %>% 
#   filter(federal==FALSE) %>% 
#   distinct(affil_id,.keep_all=TRUE) %>% 
#   filter(country=="usa"|is.na(country))








# bar chart percent by agency ---------------------------------------------
threshold<-6000

threshhold_ct<-authors_dataset %>% 
  mutate(agency=if_else(agency=="us department of the interior", "interior",agency)) %>% 
  mutate(agency=if_else(agency=="federal reserve system", "frs",agency)) %>% 
  mutate(agency=if_else(agency=="us department of defense", "dod",agency)) %>% 
  select(refID,agency_primary) %>% 
  distinct() %>% 
  drop_na() %>% 
  group_by(agency_primary) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(desc(n)) %>% 
  filter(n>=threshold) %>% 
  select(agency_primary)


# 
# pubs_by_agency_yr<-authors_dataset %>% 
#   mutate(agency=if_else(agency=="us department of the interior", "interior",agency)) %>% 
#   mutate(agency=if_else(agency=="federal reserve system", "frs",agency)) %>% 
#   mutate(agency=if_else(agency=="us department of defense", "dod",agency)) %>% 
#   select(refID,PY,agency_primary) %>% 
#   distinct() %>% 
#   drop_na() %>% 
#   group_by(agency_primary,PY) %>% 
#   summarize(n=n_distinct(refID)) %>% 
#   arrange(PY,agency_primary)



source("code/figs_fed/agency_n_decline_bar_facets.R")
agency_perc_decline_bar_fig2 <- agency_n_decline_bar_facets(agency_n_decline_first, threshhold_ct, PY_max) 

ggsave(paste(save_dir,"/","perc_change_bar_multipanel_2.png",sep=""),
       width = 6, height = 8, units = "in",
       device='png', dpi=700)  


# table for ms ------------------------------------------------------------


agency_n_decline_first <-
  authors_dataset %>%
  # filter(agency_primary %in% tolower(agency_subset)) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM<(PM_max+1)) %>%
  group_by(agency_primary, PY) %>%
  tally() %>%
  group_by(agency_primary) %>%
  mutate(decline_n = (n - lag(n))) %>%
  mutate(perc_previous = ((decline_n) / lag(n)) * 100) %>%
  mutate(author_position="first") 

# write_csv(agency_n_decline_first,"./docs/summary_info/agency_n_decline_first.csv")
write_csv(agency_n_decline_first,paste(save_dir,"/","agency_n_decline_first.csv",sep=""))

# message("error check 14")

# message("agency_n_decline_first.csv saved")

total_pubs_per_agency_first<-
  read_csv(paste(save_dir,"/","total_pubs_per_agency_first.csv",sep="")) %>%   
  mutate(agency_primary=if_else(agency=='federal reserve system',
                                "federal reserve system",
                                agency_primary)) %>% 
  group_by(agency_primary) %>%
  mutate(total=sum(n)) %>%
  select(agency_primary,total) %>% 
  group_by(agency_primary) %>%
  slice_head(n=1) %>% 
  ungroup() %>% 
  mutate(perc_first=total/sum(total)*100) %>% 
  arrange(desc(total)) %>% 
  mutate(perc_first=round(perc_first,2))

perc_data<-agency_n_decline_first %>% 
  filter(PY==2025) %>% 
  select(agency_primary,n,perc_decline=perc_previous) %>% 
  rename(Unit=agency_primary,
         n24=n) 
  
perc_data<-agency_n_decline_first %>% 
  filter(PY>2023) %>% 
  select(agency_primary,PY,n,perc_decline=perc_previous) %>% 
  pivot_wider(
    names_from = PY,
    values_from = c(n, perc_decline)
  ) %>% 
  rename(Unit=agency_primary,
         n24=n_2024,
         n25=n_2025,
         perc_decline24=perc_decline_2024,
         perc_decline25=perc_decline_2025) 


# message("error check 15")


agency_total<-authors_df_fed_anywhere %>% 
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

# message("error check 16")
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
  mutate(perc_decline24=round(perc_decline24,2)) %>%
  mutate(perc_decline25=round(perc_decline25,2)) %>%
  mutate(perc=round(perc,2)) %>%
  mutate(perc_first=round(perc_first,2)) 
  
  # write_csv(agency_table,"./docs/summary_info/agency_table.csv") 
  write_csv(agency_table,paste(save_dir,"/","agency_table.csv",sep=""))
  
  # message("error check 17")
  
other_affils<-authors_dataset %>% filter(federal==TRUE) %>%
  filter(agency_primary=="other") %>% 
  select(agency) %>% 
  distinct() %>% 
  arrange(agency) %>% 
  mutate(agency=if_else(nchar(agency)<6,str_to_upper(agency),str_to_title(agency))) 
  
# write_csv(other_affils,"./docs/summary_info/other_affils.csv") 
write_csv(other_affils,paste(save_dir,"/","other_affils.csv",sep=""))

# message("other_affils.csv saved")


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
# write_csv(journals_first,"./docs/summary_info/journals_first.csv")
write_csv(journals_first,paste(save_dir,"/","journals_first.csv",sep=""))

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
# write_csv(jrnls_overall_first,"./docs/summary_info/jrnls_overall_first.csv")
write_csv(jrnls_overall_first,paste(save_dir,"/","jrnls_overall_first.csv",sep=""))

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
         #   
         # n_yr_2025,
         # perc_yr_2025,
         # n_yr_2024,
         # perc_yr_2024,
         # n_yr_2023,
         # perc_yr_2023,
         # n_yr_2022,
         # perc_yr_2022,
         # n_yr_2021,
         # perc_yr_2021,
         # # n_yr_2019,
         # # perc_yr_2019,
         # n_yr_2020,
         # perc_yr_2020
         # )     



# write_csv(journals_n_perc_annual_first,"./docs/summary_info/journals_n_perc_annual_first.csv")
write_csv(journals_n_perc_annual_first,paste(save_dir,"/","journals_n_perc_annual_first.csv",sep=""))

# message("journals_n_perc_annual_first saved")

}