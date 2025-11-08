
library(tidyverse)
library(janitor)
library(gghighlight)
library(kableExtra)
library(cowplot)
library(ggrepel)
# library(knitr)
library(progress)
library(fs)
library(data.table)

PM_max<-8


# 
cat<-"uni"
date<-"20250901"

# cat<-"uni"
# date<-"20251010"


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


# LOAD THE DATA -----------------------------------------------

papers_df  <- setDT(read_rds(paste("./data_clean/papers_df_clean_",cat,"_",date,".rds",sep="")))
# 
# papers_df_analysis  <- setDT(read_rds("./data_clean/papers_df_analysis_uni.rds")) %>% 
#   mutate(PM=
#            case_when(
#              is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
#              .default = as.numeric(PM)
#            )
#   ) %>% 
#   mutate(PT=if_else(PT=="j","journal",PT))
# 
# 
# papers_df <- papers_df_analysis %>% 
#   filter(DT!="editorial") %>% 
#   filter(DT!="letter") 
# 

# make rds of papers with fed 1st author ----------------------------------



# papers_df %>% filter(is.na(PM))
authors_df  <- setDT(read_rds(paste("./data_clean/authors_df_clean_",cat,"_",date,".rds",sep=""))) %>% 
  mutate(uni=case_when(
    # uni == "unc_ch"~"other",
    # uni == "ohio_state"~"other",
    uni == "mass_general"~"other",
    uni == "uscd"~"ucsd",
    uni == "minnesota"~"minn",
    # uni=="beth israel deaconess medical center"~"harvard",
    # uni=="boston children’s hospital"~"harvard",
    # uni=="brigham and women’s hospital"~"harvard",
    # uni=="cambridge health alliance"~"harvard",
    # uni=="dana-farber cancer institute"~"harvard",
    # uni=="harvard pilgrim health care institute"~"harvard",
    # uni=="hebrew senior life"~"harvard",
    # uni=="joslin diabetes center"~"harvard",
    # uni=="judge baker children’s center"~"harvard",
    # uni=="massachusetts eye and ear | schepens eye research institute"~"harvard",
    # uni=="massachusetts general hospital"~"harvard",
    # uni=="mclean hospital"~"harvard",
    # uni=="mount auburn hospital"~"harvard",
    # uni=="spaulding rehabilitation hospital"~"harvard",
    # uni=="va boston healthcare system"~"harvard",
    is.na(uni) ~ "other",
    .default = as.character(uni)
  )) 

# authors_df<-authors_df_analysis
unique(authors_df$uni)



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


# set focal datasets ------------------------------------------------------


papers_dataset<-papers_with_focaluni_first

authors_data_set<-focal_first_authors


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






first_authors<-focal_first_authors %>% 
  drop_na(PY)


mo_data<-focal_first_authors %>% 
  group_by(PY,PM,uni) %>% 
  # group_by(PY,PM) %>% 
  tally() %>% 
  arrange(PM,uni) %>% 
  filter(PM<PM_max+1)


rm(
  all_authors_df_for_fed_1st_papers,
  authors_data_set,
  authors_df_complete,
  first_authors,
  papers_dataset,
  papers_df,
  papers_df_complete,
  papers_with_fed_first,
  PY_for_authors_df
)

gc() # free up memory

# mo_data %>% 
#   group_by(PM) %>% 
#   slice_sample(n=1)

# The bootstrap is of the mean for 2019-2024 to see if
# the 2025 value falls outside CIs

data_for_boot<-mo_data %>% 
  filter(PY!=2025)



set.seed(20250111)
runs <- 1000

yr<-1 # can set this up so that each run does X years) 
run_no<-seq(1:runs)
yr_no<-seq(1:yr)
bs_data<-list()


for (i in run_no){
  yrs_df<-list()  
  run_data_all<-data.frame()
  
  
  for (j in yr_no){
    run_data<-
      data_for_boot %>% 
      group_by(uni,PM) %>% 
      # group_by(PM) %>% 
      slice_sample(n=1,replace=TRUE) %>% 
      mutate(yr=j) %>% 
      group_by(yr,uni) %>% 
      mutate(cum_uni=cumsum(n))
    run_data_all<-bind_rows(run_data,run_data_all)
    # run_data<- yrs_df%>% map(~as_tibble(.)) %>% bind_rows(.id="index")
  }
  
  
  bs_data[[i]] <-  run_data_all
}

bs_output<-bs_data %>% 
  map(~as_tibble(.)) %>% 
  bind_rows(.id="run") %>% 
  mutate(run=as.integer(run)) %>% 
  rename(PY_selected=PY) %>% 
  # group_by(run,yr,PM) %>% 
  # mutate(overall_cumul_pubs=cumsum(n)) %>% 
  arrange(run,yr,uni,PM) 


# write_csv(bs_output, "./data_clean/bootstrap_output_uni.csv")

write_csv(bs_output,paste("./data_clean/bootstrap_output_uni_",date,".csv",sep=""))
# bs_output<-read_csv("./data_clean/bootstrap_output.csv")
# bs_stats<-bs_output %>% 
#   arrange(run,yr) %>% 
#   filter(PM==6) %>% 
#   group_by(run) %>% 
#   mutate(avg_cumulative=mean(cumul_pubs),
#          min_numulative=(min(cumul_pubs)),
#          max_numulative=(max(cumul_pubs))
#          )  %>% 
#   select(run,avg_cumulative, min_numulative,max_numulative) %>% 
#   distinct() %>% 
#   mutate(bs_dif=avg_cumulative-min_numulative) %>% 
#   arrange(bs_dif)
#   



# plot of diff values -----------------------------------------------------

c_pubs_25<-mo_data %>% 
  filter(PY==2025) %>% 
  group_by(PM) %>% 
  summarize(n=sum(n)) %>% 
  mutate(n_c_25=cumsum(n)) %>% 
  rename(n25=n)


# averegae cumulative publications by month 2019-2024
avg_cum_obs<-mo_data %>% 
  filter(PY!=2025) %>% 
  group_by(PY,PM) %>% 
  summarize(month_total=sum(n)) %>% 
  group_by(PM) %>% 
  mutate(avg19_24=mean(month_total)) %>% 
  select(-month_total)

bs_output_run_cml<-
  bs_output %>% 
  filter(PM==PM_max) %>% 
  # group_by(run,yr,PM) %>% 
  # filter(cumul_pubs==max(cumul_pubs)) %>% 
  # select(run,PY,PM,yr,index,cumul_pubs) %>% 
  group_by(run) %>% 
  mutate(cumul_pubs=sum(cum_uni)) %>% 
  select(run,yr,cumul_pubs) %>% 
  distinct()
# write_csv(bs_output_run_cml, "./data_clean/bs_output_run_cml.csv")

# bs_output_run_cml<-read_csv("./data_clean/bs_output_run_cml.csv")

bs_stats<-bs_output_run_cml %>% 
  ungroup() %>% 
  # select(-yr,-index) %>% 
  # group_by(PM) %>% 
  summarize(mean_bs_c_pubs=mean(cumul_pubs),
            median_bs_c_pubs=median(cumul_pubs),
            max_bs_c_pubs=max(cumul_pubs),
            min_bs_c_pubs=min(cumul_pubs)) 

# %>% 
# 
#   left_join(avg_cum_obs) %>% 
#   left_join(c_pubs_25)

bs_stats
avg_cum_obs
c_pubs_25

x_min<-c_pubs_25 %>% 
  filter(PM==PM_max) %>% 
  select(n_c_25) %>% 
  mutate(n_c_25=as.numeric(n_c_25-2000)) %>% 
  mutate(n_c_25=round(n_c_25,-3))
x_min<-as.numeric(x_min)


x_max<-bs_output_run_cml %>% 
  ungroup() %>% 
  select(cumul_pubs) %>% 
  filter(cumul_pubs==max(cumul_pubs)) %>% 
  mutate(cumul_pubs=as.numeric(cumul_pubs+1000)) %>% 
  mutate(cumul_pubs=round(cumul_pubs,-3))
x_max<-as.numeric(x_max)



final_bs_n<-bs_output_run_cml %>% 
  # filter(PM==PM_max) %>% 
  ggplot(aes(cumul_pubs)) +
  geom_histogram(color="darkgray", 
                 fill="white",
                 bins=50)+
  theme_classic()+
  labs(x = paste("Cumulative publications (N = ",runs," Bootstrap Runs)",sep=""), size=5)+
  labs(y = "No. of Bootstrap runs", size=5)+
  geom_segment(x = as.numeric(c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)), 
               y= 0 , 
               xend = as.numeric(c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)), 
               yend = 100, 
               colour = "darkred",
               linetype = 2,
               linewidth = 1)+
  annotate(geom="text", x=as.numeric(c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)),
           y=120,
           label=paste("No. Publications\n","Jan-",month[PM_max,1], " 2025",sep=""),
           color="darkred",
           size=3)+
  theme(axis.text.y = element_text(size = 6))+
  # theme(axis.text.x =element_text(size = 6))+
  theme(axis.title.y = element_text(size = 8,face = "bold"))+
  # theme(axis.title.x =element_text(size = 8,face = "bold"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(strip.text.x = element_text(face = "bold"))+
  expand_limits(x= c(x_min,x_max))+
  # expand_limits(y= c(0,150))+
  scale_x_continuous(limits = c(x_min,
                                x_max),
                     breaks = seq(x_min, x_max,by = 2500))



final_bs_n

# ggsave("./docs/images/final_bs_n_uni.png", width = 8, height = 8, units = "in", device='png', dpi=700)
# 


ggsave(paste(save_dir,"/","final_bs_n_uni.png",sep=""),
       width = 8, height =8, units = "in",
       device='png', dpi=700)  



# 

# Percent < 2025


obs<-c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)


n_runs_less_than_obs<-bs_output_run_cml %>% 
  # filter(PM==PM_max) %>% 
  ungroup() %>% 
  select(cumul_pubs) %>% 
  arrange(cumul_pubs) %>% 
  mutate(less_than_obs=if_else(cumul_pubs<as.numeric(obs),TRUE,FALSE)) 

n_runs_less_than_obs<-n_runs_less_than_obs%>% tally(less_than_obs)


percent_below<-n_runs_less_than_obs/runs*100




# LESS conservative bootstrap (any month selected) ------------------------


data_for_boot_2<-mo_data %>% 
  filter(PY!=2025)



set.seed(20250111)
runs <- 1000

yr<-1 # can set this up so that each run does X years) 
run_no<-seq(1:runs)
yr_no<-seq(1:yr)
bs_data<-list()


for (i in run_no){
  yrs_df<-list()  
  run_data_all<-data.frame()
  
  
  for (j in yr_no){
    run_data<-
      data_for_boot_2 %>% 
      group_by(uni) %>% 
      # group_by(PM) %>% 
      slice_sample(n=PM_max,replace=TRUE) %>% 
      arrange(uni) %>% 
      mutate(yr=j) %>% 
      group_by(yr,uni) %>% 
      mutate(cum_uni=cumsum(n))
    run_data_all<-bind_rows(run_data,run_data_all)
    # run_data<- yrs_df%>% map(~as_tibble(.)) %>% bind_rows(.id="index")
  }
  
  
  bs_data[[i]] <-  run_data_all
}

bs_output_less_conservative<-bs_data %>% 
  map(~as_tibble(.)) %>% 
  bind_rows(.id="run") %>% 
  mutate(run=as.integer(run)) %>% 
  group_by(run,yr,uni) %>% 
  arrange(run,yr,uni) %>% 
  rename(PY_selected=PY,
         PM_selected=PM) %>% 
  group_by(run,yr,uni) %>% 
  
  mutate(PM_bs=row_number()) %>% 
  select(run,PM_bs,PY_selected,PM_selected,uni,n,cum_uni)


# write_csv(bs_output_less_conservative, "./data_clean/bootstrap_output_less_conservative_uni.csv")
write_csv(bs_output_less_conservative,paste("./data_clean/bs_output_less_conservative_uni_",date,".csv",sep=""))
# bs_output_less_conservative<-read_csv("./data_clean/bootstrap_output_less_conservative.csv")




bs_output_run_cml_less_conservative<-
  bs_output_less_conservative %>% 
  filter(PM_bs==PM_max) %>% 
  ungroup() %>% 
  group_by(run,yr) %>% 
  mutate(cumul_pubs=sum(cum_uni)) %>% 
  select(run,yr,cumul_pubs) %>% 
  distinct()

# 
#   filter(cum_uni==max(cum_uni)) %>% 
#   select(run,yr,uni,cum_uni) %>% 
#   group_by(run) %>% 
#   mutate(cumul_pubs=sum(cum_uni)) %>% 
#   select(run,cumul_pubs) %>% 
#   group_by(run) %>% 
#   slice_head(n=1)
# write_csv(bs_output_run_cml_less_conservative, "./data_clean/bs_output_run_cml_less_conservative.csv")

# bs_output_run_cml<-read_csv("./data_clean/bs_output_run_cml.csv")

bs_stats_2<-bs_output_run_cml_less_conservative %>% 
  ungroup() %>% 
  # select(-yr,-index) %>% 
  # filter(PM_bs==PM_max) %>% 
  # group_by(run) %>% 
  # mutate(cumul_pubs=sum(cum_uni)) %>% 
  # select(cumul_pubs) %>% 
  # distinct() %>% 
  # ungroup() %>% 
  summarize(mean_bs_c_pubs=mean(cumul_pubs),
            median_bs_c_pubs=median(cumul_pubs),
            max_bs_c_pubs=max(cumul_pubs),
            min_bs_c_pubs=min(cumul_pubs)) 

# %>% 
# 
#   left_join(avg_cum_obs) %>% 
#   left_join(c_pubs_25)

bs_stats
avg_cum_obs
c_pubs_25

x_min<-c_pubs_25 %>% 
  filter(PM==PM_max) %>% 
  select(n_c_25) %>% 
  mutate(n_c_25=as.numeric(n_c_25-2000)) %>% 
  mutate(n_c_25=round(n_c_25,-3))
x_min<-as.numeric(x_min)


x_max<-bs_output_run_cml_less_conservative %>% 
  ungroup() %>% 
  select(cumul_pubs) %>% 
  filter(cumul_pubs==max(cumul_pubs)) %>% 
  mutate(cumul_pubs=as.numeric(cumul_pubs+2000)) %>% 
  mutate(cumul_pubs=round(cumul_pubs,-3))
x_max<-as.numeric(x_max)

final_bs_n_less_conservative<-bs_output_run_cml_less_conservative %>% 
  # filter(PM==PM_max) %>% 
  ggplot(aes(cumul_pubs)) +
  geom_histogram(color="darkgray", 
                 fill="white",
                 bins=50)+
  theme_classic()+
  labs(x = paste("Cumulative publications (N = ",runs," Bootstrap Runs)",sep=""), size=5)+
  labs(y = "No. of Bootstrap runs", size=5)+
  geom_segment(x = as.numeric(c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)), 
               y= 0 , 
               xend = as.numeric(c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)), 
               yend = 100, 
               colour = "darkred",
               linetype = 2,
               linewidth = 1)+
  annotate(geom="text", x=as.numeric(c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)),
           y=110,
           label=paste("No. Publications\n","Jan-",month[PM_max,1], " 2025",sep=""),
           color="darkred",
           size=3)+
  theme(axis.text.y = element_text(size = 6))+
  theme(axis.text.x = element_text(size = 6))+
  theme(axis.title.y = element_text(size = 8,face = "bold"))+
  theme(axis.title.x =element_text(size = 8,face = "bold"))+
  theme(strip.text.x = element_text(face = "bold"))+
  expand_limits(x= c(x_min,x_max))+
  expand_limits(y= c(0,150))+
  scale_x_continuous(limits = c(x_min,
                                x_max),
                     breaks = seq(x_min, x_max,by = 2500))



final_bs_n_less_conservative


# ggsave("./docs/images/final_bs_n_less_conservative_uni.png", width = 8, height = 8, units = "in", device='png', dpi=700)
# 

ggsave(paste(save_dir,"/","final_bs_n_less_conservative_uni.png",sep=""),
       width = 8, height =8, units = "in",
       device='png', dpi=700)  

# Percent < 2025

obs<-c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)


n_runs_less_than_obs<-bs_output_run_cml %>% 
  # filter(PM==PM_max) %>% 
  ungroup() %>% 
  select(cumul_pubs) %>% 
  mutate(less_than_obs=cumul_pubs<obs) %>% 
  tally(less_than_obs)

percent_below<-n_runs_less_than_obs/runs*100

n_runs_less_than_obs_2<-bs_output_run_cml_less_conservative %>% 
  arrange(cumul_pubs) %>% 
  # filter(PM==PM_max) %>% 
  ungroup() %>% 
  select(cumul_pubs) %>% 
  mutate(less_than_obs=if_else(cumul_pubs<as.numeric(obs),TRUE,FALSE)) 


n_runs_less_than_obs_2<-n_runs_less_than_obs_2 %>% tally(less_than_obs)

percent_below<-n_runs_less_than_obs/runs*100

percent_below_2<-n_runs_less_than_obs_2/runs*100



bs_stats<-bs_stats %>% 
  mutate(bs_cat="strict",
         perc_below_obs=percent_below$n)


bs_stats_2<-bs_stats_2%>% 
  mutate(bs_cat="open",
         perc_below_obs=percent_below_2$n)

bs_stats_all<-bind_rows(bs_stats,bs_stats_2)

# write_csv(bs_stats_all,"./docs/summary_info/bs_stats_all_uni.csv")
write_csv(bs_stats_all,paste(save_dir,"/","bs_stats_all_uni.csv",sep=""))




bs_composite_fig<-plot_grid(final_bs_n, final_bs_n_less_conservative, labels=c("A", "B"), ncol = 1, nrow = 2)

# 
# ggsave("./docs/images/bs_composite_fig_uni.png", width = 4, height = 6, units = "in", device='png', dpi=700)
# 


ggsave(paste(save_dir,"/","bs_composite_fig_uni.png",sep=""),
       width = 4, height = 6, units = "in",
       device='png', dpi=700)  

