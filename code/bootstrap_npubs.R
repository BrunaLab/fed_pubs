
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

PM_max<-7

papers_df_analysis  <- setDT(read_rds("./data_clean/papers_df_analysis.rds")) %>% 
  mutate(PM=
           case_when(
             is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
             .default = as.numeric(PM)
           )
  ) %>% 
  mutate(PT=if_else(PT=="j","journal",PT))


# make rds of papers with fed 1st author ----------------------------------



# papers_df %>% filter(is.na(PM))

authors_df_analysis <- setDT(readRDS("./data_clean/authors_df_analysis.rds")) %>% 
  mutate(federal=if_else(is.na(federal),FALSE,federal))



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

papers_df <- papers_df_analysis %>% 
  filter(DT!="editorial") %>% 
  filter(DT!="letter") 

authors_df<-authors_df_analysis %>% 
  filter(refID%in%papers_df$refID)




# make rds of papers with fed 1st author ----------------------------------


first_authors <- authors_df %>%
  remove_empty(c("rows","cols")) %>% 
  filter(federal == TRUE) %>%
  filter(author_order == 1) %>% 
  distinct()

papers_with_fed_first<-papers_df %>% 
  filter(refID%in%first_authors$refID) %>% 
  distinct(scopus_article_id,.keep_all=TRUE) 


PY_for_authors_df<-papers_with_fed_first %>% 
  select(refID,PY,PM) 
first_authors <- first_authors %>% 
  left_join(PY_for_authors_df)

all_authors_df_for_fed_1st_papers<-authors_df %>% 
  filter(refID%in%first_authors$refID) %>% 
  left_join(PY_for_authors_df)



# set focal datasets ------------------------------------------------------

papers_dataset<-papers_with_fed_first
authors_data_set<-first_authors


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






first_authors<-first_authors %>% 
  drop_na(PY)


mo_data<-first_authors %>% 
  group_by(PY,PM,agency_primary) %>% 
  # group_by(PY,PM) %>% 
  tally() %>% 
  arrange(PM,agency_primary) %>% 
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
      group_by(agency_primary,PM) %>% 
    # group_by(PM) %>% 
    slice_sample(n=1,replace=TRUE) %>% 
      mutate(yr=j) %>% 
      group_by(yr,agency_primary) %>% 
      mutate(cum_agency=cumsum(n))
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
  arrange(run,yr,PM) 

write_csv(bs_output, "./data_clean/bootstrap_output.csv")
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
  mutate(cumul_pubs=sum(cum_agency)) %>% 
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
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.text.x =element_text(size = 10))+
  theme(axis.title.y = element_text(size = 12,face = "bold"))+
  theme(axis.title.x =element_text(size = 12,face = "bold"))+
  theme(strip.text.x = element_text(face = "bold"))+
  expand_limits(x= c(x_min,x_max))+
  # expand_limits(y= c(0,150))+
  scale_x_continuous(limits = c(x_min,
                                x_max,
                                breaks = seq(x_min, x_max,by = 1000)))



final_bs_n

ggsave("./docs/images/final_bs_n.png", width = 8, height = 8, units = "in", device='png', dpi=700)
# 

# Percent < 2025


obs<-c_pubs_25 %>% filter(PM==PM_max) %>% select(n_c_25)


n_runs_less_than_obs<-bs_output_run_cml %>% 
  # filter(PM==PM_max) %>% 
  ungroup() %>% 
  select(cumul_pubs) %>% 
  mutate(less_than_obs=if_else(cumul_pubs<as.numeric(obs),TRUE,FALSE)) 


n_runs_less_than_obs<-n_runs_less_than_obs %>% tally(less_than_obs)


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
      group_by(agency_primary) %>% 
      # group_by(PM) %>% 
      slice_sample(n=PM_max,replace=TRUE) %>% 
      arrange(agency_primary) %>% 
      mutate(yr=j) %>% 
      group_by(yr,agency_primary) %>% 
      mutate(cum_agency=cumsum(n))
    run_data_all<-bind_rows(run_data,run_data_all)
    # run_data<- yrs_df%>% map(~as_tibble(.)) %>% bind_rows(.id="index")
  }
  
  
  bs_data[[i]] <-  run_data_all
}

bs_output_less_conservative<-bs_data %>% 
  map(~as_tibble(.)) %>% 
  bind_rows(.id="run") %>% 
  mutate(run=as.integer(run)) %>% 
  group_by(run,yr,agency_primary) %>% 
  arrange(run,yr,agency_primary) %>% 
  rename(PY_selected=PY,
         PM_selected=PM) %>% 
  group_by(run,yr,agency_primary) %>% 
  
  mutate(PM_bs=row_number()) %>% 
  select(run,PM_bs,PY_selected,PM_selected,agency_primary,n,cum_agency)
  

write_csv(bs_output_less_conservative, "./data_clean/bootstrap_output_less_conservative.csv")
bs_output_less_conservative<-read_csv("./data_clean/bootstrap_output_less_conservative.csv")




bs_output_run_cml_less_conservative<-
  bs_output_less_conservative %>% 
  filter(PM_bs==PM_max) %>% 
  ungroup() %>% 
  group_by(run,yr) %>% 
  mutate(cumul_pubs=sum(cum_agency)) %>% 
  select(run,yr,cumul_pubs) %>% 
  distinct()
  
# 
#   filter(cum_agency==max(cum_agency)) %>% 
#   select(run,yr,agency_primary,cum_agency) %>% 
#   group_by(run) %>% 
#   mutate(cumul_pubs=sum(cum_agency)) %>% 
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
  # mutate(cumul_pubs=sum(cum_agency)) %>% 
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
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.text.x =element_text(size = 10))+
  theme(axis.title.y = element_text(size = 12,face = "bold"))+
  theme(axis.title.x =element_text(size = 12,face = "bold"))+
  theme(strip.text.x = element_text(face = "bold"))+
  expand_limits(x= c(x_min,x_max))+
  expand_limits(y= c(0,150))+
  scale_x_continuous(limits = c(x_min,
                                x_max,
                                breaks = seq(x_min, x_max,by = 1000)))



final_bs_n_less_conservative


ggsave("./docs/images/final_bs_n_less_conservative.png", width = 8, height = 8, units = "in", device='png', dpi=700)
# 

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

write_csv(bs_stats_all,"./docs/summary_info/bs_stats_all.csv")





bs_composite_fig<-plot_grid(final_bs_n, final_bs_n_less_conservative, labels=c("A", "B"), ncol = 1, nrow = 2)


ggsave("./docs/images/bs_composite_fig.png", width = 8, height = 8, units = "in", device='png', dpi=700)


#' 
#' 
#' 
#' 
#' # plot of cumulative pubs BS ----------------------------------------------
#' 
#' 
#'   # 
#'   # 
#'   # 
#'   # mutate(data_mean=14000,
#'   #        data_min=12950) %>% 
#'   # mutate(data_dif=data_mean-data_min)
#'   # 
#'   # # mean = 14000
#'   # # 2025 10335
#'   # # 2024 12950
#'   # 
#' 
#' 
#' 
#' # 
#' # boot_fig<-
#' #   bs_output %>% 
#' #   mutate(label = if_else(PM == max(PM), as.character(index), NA_character_)) %>% 
#' #   ggplot(aes(x=PM, y=cumul_pubs,group=index,color=index)) +
#' #   labs(x = "Month", size=5)+
#' #   labs(y = "No. of Publications", size=5)+
#' #   geom_line() + 
#' #   # geom_point(size=0.5)+
#' #   # scale_color_manual(values=c(rep("gray",runs)))+
#' #   # expand_limits(y = 0)+
#' #   expand_limits(x= c(0,length(levels(bs_sums$PM)) + 1.25))+
#' #   theme_classic()+
#' #   # scale_x_continuous( breaks=seq(1,12,by=1))+
#' #   scale_y_continuous(expand = c(0, 5000), breaks=seq(0,(max(bs_sums %>% select(cumul_pubs))+15000),by=2500))+
#' #   theme(axis.text.y = element_text(size = 12))+
#' #   theme(axis.text.x =element_text(size = 12))+
#' #   theme(axis.title.y = element_text(size = 14))+
#' #   theme(axis.title.x =element_text(size = 14))+
#' #   theme(legend.position="none")
#' # boot_fig
#' 
#' 
#' 
#' 
#' # 
#' # # histogram of cumulative pubs month 6 ------------------------------------
#' # 
#' # 
#' # final_bs_n<-bs_output %>% 
#' #   filter(PM==6) %>% 
#' # ggplot(aes(cumul_pubs)) +
#' #   geom_histogram(color="darkgray", 
#' #                  fill="white",
#' #                  bins=50)+
#' #   theme_classic()+
#' #   geom_segment(x = 10335, y= 0 , 
#' #                xend = 10335, yend = 500, 
#' #                colour = "darkred",
#' #                linetype = 2,
#' #                linewidth = 0.05)+
#' #   geom_segment(x = 13980, y= 0 , 
#' #                xend = 13980, yend = 500, 
#' #                colour = "navy",
#' #                linetype = 2,
#' #                linewidth = 0.05)+ 
#' #   
#' #   # ylim(0,100)+
#' #   # xlim(10000,17000)+
#' #   expand_limits(x= c(9000,17000))+
#' #   # scale_x_continuous(limits = c(9000, 17000), breaks = seq(9000, 17000, by = 500))
#' # scale_x_continuous(breaks = seq(9000, 17000, by = 500))
#' # final_bs_n
#' 
#' 
#' 
#' # plot of BS minimum ------------------------------------------------------
#' 
#' 
#' 
#' final_bs_n<-bs_output %>% 
#'   filter(PM==6) %>% 
#'   # arrange(run,desc(cumul_pubs)) %>% 
#'   group_by(run) %>% 
#'   filter(cumul_pubs==min(cumul_pubs))%>% 
#'   ggplot(aes(cumul_pubs)) +
#'   geom_histogram(color="darkgray", 
#'                  fill="white",
#'                  bins=50)+
#'   theme_classic()+
#'   geom_segment(x = 10335, y= 0 , 
#'                xend = 10335, yend = 500, 
#'                colour = "darkred",
#'                linetype = 2,
#'                linewidth = 0.05)+
#'   geom_segment(x = 13980, y= 0 , 
#'                xend = 13980, yend = 500, 
#'                colour = "navy",
#'                linetype = 2,
#'                linewidth = 0.05)+ 
#'   
#'   # ylim(0,100)+
#'   # xlim(10000,17000)+
#'   expand_limits(x= c(9000,17000))+
#'   # scale_x_continuous(limits = c(9000, 17000), breaks = seq(9000, 17000, by = 500))
#'   scale_x_continuous(breaks = seq(9000, 17000, by = 500))
#' final_bs_n
#' 
#' boot_fig<-
#'   bs_stats %>% 
#'   mutate(label = if_else(PM == max(PM), as.character(index), NA_character_)) %>% 
#'   ggplot(aes(x=PM, y=cumul_pubs,group=index,color=index)) +
#'   labs(x = "Month", size=5)+
#'   labs(y = "No. of Publications", size=5)+
#'   geom_line() + 
#'   geom_point(size=0.5)+
#'   # scale_color_manual(values=c(rep("gray",runs)))+
#'   # expand_limits(y = 0)+
#'   expand_limits(x= c(0,length(levels(bs_sums$PM)) + 1.25))+
#'   theme_classic()+
#'   # scale_x_continuous( breaks=seq(1,12,by=1))+
#'   scale_y_continuous(expand = c(0, 5000), breaks=seq(0,(max(bs_sums %>% select(cumul_pubs))+15000),by=2500))+
#'   theme(axis.text.y = element_text(size = 12))+
#'   theme(axis.text.x =element_text(size = 12))+
#'   theme(axis.title.y = element_text(size = 14))+
#'   theme(axis.title.x =element_text(size = 14))+
#'   theme(legend.position="none")
#' boot_fig
#' 
#' final_bs_n<-bs_sums %>% filter(PM==6)
#' ggplot(final_bs_n, aes(cumul_pubs)) +
#'   geom_histogram(color="darkgray", 
#'                  fill="white",
#'                  bins=50)+
#'   theme_classic()+
#'   geom_segment(x = 10335, y= 0 , 
#'                xend = 10335, yend = 50, 
#'                colour = "darkred",
#'                linetype = 2,
#'                linewidth = 0.05)+
#'   geom_segment(x = 12950, y= 0 , 
#'                xend = 12950, yend = 50, 
#'                colour = "navy",
#'                linetype = 2,
#'                linewidth = 0.05)+ 
#'   
#'   # ylim(0,100)+
#'   # xlim(10000,17000)+
#'   expand_limits(x= c(9000,17000))+
#'   scale_x_continuous(limits = c(9000, 17000), breaks = seq(9000, 17000, by = 500))
#' 
#' #   
#' mean_pubs_bs<-bs_output %>% 
#' filter(PM==6) %>% 
#' group_by(run) %>% 
#' summarize(mean_cpubs=mean(cumul_pubs)) 
#' # sample.mean <- mean(final_bs_n$cumul_pubs)
#' # print(sample.mean)
#' # 
#' # sample.n <- length(final_bs_n$cumul_pubs)
#' # sample.sd <- sd(final_bs_n$cumul_pubs)
#' # sample.se <- sample.sd/sqrt(sample.n)
#' # print(sample.se)
#' # 
#' # 
#' # alpha = 0.05
#' # degrees.freedom = sample.n - 1
#' # t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
#' # print(t.score)
#' # 
#' # margin.error <- t.score * sample.se
#' # 
#' # lower.bound <- sample.mean - margin.error
#' # upper.bound <- sample.mean + margin.error
#' # print(c(lower.bound,upper.bound))
#' # # OR
#' # 
#' # t.test(mean_pubs_bs$mean_cpubs)
#' 
#' # mean = 14000
#' # 2025 10335
#' # 2024 12950
#' 
#' 
#' # +
#' #   # annotate(geom="text", x=PM_max+0.5,
#' #   #          y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-1500),
#' #   #          # label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
#' #   #          label=paste(round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,sep=""),
#' #   #          # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
#' #   #          color="black",
#' #   #          size=6)+
#' #   annotate(geom="text", x=PM_max+0.8,
#' #            y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+1800),
#' #            # label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
#' #            # label=paste(round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,sep=""),
#' #            label=paste(round(perc_change$perc_previous[PM_max+1]),"%",sep=""),
#' #            # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
#' #            color="darkgray",
#' #            size=6)+
#' #   # annotate(geom="text", x=PM_max+0.5,
#' #   #          y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-2500),
#' #   #          label=paste("(", round(perc_change_avg),"% from 2019-2024 avg.)",sep=""),
#' #   #          # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
#' #   #          color="black",
#' #   #          size=5)+
#' #   scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative %>% select(cumul_pubs))+3500))+
#' #   geom_segment(aes(xend=PM_max+.02, 
#' #                    yend = (max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))),
#' #                    x = PM_max+.02, 
#' #                    y = (max(perc_change %>% filter(PY==2024) %>% select(cumul_pubs)))),
#' #                linetype="dotted",
#' #                colour = "darkgray",
#' #                arrow = arrow(angle = 20, length = unit(0.1, "inches"),
#' #                              ends = "last", type = "closed")
#' #   )+
#' #   geom_label(aes(label = label), nudge_x = 0.65, size =4,label.size = unit(0,"mm")) +
#' #   theme(plot.background = element_rect(color = 1,
#' #                                        size = 0),
#' #         plot.margin = margin(t = 20,  # Top margin
#' #                              r = 20,  # Right margin
#' #                              b = 20,  # Bottom margin
#' #                              l = 20)  # Left margin
#' #   )
#' 
#' 
#' 
#' 
#'  
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # 
#' # bs_count<-
#' # first_authors %>% 
#' #   filter(PY>2023) %>% 
#' #   filter(PM<7) %>% 
#' #   group_by(PY) %>% 
#' #   tally()
#' # 
#' # n24<-bs_count %>% 
#' #   filter(PY==2024) %>% 
#' #   select(n) 
#' # n24<-as.numeric(n24)
#' # 
#' # 
#' # n25<-bs_count %>% 
#' #   filter(PY==2025) %>% 
#' #   select(n)
#' # n25<-as.numeric(n25)
#' # 
#' # 
#' # 
#' # bs_pubs<-
#' #   first_authors %>% 
#' #   filter(PY>2023) %>% 
#' #   filter(PM<7) %>% 
#' #   select(refID, 
#' #          agency, 
#' #          agency_primary, 
#' #          PY,
#' #          PM)
#' # 
#' # first_authors %>% 
#' #   group_by(affil_id) %>% 
#' #   tally() %>% 
#' #   arrange(desc(n))
#' # 
#' # 
#' # first_authors %>% 
#' #   drop_na(PY) %>% 
#' #   group_by(PY) %>% 
#' #   tally() %>% 
#' #   mutate(perc=n/sum(n)*100)
#' # 
#' # 
#' # total_pubs<-first_authors %>% 
#' #   drop_na(PY) %>% 
#' #   tally()
#' # total_pubs<-as.numeric(total_pubs)
#' # boot_data<-sample_n(first_authors,total_pubs,replace=T) %>% 
#' #   group_by(PY) %>% 
#' #   tally() %>% 
#' #   mutate(perc=n/sum(n)*100)
#' # 
#' # 
#' # first_authors %>% 
#' #   filter(affil_id==60013409) %>% 
#' #   group_by(PY) %>% 
#' #   tally()
#' #   
#' # 
#' # # affil_id=60013409, n=5948
#' # n_no1affil<-sample_n(first_authors,5948) %>% 
#' #   group_by(PY) %>% 
#' #   tally() %>% 
#' #   mutate(perc=n/sum(n)*100)
#' # n_no1affil
#' # 
#' # 
#' 
#' 
#' boot<-sample_n(bs_pubs, n24+n25, replace = TRUE)
#' boot_count<-boot %>% 
#'   group_by(PY) %>% 
#'   summarize(n=n()) %>% 
#'   pivot_wider(values_from=n,names_from=PY,names_prefix="yr") %>% 
#'   mutate(n_diff=yr2024-yr2025) %>% 
#'   mutate(perc_diff=((yr2025-yr2024)/yr2024*100))
#' boot_count
#' 
#' 
#' 
#' 
#' 
#' # bias corrected CI -------------------------------------------------------
#' 
#' #' Bias-correction for bootstrap percentile intervals
#' #'
#' #' Method described in Caswell (2008) ch. 12. (Eqs 12.19--12.22).
#' #'
#' #' @param t0 the parameter estimate
#' #' @param t a vector of bootstrapped parameter estimates
#' #' @param alpha confidence level of the interval
#' #'
#' #' @return a tibble with the original estimate, `t0`, and the lower and upper
#' #'   CIs
#' #'
#' #' @references Caswell, Hal. Matrix Population Models: Construction, Analysis,
#' #'   and Interpretation. 2. ed., Sunderland, Mass: Sinauer Associates, 2008.
#' 
#' #' 
#' #'
#' #' @examples
#' #' tar_load(c(lambda_bt_det_ff, ipm_det_ff))
#' #' bcpi(t0 = avg_at_month_x, t = bs_vals_that_month)
#' 
#' 
#' library(tidyverse)
#' 
#' 
#' 
#' bcpi <- function(t0, t, alpha = 0.05) {
#'   B <- length(t)
#'   z0 <- qnorm(sum(t < t0)/B)
#'   a1 <- pnorm(2 * z0 + qnorm(alpha/2))
#'   a2 <- pnorm(2 * z0 + qnorm(1 - alpha/2))
#'   c1 <- quantile(t, a1)
#'   c2 <- quantile(t, a2)
#'   return(tibble::tibble(est = t0, lower = c1, upper = c2))
#' }
#' 
#' t_all<-read_csv("./data_clean/bootstrap_output.csv") %>% 
#'   group_by(run,PM) %>% 
#'   mutate(avg_cumul=mean(cumul_pubs)) %>% 
#'   select(PM,avg_cumul) %>% 
#'   distinct(avg_cumul)
#'   
#' 
#' monthly_cum_pubs<-read_csv("./data_clean/cumulative_pubs_monthly.csv")
#' 
#' run_no<-seq(1:6)
#' 
#' ci_pm_df<-tibble()
#' for (i in run_no){
#'   
#'   
#'   
#'   t<-t_all %>% 
#'     filter(PM==i)  
#'   t<-t$avg_cumul
#'   
#'   t0<-monthly_cum_pubs %>% 
#'     filter(PM==i) %>% 
#'     filter(PY=="Avg. (all yrs)") %>% 
#'     select(cumul_pubs)
#'     t0<-as.numeric(t0)
#'   
#'     output<-bcpi(t0,t,alpha = 0.05)
#'     output<-output %>% mutate(PM=i)
#'     ci_pm_df<-bind_rows(ci_pm_df,output)
#'   }
#' 
#' c_pubs_25_fig<-c_pubs_25 %>% 
#'   ungroup() %>% 
#'   rename(n=n_c_25) %>% 
#'   select(-PY,-n25) %>% 
#'   mutate(cat="2025")
#'   
#' 
#' bs_output_fig<-bs_output %>% 
#'   ungroup() %>% 
#'   select(-n,-PY,-yr,-index) %>% 
#'   rename(n=cumul_pubs) %>% 
#'   mutate(cat="boot")
#' 
#' 
#' ci_pm_df<-ci_pm_df %>% 
#'   rename(n=est) %>% 
#'   mutate(cat="est") %>% 
#'   ci_pm_df<-ci_pm_df %>% 
#'   bind_rows(c_pubs_25_fig) %>% 
#'   bind_rows(bs_output_fig)
#' 
#' 
#' 
#' 
#' fig<-
#'   ci_pm_df %>%
#'   ggplot(aes(x=PM, y=n,group=run,color=cat)) +
#'   scale_color_manual(values=c("#36648B","#8B0000"))+
#'   labs(x = "Month", size=5)+
#'   labs(y = "Cumulative No. of Publications", size=5)+
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70")+
#'   geom_line() + 
#'   
#'   geom_point(size=0.5) +
#'   theme_classic()+
#'   # scale_x_continuous( breaks=seq(1,12,by=1))+
#'   # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative %>% filter(PM<=PM_max) %>% select(cumul_pubs))+2000),by=2500))+
#'   theme(axis.text.y = element_text(size = 12))+
#'   theme(axis.text.x =element_text(size = 12))+
#'   theme(axis.title.y = element_text(size = 14))+
#'   theme(axis.title.x =element_text(size = 14))+
#'   scale_x_continuous( breaks=seq(1,6,by=1))+
#'   scale_y_continuous(expand = c(0, 0), breaks=seq(0,17000,by=500))
#' 
#' 
