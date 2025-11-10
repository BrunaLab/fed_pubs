make_figs_uni <- function(cat, date,PM_max) {

  library(tidyverse)
library(janitor)
library(data.table)





# 
# cat<-"uni"
# date<-"20250901"

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



# max month to plot -------------------------------------------------------


# load data  --------------------------------------------------------------

# affil_info<-read_csv("./data_clean/api_uni_affils_searched_2025-10-18.csv") %>% 
#   mutate_all(tolower) %>% 
#   mutate(affil_id=as.integer(affil_id))

# Affiliations
affils_df  <- setDT(read_rds(paste("./data_clean/affils_df_clean_",cat,"_",date,".rds",sep="")))


# papers_df  <- setDT(read_rds("./data_clean/papers_df_uni_clean.rds")) 
papers_df  <- setDT(read_rds(paste("./data_clean/papers_df_clean_",cat,"_",date,".rds",sep="")))
# %>% 
#   mutate(PM=
#            case_when(
#              is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
#              .default = as.numeric(PM)
#            )
#   ) %>% 
#   mutate(PT=if_else(PT=="j","journal",PT))

# papers_df %>% filter(is.na(PM))

# authors_df <- setDT(read_rds("./data_clean/authors_df_uni_clean.rds")) %>% 
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
authors_df<-authors_df %>% 
  filter(refID%in%papers_df$refID)

unique(authors_df$uni)

# this calclulates how many papers have authors from each institution
papers_by_uni<-authors_df %>% 
  select(refID,uni,author_order) %>% 
  group_by(refID) %>% 
  count(uni) %>% 
  pivot_wider(names_from = uni, values_from = n) %>% 
replace(is.na(.), 0)


# can now count how many papers by university (note - no fractional authorship)
counts<-papers_by_uni %>% 
  ungroup() %>% 
  select(-refID) %>% 
  summarise(across(everything(), ~ sum(. > 0))) %>% 
  pivot_longer(everything(),names_to="uni", values_to = "total_papers")

# write_csv(counts,"./docs/summary_info/total_papers_by_uni.csv")
write_csv(counts,paste(save_dir,"/","total_papers_by_uni.csv",sep=""))



# save analysis dfs -------------------------------------------------------
# 
# write_rds(papers_df,"./data_clean/papers_df_analysis_uni.rds")
# 
# write_rds(authors_df,"./data_clean/authors_df_analysis_uni.rds")



# START HERE IF REMAKING FIGURES ------------------------------------------
# 
# papers_df<-read_rds("./data_clean/papers_df_analysis_uni.rds")
# authors_df<-read_rds("./data_clean/authors_df_analysis_uni.rds")


# PM_max<-6 # june
# PM_max<-8 # july
# PM_max<-8 # aug

PY_max<-2025


# papers by article category
papers_by_cat_uni<-
  papers_df %>% 
  group_by(DT) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n))

# write_csv(papers_by_cat_uni,"./docs/summary_info/papers_by_cat_uni.csv")
write_csv(papers_by_cat_uni,paste(save_dir,"/","papers_by_cat_uni.csv",sep=""))


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
  summarize(focal=sum(uni!="other"),
            nonFocal=sum(uni=="other"),
            total=sum(focal+nonFocal)) %>% 
  filter(focal!=0) 



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

scopus_id_initial<-affils_df %>% 
  filter(cat=="original") %>% 
    distinct(affil_id,.keep_all = TRUE) %>%
    select(affil_id) %>%
    distinct() %>%
    summarize(n=n_distinct(affil_id))
  
scopus_id_initial


# total number of scopus IDs in the follow-up search
# scopus_id_followup<-read_csv("./data_raw/affiliations_to_search/uni_affils/follow_up/all_uni_affils_searched.csv") %>% 
#   select(affil_id) %>% 
#   distinct() %>% 
#   summarize(n=n_distinct(affil_id))


scopus_id_followup<-affils_df %>% 
  filter(cat=="followup") %>% 
  distinct(affil_id,.keep_all = TRUE) %>%
  select(affil_id) %>%
  distinct() %>%
  summarize(n=n_distinct(affil_id))
scopus_id_followup

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
   total_NOTfocals)

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


# write_csv(pubs_uni,"./docs/summary_info/uni_total_pubs_per_uni_first.csv")
write_csv(pubs_uni,paste(save_dir,"/","uni_total_pubs_per_uni_first.csv",sep=""))



source("code/figs_uni/total_pubs_per_year.R")
pubs_yr_fig<-total_pubs_per_year(pubs_yr,2024)

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
write_csv(as.data.frame(pubs_mo_fig_cumulative_all_uni[1]),paste(save_dir,"/","cumulative_pubs_monthly_uni.csv",sep=""))
write_csv(as.data.frame(pubs_mo_fig_cumulative_all_uni[2]),paste(save_dir,"/","perc_change_uni.csv",sep=""))
pubs_mo_cum_fig_uni<-pubs_mo_fig_cumulative_all_uni[3]

ggsave(paste(save_dir,"/","pubs_mo_cum_fig_uni.png",sep=""),
       width = 6, height = 5, units = "in",
       device='png', dpi=700)  





source("code/figs_uni/pubs_per_month_cumulative_by_uni.R")
pubs_mo_fig_cumulative_by_uni<-pubs_per_month_cumulative_by_uni(papers_dataset,authors_data_set,2025,PM_max)

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




journals_first <- papers_with_focaluni_first %>%
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
# write_csv(jrnls_overall_first,"./docs/summary_info/uni_jrnls_overall_first.csv")
write_csv(jrnls_overall_first,paste(save_dir,"/","uni_jrnls_overall_first.csv",sep=""))

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



# write_csv(journals_n_perc_annual_first,"./docs/summary_info/uni_journals_n_perc_annual_first.csv")
write_csv(journals_n_perc_annual_first,paste(save_dir,"/","uni_journals_n_perc_annual_first.csv",sep=""))

}