lag_test <- function() {

# library(janitor)
library(tidyverse)
library(fs)
library(data.table)
library(viridis)
library(gghighlight)

   
papers_fed<-read_rds("data_clean/papers_df_clean_fed_20260101.rds") %>% 
  mutate(cat="fed")
papers_uni<-read_rds("data_clean/papers_df_clean_uni_20260101.rds") %>% 
  mutate(cat="uni")

papers_cat<-c("article",
              "book chapter",
              # "data paper",
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
papers_all<-bind_rows(papers_fed,papers_uni)

papers_all <- papers_all %>%
  filter(PY!=2025) %>% 
  filter(DT%in%papers_cat) %>% # KEEP the categories
  filter(!TI%in%papers_title) # EXCLUDE the titles






  
time_df<-expand_grid(PY=seq(2015,2026,by=1),PM=seq(1,12,by=1)) %>% 
mutate(counter_pub=row_number()) 



time_df2<-expand_grid(upload_yr=seq(2015,2026,by=1),upload_mo=seq(1,12,by=1)) %>% 
  mutate(counter_lag=row_number()) 


lag_data<-papers_all %>% 
  select(DT,SO,PY,PM,original_source_file,refID,cat) %>% 
  mutate(original_source_file_tosplit=original_source_file) %>%
  mutate(DT=factor(DT, ordered = TRUE, 
                   levels = c("article", "review", "note","book chapter"))) %>% 

  separate_wider_delim(original_source_file_tosplit, 
                     "_", 
                     names = c("affil_id","upload_yr","upload_mo"), 
                     too_many = "merge",
                     too_few="align_start") %>% 
  filter(affil_id!="usgs") %>% 
  filter(!is.na(upload_mo)) %>% 
  mutate(upload_yr=as.numeric(upload_yr)) %>% 
  mutate(upload_mo=as.numeric(upload_mo)) %>% 
  left_join(time_df) %>% 
  relocate(counter_pub,.after=PM) %>% 
  left_join(time_df2) %>% 
  relocate(counter_lag,.after=upload_mo) %>% 
  mutate(lag=if_else(upload_yr<=PY,0,NA)) %>% 
  mutate(lag=if_else(upload_yr==PY+1,upload_mo,lag)) %>% 
  mutate(lag=if_else(upload_yr==PY+2,12+upload_mo,lag)) %>% 
  mutate(lag=if_else(upload_yr==PY+3,24+upload_mo,lag)) %>% 
  mutate(lag=if_else(upload_yr==PY+4,36+upload_mo,lag)) %>% 
  mutate(lag=if_else(upload_yr==PY+5,48+upload_mo,lag)) %>% 
  mutate(lag=if_else(upload_yr==PY+6,60+upload_mo,lag)) %>% 
  mutate(lag2=counter_lag-counter_pub) %>% 
  relocate(c(original_source_file,
             affil_id,
             refID, 
             PY,
             PM,
             upload_yr,
             upload_mo,
             lag,
             lag2
             ),.before=1) 
  

rm(papers_fed,papers_uni)

summary_counts<-lag_data %>% 
  group_by(DT) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100)

# LAG BY DT & YEAR ALL AFFILS COMBINED

counts_per_PY<-lag_data %>% 
  group_by(DT,PY) %>% 
  tally() %>% 
  replace_na(list(n=0)) %>% 
  rename(n_PY=n)


PY_counts_per_lag<-lag_data %>% 
  group_by(DT,PY,lag) %>% 
  tally() %>% 
  rename(n_lag=n) %>% 
  replace_na(list(n_lag=0)) %>% 
  left_join(counts_per_PY,by=c("DT","PY")) %>% 
  mutate(perc=n_lag/n_PY*100) %>% 
  group_by(DT,PY) %>% 
  mutate(cumul_perc=cumsum(perc)) %>% 
  mutate(cumul_perc=round(cumul_perc,2))





# PLOT
# plot_data<-affil_counts_per_lag
plot_data<-PY_counts_per_lag

plot_data %>% group_by(DT) %>% summarize(n=sum(n_lag)) %>% mutate(perc=n/sum(n)*100)

lag_plot<-plot_data %>% 
  filter(PY!=2025) %>% 
  mutate(PY=as.factor(PY)) %>%
  ggplot(aes(x=lag, y=cumul_perc,group=PY,color=PY)) +
  scale_color_viridis(discrete = TRUE,option="A", begin = 0.2, end = 0.9) +
  # scale_color_manual(values=c(rep("lightgray",4),"#36648B","#8B0000"))+
  labs(x = "Lag (Months after PY)", size=5)+
  labs(y = "Cumulative percentage of papers downloaded in January 2025", size=5)+
  geom_line() + 
  facet_wrap(~DT,  ncol=1)+
  # geom_point()+
  expand_limits(y = 0)+
  theme_classic()+
  # scale_x_continuous( breaks=seq(1,12,by=1))+
  # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo %>% select(n))+5000),by=2500))+
  theme(axis.text.y = element_text(size = 8))+
  theme(axis.text.x =element_text(size = 8))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.title.x =element_text(size = 10))+
  # scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo %>% select(n))+500))+
  scale_y_continuous(n.breaks = 10, limits = c(90,100))+
  # scale_x_continuous(n.breaks = 20, limits = c(0,60))+
  scale_x_continuous(breaks = seq(0,60,by=3), limits = c(0,60))+
  geom_hline(yintercept = 98, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_hline(yintercept = 96, linetype = "dashed", color = "darkgray", size = 0.5)+
  # geom_hline(yintercept = 94, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 12, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 3, linetype = "dashed", color = "darkgray", size = 0.5)
  # geom_label(aes(label = PY))
  # geom_label(data = plot_data %>% filter(cumul_perc>99.99999), aes(label = as.numeric(PY))
           # position="jitter",
           # nudge_y = 0.8,
           # nudge_x = 0.55,
           # size =3.5,
           # fill=NA,
           # border.color = "white")
# label.size = unit(0,"mm")
# gghighlight(min(n) < 50)
# +gghighlight(PY==2020, line_label_type = c("ggrepel_text"))
lag_plot
summary_counts













PY_counts_per_lag2<-lag_data %>% 
  group_by(DT,PY,lag2) %>% 
  tally() %>% 
  rename(n_lag=n) %>% 
  replace_na(list(n_lag=0)) %>% 
  left_join(counts_per_PY,by=c("DT","PY")) %>% 
  mutate(perc=n_lag/n_PY*100) %>% 
  group_by(DT,PY) %>% 
  mutate(cumul_perc=cumsum(perc)) %>% 
  mutate(cumul_perc=round(cumul_perc,2)) %>% 
  filter(cumul_perc!=0)


lag_plot2<-PY_counts_per_lag2 %>% 
  filter(PY!=2025) %>% 
  mutate(PY=as.factor(PY)) %>%
  ggplot(aes(x=lag2, y=cumul_perc,group=PY,color=PY)) +
  scale_color_viridis(discrete = TRUE,option="A", begin = 0.2, end = 0.9) +
  # scale_color_manual(values=c(rep("lightgray",4),"#36648B","#8B0000"))+
  labs(x = "Lag (Months after PY)", size=5)+
  labs(y = "Cumulative percentage of papers downloaded in January 2025", size=5)+
  geom_line() + 
  facet_wrap(~DT,  ncol=1)+
  # geom_point()+
  expand_limits(y = 0)+
  theme_classic()+
  # scale_x_continuous( breaks=seq(1,12,by=1))+
  # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo %>% select(n))+5000),by=2500))+
  theme(axis.text.y = element_text(size = 8))+
  theme(axis.text.x =element_text(size = 8))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.title.x =element_text(size = 10))+
  # scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo %>% select(n))+500))+
  scale_y_continuous(n.breaks = 10, limits = c(0,100))+
  # scale_x_continuous(n.breaks = 20, limits = c(0,60))+
  scale_x_continuous(breaks = seq(-15,75,by=10), limits = c(-15,75))+
  geom_hline(yintercept = 98, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_hline(yintercept = 96, linetype = "dashed", color = "darkgray", size = 0.5)+
  # geom_hline(yintercept = 94, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 12, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 3, linetype = "dashed", color = "darkgray", size = 0.5)
# geom_label(aes(label = PY))
# geom_label(data = plot_data %>% filter(cumul_perc>99.99999), aes(label = as.numeric(PY))
# position="jitter",
# nudge_y = 0.8,
# nudge_x = 0.55,
# size =3.5,
# fill=NA,
# border.color = "white")
# label.size = unit(0,"mm")
# gghighlight(min(n) < 50)
# +gghighlight(PY==2020, line_label_type = c("ggrepel_text"))
lag_plot2






# Create a separate data frame with custom labels and their positions

lag_plot

write_csv(lag_data,"data_clean/lag_test/lag_data.csv")
ggsave("data_clean/lag_test/lag_plot.png",
       width = 6, height = 8, units = "in",
       device='png', dpi=700)  



return(list(PY_counts_per_lag,lag_plot))

}






# LAG BY DT & YEAR ALL AFFILS COMBINED

counts_per_PY_SO<-lag_data %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  replace_na(list(n=0)) %>% 
  rename(n_PY=n)


PY_counts_per_lag_SO<-lag_data %>% 
  group_by(SO,PY,lag) %>% 
  tally() %>% 
  rename(n_lag=n) %>% 
  replace_na(list(n_lag=0)) %>% 
  left_join(counts_per_PY,by=c("SO","PY")) %>% 
  mutate(perc=n_lag/n_PY*100) %>% 
  group_by(SO,PY) %>% 
  mutate(cumul_perc=cumsum(perc)) %>% 
  mutate(cumul_perc=round(cumul_perc,2))


top_jrnls_2022<-counts_per_PY_SO %>% 
  filter(PY==2022) %>% 
  arrange(desc(n_PY)) %>% 
  ungroup() %>%
  slice_head(n=1)


PY_counts_per_lag_SO_plot <-
PY_counts_per_lag_SO %>% 
  filter(PY==2022) %>% 
  filter(SO%in%top_jrnls_2022$SO) %>% 
  arrange(SO,lag)




# PLOT
# plot_data<-affil_counts_per_lag
plot_data<-PY_counts_per_lag_SO_plot

plot_data %>% 
  group_by(SO) %>% 
  summarize(n=sum(n_lag)) %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(perc))

lag_plot_SO<-plot_data %>% 
  mutate(PY=as.factor(PY)) %>%
  ggplot(aes(x=lag, y=cumul_perc,group=SO,color=SO)) +
  scale_color_viridis(discrete = TRUE,option="A", begin = 0.2, end = 0.9) +
  # scale_color_manual(values=c(rep("lightgray",4),"#36648B","#8B0000"))+
  labs(x = "Lag (Months after PY)", size=5)+
  labs(y = "Cumulative percentage of papers downloaded in January 2025", size=5)+
  geom_line() + 
  # facet_wrap(~SO,  ncol=5)+
  # geom_point()+
  expand_limits(y = 0)+
  theme_classic()+
  # scale_x_continuous( breaks=seq(1,12,by=1))+
  # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo %>% select(n))+5000),by=2500))+
  theme(axis.text.y = element_text(size = 8))+
  theme(axis.text.x =element_text(size = 8))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.title.x =element_text(size = 10))+
  # scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo %>% select(n))+500))+
  scale_y_continuous(n.breaks = 10, limits = c(0,100))+
  # scale_x_continuous(n.breaks = 20, limits = c(0,60))+
  scale_x_continuous(breaks = seq(0,33,by=3), limits = c(0,33))+
  theme(legend.position = "none")+
  geom_hline(yintercept = 100, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_hline(yintercept = 98, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_hline(yintercept = 96, linetype = "dashed", color = "darkgray", size = 0.5)+
  # geom_hline(yintercept = 94, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 12, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 6, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 3, linetype = "dashed", color = "darkgray", size = 0.5)+
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray", size = 0.5)

lag_plot_SO
# 
# 
# # lag by affil and year ---------------------------------------------------
# 
# # LAG BY AFFIL ID
# 
# counts_per_affil<-lag_data %>% 
#   group_by(DT,affil_id,PY) %>% 
#   tally() %>% 
#   replace_na(list(n=0)) %>% 
#   rename(n_PY=n)
# 
# 
# affil_counts_per_lag<-lag_data %>% 
#   group_by(DT,cat,affil_id,PY,lag) %>% 
#   tally() %>% 
#   rename(n_lag=n) %>% 
#   replace_na(list(n_lag=0)) %>% 
#   left_join(counts_per_affil,by=c("affil_id","PY","DT")) %>% 
#   mutate(perc=n_lag/n_PY*100) %>% 
#   group_by(affil_id,PY,DT) %>% 
#   mutate(cumul_perc=cumsum(perc))
# 
# # this is to plot by affiliation, need to add
# 
# counts_per_PY_affil<-lag_data %>%
#   group_by(affil_id,DT,PY) %>%
#   tally() %>%
#   replace_na(list(n=0)) %>%
#   rename(n_PY=n)
# 
# PY_counts_per_lag_affil<-lag_data %>%
#   group_by(DT,PY,affil_id,lag) %>%
#   tally() %>%
#   rename(n_lag=n) %>%
#   replace_na(list(n_lag=0)) %>%
#   left_join(counts_per_PY_affil,by=c("DT","PY","affil_id")) %>%
#   mutate(perc=n_lag/n_PY*100) %>%
#   group_by(DT,PY,affil_id) %>%
#   mutate(cumul_perc=cumsum(perc)) %>%
#   mutate(cumul_perc=round(cumul_perc,2)) %>%
#   group_by(DT,PY,lag) %>%
#   summarize(
#     mean=mean(cumul_perc),
#     sd=sd(cumul_perc),
#     upper_sd=mean+sd,
#     lower_sd=mean-sd
#   )
# 
# 
# 
# # PLOT
# # plot_data<-affil_counts_per_lag
# plot_data<-PY_counts_per_lag_affil 
# 
# 
# lag_plot2<-plot_data %>%
#   filter(PY!=2025) %>%
#   # filter(PY<2022) %>%
#   mutate(PY=as.factor(PY)) %>%
#   # ggplot(aes(x=lag, y=cumul_perc,group=PY,color=PY)) +
#   ggplot(aes(x=lag, y=mean,group=PY,color=PY)) +
#   # geom_errorbar(aes(ymin = lower_sd, ymax = upper_sd), width = 0.2)
#   scale_color_viridis(discrete = TRUE,option="A", begin = 0.2, end = 0.9) +
#   # scale_color_manual(values=c(rep("lightgray",4),"#36648B","#8B0000"))+
#   labs(x = "Lag (Months after PY)", size=5)+
#   labs(y = "Cumulative percentage of papers downloaded in January 2025", size=5)+
#   geom_line() +
#   facet_grid(vars(DT),scales="free", space="free_x")+
#   # facet_wrap(~DT,  ncol=1)+
#   # geom_point()+
#   expand_limits(y = 0)+
#   theme_classic()+
#   # scale_x_continuous( breaks=seq(1,12,by=1))+
#   # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo %>% select(n))+5000),by=2500))+
#   theme(axis.text.y = element_text(size = 8))+
#   theme(axis.text.x =element_text(size = 8))+
#   theme(axis.title.y = element_text(size = 10))+
#   theme(axis.title.x =element_text(size = 10))+
#   # scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo %>% select(n))+500))+
#   scale_y_continuous(n.breaks = 10, limits = c(90,101))+
#   # scale_x_continuous(n.breaks = 20, limits = c(0,60))+
#   scale_x_continuous(breaks = seq(0,60,by=3), limits = c(0,60))+
#   geom_hline(yintercept = 98, linetype = "dashed", color = "darkgray", size = 0.5)+
#   geom_hline(yintercept = 96, linetype = "dashed", color = "darkgray", size = 0.5)+
#   geom_hline(yintercept = 94, linetype = "dashed", color = "darkgray", size = 0.5)+
#   geom_vline(xintercept = 12, linetype = "dashed", color = "darkgray", size = 0.5)+
#   geom_vline(xintercept = 6, linetype = "dashed", color = "darkgray", size = 0.5)
# # geom_label(aes(label = PY))
# # geom_label(data = plot_data %>% filter(cumul_perc>99.99999), aes(label = as.numeric(PY))
# # position="jitter",
# # nudge_y = 0.8,
# # nudge_x = 0.55,
# # size =3.5,
# # fill=NA,
# # border.color = "white")
# # label.size = unit(0,"mm")
# # gghighlight(min(n) < 50)
# # +gghighlight(PY==2020, line_label_type = c("ggrepel_text"))
# #
# # lag_plot2
# #
# # write_csv(lag_data,"data_clean/lag_test/lag_data.csv")
# # ggsave("data_clean/lag_test/lag_plot.png",
# #        width = 6, height = 8, units = "in",
# #        device='png', dpi=700)
# # 
# # 
# # 
# 




















# 
# 
# 
# 
# lag_extractor <- function(cat){
#   PY<-seq(2020,2025,by=1)
#   results_list <- list()
#   journals_list <- list()
#   for(i in seq_along(PY)) {
#     
#   if(cat=="uni"){
#     data_dir_papers<-paste0("./data_raw/scopus_downloads/uni_20260101/papers/",PY[i])
#     
#     affil_id<-c(
#       60013959,
#       60002746,
#       60029929,
#       60025778,
#       60029445,
#       60003500,
#       60006297,
#       60012708,
#       60032838,
#       60027550,
#       60030612,
#       60023691,
#       60031970,
#       60025111,
#       60009982,
#       60033182,
#       60003711,
#       60028548,
#       60015481,
#       60005247)
#   }
#   if(cat=="fed"){
#     data_dir_papers<-paste0("./data_raw/scopus_downloads/fed_20260101/papers/",PY[i])
#     affil_id<-c(60014232,
#               60006577)
#   }
#   
#   affil_id_string <- paste(affil_id, collapse = "|")
#   affil_id_string2 <- paste(affil_id, collapse = "_|")
#   affil_id_string2 <- paste0(affil_id_string2, "_")
#   
#     csv_files_papers_monthly <- fs::dir_ls(data_dir_papers, regexp = affil_id_string) %>% 
#       as_tibble() %>% 
#       mutate(file = map_chr(str_split(value, "affil_"), -1)) %>% 
#       mutate(year = map_chr(str_split(value, affil_id_string2), -1)) %>% 
#       mutate(year = map_chr(str_split(year, "_papers"), 1)) %>% 
#       mutate(month = map_chr(str_split(year, "_"), -1)) %>% 
#       mutate(year = map_chr(str_split(year, "_"), 1)) %>% 
#       rename(path=value) %>% 
#       mutate(file_id=row_number(),.before=1) %>% 
#       mutate(PY=PY[i],.after=1) %>% 
#       mutate(year=as.numeric(year)) %>% 
#       mutate(month=as.numeric(month))
#     
#     
#     
#     csv_files_papers_lag<-csv_files_papers_monthly %>%
#       mutate(lag=if_else(year<=PY[i],0,NA)) %>% 
#       mutate(lag=if_else(year==PY[i]+1,month,lag)) %>% 
#       mutate(lag=if_else(year==PY[i]+2,12+month,lag)) %>% 
#       mutate(lag=if_else(year==PY[i]+3,24+month,lag)) %>% 
#       mutate(lag=if_else(year==PY[i]+4,36+month,lag)) %>% 
#       mutate(lag=if_else(year==PY[i]+5,48+month,lag)) 
#     
#     
#     
#     # 2. Read and combine files efficiently
#     # combined_data_all <- rbindlist(lapply(csv_files_papers_monthly$path, fread),fill=TRUE)
#     
#     # combined_data_all <- map_dfr(csv_files_papers_monthly$path, 
#     #                              ~fread(.x, colClasses = "character"), 
#     #                              .id = "file_path")
#     
#     combined_data_all <- rbindlist(lapply(setNames(csv_files_papers_monthly$path, csv_files_papers_monthly$path), 
#                                           function(x) fread(x, colClasses = "character")), 
#                                    fill=TRUE, idcol="path")
#     
#     
#     combined_data_all <- combined_data_all %>% 
#       mutate_all(tolower)
#     
#     
#     papers_cat<-c("article",
#                   "book chapter",
#                   "data paper",
#                   "note",
#                   "review")
#     
#     papers_title<-c("editorial comment",
#                     "editorial commentary",
#                     "a quick glance at selected topics in this issue",
#                     "reply by authors",
#                     "conclusion",
#                     "preface",
#                     "a word from olaw and usda",
#                     "introduction",
#                     "editorial comment",
#                     "editorial commentary",
#                     "a quick glance at selected topics in this issue",
#                     "conclusion",
#                     "reply by authors",
#                     "preface",
#                     "a word from olaw and usda",
#                     "author reply",
#                     "a word from olaw",
#                     "comment",
#                     "commentary",
#                     "conclusions",
#                     "a word from usda and olaw",
#                     "overview",
#                     "introduction")
#     #  
#     combined_data_all <- combined_data_all %>%
#       filter(subtypeDescription%in%papers_cat) %>% # KEEP the categories
#       filter(!"dc:title"%in%papers_title) %>% 
#       rename(SO="prism:publicationName")
#     
#     
#     journals_per_file<-combined_data_all %>% 
#       select(path,SO) 
#     
#     # str_match(combined_data_all$"dc:title","editorial comment")
#     # 
#     counts_per_file<-combined_data_all %>% 
#       group_by(path) %>% 
#       tally() %>% 
#       replace_na(list(n=0)) 
#     
#     csv_files_papers_lag<-csv_files_papers_lag %>% 
#       left_join(counts_per_file,by="path") %>% 
#       replace_na(list(n=0)) 
#     
#     # cum_perc_lag<-csv_files_papers_lag %>% 
#     #   group_by(lag) %>% 
#     #   summarize(n=sum(n)) %>% 
#     #   replace_na(list(n=0)) %>% 
#     #   mutate(cumsum=cumsum(n)) %>% 
#     #   mutate(cumperc=cumsum(n)/sum(n)*100) %>% 
#     #   mutate(PY=PY[i],.before=1) %>% 
#     #   mutate(cat=cat,.after=PY) 
#     
#     # results_list[[i]] <- cum_perc_lag
#     results_list[[i]] <- csv_files_papers_lag
#     journals_list[[i]] <- journals_per_file
#   }
#   
#   final_df <- bind_rows(results_list)  
#   final_journals <- bind_rows(journals_list)  
#   return(list(final_df,final_journals))
# 
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# cum_perc_lag_fed_all<-lag_extractor("fed") 
# cum_perc_lag_uni_all<-lag_extractor("uni") 
# 
# cum_perc_lag_fed<- cum_perc_lag_fed_all[[1]] %>% 
#   mutate(cat="fed")
# cum_perc_lag_uni<-cum_perc_lag_uni_all[[1]] %>% 
#   mutate(cat="uni")
# 
# lag_data<-bind_rows(cum_perc_lag_fed,cum_perc_lag_uni) %>% 
#    group_by(lag,PY) %>% 
#   summarize(n=sum(n)) %>% 
#   replace_na(list(n=0)) %>% 
#   group_by(PY) %>% 
#   mutate(cumsum=cumsum(n)) %>% 
#   mutate(cumperc=cumsum(n)/sum(n)*100) %>% 
#   mutate(PY=as.factor(PY))
# 
# lag_data 
# # %>% filter(cat=="fed")
# # plot_data<-lag_data %>% filter(PY==2023) %>% filter(cat=="fed")
# 
# 
# 
# lag_plot<-plot_data %>% 
#   filter(PY!=2025) %>% 
#   mutate(PY=as.factor(PY)) %>%
#   ggplot(aes(x=lag, y=cumperc,group=PY,color=PY)) +
#   scale_color_viridis(discrete = TRUE,option="A", begin = 0.2, end = 0.9) +
#   # scale_color_manual(values=c(rep("lightgray",4),"#36648B","#8B0000"))+
#   labs(x = "Lag (Months after PY)", size=5)+
#   labs(y = "Cumulative percentage of papers downloaded in January 2025", size=5)+
#   geom_line() + 
#   # geom_point()+
#   expand_limits(y = 0)+
#   theme_classic()+
#   # scale_x_continuous( breaks=seq(1,12,by=1))+
#   # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo %>% select(n))+5000),by=2500))+
#   theme(axis.text.y = element_text(size = 12))+
#   theme(axis.text.x =element_text(size = 12))+
#   theme(axis.title.y = element_text(size = 14))+
#   theme(axis.title.x =element_text(size = 14))+
#   # scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo %>% select(n))+500))+
#   scale_y_continuous(n.breaks = 10, limits = c(90,100))+
#   scale_x_continuous(n.breaks = 20, limits = c(0,60))+
#   geom_hline(yintercept = 98, linetype = "dashed", color = "darkgray", size = 0.5)+
#   geom_hline(yintercept = 97, linetype = "dashed", color = "darkgray", size = 0.5)+
#   geom_vline(xintercept = 6, linetype = "dashed", color = "darkgray", size = 0.5)
#   # geom_label(aes(label = PY), 
#   #            # position="jitter",
#   #            nudge_y = 0.8, 
#   #            nudge_x = 0.55, 
#   #            size =3.5,
#   #            fill=NA,
#   #            border.color = "white")
#              # label.size = unit(0,"mm")
#   # gghighlight(min(n) < 50)
#   # +gghighlight(PY==2020, line_label_type = c("ggrepel_text"))
# 
# lag_plot
# 
# write_csv(lag_data,"data_clean/lag_test/lag_data.csv")
# ggsave("data_clean/lag_test/lag_plot.png",
#        width = 6, height = 8, units = "in",
#        device='png', dpi=700)  
# 
# 
# 
# 
# 
# 
# jrnl_lag_fed<- cum_perc_lag_fed_all[[2]] %>% 
#   mutate(cat="fed")
# jrnl_lag_uni<-cum_perc_lag_uni_all[[2]] %>% 
#   mutate(cat="uni")
# 
# jrnl_lag_data<-bind_rows(jrnl_lag_fed,jrnl_lag_uni)
# 
# journal_rank<-jrnl_lag_data %>% 
#   group_by(SO) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>% 
#   mutate(perc=n/sum(n)*100) %>% 
#   mutate(rank=row_number())
# 
# 
# 
# journals_per_file<-
#   jrnl_lag_data %>% 
#   left_join(csv_files_papers_lag) %>% 
#   group_by(lag,SO) %>% 
#   tally() %>% 
#   arrange(desc(lag))
# 
# journals_per_file2<-journals_per_file %>%
#   filter(lag>6) %>% 
#   left_join(journal_rank,by="SO")
# 
# 
# 
# return(list(lag_data,lag_plot))
# }
# 


