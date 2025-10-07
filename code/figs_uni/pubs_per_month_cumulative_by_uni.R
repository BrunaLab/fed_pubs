pubs_per_month_cumulative_by_uni <- function(papers_dataset,authors_data_set,PY_max,PM_max) {
  
  library(gghighlight)
  library(ggrepel)
  library(forcats)
  
  
  
  papers_df_info<-papers_dataset %>% 
    select(refID,PY,PM)
  
  
  authors_df_info<- authors_data_set %>% 
    select(refID,uni,author_order) 
  
  
  pubs_mo_info <- left_join(papers_df_info,authors_df_info)
  unique(pubs_mo_info$uni)
  
  
  pubs_mo<-pubs_mo_info %>% 
    filter(!is.na(uni)) %>% 
    group_by(PM, PY,uni) %>%
    tally() %>%
    mutate(PM=as.numeric(PM),
           PY=as.numeric(PY)) %>% 
    arrange(PY, PM) %>% 
    ungroup() %>% 
    mutate(month=row_number()) %>% 
    left_join(month) %>% 
    mutate(month_name=reorder(month_name,PM))
  
  unique(pubs_mo$uni)
  
  
  
  
  
  pubs_mo_cum<-pubs_mo %>% 
    group_by(PY,uni) %>% 
    mutate(cumul_pubs=cumsum(n)) 
  
  final_yr<-pubs_mo_cum %>% 
    filter(PY==PY_max) %>% 
    filter(PM<PM_max+1) 
  
  
  prior_yrs<-pubs_mo_cum %>% 
    filter(PY<PY_max+1)
  
  counter<-pubs_mo_cum %>% 
    select(PM,month_name) %>% 
    ungroup() %>% 
    select(-PY) %>% 
    distinct()
  
  prior_yrs_avg<-
    prior_yrs %>% 
    group_by(uni,PM) %>% 
    mutate(n=mean(n)) %>% 
    group_by(uni,PM) %>% 
    mutate(cumul=cumsum(n)) %>% 
    arrange(uni,PM) %>% 
    select(PM,uni,month_name,n) %>% 
    group_by(uni,PM) %>% 
    slice_head(n=1) %>% 
    mutate(PY="avg") %>% 
    arrange(uni,PM) %>% 
    group_by(uni) %>% 
    mutate(cumul_pubs=cumsum(n)) 
  
  # 
  # prior_yrs_avg<-pubs_mo_cum %>% 
  #   filter(PY<PY_max) %>% 
  #   group_by(PM,uni) %>% 
  #   summarize(n=mean(n)) %>% 
  #   mutate(cumul_pubs=cumsum(n)) %>% 
  #   mutate(PY="avg") %>% 
  #   left_join(counter) %>% 
  #   arrange(uni,PM,cumul_pubs)
  
  
  plot_data<-bind_rows(final_yr,prior_yrs) %>% 
    mutate(PY=as.character(PY)) %>% 
    bind_rows(prior_yrs_avg) 
  # %>% 
  #   mutate(uni=if_else(nchar(uni)<5,str_to_upper(uni),str_to_title(uni)))
  # 
  
  # perc_change<-pubs_mo_cum %>% 
  #   filter(PM==PM_max)%>% 
  #   ungroup() %>% 
  #   mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
  # mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
  #   mutate(perc_previous=round(perc_previous,2))
  
  
  perc_change_avg<-final_yr %>% 
    mutate(PY=as.character(PY)) %>% 
    bind_rows(prior_yrs_avg) %>% 
    filter(PM==PM_max) 
  
  # nom<-perc_change_avg %>% filter(PY==PY_max) %>% select(cumul_pubs)
  # denom<-perc_change_avg %>% filter(PY=="avg") %>% select(cumul_pubs)  
  # nom<-perc_change_avg %>% filter(PY==PY_max) %>% arrange(uni,PM) %>% rename(n25=n,cumul25=cumul_pubs)
  # denom<-perc_change_avg %>% filter(PY=="avg") %>% arrange(uni,PM) %>% select(-month)
  # 
  # perc_change_avg<-left_join(nom,denom,by=c("uni"))
  # perc_change_avg<-perc_change_avg %>% 
  #   mutate(perc_change=((cumul_pubs.x-cumul_pubs.y)/cumul_pubs.y*100))
  # 
  perc_change<-pubs_mo_cum %>% 
    filter(PM==PM_max)%>% 
    ungroup() %>% 
    arrange(uni,PY) %>% 
    group_by(uni) %>% 
    mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
    mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
    mutate(perc_previous=round(perc_previous,1))
  # data_cumulative<-pubs_mo_cum %>% left_join(perc_change,by=c("PY","PM","month"))
  
  # label<-plot_data %>% group_by(PY) %>% filter(PM==max(PM)) %>% select(PM)
  
  write_csv(perc_change,"./docs/summary_info/perc_change_uni_uni.csv")
  
  perc_change<-perc_change %>% 
    mutate(PY=as.character(PY)) %>% 
    rename(perc_change=perc_previous) %>% 
    filter(PY==2025) %>% 
    select(perc_change,uni)
  plot_data<-plot_data %>% left_join(perc_change,by="uni") %>% 
    
    # mutate(uni=if_else(nchar(uni)<5,str_to_upper(uni),str_to_title(uni))) %>% 
    # mutate(uni=case_when(
    #   uni == "Unc_ch"~"UNC Chapel Hill",
    #   uni == "Ohio_state"~"Ohio State",
    #   uni == "PENN"~"Penn",
    #   uni == "MINN"~"Minnesota",
    #   .default = as.character(uni)
    #   )
    # ) %>% 
    mutate(uni=case_when(
      uni == "harvard"~"(A) Harvard Univ",
      uni == "michigan"~"(B) Univ of Michigan",  
      # uni == "michigan"~"(B) Univ of Michigan, Ann Arbor",  
      uni == "penn"~"(C) Univ of Pennsylvania",
      uni == "stanford"~"(D) Stanford Univ",
      uni == "ucla"~"(E) UC Los Angeles",
      uni == "washington"~"(F) Univ of Washington",
      uni == "florida"~"(G) Univ of Florida",
      uni == "ohio_state"~"(H) The Ohio State Univ",
      # uni == "minn"~"(I) Univ of Minnesota, Twin Cities",
      uni == "minn"~"(I) Univ of Minnesota",
      uni == "ucsd"~"(J) UC San Diego",
      uni == "unc_ch"~"(K) UNC Chapel Hill",
      uni == "ucsf"~"(L) UC San Francisco",
      uni == "mass_general"~"Massachusetts General Hospital",
      .default = as.character(uni)
    )) %>% 
    mutate(uni=paste(uni," (",perc_change,"%)", sep=""))
  
  order<-plot_data %>% group_by(uni) %>% 
    mutate(rank=sum(n)) %>% 
    slice_head(n=1) %>% 
    arrange(desc(rank)) %>% 
    mutate(uni = fct_reorder(uni, rank)) %>%
    select(uni)
  
  
  
  
  
  
  
  
  pubs_mo_cum_fig<-
    plot_data %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "avg", NA, as.character(label))) %>% 
    # mutate(label = if_else(PY == "Avg. (all yrs)", NA, as.character(label))) %>% 
    filter(PM<PM_max+1) %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "avg", NA, as.character(label))) %>% 
    mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>% 
    ggplot(aes(x=month_name, y=cumul_pubs,group=PY,color=PY,  linetype=PY))+
    labs(x = "Month", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line()+
    # geom_line(linewidth = if_else(plot_data$PY == "avg",1.5,1)) + 
    geom_point(size=1.5)+
    scale_color_manual(values=c(rep("darkgray",6),"#8B0000","#36648B"))+
    scale_linetype_manual(values = c(rep("solid", 6), "solid", "longdash"))+
    # expand_limits(y = 0)+
    expand_limits(x= c(0,PM_max + 1.25))+
    theme_classic()+
    # facet_wrap(~factor(uni, c(as.vector(order$uni))),ncol = 4, scales = "free",labeller = label_wrap_gen(width=30))+ # label_wrap_gen cnmtrols length of facet strip text
    facet_wrap(~factor(uni, c(as.vector(order$uni))),ncol = 4, scales = "free")+ # label_wrap_gen cnmtrols length of facet strip text
    theme(panel.spacing = unit(0.5, "cm", data = NULL))+
    # scale_x_continuous( breaks=seq(1,12,by=1))+
    # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative %>% select(cumul_pubs))+5000),by=2500))+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.text.x =element_text(size = 10))+
    theme(axis.title.y = element_text(size = 16,face = "bold"))+
    theme(axis.title.x =element_text(size = 16,face = "bold"))+
    theme(strip.text.x = element_text(face = "bold",hjust = 0,size=10))+ #hjust makes it flush left in strip instead of default center
    theme(strip.background.x = element_rect(fill = "white", color = "white", linetype = "solid", linewidth = 0))+
    theme(
      strip.placement = "outside")+
    theme(legend.position="none") + 
    geom_label(aes(label = label), 
               # position="jitter",
               nudge_y = 0.8, 
               nudge_x = 0.7, 
               size =2.5,
               border.color = "white",
               fill=NA
               # ,
               # label.size = unit(0,"mm")
    )
  
  # +
  #   theme(plot.background = element_rect(color = 1,
  #                                        size = 0),
  #         plot.margin = margin(t = 20,  # Top margin
  #                              r = 20,  # Right margin
  #                              b = 20,  # Bottom margin
  #                              l = 20))  # Left margin
  # 
  ggsave("./docs/images/pubs_mo_cum_uni_lines.png", width = 12, height = 8, units = "in", device='png', dpi=700)
  
  return(pubs_mo_cum_fig)
}