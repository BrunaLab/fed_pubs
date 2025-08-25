pubs_per_month_cumulative_agency <- function(pubs_mo, PY_max,PM_max) {

  
  papers_df_info<-papers_df %>% 
    select(refID,PY,PM)
  authors_df_info<-authors_df %>% select(refID,agency_primary,agency,author_order)
  pubs_mo_info <- left_join(papers_df_info,authors_df_info)
  
  pubs_mo<-pubs_mo_info %>% 
    group_by(PM, PY,agency_primary) %>%
    tally() %>%
    mutate(PM=as.numeric(PM),
           PY=as.numeric(PY)) %>% 
    arrange(PY, PM) %>% 
    ungroup() %>% 
    mutate(month=row_number()) %>% 
    left_join(month) %>% 
    mutate(month_name=reorder(month_name,PM))
  
  library(gghighlight)
  
  pubs_mo_cum<-pubs_mo %>% 
    group_by(PY,agency_primary) %>% 
    mutate(cumul_pubs=cumsum(n)) 
  
  final_yr<-pubs_mo_cum %>% 
    filter(PY==PY_max) %>% 
    filter(PM<PM_max+1) 
  
  
  prior_yrs<-pubs_mo_cum %>% 
    filter(PY<PY_max) 
  
  counter<-pubs_mo_cum %>% 
    select(PM,month_name) %>% 
    ungroup() %>% 
    select(-PY) %>% 
    distinct()
  
  prior_yrs_avg<-pubs_mo_cum %>% 
    filter(PY<PY_max) %>% 
    group_by(PM,agency_primary) %>% 
    summarize(n=mean(n)) %>% 
    mutate(cumul_pubs=cumsum(n)) %>% 
    mutate(PY="avg") %>% 
    left_join(counter)
  
  plot_data<-bind_rows(final_yr,prior_yrs) %>% 
    mutate(PY=as.character(PY)) %>% 
    bind_rows(prior_yrs_avg)
  
  
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
  
  nom<-perc_change_avg %>% filter(PY==PY_max) %>% select(cumul_pubs)
  denom<-perc_change_avg %>% filter(PY=="avg") %>% select(cumul_pubs)  
  
  perc_change_avg<-((round(nom$cumul_pubs/denom$cumul_pubs,3)*100)-100)
  
  
  
  perc_change<-pubs_mo_cum %>% 
    filter(PM==PM_max)%>% 
    ungroup() %>% 
    mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
    mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
    mutate(perc_previous=round(perc_previous,2))
  # data_cumulative<-pubs_mo_cum %>% left_join(perc_change,by=c("PY","PM","month"))
  library(ggrepel)
  # label<-plot_data %>% group_by(PY) %>% filter(PM==max(PM)) %>% select(PM)
  
  
  pubs_mo_cum_fig<-
    plot_data %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "avg", NA, as.character(label))) %>% 
    ggplot(aes(x=month_name, y=cumul_pubs,group=PY,color=PY)) +
    labs(x = "Month", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line() + 
    geom_point(size=0.5)+
    scale_color_manual(values=c(rep("gray",6),"#8B0000","#36648B"))+
    # expand_limits(y = 0)+
    expand_limits(x= c(0,length(levels(plot_data$month_name)) + 1.25))+
    theme_classic()+
    # scale_x_continuous( breaks=seq(1,12,by=1))+
    scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative %>% select(cumul_pubs))+5000),by=2500))+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x =element_text(size = 14))+
    theme(legend.position="none")+
    # annotate(geom="text", x=PM_max+0.5,
    #          y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-1500),
    #          # label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
    #          label=paste(round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,sep=""),
    #          # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
    #          color="black",
    #          size=6)+
    annotate(geom="text", x=PM_max+0.8,
             y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+1800),
             # label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
             # label=paste(round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,sep=""),
             label=paste(round(perc_change$perc_previous[PM_max+1]),"%",sep=""),
             # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
             color="darkgray",
             size=6)+
    # annotate(geom="text", x=PM_max+0.5,
    #          y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-2500),
    #          label=paste("(", round(perc_change_avg),"% from 2019-2024 avg.)",sep=""),
    #          # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
    #          color="black",
    #          size=5)+
    scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative %>% select(cumul_pubs))+3500))+
    geom_segment(aes(xend=PM_max+.02, 
                     yend = (max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))),
                     x = PM_max+.02, 
                     y = (max(perc_change %>% filter(PY==2024) %>% select(cumul_pubs)))),
                     linetype="dotted",
                 colour = "darkgray",
                 arrow = arrow(angle = 20, length = unit(0.1, "inches"),
                                     ends = "last", type = "closed")
                 )+
    geom_label(aes(label = label), nudge_x = 0.65, size =4,label.size = unit(0,"mm")) +
    theme(plot.background = element_rect(color = 1,
                                         size = 0),
          plot.margin = margin(t = 20,  # Top margin
                         r = 20,  # Right margin
                         b = 20,  # Bottom margin
                         l = 20)  # Left margin
          )+
    facet_wrap(vars(agency_primary),ncol = 3)+
    theme(strip.text = element_text(
      size = 10, color = "black"))
    
  
  
  
  
                     # ))
                     # 
                     # 
  # +
    # geom_label(aes(label = label), nudge_x = 0.25, size =4) 
    # geom_label(aes(label = label), nudge_x = 0.25,nudge_y = -1.5, size =4) 
    # geom_label_repel(aes(label = label),
    #                  nudge_x = .2,
    #                  na.rm = TRUE)
  
  ggsave("./docs/images/pubs_mo_cum_fig.png", width = 7, height = 7, units = "in", device='png', dpi=700)
  # 
  return(pubs_mo_cum_fig)
}