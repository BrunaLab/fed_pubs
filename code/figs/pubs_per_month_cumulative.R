pubs_per_month_cumulative <- function(pubs_mo, PY_max,PM_max) {

  
  library(gghighlight)
  
  pubs_mo_cum<-pubs_mo %>% 
    group_by(PY) %>% 
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
    group_by(PM) %>% 
    summarize(n=mean(n)) %>% 
    mutate(cumul_pubs=cumsum(n)) %>% 
    mutate(PY="Avg. (all yrs)") %>% 
    left_join(counter)
  
  plot_data<-bind_rows(final_yr,prior_yrs) %>% 
    mutate(PY=as.character(PY)) %>% 
    bind_rows(prior_yrs_avg)
  
  write_csv(plot_data,"./data_clean/cumulative_pubs_monthly.csv")
  
  # perc_change<-pubs_mo_cum %>% 
  #   filter(PM==PM_max)%>% 
  #   ungroup() %>% 
  #   mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
  # mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
  #   mutate(perc_previous=round(perc_previous,2))
  
  
  perc_change_avg<-final_yr %>% 
    mutate(PY=as.character(PY)) %>% 
    bind_rows(prior_yrs_avg) %>% 
    filter(PM==PM_max) %>% 
    ungroup() %>% 
    mutate(change_n = (cumul_pubs - lead(cumul_pubs))) %>%
    mutate(perc_previous = ((change_n) / lead(cumul_pubs)) * 100) %>% 
    mutate(perc_previous=round(perc_previous,2)) %>% 
    filter(PY==PY_max) %>% 
    rename(perc_mean=perc_previous)
  
  # nom<-perc_change_avg %>% filter(PY==PY_max) %>% select(cumul_pubs)
  # denom<-perc_change_avg %>% filter(PY=="Avg. (all yrs") %>% select(cumul_pubs)  
  # 
  # perc_change_avg<-((round(nom$cumul_pubs/denom$cumul_pubs,3)*100)-100)
  # 
  
  
  perc_change<-pubs_mo_cum %>% 
    filter(PM==PM_max)%>% 
    ungroup() %>% 
    mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
    mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
    mutate(perc_previous=round(perc_previous,2))
  
  write_csv(perc_change,"./docs/summary_info/perc_change_fed.csv")
  # data_cumulative<-pubs_mo_cum %>% left_join(perc_change,by=c("PY","PM","month"))
  library(ggrepel)
  # label<-plot_data %>% group_by(PY) %>% filter(PM==max(PM)) %>% select(PM)
  
  
  pubs_mo_cum_fig<-
    plot_data %>% 
    # filter(PM<=PM_max) %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "Avg. (all yrs)", NA, as.character(label))) %>% 
    # mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>% 
    ggplot(aes(x=month_name, 
               y=cumul_pubs,
               group=PY,
               color=PY,
               linetype=PY))+
    labs(x = "Month", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line(linewidth = 1.5) + 
    geom_point(size=0.5)+
    scale_color_manual(values=c(rep("gray",6),"#8B0000","#36648B"))+
    scale_linetype_manual(values = c(rep("solid", 6), "solid", "dashed"))+
    # scale_linetype_manual(values = if_else(plot_data$PY == "Avg. (all yrs)", "dashed", "solid"))+
    # expand_limits(y = 0)+
    # expand_limits(x= c(0,length(levels(plot_data$month_name)) + 1.25))+
    expand_limits(x= c(1,length(levels(plot_data %>% filter(PM<=PM_max) %>% select(month_name))) + 5))+
    theme_classic()+
    # scale_x_continuous( breaks=seq(1,12,by=1))+
    # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative %>% filter(PM<=PM_max) %>% select(cumul_pubs))+2000),by=2500))+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.text.x =element_text(size = 10))+
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
    annotate(geom="text", 
             # x=PM_max+0.32,
             x=PM_max+1.6,
             y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+2300),
             # y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-2000),
             # label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
             # label=paste(round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,sep=""),
             label=paste(round(perc_change %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
             # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
             color="black",
             size=12)+
    annotate(geom="text", 
             # x=PM_max+0.32,
             x=PM_max+0.9,
             y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+2300),
             # y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-2000),
             # label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
             # label=paste(round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,sep=""),
             label="} ",
             # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
             color="black",
             size=20)+
    # annotate(geom="text", x=PM_max+0.5,
    #          y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-2500),
    #          label=paste("(", round(perc_change_avg$perc_mean),"% from 2019-2024 avg.)",sep=""),
    #          # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
    #          color="black",
    #          size=5)+
    # scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative %>% filter(PM<=PM_max) %>% select(cumul_pubs))+1500))+
    scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative %>% select(cumul_pubs))+1500))+
   # scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
    # geom_segment(aes(xend=PM_max+.05, 
    #                  yend = (max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))),
    #                  x = PM_max+.05, 
    #                  y = (max(perc_change %>% filter(PY==2024) %>% select(cumul_pubs)))),
    #                  linetype="dotted",
    #              colour = "darkgray",
    #              arrow = arrow(angle = 20, length = unit(0.1, "inches"),
    #                                  ends = "last", type = "closed")
    #              )+
    geom_label(aes(label = label), 
               # position ="jitter",
               nudge_y = 1.3, 
               nudge_x = .35, 
               size = 5,
               fill=NA,
               border.color = "white"
               # label.size = unit(0,"mm")
               ) +
    theme(plot.background = element_rect(color = 1,
                                         size = 0),
          plot.margin = margin(t = 20,  # Top margin
                         r = 25,  # Right margin
                         b = 20,  # Bottom margin
                         l = 20)  # Left margin
          )
    
  
  
  
  
                     # ))
                     # 
                     # 
  # +
    # geom_label(aes(label = label), nudge_x = 0.25, size =4) 
    # geom_label(aes(label = label), nudge_x = 0.25,nudge_y = -1.5, size =4) 
    # geom_label_repel(aes(label = label),
    #                  nudge_x = .2,
    #                  na.rm = TRUE)
  
  ggsave("./docs/images/pubs_mo_cum_fig.png", width = 13, height = 10, units = "in", device='png', dpi=700)
  # 
  return(pubs_mo_cum_fig)
}