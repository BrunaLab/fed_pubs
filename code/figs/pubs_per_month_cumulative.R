pubs_per_month_cumulative <- function(plot_data,perc_change, PY_max,PM_max) {

  
  library(gghighlight)
  
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
    # geom_point(size=0.5)+
    scale_color_manual(values=c(rep("gray",6),"#8B0000","#36648B"))+
    scale_linetype_manual(values = c(rep("dotted", 6), "solid", "longdash"))+
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
             # y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+2300), # 1st authors
             y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+1300), # for pubs with only feds
             # label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
             # label=paste(round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,sep=""),
             label=paste(round(perc_change %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
             # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
             color="black",
             size=12)+
    annotate(geom="text", 
             # x=PM_max+0.32,
             x=PM_max+0.9,
             # y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+2300), # fed 1st authors 
             y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+1300), # pubs with only feds
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
    
  
  
  
  # 
  return(pubs_mo_cum_fig)
}