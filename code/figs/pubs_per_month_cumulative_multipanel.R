pubs_per_month_cumulative_multipanel <- function(dataset_1,dataset_2,PM_max,PY_max) {
  
  library(tidyverse)
  library(gghighlight)
  library(ggrepel)
  # library(gridExtra)
  library(patchwork)

  dataset_1_cumulative<-count_cumul_pubs_per_month(dataset_1)
  dataset_2_cumulative<-count_cumul_pubs_per_month(dataset_2)
  
  plot_data_1<-dataset_1_cumulative[[1]]
  perc_change_1<-dataset_1_cumulative[[2]]
  pubs_mo_cumulative_1<-dataset_1_cumulative[[3]]
  
  plot_data_2<-dataset_2_cumulative[[1]]
  perc_change_2<-dataset_2_cumulative[[2]]
  pubs_mo_cumulative_2<-dataset_2_cumulative[[3]]
  
  
# FED FIRST AUTHOR (CO-AUTHORS FED AND NON-FED) ---------------------------

  
    
  
    p1<-
    plot_data_1 %>% 
    # filter(PM<=PM_max) %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "Avg. (all yrs)", NA, as.character(label))) %>% 
    # mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>% 
    ggplot(aes(x=month_name, 
               y=cumul_pubs,
               group=PY,
               color=PY,
               linetype=PY))+
    labs(x = "Month", size=7)+
    labs(y = "No. of Publications", size=7)+
    # geom_line(linewidth = 1) + 
    geom_line(linewidth = if_else(plot_data_1$PY == "Avg. (all yrs)",1.5,1)) + 
    geom_point(size=1.5)+
    scale_color_manual(values=c(rep("darkgray",6),"#8B0000","#36648B"))+
    scale_linetype_manual(values = c(rep("solid", 6), "solid", "longdash"))+
    # expand_limits(y = 0)+
    # expand_limits(x= c(0,length(levels(plot_data_1$month_name)) + 1.25))+
    expand_limits(x= c(1,length(levels(plot_data_1 %>% filter(PM<=PM_max) %>% select(month_name))) + 13))+
    theme_classic()+
    # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative_1 %>% filter(PM<=PM_max) %>% select(cumul_pubs))+2000),by=2500))+
    theme(axis.text.y = element_text(size = 10))+
    # theme(axis.text.x = element_text(size = 10))+
    theme(axis.title.y = element_text(size = 16))+
    # theme(axis.title.x =element_text(size = 16))+
    theme(axis.title.x = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(legend.position="none")+
    annotate(geom="text", 
             # x=PM_max+0.32,
             x= PM_max+2.3,
             y=(max(perc_change_1 %>% filter(PY==2025) %>% select(cumul_pubs))+2400), # 1st authors
             label=paste(round(perc_change_1 %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
             color="black",
             size=7)+
    annotate(geom="text", 
             # x=PM_max+0.32,
             x=PM_max+1.2,
             y=(max(perc_change_1 %>% filter(PY==2025) %>% select(cumul_pubs))+2400), # fed 1st authors 
             label="} ",
             color="black",
             size=10)+
    scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative_1 %>% select(cumul_pubs))+1500))+
    geom_label(aes(label = label), 
               # position ="jitter",
               nudge_y = 1.3, 
               nudge_x = .55, 
               size = 3,
               fill=NA,
               border.color = "white"
               # label.size = unit(0,"mm")
               ) +
    ggtitle('(A) Federally affiliated 1st author' )+
    theme(plot.title=element_text(face='bold'))
  
  # +
  #   theme(plot.background = element_rect(color = 1,
  #                                        size = 0),
  #         plot.margin = margin(t = 20,  # Top margin
  #                        r = 25,  # Right margin
  #                        b = 20,  # Bottom margin
  #                        l = 20)  # Left margin
  #         )
  #   

# PUBS WITH ONLY FED AUTHORS ----------------------------------------------

    
    
    p2<-
      plot_data_2 %>% 
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
    geom_line(linewidth = if_else(plot_data_1$PY == "Avg. (all yrs)",1.5,1)) + 
    geom_point(size=1.5)+
    scale_color_manual(values=c(rep("darkgray",6),"#8B0000","#36648B"))+
    scale_linetype_manual(values = c(rep("solid", 6), "solid", "longdash"))+
      # expand_limits(y = 0)+
      # expand_limits(x= c(0,length(levels(plot_data_2$month_name)) + 1.25))+
      expand_limits(x= c(1,length(levels(plot_data_2 %>% filter(PM<=PM_max) %>% select(month_name))) + 13))+
      theme_classic()+
      # scale_x_continuous( breaks=seq(1,12,by=1))+
      # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative_2 %>% filter(PM<=PM_max) %>% select(cumul_pubs))+2000),by=2500))+
      theme(axis.text.y = element_text(size = 10))+
      theme(axis.text.x =element_text(size = 10))+
      theme(axis.title.y = element_text(size = 16))+
      theme(axis.title.x =element_text(size = 16))+
      theme(legend.position="none")+
      annotate(geom="text", 
               # x=PM_max+0.32,
               x=PM_max+2.2,
               y=(max(perc_change_2 %>% filter(PY==2025) %>% select(cumul_pubs))+1300), # for pubs with only feds
               label=paste(round(perc_change_2 %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
               color="black",
               size=7)+
      annotate(geom="text", 
               # x=PM_max+0.32,
               x=PM_max+1.2,
               y=(max(perc_change_2 %>% filter(PY==2025) %>% select(cumul_pubs))+1300), # pubs with only feds
               label="} ",
               color="black",
               size=10)+
      scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative_2 %>% select(cumul_pubs))+1500))+
      geom_label(aes(label = label), 
                 # position ="jitter",
                 nudge_y = 1.3, 
                 nudge_x = .55, 
                 size = 3,
                 fill=NA,
                 border.color = "white"
                 # label.size = unit(0,"mm")
      ) +
    ggtitle('(B) All authors federally affiliated')+
    theme(plot.title=element_text(face='bold'))
  # +
  #     theme(plot.background = element_rect(color = 1,
  #                                          size = 0),
  #           plot.margin = margin(t = 20,  # Top margin
  #                                r = 25,  # Right margin
  #                                b = 20,  # Bottom margin
  #                                l = 20)  # Left margin
  #     )
    
    
    
    

# BIND THEM UP ------------------------------------------------------------

    # pubs_mo_cum_fig<-grid.arrange(p1, p2, ncol = 1)
  
  pubs_mo_cum_fig<- p1/ p2
  # +
  #   plot_annotation(tag_levels = 'A')
# RETURN FROM FUNCTION ----------------------------------------------------

  
  return(pubs_mo_cum_fig)
}