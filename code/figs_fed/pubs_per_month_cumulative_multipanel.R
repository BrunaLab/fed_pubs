pubs_per_month_cumulative_multipanel <- function(dataset_1,dataset_2,PM_max,PY_max) {
  
  # dataset_1<-papers_dataset
  # dataset_2<-papers_with_all_feds
  # 
  library(tidyverse)
  library(gghighlight)
  library(ggrepel)
  # library(gridExtra)
  library(patchwork)
  

  count_cumul_pubs_per_month <- function(df) {
# 
    month<-data.frame(month_name=month.abb,PM=seq(1:12)) %>% 
      mutate(month_name=as.factor(month_name)) 
    
#     pubs_mo_cumulative <-
#       df %>%
#       group_by(PM, PY) %>%
#       tally() %>%
#       mutate(PM=as.numeric(PM),
#              PY=as.numeric(PY)) %>%
#       arrange(PY, PM) %>%
#       ungroup() %>%
#       mutate(month=row_number()) %>%
#       group_by(PY) %>%
#       mutate(cumul_pubs=cumsum(n))

  # pubs_mo_cumulative<-count_cumul_pubs_per_month(papers_with_fed_first)
  # last number is max month of focal year (ie 2025)

  # pubs_mo_cumulative %>% filter(PM<(PM_max+1)) %>% arrange(PM,desc(PY))
    
    pubs_mo <-
      df %>%
      group_by(PM, PY) %>%
      tally() %>%
      mutate(PM=as.numeric(PM),
             PY=as.numeric(PY)) %>% 
      arrange(PY, PM) %>% 
      ungroup() %>% 
      mutate(month=row_number()) %>% 
      left_join(month) %>% 
      mutate(month_name=reorder(month_name,PM))
    
  # pubs_mo<-count_pubs_per_mo(df)

  pubs_mo_cum<-pubs_mo %>%
    group_by(PY) %>%
    mutate(cumul_pubs=cumsum(n))

  final_yr<-pubs_mo_cum %>%
    filter(PY==PY_max) %>%
    filter(PM<PM_max+1)


  prior_yrs<-pubs_mo_cum %>%
    filter(PY<PY_max)

  prior_yrs %>%
    filter(PM==12) %>%
    ungroup() %>%
    mutate(perc=(cumul_pubs-lag(cumul_pubs))/lag(cumul_pubs)*100)

  counter<-pubs_mo_cum %>%
    ungroup() %>%
    select(PM,month_name) %>%
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

  # percent change from previous years

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

  perc_change<-pubs_mo_cum %>%
    filter(PM==PM_max)%>%
    ungroup() %>%
    mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
    mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>%
    mutate(perc_previous=round(perc_previous,2))




  return(list(plot_data,perc_change,pubs_mo_cum))

  }
  
  dataset_1_cumulative<-count_cumul_pubs_per_month(dataset_1)
  
  dataset_2_cumulative<-count_cumul_pubs_per_month(dataset_2)
  
  
  plot_data_1<-as.data.frame(dataset_1_cumulative[[1]])
  
  perc_change_1<-as.data.frame(dataset_1_cumulative[[2]])
  pubs_mo_cumulative_1<-as.data.frame(dataset_1_cumulative[[3]])
  
  plot_data_2<-as.data.frame(dataset_2_cumulative[[1]])
  perc_change_2<-as.data.frame(dataset_2_cumulative[[2]])
  pubs_mo_cumulative_2<-as.data.frame(dataset_2_cumulative[[3]])

  
# FED FIRST AUTHOR (CO-AUTHORS FED AND NON-FED) ---------------------------

  
    
  
    p1<-
    plot_data_1 %>% 
    # filter(PM<=PM_max) %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "Avg. (all yrs)", NA, as.character(label))) %>% 
    mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>% 
    ggplot(aes(x=month_name, 
               y=cumul_pubs,
               group=PY,
               color=PY,
               linetype=PY))+
    labs(x = "Month", size=7)+
    labs(y = "No. of Publications", size=7)+
    # geom_line(linewidth = 1) + 
    geom_line(linewidth = if_else(plot_data_1$PY == "Avg. (all yrs)",1.5,0.75)) + 
    geom_point(size=1.0)+
    # scale_color_manual(values=c(rep("darkgray",5),"#36648B", "#8B0000","#36648B"))+
    # scale_linetype_manual(values = c(rep("solid", 5),"solid", "solid", "longdash"))+
    scale_color_manual(values=c(rep("lightgray",5),"#36648B","#8B0000","black"))+
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
             # x= PM_max+2.3,
             # y=(max(perc_change_1 %>% filter(PY==2025) %>% select(cumul_pubs))+2400), # 1st authors
             x=PM_max+0.55,
             y=(max(perc_change_1 %>% filter(PY==2025) %>% select(cumul_pubs))-2500), # for pubs with only feds
             label=paste(round(perc_change_1 %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
             color="black",
             size=7)+
    # annotate(geom="text", 
    #          # x=PM_max+0.32,
    #          x=PM_max+1.2,
    #          y=(max(perc_change_1 %>% filter(PY==2025) %>% select(cumul_pubs))+2400), # fed 1st authors 
    #          label="} ",
    #          color="black",
    #          size=10)+
    # scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative_1 %>% select(cumul_pubs))+1500))+
    scale_y_continuous(n.breaks = 15, limits = c(0, max(pubs_mo_cumulative_1 %>% select(cumul_pubs))+1500))+
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
      mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>%
      ggplot(aes(x=month_name, 
                 y=cumul_pubs,
                 group=PY,
                 color=PY,
                 linetype=PY))+
      labs(x = "Month", size=5)+
      labs(y = "No. of Publications", size=5)+
    geom_line(linewidth = if_else(plot_data_2$PY == "Avg. (all yrs)",1.5,0.75)) + 
    geom_point(size=1.0)+
    # scale_color_manual(values=c(rep("lightgray",5),"#36648B", "#8B0000","#36648B"))+
    # scale_linetype_manual(values = c(rep("solid", 5),"solid", "solid", "longdash"))+
    # scale_color_manual(values=c(rep("lightgray",6),"#8B0000","black"))+
    scale_color_manual(values=c(rep("lightgray",5),"#36648B","#8B0000","black"))+
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
               # x=PM_max+2.2,
               x=PM_max+0.55,
               y=(max(perc_change_2 %>% filter(PY==2025) %>% select(cumul_pubs))-2500), # for pubs with only feds
               label=paste(round(perc_change_2 %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
               color="black",
               size=7)+
      # annotate(geom="text", 
      #          # x=PM_max+0.32,
      #          x=PM_max+1.2,
      #          y=(max(perc_change_2 %>% filter(PY==2025) %>% select(cumul_pubs))+1300), # pubs with only feds
      #          label="} ",
      #          color="black",
      #          size=10)+
      # scale_y_continuous(expand = c(0, 0), n.breaks = 24, limits = c(0, max(pubs_mo_cumulative_2 %>% select(cumul_pubs))+1500))+
      scale_y_continuous(n.breaks = 15, limits = c(0, max(pubs_mo_cumulative_1 %>% select(cumul_pubs))+1500))+
    
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

  

# now do with the x axis only to PM_max -----------------------------------

  
  # plot_data_1 <-plot_data_1 %>%filter(PM<=PM_max) 
  # 
  # p1_short<-
  #   plot_data_1 %>% 
  #   # filter(PM<=PM_max) %>% 
  #   mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
  #   mutate(label = if_else(PY == "Avg. (all yrs)", NA, as.character(label))) %>% 
  #   # mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>% 
  #   ggplot(aes(x=month_name, 
  #              y=cumul_pubs,
  #              group=PY,
  #              color=PY,
  #              linetype=PY))+
  #   labs(x = "Month", size=7)+
  #   labs(y = "No. of Publications", size=7)+
  #   # geom_line(linewidth = 1) + 
  #   geom_line(linewidth = if_else(plot_data_1$PY == "Avg. (all yrs)",1.5,1)) + 
  #   geom_point(size=1.5)+
  #   scale_color_manual(values=c(rep("darkgray",6),"#8B0000","#36648B"))+
  #   scale_linetype_manual(values = c(rep("solid", 6), "solid", "longdash"))+
  #   # expand_limits(y = 0)+
  #   # expand_limits(x= c(0,length(levels(plot_data_1$month_name)) + 1.25))+
  #   expand_limits(x= c(1,length(levels(plot_data_1 %>% filter(PM<=PM_max) %>% select(month_name))) + 2))+
  #   theme_classic()+
  #   # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative_1 %>% filter(PM<=PM_max) %>% select(cumul_pubs))+2000),by=2500))+
  #   theme(axis.text.y = element_text(size = 10))+
  #   # theme(axis.text.x = element_text(size = 10))+
  #   theme(axis.title.y = element_text(size = 16))+
  #   # theme(axis.title.x =element_text(size = 16))+
  #   theme(axis.title.x = element_blank())+
  #   theme(axis.text.x = element_blank())+
  #   theme(legend.position="none")+
  #   annotate(geom="text", 
  #            # x=PM_max+0.32,
  #            x= PM_max+2.3,
  #            y=(max(perc_change_1 %>% filter(PY==2025) %>% select(cumul_pubs))+2400), # 1st authors
  #            label=paste(round(perc_change_1 %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
  #            color="black",
  #            size=7)+
  #   annotate(geom="text", 
  #            # x=PM_max+0.32,
  #            x=PM_max+1.2,
  #            y=(max(perc_change_1 %>% filter(PY==2025) %>% select(cumul_pubs))+2400), # fed 1st authors 
  #            label="} ",
  #            color="black",
  #            size=10)+
  #   scale_y_continuous(expand = c(0, 0), n.breaks = 15, limits = c(0, max(pubs_mo_cumulative_1 %>% filter(PM<=PM_max) %>% select(cumul_pubs))+1500))+
  #   geom_label(aes(label = label), 
  #              # position ="jitter",
  #              nudge_y = 1.3, 
  #              nudge_x = .55, 
  #              size = 3,
  #              fill=NA,
  #              border.color = "white"
  #              # label.size = unit(0,"mm")
  #   ) +
  #   ggtitle('(A) Federally affiliated 1st author' )+
  #   theme(plot.title=element_text(face='bold'))
  # 
  # # +
  # #   theme(plot.background = element_rect(color = 1,
  # #                                        size = 0),
  # #         plot.margin = margin(t = 20,  # Top margin
  # #                        r = 25,  # Right margin
  # #                        b = 20,  # Bottom margin
  # #                        l = 20)  # Left margin
  # #         )
  # #   
  # 
  # # PUBS WITH ONLY FED AUTHORS ----------------------------------------------
  # 
  # 
  # plot_data_2 <- plot_data_2 %>% filter(PM<=PM_max)
  # p2_short<-
  #   plot_data_2 %>% 
  #   # filter(PM<=PM_max) %>% 
  #   mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
  #   mutate(label = if_else(PY == "Avg. (all yrs)", NA, as.character(label))) %>% 
  #   # mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>% 
  #   ggplot(aes(x=month_name, 
  #              y=cumul_pubs,
  #              group=PY,
  #              color=PY,
  #              linetype=PY))+
  #   labs(x = "Month", size=5)+
  #   labs(y = "No. of Publications", size=5)+
  #   geom_line(linewidth = if_else(plot_data_1$PY == "Avg. (all yrs)",1.5,1)) + 
  #   geom_point(size=1.5)+
  #   scale_color_manual(values=c(rep("darkgray",6),"#8B0000","#36648B"))+
  #   scale_linetype_manual(values = c(rep("solid", 6), "solid", "longdash"))+
  #   # expand_limits(y = 0)+
  #   # expand_limits(x= c(0,length(levels(plot_data_2$month_name)) + 1.25))+
  #   expand_limits(x= c(1,length(levels(plot_data_1 %>% filter(PM<=PM_max) %>% select(month_name))) + 2))+
  #   theme_classic()+
  #   # scale_x_continuous( breaks=seq(1,12,by=1))+
  #   # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative_2 %>% filter(PM<=PM_max) %>% select(cumul_pubs))+2000),by=2500))+
  #   theme(axis.text.y = element_text(size = 10))+
  #   theme(axis.text.x =element_text(size = 10))+
  #   theme(axis.title.y = element_text(size = 16))+
  #   theme(axis.title.x =element_text(size = 16))+
  #   theme(legend.position="none")+
  #   annotate(geom="text", 
  #            # x=PM_max+0.32,
  #            x=PM_max+2.2,
  #            y=(max(perc_change_2 %>% filter(PY==2025) %>% select(cumul_pubs))+1300), # for pubs with only feds
  #            label=paste(round(perc_change_2 %>% filter(PY==PY_max) %>% select(perc_previous)),"%",sep=""),
  #            color="black",
  #            size=7)+
  #   annotate(geom="text", 
  #            # x=PM_max+0.32,
  #            x=PM_max+1.2,
  #            y=(max(perc_change_2 %>% filter(PY==2025) %>% select(cumul_pubs))+1300), # pubs with only feds
  #            label="} ",
  #            color="black",
  #            size=10)+
  #   scale_y_continuous(expand = c(0, 0), n.breaks = 15, limits = c(0, max(pubs_mo_cumulative_1 %>% filter(PM<=PM_max) %>% select(cumul_pubs))+1500))+
  #   geom_label(aes(label = label), 
  #              # position ="jitter",
  #              nudge_y = 1.3, 
  #              nudge_x = .55, 
  #              size = 3,
  #              fill=NA,
  #              border.color = "white"
  #              # label.size = unit(0,"mm")
  #   ) +
  #   ggtitle('(B) All authors federally affiliated')+
  #   theme(plot.title=element_text(face='bold'))
  # # +
  # #     theme(plot.background = element_rect(color = 1,
  # #                                          size = 0),
  # #           plot.margin = margin(t = 20,  # Top margin
  # #                                r = 25,  # Right margin
  # #                                b = 20,  # Bottom margin
  # #                                l = 20)  # Left margin
  # #     )
  # 
  # 
  # 
  # 
  # 
  # # BIND THEM UP ------------------------------------------------------------
  # 
  # # pubs_mo_cum_fig<-grid.arrange(p1, p2, ncol = 1)
  # 
  # pubs_mo_cum_fig_short<- p1_short/ p2_short
  
  
  # RETURN FROM FUNCTION ----------------------------------------------------

  
  
  
  return(list(plot_data_1,perc_change_1,plot_data_2,perc_change_2, pubs_mo_cum_fig))
  
  
  
  
  
  
  
}