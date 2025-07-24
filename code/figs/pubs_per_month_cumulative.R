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
  
  
  pubs_mo_cum_fig<-plot_data %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>%
    ggplot(aes(x=month_name, y=cumul_pubs,group=PY,color=PY)) +
    labs(x = "Month", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line() + 
    geom_point(size=0.5)+
    scale_color_manual(values=c(rep("gray",6),"#8B0000","#36648B"))+
    expand_limits(y = 0)+
    theme_classic()+
    # scale_x_continuous( breaks=seq(1,12,by=1))+
    scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative %>% select(cumul_pubs))+5000),by=2500))+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x =element_text(size = 14))+
    annotate(geom="text", x=PM_max+0.5,
             y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-3500),
             label=paste("(", round(perc_change$perc_previous[PM_max+1]),"% from ",PY_max-1,")",sep=""),
             # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
             color="#8B0000",
             size=4)+
    # annotate(geom="text", x=PM_max+0.5,
    #          y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))-2000),
    #          label="2025",
    #          # label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
    #          color="#8B0000",
    #          size=5)+
    scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo_cumulative %>% select(cumul_pubs))+500))+
    geom_label_repel(aes(label = label),
                     nudge_x = .2,
                     na.rm = TRUE)
  
  
  # 
  # library(gghighlight)
  # 
  # pubs_mo_cum<-pubs_mo %>% 
  #   group_by(PY) %>% 
  #   mutate(cumul_pubs=cumsum(n)) 
  # 
  # 
  # 
  # final_yr<-pubs_mo_cum %>% 
  #   filter(PY==PY_max) %>% 
  #   filter(PM<PM_max+1) 
  #   
  # 
  # prior_yrs<-pubs_mo_cum %>% 
  #   filter(PY<PY_max) 
  # 
  # plot_data<-bind_rows(final_yr,prior_yrs)
  # 
  # 
  # perc_change<-pubs_mo_cum %>% 
  #   filter(PM==PM_max)%>% 
  #   ungroup() %>% 
  #   mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
  # mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
  #   mutate(perc_previous=round(perc_previous,2))
  # 
  # # data_cumulative<-pubs_mo_cum %>% left_join(perc_change,by=c("PY","PM","month"))
  # 
  # pubs_mo_cum_fig<-plot_data %>% 
  #   ggplot(aes(x=PM, y=cumul_pubs,group=PY,color=PY)) +
  #   labs(x = "Month", size=5)+
  #   labs(y = "No. of Publications", size=5)+
  #   geom_line() + 
  #   geom_point()+
  #   expand_limits(y = 0)+
  #   theme_classic()+
  #   scale_x_continuous( breaks=seq(1,12,by=1))+
  #   scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative %>% select(cumul_pubs))+5000),by=2500))+
  #   theme(axis.text.y = element_text(size = 12))+
  #   theme(axis.text.x =element_text(size = 12))+
  #   theme(axis.title.y = element_text(size = 14))+
  #   theme(axis.title.x =element_text(size = 14))+
  #   annotate(geom="text", x=PM_max, 
  #            y=(max(perc_change %>% filter(PY==2025) %>% select(cumul_pubs))+700), 
  #            label=paste("(", round(perc_change$perc_previous[PM_max+1]),"%)",sep=""),
  #            label=(round(perc_change %>% filter(PY==2025 & PM==6) %>% select(perc_previous))),
  #            color="red", 
  #            size=4)+
  #   scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo_cumulative %>% select(cumul_pubs))+500))+
  #   # gghighlight(min(n) < 50)
  #   gghighlight(PY == 2025)
  # 
  ggsave("./images/pubs_mo_cum_fig.png", width = 10, height = 10, units = "in")
  # 
  return(pubs_mo_cum_fig)
}