
total_pubs_to_month_x <- function(pubs_mo, PM_max) {
  
  total_jan_may<-pubs_mo %>%
    # mutate(PM=if_else(PY==2025,5,PM)) %>% ######## THIS IS KEY
    filter(PM<PM_max+1) %>% 
    group_by(PY) %>% 
    summarize(n=sum(n)) %>% 
    mutate(n_diff=n-lag(n)) %>% 
    mutate(perc_previous_yr=n_diff/lag(n)*100)
  
  
  # n_2024<-total_jan_may %>% filter(PY==2024) %>% select(n)
  # n_2025<-total_jan_may %>% filter(PY==2025) %>% select(n)
  # perc_2425<-as.numeric((n_2025-n_2024)/n_2024*100)
  # diff_2425<-n_2025-n_2024
  
  total_pubs_to_month_x_fig<-
    total_jan_may %>% 
    ggplot(aes(x=PY, y=n)) +
    labs(x = "Year", size=5)+
    labs(y = "No. of Publications (Jan-July)", size=5)+
    geom_bar(stat="identity")+
    expand_limits(y = 0)+
    theme_classic()+
    annotate(geom="text", x=2024, y=(max(total_jan_may$n)-.02*max(total_jan_may$n)), label=(total_jan_may %>% filter(PY==2024) %>% select(n)),
             color="navyblue", size=4)+
    annotate(geom="text", x=2025, y=(max(total_jan_may$n)-.02*max(total_jan_may$n)), label=(total_jan_may %>% filter(PY==2025) %>% select(n)),
             color="navyblue", size=4)+
    
    annotate(geom="text", x=2025, y=(max(total_jan_may$n)-.13*max(total_jan_may$n)), label=paste("(", round((total_jan_may %>% filter(PY==2025) %>% select(perc_previous_yr)),2),"%)",sep=""),
             color="red", size=4)+
    scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(total_jan_may %>% select(n))+500),by=3000))+
    # scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(total_jan_may %>% select(n))+500))+
    scale_x_continuous( breaks=seq(2019,2025,by=1))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x = element_text(size = 14))+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    gghighlight(PY == 2025)
  ggsave("./docs/images/total_pubs_to_month_x_uni.png", width = 6, height = 4, units = "in")
  return(total_pubs_to_month_x_fig)
}