pubs_jan_to_month_x <- function(pubs_mo, PM_max) {
  
  
  library(ggrepel)
  monthly_pubs_1<-pubs_mo %>%
    # mutate(PM=if_else(PY==2025,5,PM)) %>% ######## THIS IS KEY
    filter(PM<PM_max+1) %>% 
    ggplot(aes(x=month_name, y=n,group=PY,color=PY)) +
    labs(x = "Month", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line() + 
    geom_point()+
    expand_limits(y = 0)+
    theme_classic()+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x =element_text(size = 14))+
    # scale_x_continuous( breaks=seq(1,7,by=1))+
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, max(pubs_mo %>% select(n))+5000,by=2500))+
    # gghighlight(min(n) < 50)
    gghighlight(PY == 2025) 
  
  data_ends <- pubs_mo %>% filter(PM == PM_max)
  
  # data_ends <- pubs_mo %>% 
  #   filter(PM<7) %>% 
  #   group_by(PY) %>% 
  #   summarize(n=sum(n)) %>% 
  #   mutate(PM=6)
  
  monthly_pubs_1 + 
    geom_text_repel(
      aes(label = n), data = data_ends,
      fontface ="plain", color = "black", size = 3,vjust=-.2
    )
  
  return(monthly_pubs_1)
}