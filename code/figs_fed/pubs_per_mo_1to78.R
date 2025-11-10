function(pubs_mo, month_max) {
  
  pubs_mo_avg<-pubs_mo %>% 
    filter(month<month_max+1) %>% 
    group_by(PY) %>% 
    mutate(total_n=sum(n)) %>% 
    mutate(no_months=n()) %>% 
    mutate(avg_per_mo=(total_n/no_months)) %>% 
    
  library(gghighlight)
  pubs_mo_fig<-pubs_mo_avg %>% 
    ggplot(aes(x=month, y=avg_per_mo,group=PY,color=PY)) +
    # ggplot(aes(x=month, y=avg_per_mo)) +
    labs(x = "Month", size=5)+
    labs(y = "Avg. No. of Publications in Yr", size=5)+
    geom_line() + 
    geom_point()+
    expand_limits(y = 0)+
    theme_classic()+
    # scale_x_continuous( breaks=seq(1,12,by=1))+
    # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo %>% select(n))+5000),by=2500))+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x =element_text(size = 14))+
    scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo %>% select(n))+500))+
    # gghighlight(min(n) < 50)
    gghighlight(PY == 2024)
  
  ggsave("./docs/images/pubs_per_month.png", width = 6, height = 4, units = "in", device='png', dpi=700)
  
  return(pubs_mo_fig)
}