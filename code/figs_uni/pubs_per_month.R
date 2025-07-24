pubs_per_month <- function(pubs_mo, PY_max) {
  
  library(gghighlight)
  pubs_mo_fig<-pubs_mo %>% 
    filter(PY<PY_max+1) %>% 
    ggplot(aes(x=month_name, y=n,group=PY,color=PY)) +
    labs(x = "Month", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line() + 
    geom_point()+
    expand_limits(y = 0)+
    theme_classic()+
    # scale_x_continuous( breaks=seq(1,12,by=1))+
    scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo %>% select(n))+5000),by=2500))+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x =element_text(size = 14))+
    scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_mo %>% select(n))+500))+
    scale_x_discrete(labels=c(month))+
    
    # gghighlight(min(n) < 50)
    gghighlight(PY == 2024)
  
  ggsave("./images/pubs_per_month_uni.png", width = 10, height = 10, units = "in")
  
  return(pubs_mo_fig)
}