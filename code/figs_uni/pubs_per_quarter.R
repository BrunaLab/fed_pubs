pubs_per_quarter <- function(pubs_mo, PY_max) {
  
  
  
  
  
  
  
  pubs_per_quarter<-pubs_mo %>% 
    mutate(PM=as.numeric(PM)) %>% 
    mutate(Q=cut(PM, breaks = c(0, 3, 6, 9,12), 
                 labels = c("Q1", "Q2", "Q3", "Q4"))) %>% 
    group_by(PY,Q) %>% 
    summarize(n=sum(n))
  
  
  pubs_per_quarter_fig<-pubs_per_quarter %>% 
    filter(PY<2025) %>% 
    ggplot(aes(x=Q, y=n,group=PY,color=PY)) +
    labs(x = "Quarter", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line() + 
    geom_point()+
    expand_limits(y = 0)+
    theme_classic()+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x =element_text(size = 14))+
    # scale_y_continuous(expand = c(0, 0), n.breaks = 4, limits = c(1, ))+
    scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_per_quarter %>% select(n))+5000),by=2500))+
    # scale_y_continuous(expand = c(0, 0), n.breaks = 20, limits = c(0, max(pubs_per_quarter %>% select(n))+500))+
    # gghighlight(min(n) < 50)
    gghighlight(PY == 2024)
  ggsave("./docs/images/pubs_per_quarter_uni.png", width = 6, height = 4, units = "in", device='png', dpi=700)
  
  return(pubs_per_quarter_fig)
  
}
