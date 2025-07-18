# PY = the max year for which you want pubs per year

total_pubs_per_year <- function(pubs_yr, PY_max) {
  pubs_yr_fig<-pubs_yr %>% 
    filter(PY<PY_max+1) %>% 
    ggplot(aes(x=PY, y=n)) +
    labs(y = "No. of Publications", size=5)+
    labs(x = "Year", size=5)+
    geom_bar(stat="identity")+
    expand_limits(y = 0)+
    theme_classic()+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.title.x =element_text(size = 14))+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    scale_y_continuous(expand = c(0, 0), breaks=seq(0,max(pubs_yr$n)+5000,by=5000), limits = c(0, max(pubs_yr$n)+5000))+
    scale_x_continuous(seq(2019, 2024, by=1))
  
  ggsave("./images/total_pubs_per_yr.png", width = 8, height = 8, units = "in")
}