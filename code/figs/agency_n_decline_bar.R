agency_n_decline_bar <- function(agency_n_decline_sum, PY_max) {
  
  agency_n_decline_sum_fig<- agency_n_decline_sum %>%
    ggplot(aes(x=PY, y=n)) +
    labs(x = "Year", size=5)+
    labs(y = "No. of Publications  (Jan-May)", size=5)+
    geom_bar(stat="identity")+
    expand_limits(y = 0)+
    theme_classic()+
    geom_hline(yintercept = 0)+
    annotate(geom="text", x=2024, y=(max(agency_n_decline_sum$n)-.02*max(agency_n_decline_sum$n)), label=(agency_n_decline_sum %>% filter(PY==2024) %>% select(n)),
             color="navyblue", size=4)+
    annotate(geom="text", x=2025, y=(max(agency_n_decline_sum$n)-.02*max(agency_n_decline_sum$n)), label=(agency_n_decline_sum %>% filter(PY==2025) %>% select(n)),
             color="navyblue", size=4)+
    annotate(geom="text", x=2025, y=(max(agency_n_decline_sum$n)-.17*max(agency_n_decline_sum$n)), label=paste("(", round((agency_n_decline_sum %>% filter(PY>2024) %>% select(perc_previous_yr)),2),"%)",sep=""),
             color="red", size=4)+
    scale_y_continuous(expand = c(0, 0), breaks=seq(0, max(agency_n_decline_sum %>% select(n))+5000,by=2500))+
    scale_x_continuous( breaks=seq(2019,2025,by=1))+
    theme(axis.title.x = element_text(size = 14))+
    theme(axis.title.y = element_text(size = 14))+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x =element_text(size = 12))+
    gghighlight(PY == 2025)
  

  return(agency_n_decline_sum_fig)
  
}
