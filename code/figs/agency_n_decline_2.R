agency_n_decline_2 <- function(agency_n_decline) {
  
  agency_n_decline_2_fig<- agency_n_decline %>%
      group_by(PY) %>% 
      mutate(yr_total=sum(n)) %>% 
      mutate(agency_primary = fct_reorder(agency_primary, desc(n))) %>%
      mutate(PY=as.factor(PY)) %>% 
      drop_na() %>%
      # filter(author_position=="any") %>%
      filter(PY == "2023"|PY == "2024"| PY == "2025") %>%
      ggplot(aes(x = PY, y = perc_previous, fill = PY)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(limits=c(-30,20),breaks = seq(-30,20,by=5))+
      labs(x = "Year", size=5)+
      labs(y = "Percent Change in Productivity from Previous Year", size=5)+
      scale_fill_manual("legend",  values = c("#36648B","#36648B","#8B0000"))+
      theme_classic() +
      theme(legend.position="none")+
      theme(axis.text.y = element_text(size = 12))+
      theme(axis.text.x =element_text(size = 12))+
      theme(axis.title.y = element_text(size = 14))+
      theme(axis.title.x =element_text(size = 14))+
      geom_hline(yintercept = 0) +
      facet_wrap(vars(agency_primary),ncol = 3)+
      theme(strip.text = element_text(
        size = 10, color = "black"))
    # +
    #   gghighlight(PY==2025)
    
    ggsave("./docs/images/agency_n_decline_2.png", width = 6, height = 8, units = "in", device='png', dpi=700)
  
  
  return(agency_n_decline_2_fig)
  
}
