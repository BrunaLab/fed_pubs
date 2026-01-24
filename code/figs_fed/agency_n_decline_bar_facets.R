agency_n_decline_bar_facets <- function(agency_n_decline_first,threshhold_ct, PY_max) {
  
  library(tidyverse)
  library(patchwork)
  
  
  
    fig_data<-agency_n_decline_first %>% 
    filter(PY>2019) %>% 
      filter(agency_primary%in%threshhold_ct$agency_primary) %>% 
      mutate(PY=as.factor(PY)) %>% 
      mutate(agency_primary=if_else(nchar(agency_primary)<5,str_to_upper(agency_primary),str_to_title(agency_primary))) %>% 
      
      mutate(agency_primary=case_when(
        agency_primary == "DOE"~"(A) Dept Energy",
        agency_primary == "HHS"~"(B) Dept Health & Human Services",
        agency_primary == "VA"~"(C) Dept Veterans Affairs",
        agency_primary == "DOD"~"(D) Dept Defense",
        agency_primary == "USDA"~"(E) Dept Agriculture",
        agency_primary == "Commerce"~"(F) Dept Commerce",
        agency_primary == "Interior"~"(G) Dept Interior",
        agency_primary == "NASA"~"(H) NASA",
        agency_primary == "Smithsonian"~"(I) Smithsonian",
        agency_primary == "NSF"~"National Science Foundation",
        agency_primary == "EPA"~"Environmental Protection Agency",
        agency_primary == "DOJ"~"Justice",
        agency_primary == "DOT"~"Transportation",
        agency_primary == "HUD"~"Housing & Urban Development",
        agency_primary == "DHS"~"Homeland Security",
        agency_primary == "Other"~"Other federal units",
        .default = as.character(agency_primary)
      )
      )
    
    
  
    
    
    
    
    order<-fig_data %>% group_by(agency_primary) %>% 
      mutate(rank=sum(n)) %>% 
      slice_head(n=1) %>% 
      arrange(desc(rank)) %>% 
      mutate(agency_primary = fct_reorder(agency_primary, rank)) %>%
      select(agency_primary)
    
  
  # only_feds

  feds_fig<- fig_data %>%
        ggplot(aes(x=PY, 
               y=perc_previous,
               # group=PY,
               color=PY,
               fill=PY))+
    
    labs(x = "Year", size=7)+
    labs(y = "Percent change in productivity", size=5)+
    geom_bar(stat="identity", color="black")+
    scale_fill_manual(values=c("#8B0000",rep("#36648B",4),"#8B0000"))+
    geom_text(aes(label=n), position=position_dodge(width=0.9), color="black", size = 3)+
    
    facet_wrap(~factor(agency_primary, c(as.vector(order$agency_primary))),
               ncol = 3, 
               scales = "fixed",
               axes="all")+
    scale_y_continuous(breaks = scales::breaks_pretty(n=8))+
    # scale_y_continuous(expand = c(0, 0),breaks = scales::breaks_pretty(n=8))+ 
    theme(panel.spacing = unit(0.5, "cm", data = NULL))+
    
    theme_classic()+
    geom_hline(yintercept = 0)+
    theme(axis.text.y = element_text(size = 8))+
    theme(axis.title.y = element_text(size = 16))+
    # theme(axis.title.y = element_blank())+
    theme(axis.title.x =element_text(size = 16))+
    theme(axis.text.x =element_text(size = 8,angle = 45, vjust = 1, hjust=1))+
    # theme(axis.title.x = element_blank())+
    # theme(axis.text.x = element_blank())+
    theme(strip.text.x = element_text(face = "bold",hjust = 0,size=6))+ #hjust makes it flush left in strip instead of default center
    theme(strip.background.x = element_rect(fill = "white", color = "white", linetype = "solid", linewidth = 0))+
    theme(
      strip.placement = "outside")+
    theme(legend.position="none")+
    scale_y_continuous(n.breaks = 15, limits = c(min(fig_data$perc_previous)-1.5, max(fig_data$perc_previous)+1.5))+
    # ggtitle('(A) Federally affiliated first author' )+
    theme(plot.title=element_text(face='bold'))
  
    
    
  return(feds_fig)
  
}
