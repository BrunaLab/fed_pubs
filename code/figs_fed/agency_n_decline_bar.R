agency_n_decline_bar <- function(agency_n_decline_sum, PY_max) {
  
  library(tidyverse)
  library(patchwork)
  
  agency_n_decline_sum<-agency_n_decline_sum %>% 
    # filter(PY>2019) %>% 
    filter(PY>2020) %>%   
    mutate(PY=as.factor(PY)) 
    
  
  # only_feds

  only_feds <-agency_n_decline_sum %>%
    filter(cat=="only_feds") 
  
  only_feds_fig<- only_feds %>%
        ggplot(aes(x=PY, 
               y=perc_previous,
               # group=PY,
               color=PY,
               fill=PY))+
    
    labs(x = "Year", size=7)+
    labs(y = "Percent change in productivity", size=5)+
    geom_bar(stat="identity", color="black")+
    scale_fill_manual(values=c(rep("#36648B",4),"#8B0000"))+
    # scale_fill_manual(values=c("#8B0000",rep("#36648B",4),"#8B0000"))+
    geom_text(aes(label=cumul_pubs), position=position_dodge(width=0.9), color="black")+
    theme_classic()+
    geom_hline(yintercept = 0)+
    theme(axis.text.y = element_text(size = 10))+
    # theme(axis.title.y = element_text(size = 16))+
    theme(axis.title.y = element_blank())+
    # theme(axis.title.x =element_text(size = 16))+
    # theme(axis.text.x =element_text(size = 10))+
    # theme(axis.title.x = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(legend.position="none")+
    scale_y_continuous(n.breaks = 10, limits = c(min(agency_n_decline_sum$perc_previous)-1.5, max(agency_n_decline_sum$perc_previous)+1.5))+
    ggtitle('(A) All authors federally affiliated' )+
    theme(plot.title=element_text(face='bold'))
  
    
    
  
  # firs_feds
  
  
  first_feds <-agency_n_decline_sum %>%
    filter(cat=="first_fed") 
  
    first_feds_fig<- first_feds %>%
    ggplot(aes(x=PY, 
               y=perc_previous,
               # group=PY,
               color=PY,
               fill=PY))+
    
    labs(x = "Year", size=7)+
    labs(y = "Percent change in productivity", size=5)+
    geom_bar(stat="identity", color="black")+
    # scale_fill_manual(values=c("#8B0000",rep("#36648B",4),"#8B0000"))+
      scale_fill_manual(values=c(rep("#36648B",4),"#8B0000"))+
    geom_text(aes(label=cumul_pubs), position=position_dodge(width=0.9), color="black")+
    theme_classic()+
    geom_hline(yintercept = 0)+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.title.y = element_text(size = 16))+
    # theme(axis.title.y = element_blank())+
    # theme(axis.title.x =element_text(size = 16))+
    # theme(axis.text.x =element_text(size = 10))+
    theme(axis.title.x = element_blank())+
    theme(axis.text.x = element_blank())+
    theme(legend.position="none")+
    scale_y_continuous(n.breaks = 12, limits = c(min(agency_n_decline_sum$perc_previous)-1.5, max(agency_n_decline_sum$perc_previous)+1.5))+
    ggtitle('(B) Federally affiliated first author' )+
    theme(plot.title=element_text(face='bold'))
  
  
  
    # any_feds
    
    
    any_feds <-agency_n_decline_sum %>%
      filter(cat=="any_fed") 
    
    any_feds_fig<- any_feds %>%
      ggplot(aes(x=PY, 
                 y=perc_previous,
                 # group=PY,
                 color=PY,
                 fill=PY))+
      
      labs(x = "Year", size=7)+
      labs(y = "Percent change in productivity", size=5)+
      geom_bar(stat="identity", color="black")+
      # scale_fill_manual(values=c("#8B0000",rep("#36648B",4),"#8B0000"))+
      scale_fill_manual(values=c(rep("#36648B",4),"#8B0000"))+
      geom_text(aes(label=cumul_pubs), position=position_dodge(width=0.9), color="black")+
      theme_classic()+
      geom_hline(yintercept = 0)+
      theme(axis.text.y = element_text(size = 10))+
      # theme(axis.title.y = element_text(size = 16))+
      theme(axis.title.y = element_blank())+
      theme(axis.title.x =element_text(size = 16))+
      theme(axis.text.x =element_text(size = 10))+
      # theme(axis.title.x = element_blank())+
      # theme(axis.text.x = element_blank())+
      theme(legend.position="none")+
      scale_y_continuous(n.breaks = 12, limits = c(min(agency_n_decline_sum$perc_previous)-1.5, max(agency_n_decline_sum$perc_previous)+1.5))+
      ggtitle('(C) Any author federally affiliated' )+
      theme(plot.title=element_text(face='bold'))
    
    
    
    agency_n_decline_sum_fig<- only_feds_fig/first_feds_fig/any_feds_fig
    
    
  return(agency_n_decline_sum_fig)
  
}
