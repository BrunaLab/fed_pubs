pubs_per_month_cumulative_agency <- function(papers_dataset,authors_data_set, PY_max,PM_max) {

  
  papers_df_info<-papers_dataset %>% 
    select(refID,PY,PM)
  
  agencies<-c("doe",
              "hhs",
              "va",
              "dod",
              "usda",
              "commerce",
              "interior",
              "nasa",
              "smithsonian") %>% 
    tibble() %>% 
    rename(agency_primary=".")
  
  authors_df_info<- authors_data_set %>% 
    select(refID,agency_primary,agency,author_order) %>% 
    filter(agency_primary%in%agencies$agency_primary)
  
  
  
  pubs_mo_info <- left_join(papers_df_info,authors_df_info)
  unique(pubs_mo_info$agency_primary)

  
  pubs_mo<-pubs_mo_info %>% 
    filter(!is.na(agency_primary)) %>% 
    group_by(PM, PY,agency_primary) %>%
    tally() %>%
    mutate(PM=as.numeric(PM),
           PY=as.numeric(PY)) %>% 
    arrange(PY, PM) %>% 
    ungroup() %>% 
    mutate(month=row_number()) %>% 
    left_join(month) %>% 
    mutate(month_name=reorder(month_name,PM))
  
  unique(pubs_mo$agency_primary)
  
  library(gghighlight)
  
  
  
  pubs_mo_cum<-pubs_mo %>% 
    group_by(PY,agency_primary) %>% 
    mutate(cumul_pubs=cumsum(n)) 
  
  final_yr<-pubs_mo_cum %>% 
    filter(PY==PY_max) %>% 
    filter(PM<PM_max+1) 
  
  
  prior_yrs<-pubs_mo_cum %>% 
    filter(PY<PY_max+1)
  
  counter<-pubs_mo_cum %>% 
    select(PM,month_name) %>% 
    ungroup() %>% 
    select(-PY) %>% 
    distinct()
  
  prior_yrs_avg<-
  prior_yrs %>% 
    group_by(agency_primary,PM) %>% 
    mutate(n=mean(n)) %>% 
    group_by(agency_primary,PM) %>% 
    mutate(cumul=cumsum(n)) %>% 
    arrange(agency_primary,PM) %>% 
    select(PM,agency_primary,month_name,n) %>% 
    group_by(agency_primary,PM) %>% 
    slice_head(n=1) %>% 
    mutate(PY="avg") %>% 
    arrange(agency_primary,PM) %>% 
    group_by(agency_primary) %>% 
    mutate(cumul_pubs=cumsum(n)) 
    
  # 
  # prior_yrs_avg<-pubs_mo_cum %>% 
  #   filter(PY<PY_max) %>% 
  #   group_by(PM,agency_primary) %>% 
  #   summarize(n=mean(n)) %>% 
  #   mutate(cumul_pubs=cumsum(n)) %>% 
  #   mutate(PY="avg") %>% 
  #   left_join(counter) %>% 
  #   arrange(agency_primary,PM,cumul_pubs)
    
  
  plot_data<-bind_rows(final_yr,prior_yrs) %>% 
    mutate(PY=as.character(PY)) %>% 
    bind_rows(prior_yrs_avg) %>% 
    mutate(agency_primary=if_else(nchar(agency_primary)<5,str_to_upper(agency_primary),str_to_title(agency_primary)))
  
  
  # perc_change<-pubs_mo_cum %>% 
  #   filter(PM==PM_max)%>% 
  #   ungroup() %>% 
  #   mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
  # mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
  #   mutate(perc_previous=round(perc_previous,2))
  
  
  perc_change_avg<-final_yr %>% 
    mutate(PY=as.character(PY)) %>% 
    bind_rows(prior_yrs_avg) %>% 
    filter(PM==PM_max) 
  
  # nom<-perc_change_avg %>% filter(PY==PY_max) %>% select(cumul_pubs)
  # denom<-perc_change_avg %>% filter(PY=="avg") %>% select(cumul_pubs)  
  # nom<-perc_change_avg %>% filter(PY==PY_max) %>% arrange(agency_primary,PM) %>% rename(n25=n,cumul25=cumul_pubs)
  # denom<-perc_change_avg %>% filter(PY=="avg") %>% arrange(agency_primary,PM) %>% select(-month)
  # 
  # perc_change_avg<-left_join(nom,denom,by=c("agency_primary"))
  # perc_change_avg<-perc_change_avg %>% 
  #   mutate(perc_change=((cumul_pubs.x-cumul_pubs.y)/cumul_pubs.y*100))
  # 
  perc_change<-pubs_mo_cum %>% 
    filter(PM==PM_max)%>% 
    ungroup() %>% 
    arrange(agency_primary,PY) %>% 
    group_by(agency_primary) %>% 
    mutate(change_n = (cumul_pubs - lag(cumul_pubs))) %>%
    mutate(perc_previous = ((change_n) / lag(cumul_pubs)) * 100) %>% 
    mutate(perc_previous=round(perc_previous,1))
  # data_cumulative<-pubs_mo_cum %>% left_join(perc_change,by=c("PY","PM","month"))
  library(ggrepel)
  # label<-plot_data %>% group_by(PY) %>% filter(PM==max(PM)) %>% select(PM)
  library(forcats)
  
  write_csv(perc_change,"./docs/summary_info/perc_change_agency_fed.csv")
  perc_change<-perc_change %>% 
  mutate(agency_primary=if_else(nchar(agency_primary)<5,str_to_upper(agency_primary),str_to_title(agency_primary)))
  perc_change<-perc_change %>% 
    mutate(PY=as.character(PY)) %>% 
    rename(perc_change=perc_previous) %>% 
    filter(PY==2025) %>% 
    select(perc_change,agency_primary)
  
  
  plot_data<-plot_data %>% 
    left_join(perc_change,by="agency_primary") %>% 
    
    mutate(agency_primary=case_when(
      agency_primary == "DOE"~"Energy",
      agency_primary == "HHS"~"Health & Human Services",
      agency_primary == "VA"~"Veterans Affairs",
      agency_primary == "DHS"~"Homeland Security",
      agency_primary == "DOD"~"Defense",
      agency_primary == "USDA"~"Agriculture",
      agency_primary == "NASA"~"National Aeronautics & Space Admin",
      agency_primary == "Smithsonian"~"Smithsonian Institution",
      agency_primary == "NSF"~"National Science Foundation",
      agency_primary == "EPA"~"Environmental Protection Agency",
      agency_primary == "DOJ"~"Justice",
      agency_primary == "DOT"~"Transportation",
      agency_primary == "HUD"~"Housing & Urban Development",
      agency_primary == "Other"~"Other federal units",
      .default = as.character(agency_primary)
    )
    ) %>% 
    mutate(agency_primary=paste(agency_primary," (",perc_change,"%)", sep=""))
  
  
  
  order<-plot_data %>% group_by(agency_primary) %>% 
    mutate(rank=sum(n)) %>% 
    slice_head(n=1) %>% 
    arrange(desc(rank)) %>% 
    mutate(agency_primary = fct_reorder(agency_primary, rank)) %>%
    select(agency_primary)
  
  
  
  
  
  
  pubs_mo_cum_fig<-
    plot_data %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "avg", NA, as.character(label))) %>% 
    # mutate(label = if_else(PY == "Avg. (all yrs)", NA, as.character(label))) %>% 
    filter(PM<PM_max+1) %>% 
    mutate(label = if_else(PM == max(PM), as.character(PY), NA_character_)) %>% 
    mutate(label = if_else(PY == "avg", NA, as.character(label))) %>% 
    mutate(label = if_else((PY == "2019"|PY == "2020"|PY == "2021"|PY == "2022"|PY == "2023"), NA, as.character(label))) %>% 
    ggplot(aes(x=month_name, y=cumul_pubs,group=PY,color=PY,  linetype=PY))+
    labs(x = "Month", size=5)+
    labs(y = "No. of Publications", size=5)+
    geom_line() + 
    geom_point(size=0.5)+
    scale_color_manual(values=c(rep("lightgray",6),"#8B0000","#36648B"))+
    scale_linetype_manual(values = c(rep("solid", 6), "solid", "longdash"))+
    # expand_limits(y = 0)+
    expand_limits(x= c(0,PM_max + 1.25))+
    theme_classic()+
    facet_wrap(~factor(agency_primary, c(as.vector(order$agency_primary))),ncol = 3, scales = "free")+
    theme(panel.spacing = unit(0.5, "cm", data = NULL))+
    # scale_x_continuous( breaks=seq(1,12,by=1))+
    # scale_y_continuous(expand = c(0, 0), breaks=seq(0,(max(pubs_mo_cumulative %>% select(cumul_pubs))+5000),by=2500))+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.text.x =element_text(size = 10))+
    theme(axis.title.y = element_text(size = 12,face = "bold"))+
    theme(axis.title.x =element_text(size = 12,face = "bold"))+
    theme(strip.text.x = element_text(face = "bold"))+
    theme(legend.position="none") + 
    geom_label(aes(label = label), 
               # position="jitter",
               nudge_y = 0.8, 
               nudge_x = 0.5, 
               size =2.5,
               fill=NA
               # ,
               # label.size = unit(0,"mm")
    ) +
    theme(plot.background = element_rect(color = 1,
                                         size = 0),
          plot.margin = margin(t = 20,  # Top margin
                               r = 20,  # Right margin
                               b = 20,  # Bottom margin
                               l = 20))  # Left margin
  
  return(pubs_mo_cum_fig)

  }
