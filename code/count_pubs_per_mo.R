count_pubs_per_mo <- function(papers_dataset) {
    
    pubs_mo <-
      papers_dataset %>%
      group_by(PM, PY) %>%
      tally() %>%
      mutate(PM=as.numeric(PM),
             PY=as.numeric(PY)) %>% 
      arrange(PY, PM) %>% 
      ungroup() %>% 
      mutate(month=row_number()) %>% 
      left_join(month) %>% 
      mutate(month_name=reorder(month_name,PM))
    
    return(pubs_mo)
  }