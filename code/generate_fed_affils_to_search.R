generate_fed_affils_to_search<-function(){
    library(tidyverse)
  
  # need to bind with orginal_search.txt or unique_affils.txt
  
  fed_affils1<-read_csv("./data_raw/affiliations_to_search/fed_affils/agency_redux.csv") %>% 
    rename(affil_id=scopus_code) %>% 
    distinct()
  fed_affils2<-read_csv("./data_raw/affiliations_to_search/fed_affils/agencies_redux_clean.csv") %>% 
    distinct()
  fed_affils3<-read_csv("./data_raw/affiliations_to_search/fed_affils/agencies_orig_clean.csv") %>% 
    distinct()
  
  fed_affils_search<-fed_affils1 %>% 
    full_join(fed_affils2) %>% 
    mutate(df="redux") %>% 
    full_join(fed_affils3,by=c("affil_id","affiliation")) %>% 
    mutate(city.x=if_else(is.na(city.x),city.y,city.x)) %>% 
    mutate(city.y=if_else(city.x==city.y,NA,city.y)) %>% 
    mutate(df.y=if_else(is.na(df.y),df.x,df.y)) %>% 
    select(-pub_count.x,
           -pub_count.y,
           -country.x,
           -country.y,
           -df.x,
           -city.y
    ) %>% 
    distinct(affil_id,agency,affiliation,city.x,df.y) %>% 
    rename(city=`city.x`,
           search=`df.y`)
  
  
  all_affils_list<-read_rds("./data_clean/affils_df_clean.rds") %>% 
    filter(federal==TRUE) %>% 
    select(affil_id,affiliation,agency,agency_primary,city) %>% 
    distinct(affil_id,.keep_all = TRUE) %>% 
    arrange(agency_primary,agency,affiliation,affil_id) %>% 
    mutate(affiliation=gsub("[(]","",affiliation)) %>% 
    mutate(affiliation=gsub("[)]","",affiliation)) 
  

  # ADD THESE TO SEARCH
  add<-anti_join(fed_affils_search,all_affils_list,by="affil_id") %>% 
    mutate(agency_primary=agency) %>%  
    mutate_all(tolower) %>% 
    mutate(agency_primary=
             case_when(
               agency_primary=="agriculture"~"usda",
               agency_primary=="defense"~"dod",
               agency_primary=="health and human services"~"hhs",
               agency_primary=="transportation"~"dot",
               agency_primary=="homeland security"~"dhs",
               agency_primary=="justice"~"doj",
               agency_primary=="energy"~"doe",
               agency_primary=="veterans affairs"~"va",
               agency_primary=="usaid"~"state",
               affil_id==122706146~"usda",
               affil_id==100460855~"epa",
               affil_id==112883912~"epa",
               affil_id==106755309~"va",
               affil_id==106704878~"dod",
               affil_id==130182073~"dod",
               affil_id==109340860~"dod",
               affil_id==124348139~"dod",
               affil_id==129812372~"dod",
               affil_id==60023515~"dod",
               affil_id==130181631~"dod",
               affil_id==130182348~"dod",
               affil_id==132480895~"dod",
               affil_id==130760098~"dod",
               affil_id==125199744~"dhs", 
               affil_id==107992717~"state",
               affil_id==132469325~"nih", 
               affil_id==129286023~"nih", 
               affil_id==118866485~"nih", 
               affil_id==109562444~"nih",
               affil_id==101958371~"interior", 
               affil_id==130982352~"interior", 
               affil_id==125199744~"interior", 
               affil_id==113421607~"interior", 
               affil_id==100619967~"interior", 
               affil_id==128153744~"interior", 
               affil_id==126471690~"interior",
               affil_id==127070220~"interior", 
               affil_id==127823370~"interior",
               affil_id==130337838~"usda", 
               affil_id==110325684~"usda", 
               affil_id==100659542~"usda",
               affil_id==130982100~"dot", 
               affil_id==123917710~"commerce", 
               affil_id==130374704~"commerce", 
               affil_id==131558893~"other", 
               affil_id==113210840~"va", 
               affil_id==121947101~"va", 
               affil_id==121623565~"va", 
               affil_id==109525091~"va", 
               affil_id==115489430~"va", 
               affil_id==122400747~"va", 
               affil_id==113216446~"va", 
               affil_id==105840497~"va", 
               affil_id==129482029~"va", 
               affil_id==131695955~"va", 
               affil_id==108199661~"va", 
               .default = as.character(agency_primary)
             )
    ) %>% 
    mutate(affil_id=as.double(affil_id)) %>% 
    filter(!is.na(agency_primary))
  
  affils_to_search_api<-bind_rows(all_affils_list,add)
  
  affils_to_search_api<-affils_to_search_api %>% 
    mutate(affiliation = gsub("u.s.","us", affiliation)) %>%
    mutate(affiliation = gsub("u. s.","us", affiliation)) %>%
    mutate(affiliation = gsub(" - ","-", affiliation)) %>% 
    mutate(federal=TRUE) %>% 
    mutate(search=replace_na(search,"returned_in_search"))
  
  
  write_csv(affils_to_search_api,paste("./data_clean/api_fed_affils_searched_",Sys.Date(),".csv",sep=""))
  return(affils_to_search_api)
}