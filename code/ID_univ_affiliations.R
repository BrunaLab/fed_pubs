ID_univ_affiliations <- function(affils_df) {
uni_affils<-read_csv("./data_clean/api_uni_affils_searched_2025-09-01.csv") %>% 
  mutate_all(tolower) %>% 
  mutate(affil_id=as.numeric(affil_id)) %>% 
  mutate(uni=if_else(str_detect(uni,"uscd"),"ucsd",uni)) 


  affils_df   <-   affils_df %>% mutate_all(tolower) %>%
  mutate(affil_id = as.numeric(affil_id)) %>% 
  mutate(country = case_when(
    country == "united states" ~ "usa",
    country == "virgin islands (u.s.)" ~ "us virgin islands",
    country == "virgin islands (british)" ~ "british virgin islands",
    country == "viet nam" ~ "vietnam",
    country == "united kingdom" ~ "uk",
    country == "cote d'ivoire" ~ "ivory coast",
    TRUE ~ country  # keep original value if no match
  ))

  # standardize COUNTRIES -----------------------------------------------
  # levels(as.factor(affils_df$country))

  
  affils_df <- affils_df %>%
    mutate(affil_id=as.numeric(affil_id)) %>% 
    mutate(country = case_when(
      country == "united states" ~ "usa",
      country == "virgin islands (u.s.)" ~ "us virgin islands",
      country == "virgin islands (british)" ~ "british virgin islands",
      country == "viet nam" ~ "vietnam",
      country == "united kingdom" ~ "uk",
      country == "cote d'ivoire" ~ "ivory coast",
      TRUE ~ country  # keep original value if no match
    ))
  
# 
  affils_df <- left_join(affils_df, uni_affils, by = c("affil_id")) %>%
    mutate(uni=if_else(str_detect(uni,"uscd"),"ucsd",uni)) %>% 
    relocate(c(affil_id), .after = entry_no) %>%
    mutate(city=coalesce(city.x,city.y)) %>% 
    mutate(affiliation=coalesce(affiliation.x,affiliation.y)) %>% 
    mutate(country=coalesce(country.x,country.y)) %>% 
    # mutate(uni=coalesce(uni.x,uni.y)) %>% 
    # mutate(cat=coalesce(cat.x,cat.y)) %>% 
    select(-c(city.y, 
              city.x,
              country.x,
              country.y,
              # uni.x,
              # uni.y,
              affiliation.x,
              affiliation.y
              )) 


  
  
  penn<-c(
    114817694 
  )
  
  missing_uni<-affils_df %>% 
    filter(is.na(uni)) %>% 
    select(affil_id,affiliation,uni,city,country,cat) %>% 
    distinct() %>% 
    mutate(uni=if_else(str_detect(affiliation,"harvard"),"harvard",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"stanford"),"stanford",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"stanford"),"stanford",uni)) %>% 
    # mutate(uni=if_else(str_detect(affiliation,"university of california"),"uc",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"ucla"),"ucla",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"ucsf"),"ucsf",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"ucsd"),"ucsd",uni)) %>% 
    mutate(uni=if_else((str_detect(affiliation,"university of california")&(str_detect(affiliation,"san diego"))),
                                  "ucsd",uni)) %>% 
    mutate(uni=if_else((str_detect(affiliation,"university of california")&(str_detect(affiliation,"los angeles"))),
                       "ucla",uni)) %>% 
    mutate(uni=if_else((str_detect(affiliation,"university of california")&(str_detect(affiliation,"san francisco"))),
                       "ucsf",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"massachusetts general"),"mass_general",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"university of north carolina"),"unc_ch",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"university of minnesota"),"minn",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"ohio state university"),"ohio_state",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"university of washington"),"washington",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"university of michigan"),"michigan",uni)) %>% 
    mutate(uni=if_else(str_detect(affiliation,"university of pennsylvania"),"penn",uni)) %>% 
    mutate(uni=case_when(
      affil_id%in%penn~"penn",
      TRUE ~ uni  # keep original value if no match
    )
    )
  

  
   not_focal<-c(
    60021492,
    60006371,
    131352402,
    60016655,
    123697382,
    60029245,
    130143638,
    114437724,
    113110961,
    60000149,
    60022682,
    128970932,
    132389340,
    60019187,
    60122465,
    60006951,
    60029194,
    60028785,
    60093602,
    60018474
  )
  
   
  missing_uni<-missing_uni %>% 
    mutate(uni=if_else(affil_id%in%not_focal,NA,uni)) %>% 
    select(affil_id,uni) %>% 
    filter(!is.na(uni)) %>% 
    mutate(cat="final_check")
  
  affils_df<-affils_df %>% 
    left_join(missing_uni,by="affil_id") %>% 
    mutate(uni=coalesce(uni.x,uni.y)) %>% 
    mutate(cat=coalesce(cat.x,cat.y)) %>% 
    select(-uni.x,
           -uni.y,
           -cat.x,
           -cat.y)
  
  
  
  # mutate(uni=if_else(str_detect(affiliation,"beth israel deaconess medical center"),"harvard",uni)) %>% 
  # these are the harvard hispitals, not counting as harvbard unless changed here
  # mutate(uni=if_else(str_detect(affiliation,"boston children’s hospital"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"brigham and women’s hospital"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"cambridge health alliance"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"dana-farber cancer institute"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"harvard pilgrim health care institute"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"hebrew senior life"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"joslin diabetes center"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"judge baker children’s center"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"massachusetts eye and ear | schepens eye research institute"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"massachusetts general hospital"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"mclean hospital"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"mount auburn hospital"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"spaulding rehabilitation hospital"),"harvard",uni)) %>% 
  # mutate(uni=if_else(str_detect(affiliation,"va boston healthcare system"),"harvard",uni)) %>% 
  
  return(affils_df)
}
