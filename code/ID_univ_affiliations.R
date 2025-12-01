ID_univ_affiliations <- function(affils_df,date) {

  
  
  
  
  
  
  if(date=="20250901"){
    uni_affils<-read_csv("./data_clean/api_uni_affils_searched_2025-09-01.csv")
  }
  
  
  if(date=="20251010"){
    uni_affils<-read_csv("./data_clean/api_uni_affils_searched_2025-10-18.csv")
    
  }
  
  
  uni_affils<-uni_affils %>% 
  mutate_all(tolower) %>% 
  mutate(affil_id=as.numeric(affil_id))


  
  
  affils_df   <-   affils_df %>% 
    mutate_all(tolower) %>%
    mutate(affil_id = as.numeric(affil_id))
  
  
  
  affils_df_clean<-affils_df %>% 
    select( affil_id,affiliation,city, country) %>% 
    distinct()
  
  affils_df_clean<-affils_df_clean  %>% 
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

  
  affils_df_clean <- affils_df_clean %>%
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
  affils_df_clean <- left_join(affils_df_clean, uni_affils, by = c("affil_id")) %>%
    relocate(c(affil_id), .before = 1) %>%
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
  
  
  
  
  
  
  # 
  # missing_uni<-affils_df %>% 
  #   filter(is.na(uni)) %>% 
  #   select(affil_id,affiliation,uni,city,country,cat) %>% 
  #   distinct() %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"harvard"),"harvard",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"stanford"),"stanford",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"stanford"),"stanford",uni)) %>% 
  #   # mutate(uni=if_else(str_detect(affiliation,"university of california"),"uc",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"ucla"),"ucla",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"ucsf"),"ucsf",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"ucsd"),"ucsd",uni)) %>% 
  #   mutate(uni=if_else((str_detect(affiliation,"university of california")&(str_detect(affiliation,"san diego"))),
  #                                 "ucsd",uni)) %>% 
  #   mutate(uni=if_else((str_detect(affiliation,"university of california")&(str_detect(affiliation,"los angeles"))),
  #                      "ucla",uni)) %>% 
  #   mutate(uni=if_else((str_detect(affiliation,"university of california")&(str_detect(affiliation,"san francisco"))),
  #                      "ucsf",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"massachusetts general"),"mass_general",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"university of north carolina"),"unc_ch",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"university of minnesota"),"minn",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"ohio state university"),"ohio_state",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"university of washington"),"washington",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"university of michigan"),"michigan",uni)) %>% 
  #   mutate(uni=if_else(str_detect(affiliation,"university of pennsylvania"),"penn",uni)) 
  # %>% 
    # mutate(uni=case_when(
    #   affil_id%in%penn~"penn",
      # TRUE ~ uni  # keep original value if no match
    # )
    # )
  

  
  missing_uni <- affils_df_clean %>%
    filter(is.na(uni)) %>%
    select(affil_id, affiliation, uni, city, country, cat) %>%
    distinct() %>%
    mutate(
      uni = case_when(
        str_detect(affiliation, "harvard") ~ "harvard",
        str_detect(affiliation, "stanford") ~ "stanford",
        str_detect(affiliation, "ucla") ~ "ucla",
        str_detect(affiliation, "ucsf") ~ "ucsf",
        str_detect(affiliation, "ucsd") ~ "ucsd",
        str_detect(affiliation, "university of california") & str_detect(affiliation, "san diego") ~ "ucsd",
        str_detect(affiliation, "university of california") & str_detect(affiliation, "los angeles") ~ "ucla",
        str_detect(affiliation, "university of california") & str_detect(affiliation, "san francisco") ~ "ucsf",
        str_detect(affiliation, "massachusetts general") ~ "mass_general",
        str_detect(affiliation, "university of north carolina") ~ "unc_ch",
        str_detect(affiliation, "university of minnesota") ~ "minn",
        str_detect(affiliation, "ohio state university") ~ "ohio_state",
        str_detect(affiliation, "university of washington") ~ "washington",
        str_detect(affiliation, "university of michigan") ~ "michigan",
        str_detect(affiliation, "university of pennsylvania") ~ "penn",
        str_detect(uni,"uscd")~"ucsd", 
        TRUE ~ uni
      )
    )
   
   
  
  affils_df_clean<-affils_df_clean %>% 
    left_join(missing_uni,by="affil_id") %>% 
    mutate(uni=coalesce(uni.x,uni.y)) %>% 
    mutate(cat=coalesce(cat.x,cat.y)) %>% 
    mutate(city=coalesce(city.x,city.y)) %>% 
    mutate(affiliation=coalesce(affiliation.x,affiliation.y)) %>% 
    mutate(country=coalesce(country.x,country.y)) %>% 
    select(-uni.x,
           -uni.y,
           -cat.x,
           -cat.y,
           -city.x,
           -city.y,
           -affiliation.x,
           -affiliation.y,
           -country.x,
           -country.y)
  
  
  
  
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
    60138958,
    60093602,
    60272417,
    60014232,
    60073077,
    60292186,
    60018474,
    60006602,
    60016643,
    60020284,
    60274218,
    60274219,
    60002860,
    60013177,
    60018833,
    60030273,
    60104690,
    126229901, # stanford hall
    124936723, # defence medical rehabilitation centre stanford hall
    101768949, 131497585, 131525999, # harvard eye associates
    121401745, # harvard pilgrim healthcare
    100588742, # harvard shoulder service
    132494657, # harvard street neighborhood health center
    127944790, # harvard street neighborhood health center inc
    129188623, # harvard vanguard medical
    107921429, # harvard vanguard medical association
    100686306, # harvard-westlake school
    60272417, # massachusetts green high performance computing center
    130330420, # mdunc family medicine (adjunct faculty)
    131752484, # pardee/ unc healthcare
    124032297, #pastoral care services rex healthcare university of north carolina
    60138929, # the ohio state university at marion
    132318527, # usc- university of michigan
    132246908, # university of michigan-flin
    60076959, # saint mary’s university of minnesota
    129575199, # university of minnesota independent consultant
    60073077, # health alliance international
    60009875, # university of minnesota duluth
    60138958, # national center on homelessness among veterans
    125262309, # university of minnesota morris (umn morris)
    125785988, # va puget sound and the university of washington
    60292186, # washington sea grant
    60120583,
    60121420, # policy analysis for california education
    60028457,
    60100090,
    60097058,
    128326215,
    60019829
  )
  
  
  penn<-c(
    114817694 
  )
  
  affils_df_clean <- affils_df_clean %>%
    mutate(uni = case_when(
      affil_id%in%penn ~ "penn",
      affil_id%in%not_focal ~ NA,
      TRUE ~ uni  # keep original value if no match
    ))
  
  
  # affils_df %>% filter(affil_id==114817694)
  # affils_df %>% filter(affil_id==60097058)
  
  # 
  # affils_df<-affils_df %>% 
  #   mutate(affiliation=gsub("[.]","",affiliation)) %>% 
  #   mutate(affiliation=gsub("-"," ",affiliation)) %>% 
  #   mutate(affiliation=gsub("[/ ]"," ",affiliation)) %>% 
  #   mutate(affiliation=gsub("[; ]","  ",affiliation)) %>% 
  #   mutate(affiliation=gsub("[;]","",affiliation)) %>% 
  #   mutate(affiliation=gsub("[/]"," ",affiliation)) %>% 
  #   mutate(affiliation=gsub(", "," ",affiliation)) %>% 
  #   mutate(affiliation=gsub(","," ",affiliation)) %>% 
  #   mutate(affiliation=gsub("(","",affiliation)) %>% 
  #   mutate(affiliation=gsub(")","",affiliation)) %>% 
  #   mutate(affiliation=gsub("'s","",affiliation)) %>% 
  #   mutate(affiliation=gsub("depart ","dept ",affiliation)) %>% 
  #   mutate(affiliation=gsub("department ","dept ",affiliation)) %>% 
  #   mutate(affiliation=gsub("the university ","university  ",affiliation)) %>% 
  #   mutate(affiliation=gsub("harvard university","harvard",affiliation)) %>% 
  #   # mutate(affiliation=gsub("michigan's","michigan",affiliation)) %>% 
  #   # mutate(affiliation=gsub("minnesota's","minnesota",affiliation)) %>% 
  #   # mutate(affiliation=gsub("washington's","washington",affiliation)) %>% 
  #   mutate(affiliation=gsub("university of pennsylvania","penn",affiliation)) %>% 
  #   mutate(affiliation=gsub("upenn","penn",affiliation)) %>% 
  #   mutate(affiliation=gsub("sciencesuniversity of florida","sciences university of florida",affiliation)) %>% 
  #   mutate(affiliation=if_else(str_detect(affiliation,"california san diego u of"),"ucsd",affiliation)) %>% 
  #   mutate(affiliation=if_else(str_detect(affiliation,"uc san diego"),"ucsd",affiliation)) %>% 
  #   mutate(affiliation=if_else(str_detect(affiliation,"at sail diego"),"ucsd",affiliation)) %>% 
  #   mutate(affiliation=if_else(str_detect(affiliation,"at sail diego"),"ucsd",affiliation)) %>% 
  #   mutate(affiliation=if_else(str_detect(affiliation,"university of california san diego"),"ucsd",affiliation)) %>% 
  #   mutate(affiliation=if_else(str_detect(affiliation,"university of california san francisco"),"ucsf",affiliation)) %>% 
  #   # mutate(affiliation=if_else(str_detect(affiliation," ucsf "),"",affiliation)) %>% 
  #   mutate(affiliation=if_else(str_detect(affiliation,"university of california san francusco"),"ucsf",affiliation)) 
  # 
  
  affils_df_clean <- affils_df_clean %>%
    mutate(
      affiliation = str_replace_all(
        affiliation,
        c(
          "[.]" = "",
          "-" = " ",
          "[/ ]" = " ",
          "[; ]" = " ",
          "[;]" = "",
          "[/]" = " ",
          ", " = " ",
          "," = " ",
          "\\(" = "",
          "\\)" = "",
          "'s" = "",
          "\\bdepart\\b" = "dept",
          "\\bdepartment\\b" = "dept",
          "\\bthe university\\b" = "university",
          "\\bharvard university\\b" = "harvard",
          "\\buniversity of pennsylvania\\b" = "penn",
          "\\bupenn\\b" = "penn",
          "sciencesuniversity of florida" = "sciences university of florida"
        )
      ),
      affiliation = case_when(
        str_detect(affiliation, "california san diego u of") ~ "ucsd",
        str_detect(affiliation, "uc san diego") ~ "ucsd",
        str_detect(affiliation, "at sail diego") ~ "ucsd",
        str_detect(affiliation, "university of california san diego") ~ "ucsd",
        str_detect(affiliation, "university of california san francisco") ~ "ucsf",
        str_detect(affiliation, "university of california san francusco") ~ "ucsf",
        affil_id==60121668~"department of bioengineering",
        affil_id==60006369~"koç university",
        affil_id==60021351~"memorial hospital central",
        TRUE ~ affiliation
      )
    )
  
  
  # # THESE ARE PARTNERSHIPS WITH UNI MED SCHOOL, not included in analyses or other campuses
  # # UW
  # 60006602 university of washington, tacoma
  # 60016643 university of washington-bothell
  # # PENN
  # 60020284 lancaster general hospital
  # 60274218 chester county hospital
  # 60274219 princeton health
  # # UCSF
  # 60002860 children’s hospital oakland research institute
  # #OSU
  # 60013177 the ohio state university at newark
  # 60018833 the ohio state university at mansfield
  # 60030273 the ohio state university at lima
  # 
  # #harvard
  # 
  # 60104690 harvard pilgrim health care institute
  # 60120583 ariadne labs
  # 60028457 partners healthcare personalized medicine
  # 60019829 jdrf center for immunological tolerance in type 1 diabetes
  
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
  
  treat_as_other<-c(
    60006602,
    60016643,
    60020284,
    60274218,
    60274219,
    60002860,
    60013177,
    60018833,
    60030273,
    108006165,
    60104690,
    121401745,
    60120583,
    60028457,
    60019829,
    60120814, #UCSD and UCSF databank
    128505247,
    124376601, # stanford va alzheimer’s center
    60032063, # lucile packard children’s hospital stanford
    125822358, # palo alto vahcs/stanford som
    131765196, # family support services at ronald mcdonald house at stanford
    60026163, # hoover institution
    108061986, 109850837, # mit and harvard
    113949049, # multi-regional clinical trials center of brigham and women’s hospital and harvard
    125436009, # multi-regional clinical trials center of brigham and women’s hospital and harvard
    126050963, # ucsf-stanford pediatric device consortium
    132854924, # ucsf–stanford pediatric device consortium
    115019335, # uscf-stanford center of excellence in regulatory science and innovation (cersi)
    126907022, # los angeles county department of mental health + ucla strike team
    130457150, # olive view- ucla education and research institute
    60014575, # olive view-ucla medical center
    128550328, # gladstone|ucsf center for cell circuitry
    127630794, # gladstone–ucsf institute of genomic immunology
    127941013, #from mass general brigham harvard plastic surgery
    60007838, # dana-farber/harvard cancer center
    131778861, # broad inst
    60001001   # broad inst
  )
  
  
  affils_df_clean<-affils_df_clean %>% 
  mutate(uni=if_else(affil_id%in%treat_as_other,"other",uni)) %>% 
    distinct(affil_id,cat,uni)
  
  
  
  affils_df<-affils_df %>% select(refID,affil_url,affil_id,affiliation,entry_no) %>% 
    left_join(affils_df_clean,by="affil_id")
  
  
  return(affils_df)
}
