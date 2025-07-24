
# load packages -----------------------------------------------------------



library(rscopus)
library(tidyverse)
library(janitor)


# read in the affiliations and query scopus for details -------------------

# REDUX (25 June 2025) -------------------------------------------------------


affils_df<-read_csv("./data_raw/agencies/agency_redux.csv") 

search_term<-affils_df
term <- seq_along(search_term$scopus_code)


all_affils <- data.frame()

for (h in term){
  
  query_string <-search_term[h,1]
  
  scopus_data_raw <- tryCatch({
    get_affiliation_info(
      affil_id = query_string,
      api_key = "e72ca4ba01261a08226d7ea9775ace33",
      verbose = TRUE
    )
  }, error = function(e) {
    message("Error retrieving data for affil_id: ", query_string)
    return(NULL)
  })
  
  
  # Skip if the result is NULL or empty
  if (is.null(scopus_data_raw)){
    next
  }else{
    all_affils <- bind_rows(all_affils, scopus_data_raw)
  }
}


all_affils_redux<-all_affils %>% 
  select("affil_id",
         affiliation="affil_name",
         pub_count="document-count",
         "city",
         "country")  
           

write_csv(all_affils_redux,  "./data_raw/agencies/agencies_redux_clean.csv")


# fed_affiliations_list Number 1. -------------------------------------


fed_affils_original<-read_rds("./data_clean/fed_affiliations_list.rds") 

search_term<-fed_affils_original
term <- seq_along(search_term$affil_id)


all_affils <- data.frame()

for (h in term){
  
  query_string <-search_term[h,1]
  
  
  scopus_data_raw <- tryCatch({
    get_affiliation_info(
      affil_id = query_string,
      api_key = "8e204bc721cb41c0251c8846351342b0",
      verbose = TRUE
    )
  }, error = function(e) {
    message("Error retrieving data for affil_id: ", query_string)
    return(NULL)
  })
  
  
  # Skip if the result is NULL or empty
  if (is.null(scopus_data_raw)){
    next
  }else{
    all_affils <- bind_rows(all_affils, scopus_data_raw)
  }
}
 

all_affils_orig<-all_affils %>% 
  select("affil_id",
         affiliation="affil_name",
         pub_count="document-count",
         "city",
         "country") 
  



write_csv(all_affils_orig,  "./data_raw/agencies/agencies_orig_clean.csv")





# Load and stitch all lists of affils together ------------------------

affils_redux <- read_csv("./data_raw/agencies/agencies_redux_clean.csv") %>%
  mutate(affil_id=as.character(affil_id)) %>% 
  mutate_all(tolower) %>%
  mutate(df="redux") %>% 
  relocate(df,.before=1)

affils_original <- read_csv("./data_raw/agencies/agencies_orig_clean.csv") %>% 
  mutate_all(tolower) %>% 
  mutate(affil_id=as.character(affil_id)) %>% 
  mutate(df="original_1") %>% 
  relocate(df,.before=1)

affils_df_original_1 <- full_join(affils_redux,affils_original,by="affil_id")

affils_df_original_2<-read_rds("./data_clean/affils_df_clean.rds") %>% 
  filter(federal==TRUE) %>% 
  select(affil_id,affiliation,agency_full.x,agency) %>% 
  mutate(df="original_2") %>% 
  relocate(df,.before=1) %>% 
  mutate(affil_id=as.character(affil_id))



affils_all<-full_join(affils_df_original_1,affils_df_original_2,by="affil_id") %>% 
  relocate("df.y",.after=1) %>% 
  relocate("df",.before=1)



rm(affils_redux,
   affils_original,
   affils_df_original_1,
   affils_df_original_2)


# cleanup -----------------------------------------------------------------

affils_all<-affils_all %>% 
  mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  rename(affil1=affiliation.x,
         affil2=affiliation.y,
         affil3=agency_full.x,
         abbrev=affiliation) 


affils_all<-affils_all %>% 
mutate(country.x=
         case_when(
           country.x == "united states" ~ "usa",
           country.x == "viet nam" ~ "vietnam",
           .default = as.character(country.x)
         )) %>% 
  mutate(across(
    c(country.x, country.y),
    ~ case_when(
      country.y == "united states" ~ "usa",
      country.y == "viet nam" ~ "vietnam",
      .default = as.character(.)
  ))) %>% 
  mutate(country.x=if_else(is.na(country.x),country.y,country.x)) %>% 
  rename(country=country.x) %>% 
  select(-country.y) %>% 
  mutate(city.x=if_else(is.na(city.x),city.y,city.x)) %>% 
  rename(city=city.x) %>% 
  select(-city.y) %>% 
  mutate(pub_count.x=if_else(is.na(pub_count.x),pub_count.y,pub_count.x)) %>% 
  rename(pub_count=pub_count.x) %>% 
  select(-pub_count.y)

affils_all<-affils_all %>% 
  rename(orig_2=df,
         orig_1=df.y,
         redux=df.x
         ) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("[.]", "", .)
  )) %>% 
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("[,]", "", .)
  )) %>% 
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub(" - ", "", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub(" – ", "", .)
  )) %>% 
  
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("u s ", "us ", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("the us ", "us ", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("united states ", "us ", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("amp;", "", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("engineers ’ engineer", "engineers engineer", .)
  )) %>% 
  
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("corps of engineers", "corps of engineers ", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("corps of engineers  ", "corps of engineers ", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("the us ", "us ", .)
  )) %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev),
    ~ gsub("united state ", "us ", .)
  ))  %>% 
  
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev,agency),
    ~ gsub("americorps vista", "americorps", .)
  ))  %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev,agency),
    ~ gsub("corporation for national and community service", "americorps", .)
  ))  %>% 
  
  mutate(across(
    c(affil1, affil2, affil3, abbrev,agency),
    ~ gsub("americorps vista", "americorps", .)
  ))  




affils_all



    affils_all$affil_check<-affils_all$affil2==affils_all$affil1
    affils_all<-affils_all %>% 
      mutate(affil2=if_else((affil_check==FALSE | is.na(affil_check)), affil2, NA))
    
    affils_all$affil_check<-affils_all$affil2==affils_all$affil3
    
    affils_all<-affils_all %>% 
      mutate(affil3=if_else((affil_check==FALSE | is.na(affil_check)), affil3, NA)) %>% 
      mutate(affil1=if_else(is.na(affil1),affil2,affil1))
    
    affils_all$affil_check<-affils_all$abbrev==affils_all$agency
    
    affils_all<-affils_all %>% 
      mutate(agency=if_else((affil_check==FALSE | is.na(affil_check)), agency, NA)) %>% 
      rename(abbrev2=agency) %>% 
      mutate(affil3=if_else(affil3==affil2,NA,affil3)) %>% 
  mutate(affil1=if_else(is.na(affil1),affil3,affil1))

      
      affils_all$affil_check<-affils_all$affil3==affils_all$abbrev2
      
      affils_all<-affils_all %>% 
        mutate(abbrev2=if_else((affil_check==FALSE | is.na(affil_check)), abbrev2, NA)) 
      
affils_all<-affils_all %>% 
  remove_empty(c("cols","rows")) %>% 
  select(-affil_check) %>% 
  mutate_all(trimws)



# find missing affiliations -----------------------------------------------


affils_all<- distinct(affils_all)



run_for_affil<-affils_all %>% 
  filter(is.na(affil1)) %>% 
  select(affil_id)


search_term<-run_for_affil
term <- seq_along(search_term$affil_id)


all_affils_missing <- data.frame()

for (h in term){
  
  query_string <-search_term[h,1]
  
  
  scopus_data_raw <- tryCatch({
    get_affiliation_info(
      affil_id = query_string,
      api_key = "8d8d7b628fae6e1a5a04db969b0bca93",
      verbose = TRUE
    )
  }, error = function(e) {
    message("Error retrieving data for affil_id: ", query_string)
    return(NULL)
  })
  
  
  # Skip if the result is NULL or empty
  if (is.null(scopus_data_raw)){
    next
  }else{
    all_affils_missing <- bind_rows(all_affils_missing, scopus_data_raw)
  }
}



missing_affils<-all_affils_missing %>% 
  mutate_all(tolower) %>% 
  select(affil_id,
         affil4=affil_name,
         country,
         city,
         pub_count = `document-count`
         ) %>% 
  mutate(df="missing") %>% 
  mutate(affil_id=as.character(affil_id)) %>% 
  tibble()



affils_all<-left_join(affils_all, missing_affils,by=c("affil_id")) %>% 
  relocate(affil2,.after="affil1") %>% 
  relocate(affil3,.after="affil2") %>% 
  relocate(df,.before=1) %>% 
  rename(missing=df)


affils_all<-affils_all %>% 
  mutate(affil1=if_else(is.na(affil1),affil4,affil1)) %>% 
  mutate(country.x=if_else(is.na(country.x),country.y,country.x)) %>% 
  mutate(city.x=if_else(is.na(city.x),city.y,city.x)) %>% 
  mutate(pub_count.x=if_else(is.na(pub_count.x),pub_count.y,pub_count.x)) %>% 
  select(-pub_count.y,
         -city.y,
         -country.y)
# duplicates? -------------------------------------------------------------

write_rds(affils_all,"./data_intermediate/redux_affils.rds")

# affils_all<-read_rds("./data_intermediate/redux_affils.rds")

affils_all %>% 
  group_by(affil_id,abbrev) %>% 
  tally() %>% 
  filter(n>1) %>% 
  arrange(desc(n))



affils_all<-
affils_all %>% 
  group_by(affil_id,abbrev) %>% 
  slice_head(n=1)



# remove from fed ---------------------------------------------------------

nonfed<-c(127078141,
132488945,
128609279,
123764501,
128145323,
126369743,
60106655,
125117795,
106562201,
126149601,
125136769,
128064856,
121848755,
125377399,
113727207,
128853540,
119493734,
113727207,
129556287,
101869475,
129349573,
116261440,
110141225,
101967749,
101967749,
120290071,
121302036,
128186702,
122485294,
128186702,
60096341,
123794871,
118462400,
131383200,
105428030,
127687531,
60096341,
60023260,
60004062,
60162091,
121302036,
130623897,
101967749,
127677508,
110075293,
121502463,
125125015,
129587280,
125868899,
130790299,
126243689,
124235648,
109520920)

affils_all<-affils_all %>% filter(!affil_id%in%nonfed)


write_rds(affils_all,"./data_intermediate/redux_affils.rds")


# 2x
# 109520920 national center for genome resources
# 124520435 Riverside Technology, Inc. (Government Contractor for NOAA/NCEI)
# 126412492 LLC (Stationed at U.S. Army Combat Capabilities Development Command (DEVCOM) Army Research Laboratory (ARL) Biotechnology Branch)
# 124366686 DHW Consulting and US Census Bureau
# 129074890 LLC. Under Contract to NOAA Southwest Fisheries Science Center
# 60103346 Consortium of Universities for the Advancement of Hydrologic Science, Inc
# 125366315 Walker Environmental Research LLC Contractor to the U.S. Geological Survey
# 131099988 CASE Consultants International under contract to NOAA Fisheries NEFSC
# 127423673 Consultant for the US Forest Service International Programs
# 129027339 Inc. under contract to the U.S. Geological                                                  
# 124284470 U.S. Naval Observatory (contractor)                                                         
# 124218979 US Department of State contractor                                                           
# 129787947 USAID Global Health Technical Professionals contract                                        
# 130939726 General Dynamics Information Technology (GDIT) contractor supporting CDCs COVID-19 Response
# 129402606 JHT on contract with NOAA National Ocean Service                                            
# 131291408 under contract to NOAA Office of Coastal Management                                         
# 131291373 under contract to NOAA Coral Reef Conservation Program                                      
# 119756321 Cherokee Nation Technologies contracted to U.S. Geological Survey                           
# 125341111 National Oceanic and Atmospheric Administration contractor                                  
# 125125015 CSS Inc. Under contract to NOS/NOAA                                                         
# 131099988 CASE Consultants International under contract to NOAA Fisheries NEFSC                       
# 123102696 Work done under contract to U.S. Geological Survey                                          
# 118735849 ASRC Federal InuTeq (contractor to the U.S. Geological Survey)





# need to search for pubs for these for all years -------------------------

search_for_these1<-affils_all %>% 
  filter(!is.na(redux) &
           is.na(orig_2))



search_for_these2<-affils_all %>% 
  filter(!is.na(redux) &
           is.na(orig_1))


search_for_these<-bind_rows(search_for_these1,search_for_these2)

# download records for each affil in each year ----------------------------

new_2025_search<-affils_all %>% 
  select(affil_id) %>% 
  distinct()

yr1=2025
yr2=2025

search_term<-new_2025_search$affil_id

date_range <- seq(yr1,yr2)
year <- seq_along(date_range)
term <- seq_along(search_term)

for (h in term){
  
  for (j in year) {
    
    # a<-paste("(AF-ID('",search_term[h],"') AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
    a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
    # a<-paste("((AFFIL(",search_term[h],")"," AND AFFILCOUNTRY('united states')) AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
    
    # c <- " AND PUBYEAR > 2018"
    # query_string <-paste0(a, c,")",sep = "")
    c <- " AND PUBYEAR = "
    
    query_string <-paste0(a, c, date_range[j],")",sep = "")
    
    
    
    # api1: 38c1ea28aed25f40f11034d20557ccde
    # 8d8d7b628fae6e1a5a04db969b0bca93
    # api2: 8e204bc721cb41c0251c8846351342b0
    # api3: c253aa47dd592442b1d5ad7ded7b0514
    # api4: 8d8d7b628fae6e1a5a04db969b0bca93
    
    # c253aa47dd592442b1d5ad7ded7b0514 throttled 5/16
    scopus_data <- rscopus::scopus_search(query_string,
                                          max_count=8000,
                                          view = "COMPLETE",
                                          api_key = "e72ca4ba01261a08226d7ea9775ace33")
    
    
    
    # query_string <- paste0("eid(2-s2.0-0024266051)")
    scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
    # nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3
    if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
      next
    }else{
      scopus_papers <- scopus_data_raw$df
      
      term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
      papers <- paste("./data_raw/scopus_api/papers/",term_for_file,"_papers", ".csv", sep = "")
      write_csv(scopus_papers, papers)
      
      scopus_affiliations <- scopus_data_raw$affiliation
      affils <- paste("./data_raw/scopus_api/affils/",term_for_file,"_affils_", ".csv", sep = "")
      write_csv(scopus_affiliations, affils)
      
      scopus_authors <- scopus_data_raw$author
      authors <- paste("./data_raw/scopus_api/authors/",term_for_file,"_authors", ".csv",sep = "")
      write_csv(scopus_authors, authors)
    }
  }
}








# final join fed affils ---------------------------------------------------

affils_1<-read_rds("./data_intermediate/redux_affils.rds")

affils_2 <- read_csv("./data_clean/complete_affil_list.csv") %>%
  rename(affil_id = scopus_affil_id) %>%
  select(-source.x, -source.y) %>%
  mutate(federal = TRUE) %>%
  # select(-agency_full) %>%
  distinct(affil_id, agency, city, agency_full, .keep_all = TRUE) %>%
  select(-acronym) %>% 
  mutate(affil_id=as.character(affil_id))
# 
# affils_df$affil_id <- as.numeric(affils_df$affil_id)
# fed_affils_scopus
# affils_3 <- read_csv("./data_clean/results_datasetv1/fed_affils_scopus.csv") %>%
#   select(
#     affil_id = scopus_affil_id,
#     agency_full = affiliation,
#     agency,
#     country
#   ) %>% 
#   mutate(affil_id=as.character(affil_id))

all_fed_affils <- full_join(affils_1, affils_2, by = "affil_id")
# all_fed_affils <- full_join(all_fed_affils, affils_3, by = "affil_id")

all_fed_affils<-all_fed_affils %>% ungroup()


all_fed_affils <- all_fed_affils %>%
  mutate_all(tolower) %>%
  # relocate(agency.y, .after = "agency.x") %>%
  # relocate(agency_full.y, .after = "agency_full.x") %>%
  # relocate(agency_full.y, .after = "agency_full.x") %>%
  relocate(city.x, .after = "city") %>%
  relocate(affil4, .after = "affil3") %>%
  # relocate(country.x.x, .after = "country.x") %>%
  relocate(country.x, .after = "country") 


all_fed_affils$country.x[all_fed_affils$country.x == "united states"] <- "usa"
all_fed_affils$country.x[all_fed_affils$country.x == "virgin islands (u.s.)"] <- "us virgin islands"
all_fed_affils$country.x[all_fed_affils$country.x == "viet nam"] <- "vietnam"
all_fed_affils$country.x[all_fed_affils$country.x == "cote d'ivoire"] <- "ivory coast"

all_fed_affils$country[all_fed_affils$country == "united states"] <- "usa"
all_fed_affils$country[all_fed_affils$country == "virgin islands (u.s.)"] <- "us virgin islands"
all_fed_affils$country[all_fed_affils$country == "viet nam"] <- "vietnam"
all_fed_affils$country[all_fed_affils$country == "cote d'ivoire"] <- "ivory coast"


# 
# 
# all_fed_affils$country.x.x[all_fed_affils$country.x.x == "united states"] <- "usa"
# all_fed_affils$country.x.x[all_fed_affils$country.x.x == "virgin islands (u.s.)"] <- "us virgin islands"
# all_fed_affils$country.x.x[all_fed_affils$country.x.x == "viet nam"] <- "vietnam"
# all_fed_affils$country.x.x[all_fed_affils$country.x.x == "cote d'ivoire"] <- "ivory coast"
# 

# all_fed_affils$country.y[all_fed_affils$country.y == "united states"] <- "usa"
# all_fed_affils$country.y[all_fed_affils$country.y == "virgin islands (u.s.)"] <- "us virgin islands"
# all_fed_affils$country.y[all_fed_affils$country.y == "viet nam"] <- "vietnam"
# all_fed_affils$country.y[all_fed_affils$country.y == "cote d'ivoire"] <- "ivory coast"



all_fed_affils$affil_check<-all_fed_affils$country.x==all_fed_affils$country
all_fed_affils<-all_fed_affils %>% 
  mutate(country.x=if_else((affil_check==FALSE | is.na(affil_check)), country.x, NA))
# 
# all_fed_affils$affil_check<-all_fed_affils$country.x==all_fed_affils$country.x.x
# all_fed_affils<-all_fed_affils %>% 
#   mutate(country.x.x=if_else((affil_check==FALSE | is.na(affil_check)), country.x.x, NA))
# 
all_fed_affils<-all_fed_affils %>% 
  mutate(country=if_else(is.na(country), country.x, country)) 

all_fed_affils<-all_fed_affils %>% 
  select(-country.x)



all_fed_affils$affil_check<-all_fed_affils$city==all_fed_affils$city.x

all_fed_affils<-all_fed_affils %>% 
  mutate(city.x=if_else((affil_check==FALSE | is.na(affil_check)), city.x, NA))
all_fed_affils<-all_fed_affils %>% 
  mutate(city=if_else(is.na(city), city.x, city)) %>% 
  select(-city.x)


all_fed_affils$affil_check<-all_fed_affils$agency==all_fed_affils$agency_full

all_fed_affils<-all_fed_affils %>% 
  mutate(agency=if_else((affil_check==FALSE | is.na(affil_check)), agency, NA))


all_fed_affils$affil_check<-all_fed_affils$affil1==all_fed_affils$affil4

all_fed_affils<-all_fed_affils %>% 
  mutate(affil4=if_else((affil_check==FALSE | is.na(affil_check)), affil4, NA)) %>% 
select(-affil4)


all_fed_affils$affil_check<-all_fed_affils$affil1==all_fed_affils$affil2

all_fed_affils<-all_fed_affils %>% 
  mutate(affil2=if_else((affil_check==FALSE | is.na(affil_check)), affil2, NA)) %>% 
  select(-affil2)


all_fed_affils$affil_check<-all_fed_affils$affil1==all_fed_affils$agency_full

all_fed_affils<-all_fed_affils %>% 
  mutate(affil1=if_else((affil_check==FALSE | is.na(affil_check)), affil1, NA)) %>% 
  mutate(agency_full=if_else(is.na(agency_full), affil1, agency_full)) %>% 
  select(-affil1)


all_fed_affils$affil_check<-all_fed_affils$affil3==all_fed_affils$agency_full

all_fed_affils<-all_fed_affils %>% 
  mutate(affil3=if_else((affil_check==FALSE | is.na(affil_check)), affil3, NA)) %>% 
  select(-affil3)
  

all_fed_affils$affil_check<-all_fed_affils$abbrev2==all_fed_affils$agency_full

all_fed_affils<-all_fed_affils %>% 
  mutate(abbrev2=if_else((affil_check==FALSE | is.na(affil_check)), abbrev2, NA)) 

all_fed_affils$affil_check<-all_fed_affils$abbrev2==all_fed_affils$agency

all_fed_affils<-all_fed_affils %>% 
  mutate(abbrev2=if_else((affil_check==FALSE | is.na(affil_check)), abbrev2, NA)) %>% 
  mutate(agency=if_else(is.na(agency), abbrev2, agency)) %>% 
  select(-abbrev2)

all_fed_affils<-all_fed_affils %>% 
  mutate(agency=if_else(is.na(agency), abbrev, agency)) %>% 
  select(-abbrev) %>% 
  rename(abbrev=agency)


names(all_fed_affils)
all_fed_affils<-all_fed_affils %>% 
  select(-missing,
         -orig_2,
         # -redux,
         -orig_1,
         -pub_count.x,
         -affil_check) %>% 
  mutate(federal=TRUE) 

all_fed_affils<-all_fed_affils %>% 
  relocate(federal,.after=1) %>% 
  relocate(country,.after="agency_full")

all_fed_affils<-all_fed_affils %>% 
  group_by(affil_id,abbrev,agency_full) %>% 
  slice_head(n=1) %>% 
  group_by(affil_id,agency_full) %>% 
  slice_head(n=1) %>% 
  mutate(redux=if_else(is.na(redux),"2x_affil",redux)) %>% 
  group_by(affil_id,agency_full) %>% 
  slice_head(n=1) 

  

write_csv(all_fed_affils,"./data_clean/fed_affil_list_9july2025.csv")



# simplified federal affils -----------------------------------------------


library(tidyverse)
library(janitor)



affil1<-read_csv("./data_clean/complete_affil_list.csv") %>% 
  rename(affil_id=scopus_affil_id) %>% 
  select(affil_id,
        agency_short=agency,
        affiliation= agency_full,
         acronym,
         city,
         country) %>% 
  mutate_all(tolower)

affil2<-read_csv("./data_raw/agencies/agencies_orig_clean.csv") %>% 
  select(affil_id,
         affiliation,
         city,
         country) %>% 
  mutate_all(tolower)


affil3<-read_csv("./data_raw/agencies/agencies_redux_clean.csv") %>% 
    mutate_all(tolower)


affil_ids<-full_join(affil1,affil2,by="affil_id") %>% 
  full_join(affil3,by="affil_id") %>% 
  distinct() 
  
  affil_ids<-affil_ids %>% 
  mutate(affiliation = gsub("&amp;", "and", affiliation)) %>%
  mutate(affiliation = gsub("u s ", "us ", affiliation)) %>%
  mutate(affiliation = gsub("united states ", "us ", affiliation)) %>%
  mutate(affiliation = gsub("u.s. ", "us ", affiliation)) %>%
  mutate(affiliation = gsub("u. s. ", "us ", affiliation)) %>%
  mutate(affiliation = gsub("[.]", "", affiliation)) %>% 
  
  mutate(affiliation.y = gsub("&amp;", "and", affiliation.y)) %>%
  mutate(affiliation.y = gsub("u s ", "us ", affiliation.y)) %>%
  mutate(affiliation.y = gsub("united states ", "us ", affiliation.y)) %>%
  mutate(affiliation.y = gsub("u.s. ", "us ", affiliation.y)) %>%
  mutate(affiliation.y = gsub("u. s. ", "us ", affiliation.y)) %>%
  mutate(affiliation.y = gsub("[.]", "", affiliation.y))




affil_ids$country.x[affil_ids$country.x == "united states"] <- "usa"
affil_ids$country.x[affil_ids$country.x == "virgin islands (u.s.)"] <- "us virgin islands"
affil_ids$country.x[affil_ids$country.x == "viet nam"] <- "vietnam"
affil_ids$country.x[affil_ids$country.x == "cote d'ivoire"] <- "ivory coast"


affil_ids$country.y[affil_ids$country.y == "united states"] <- "usa"
affil_ids$country.y[affil_ids$country.y == "virgin islands (u.s.)"] <- "us virgin islands"
affil_ids$country.y[affil_ids$country.y == "viet nam"] <- "vietnam"
affil_ids$country.y[affil_ids$country.y == "cote d'ivoire"] <- "ivory coast"


affil_ids$country[affil_ids$country == "united states"] <- "usa"
affil_ids$country[affil_ids$country == "virgin islands (u.s.)"] <- "us virgin islands"
affil_ids$country[affil_ids$country == "viet nam"] <- "vietnam"
affil_ids$country[affil_ids$country == "cote d'ivoire"] <- "ivory coast"




affil_ids$affil_check<-affil_ids$country.x==affil_ids$country
affil_ids<-affil_ids %>% 
  mutate(country.x=if_else((affil_check==FALSE | is.na(affil_check)), country.x, NA)) %>% 
  mutate(country=if_else(is.na(country), country.x, country)) %>% 
  select(-country.x,
         -pub_count)

affil_ids$affil_check<-affil_ids$country.y==affil_ids$country
affil_ids<-affil_ids %>% 
  mutate(country.y=if_else((affil_check==FALSE | is.na(affil_check)), country.y, NA)) %>% 
  mutate(country=if_else(is.na(country), country.y, country)) %>% 
  select(-country.y)





affil_ids$affil_check<-affil_ids$city.x==affil_ids$city
affil_ids<-affil_ids %>% 
  mutate(city.x=if_else((affil_check==FALSE | is.na(affil_check)), city.x, NA)) %>% 
  mutate(city=if_else(is.na(city), city.x, city)) %>% 
  select(-city.x
         )

affil_ids$affil_check<-affil_ids$city.y==affil_ids$city
affil_ids<-affil_ids %>% 
  mutate(city.y=if_else((affil_check==FALSE | is.na(affil_check)), city.y, NA)) %>% 
  mutate(city=if_else(is.na(city), city.y, city)) %>% 
  select(-city.y)







affil_ids$affil_check<-affil_ids$affiliation==affil_ids$affiliation.x
affil_ids<-affil_ids %>% 
  mutate(affiliation.x=if_else((affil_check==FALSE | is.na(affil_check)), affiliation.x, NA)) %>% 
  mutate(affiliation=if_else(is.na(affiliation), affiliation.x, affiliation))

affil_ids$affil_check<-affil_ids$affiliation==affil_ids$affiliation.y
affil_ids<-affil_ids %>% 
  mutate(affiliation.y=if_else((affil_check==FALSE | is.na(affil_check)), affiliation.y, NA)) %>% 
  mutate(affiliation=if_else(is.na(affiliation), affiliation.y, affiliation)) %>% 

  select(-affiliation.x,
         -affiliation.y)






affil_ids<-affil_ids %>% 
mutate(agency_short = if_else(country == "usa" & str_detect(affiliation, "brigade|group|combat|battalion|usmc|regiment|command"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(country == "usa" & str_detect(affiliation, "defence|wing|squadron|division|cavalry| usn |field hospital|medical wing"), "dod", agency_short)) %>%
  mutate(agency_short = case_when(
    affiliation %in% c("inova center of outcomes research") ~ NA,
    TRUE ~ as.character(agency_short)
  )) %>% 
  mutate(agency_short = if_else(affiliation == "americorps vista", "americorps", agency_short)) %>%
  
  mutate(
    agency_short =
      case_when(
        str_detect(affiliation, "agency for healthcare research and quality") ~ "ahrq",
        str_detect(affiliation, "us department of health and human services") ~ "hhs",
        str_detect(affiliation, "us army") ~ "dod",
        country == "usa" & str_detect(affiliation, "department of the air force") ~ "dod",
        str_detect(affiliation, "smithsonian") ~ "smithsonian",
        str_detect(affiliation, "us department of energy") ~ "doe",
        str_detect(affiliation, "us epa") ~ "epa",
        str_detect(affiliation, "usepa") ~ "epa",
        str_detect(affiliation, "epa region") ~ "epa",
        str_detect(affiliation, "department of the army") ~ "dod",
        str_detect(affiliation, "usda") ~ "usda",
        str_detect(affiliation, "us department of labor") ~ "labor",
        country == "usa" & str_detect(affiliation, "naval") ~ "dod",
        country == "usa" & str_detect(affiliation, "veteran ") ~ "va",
        country == "puerto rico" & str_detect(affiliation, "veteran") ~ "va",
        country == "usa" & str_detect(affiliation, "veterans ") ~ "va",
        country == "usa" & str_detect(affiliation, "veteran's ") ~ "va",
        str_detect(affiliation, "us department of commerce") ~ "commerce",
        str_detect(affiliation, "us department of defense") ~ "dod",
        str_detect(affiliation, "us department of housing and urban development") ~ "hud",
        str_detect(affiliation, "us department of veteran") ~ "va",
        # str_detect(affiliation,"us department of veterans affairs") ~ "va",
        str_detect(affiliation, "veterans affairs medical center") ~ "va",
        # str_detect(affiliation,"us department of veteran affairs") ~ "va",
        str_detect(affiliation, "us environmental protection agency") ~ "epa",
        str_detect(affiliation, "us department of transportation") ~ "dot",
        str_detect(affiliation, "us attorney general") ~ "doj",
        country == "usa" & str_detect(affiliation, "national institutes of health") ~ "nih",
        country == "usa" & str_detect(affiliation, "army ") ~ "dod",
        str_detect(affiliation, "us department of the interior") ~ "interior",
        str_detect(affiliation, "us department of agriculture") ~ "usda",
        str_detect(affiliation, "national oceanic and atmospheric administration") ~ "noaa",
        country == "usa" & str_detect(affiliation, "nasa") ~ "nasa",
        country == "usa" & str_detect(affiliation, "national science foundation") ~ "nsf",
        country == "usa" & str_detect(affiliation, "navy ") ~ "dod",
        str_detect(affiliation, "us department of the navy") ~ "dod",
        str_detect(affiliation, "us nav") ~ "dod",
        str_detect(affiliation, "us air force") ~ "dod",
        str_detect(affiliation, "us marine corps") ~ "dod",
        str_detect(affiliation, "us air force") ~ "dod",
        str_detect(affiliation, "usaid") ~ "usaid",
        country == "usa" & str_detect(affiliation, "centers for disease control and prevention") ~ "cdc",
        country == "usa" & str_detect(affiliation, "national institutes of health") ~ "nih",
        str_detect(affiliation, "us department of state") ~ "state",
        str_detect(affiliation, "us fish and wildlife service") ~ "interior",
        country == "usa" & str_detect(affiliation, "food and drug administration") ~ "fda",
        country == "usa" & str_detect(affiliation, "us department of agriculture") ~ "usda",
        str_detect(affiliation, "lawrence berkeley national laboratory") ~ "doe",
        str_detect(affiliation, "smithsonian") ~ "smithsonian",
        country == "usa" & str_detect(affiliation, "national park service") ~ "interior",
        country == "usa" & str_detect(affiliation, "veterans") ~ "va",
        country == "usa" & str_detect(affiliation, "national science foundation") ~ "nsf",
        country == "usa" & str_detect(affiliation, "usdi bureau of land management") ~ "interior",
        country == "usa" & str_detect(affiliation, "national academy of medicine") ~ "nasem",
        country == "usa" & str_detect(affiliation, "national academies of sciences, engineering, and medicine") ~ "nasem",
        country == "usa" & str_detect(affiliation, "national estuary program") ~ "interior",
        country == "usa" & str_detect(affiliation, "division of strategic national stockpile") ~ "hhs",
        country == "usa" & str_detect(affiliation, "bureau of oceans and international environmental") ~ "nsf",
        country == "usa" & str_detect(affiliation, "us dot university transportation center for") ~ "dot",
        country == "usa" & str_detect(affiliation, "bureau of ocean energy management") ~ "interior",
        str_detect(affiliation, "us department of energy") ~ "doe",
        str_detect(affiliation, "us geological survey") ~ "interior",
        str_detect(affiliation, "us department of education") ~ "interior",
        str_detect(affiliation, "us department of homeland security") ~ "dhs",
        str_detect(affiliation, "us department of justice") ~ "doj",
        str_detect(affiliation, "us department of the treasury") ~ "treasury",
        str_detect(affiliation, "us department of transportation") ~ "dot",
        str_detect(affiliation, "us cdc") ~ "cdc",
        str_detect(affiliation, "us embassy") ~ "state",
        str_detect(affiliation, "us peace corps") ~ "state",
        str_detect(affiliation, "us office of naval") ~ "dod",
        str_detect(affiliation, "us 8th army") ~ "dod",
        str_detect(affiliation, "us president") ~ "eop",
        str_detect(affiliation, "us forces") ~ "dod",
        str_detect(affiliation, "us military") ~ "dod",
        str_detect(affiliation, "us national institute of allergy and infectious diseases") ~ "nih",
        str_detect(affiliation, " us forest service") ~ "usda",
        str_detect(affiliation, " us aid") ~ "state",
        str_detect(affiliation, " us department of defence") ~ "dod",
        str_detect(affiliation, " us walter reed") ~ "dod",
        str_detect(affiliation, " us mission") ~ "state",
        str_detect(affiliation, " us bureau of land management") ~ "interior",
        str_detect(affiliation, " us antarctic program") ~ "nsf",
        str_detect(affiliation, " niaid/nih international centers for excellence in ") ~ "nih",
        str_detect(affiliation, " us centers for disease control") ~ "cdc",
        str_detect(affiliation, "uniformed services university of the health sciences") ~ "dod",
        str_detect(affiliation, "walter reed") ~ "dod",
        str_detect(affiliation, "us agency for international development") ~ "usaid",
        country == "usa" & str_detect(affiliation, "national park") ~ "interior",
        country == "usa" & str_detect(affiliation, "national park") ~ "interior",
        country == "usa" & str_detect(affiliation, "national seashore") ~ "interior",
        country == "puerto rico" & str_detect(affiliation, "national park") ~ "interior",
        country == "northern mariana islands" & str_detect(affiliation, "national park") ~ "interior",
        country == "american samoa" & str_detect(affiliation, "national park") ~ "interior",
        country == "guam" & str_detect(affiliation, "national park") ~ "interior",
        country == "virgin islands (u.s.)" & str_detect(affiliation, "national park") ~ "interior",
        str_detect(affiliation, "us department of veterans affairs portland") ~ "va",
        str_detect(affiliation, "national telecommunications and information administration") ~ "commerce",
        str_detect(affiliation, "us national archives and records administration") ~ "national archives",
        str_detect(affiliation, "national institute for mathematical and biological synthesis") ~ "nsf",
        str_detect(affiliation, "office of national drug control policy") ~ "eop",
        str_detect(affiliation, "national center for preparedness, detection, and control of infectious diseases") ~ "cdc",
        str_detect(affiliation, "national socio-environmental synthesis center") ~ "nsf",
        str_detect(affiliation, "national evolutionary synthesis center") ~ "nsf",
        str_detect(affiliation, "national ecological observatory network") ~ "nsf",
        str_detect(affiliation, "us department of interior") ~ "interior",
        str_detect(affiliation, "national historical park") ~ "interior",
        str_detect(affiliation, "fredrick national laboratory") ~ "nih",
        str_detect(affiliation, "national park service social science program") ~ "interior",
        affiliation == "national park service social science program" ~ "interior",
        affiliation == "us preventive services task force" ~ "ahrq",
        affiliation == "ahrq" ~ "ahrq",
        affiliation == "congressional budget office" ~ "cbo",
        affiliation == "national center for injury prevention and control" ~ "cdc",
        affiliation == "national center for emerging and zoonotic infectious diseases" ~ "cdc",
        affiliation == "national center for health statistics" ~ "cdc",
        affiliation == "national center for chronic disease prevention and health promotion" ~ "cdc",
        affiliation == "national center for birth defects and developmental disabilities" ~ "cdc",
        affiliation == "national center for immunization and respiratory diseases" ~ "cdc",
        affiliation == "cdc" ~ "cdc",
        affiliation == "national center for environmental health" ~ "cdc",
        affiliation == "national center for disease control and public health" ~ "cdc",
        affiliation == "agency for toxic substance and disease registry" ~ "cdc",
        affiliation == "contracting agency to the division of viral diseases" ~ "cdc",
        affiliation == "central intelligence agency" ~ "cia",
        affiliation == "us patent and trademark office" ~ "commerce",
        affiliation == "us house of representatives" ~ "congress",
        affiliation == "us senate" ~ "congress",
        affiliation == "us botanic garden" ~ "congress",
        affiliation == "national gallery of art" ~ "congress",
        affiliation == "national museum of health and medicine" ~ "dha",
        affiliation == "defense health agency" ~ "dod",
        affiliation == "national research institute" ~ "dod",
        affiliation == "defense threat reduction agency" ~ "dod",
        affiliation == "defense advanced research projects agency" ~ "dod",
        affiliation == "us pacific fleet" ~ "dod",
        affiliation == "defense logistics agency" ~ "dod",
        affiliation == "national geospatial-intelligence agency" ~ "dod",
        affiliation == "san antonio military medical center" ~ "dod",
        affiliation == "air force institute of technology" ~ "dod",
        affiliation == "82nd airborne division" ~ "dod",
        affiliation == "us fleet forces command" ~ "dod",
        affiliation == "national guard bureau" ~ "dod",
        affiliation == "96th medical group" ~ "dod",
        affiliation == "us dep of the navy" ~ "dod",
        affiliation == "defense nuclear agency" ~ "dod",
        affiliation == "nmrc" ~ "dod",
        affiliation == "institute of infectious diseases" ~ "dod",
        affiliation == "wrair" ~ "dod",
        affiliation == "us combat casualty care research program" ~ "dod",
        affiliation == "defense intelligence agency" ~ "dod",
        affiliation == "national defense university" ~ "dod",
        affiliation == "northwest national laboratory" ~ "dod",
        affiliation == "99th medical group" ~ "dod",
        affiliation == "88th medical group" ~ "dod",
        affiliation == "national center for telehealth and technology" ~ "dod",
        affiliation == "us department of army" ~ "dod",
        affiliation == "national strategic research institute" ~ "dod",
        affiliation == "us air war college" ~ "dod",
        affiliation == "us baylor military graduate program in nutrition" ~ "dod",
        affiliation == "defense information systems agency" ~ "dod",
        affiliation == "air force" ~ "dod",
        affiliation == "national war college" ~ "dod",
        affiliation == "us armed forces health surveillance division" ~ "dod",
        affiliation == "us dep of the army" ~ "dod",
        affiliation == "us marine" ~ "dod",
        affiliation == "erdc" ~ "dod",
        affiliation == "us marine forces cyberspace command" ~ "dod",
        affiliation == "us armed services blood program office" ~ "dod",
        affiliation == "us coast guard" ~ "dod",
        affiliation == "military vaccine agency" ~ "dod",
        affiliation == "defence health agency" ~ "dod",
        affiliation == "air combat command" ~ "dod",
        affiliation == "hurricane flood risk reduction design branch" ~ "dod",
        affiliation == "97th military police battalion" ~ "dod",
        affiliation == "us second fleet" ~ "dod",
        affiliation == "defense pow/mia accounting agency" ~ "dod",
        affiliation == "national immunization program" ~ "dod",
        affiliation == "lawrence livermore national laboratory" ~ "doe",
        affiliation == "doe" ~ "doe",
        affiliation == "pacific northwest national laboratory" ~ "doe",
        affiliation == "oak ridge national laboratory" ~ "doe",
        affiliation == "brookhaven national laboratory" ~ "doe",
        affiliation == "national renewable energy laboratory" ~ "doe",
        affiliation == "national center for electron microscopy" ~ "doe",
        affiliation == "los alamos national laboratory" ~ "doe",
        affiliation == "argonne national laboratory" ~ "doe",
        affiliation == "jet propulsion laboratory" ~ "doe",
        affiliation == "princeton plasma physics laboratory" ~ "doe",
        affiliation == "us iter project office" ~ "doe",
        affiliation == "fermi national accelerator laboratory" ~ "doe",
        affiliation == "nevada national security site" ~ "doe",
        affiliation == "slac national accelerator laboratory" ~ "doe",
        affiliation == "national energy technology laboratory" ~ "doe",
        affiliation == "savannah river national laboratory" ~ "doe",
        affiliation == "federal energy regulatory commission" ~ "doe",
        affiliation == "idaho national laboratory" ~ "doe",
        affiliation == "oak ridge" ~ "doe",
        affiliation == "advanced research projects agency - energy" ~ "doe",
        affiliation == "thomas jefferson national accelerator facility" ~ "doe",
        affiliation == "national nuclear security administration" ~ "doe",
        affiliation == "ames laboratory" ~ "doe",
        affiliation == "national high magnetic field laboratory los almos" ~ "doe",
        affiliation == "federal bureau of investigation" ~ "doj",
        affiliation == "federal medical center, rochester" ~ "doj",
        affiliation == "us bureau of alcohol" ~ "doj",
        affiliation == "national institute of justice" ~ "doj",
        affiliation == "fbi" ~ "doj",
        affiliation == "federal highway administration" ~ "dot",
        affiliation == "national highway traffic safety administration" ~ "dot",
        affiliation == "federal railroad administration" ~ "dot",
        affiliation == "us national security council" ~ "eop",
        affiliation == "us national security advisor" ~ "eop",
        affiliation == "national health and environmental effects research laboratory" ~ "epa",
        affiliation == "us research laboratory" ~ "epa",
        affiliation == "enivronmental protection agency" ~ "epa",
        affiliation == "federal aviation administration" ~ "faa",
        affiliation == "national center for toxicological research" ~ "fda",
        affiliation == "us food and drug admnistration" ~ "fda",
        affiliation == "center for biologics evaluation and research" ~ "fda",
        affiliation == "federal emergency management agency" ~ "fema",
        affiliation == "federal housing finance agency" ~ "fhfa",
        affiliation == "federal reserve system" ~ "frs",
        affiliation == "denver federal center" ~ "gsa",
        affiliation == "hrsa" ~ "hhs",
        affiliation == "health resources and services administration" ~ "hhs",
        affiliation == "department of housing and urban development" ~ "hud",
        affiliation == "patuxent wildlife research center" ~ "interior",
        affiliation == "bureau of land management" ~ "interior",
        affiliation == "national wetlands research center" ~ "interior",
        affiliation == "us fish and wildlife national forensics laboratory" ~ "interior",
        affiliation == "fish and wildlife service" ~ "interior",
        affiliation == "usfws" ~ "interior",
        affiliation == "usfs medicine bow/routt national forests and thunder basin national grassland" ~ "interior",
        affiliation == "us geoheritage and geoparks advisory group" ~ "interior",
        affiliation == "us geologic survey" ~ "interior",
        affiliation == "national fish and wildlife refuge" ~ "interior",
        affiliation == "usgs" ~ "interior",
        affiliation == "national aeronautics and space administration" ~ "nasa",
        affiliation == "national academy of sciences" ~ "nasem",
        affiliation == "national academy of engineering" ~ "nasem",
        affiliation == "national credit union administration" ~ "ncua",
        affiliation == "national institute for occupational safety and health" ~ "nih",
        affiliation == "national institute of mental health" ~ "nih",
        affiliation == "national center for complementary and integrative health" ~ "nih",
        affiliation == "nci" ~ "nih",
        affiliation == "national cancer institute" ~ "nih",
        affiliation == "national library of medicine" ~ "nih",
        affiliation == "national institute on aging" ~ "nih",
        affiliation == "national institute of neurological disorders and stroke" ~ "nih",
        affiliation == "national institute on drug abuse" ~ "nih",
        affiliation == "nhgri" ~ "nih",
        affiliation == "nhlbi" ~ "nih",
        affiliation == "national institute of allergy and infectious diseases" ~ "nih",
        affiliation == "national institute of dental and craniofacial research" ~ "nih",
        affiliation == "us national library of medicine" ~ "nih",
        affiliation == "national institute of child health and human development" ~ "nih",
        affiliation == "nida" ~ "nih",
        affiliation == "niaaa" ~ "nih",
        affiliation == "national institute on alcohol abuse and alcoholism" ~ "nih",
        affiliation == "national eye institute" ~ "nih",
        affiliation == "national institute of biomedical imaging and bioengineering" ~ "nih",
        affiliation == "national human genome research institute" ~ "nih",
        affiliation == "national institute of diabetes and digestive and kidney diseases" ~ "nih",
        affiliation == "nia" ~ "nih",
        affiliation == "national center for advancing translational sciences" ~ "nih",
        affiliation == "nimhd" ~ "nih",
        affiliation == "niddk" ~ "nih",
        affiliation == "fogarty international center" ~ "nih",
        affiliation == "niams" ~ "nih",
        affiliation == "national institute on minority health and health disparities" ~ "nih",
        affiliation == "national institute of nursing research" ~ "nih",
        affiliation == "national center for infectious diseases" ~ "nih",
        affiliation == "us nih" ~ "nih",
        affiliation == "national institute of standards and technology" ~ "nist",
        affiliation == "national oceanic/atmospheric admin" ~ "noaa",
        affiliation == "nat oceanic atmospheric adm" ~ "noaa",
        affiliation == "national center for environmental prediction" ~ "noaa",
        affiliation == "national weather service" ~ "noaa",
        affiliation == "national oceanographic and atmospheric administration" ~ "noaa",
        affiliation == "national centers for coastal ocean science" ~ "noaa",
        affiliation == "national hurricane center" ~ "noaa",
        affiliation == "us integrated ocean observing system" ~ "noaa",
        affiliation == "central pacific hurricane center" ~ "noaa",
        affiliation == "national centers for environmental information" ~ "noaa",
        affiliation == "national estuarine research reserve" ~ "noaa",
        affiliation == "national atmospheric and oceanic administration fisheries" ~ "noaa",
        affiliation == "national centers for environmental prediction" ~ "noaa",
        affiliation == "national center for atmospheric research" ~ "nsf",
        affiliation == "national radio astronomy observatory" ~ "nsf",
        affiliation == "national science board" ~ "nsf",
        affiliation == "nsf" ~ "nsf",
        affiliation == "national solar observatory" ~ "nsf",
        affiliation == "national center for science and engineering statistics" ~ "nsf",
        affiliation == "us office of personnel management" ~ "opm",
        affiliation == "national research council" ~ "nrc",
        affiliation == "us government" ~ "other",
        affiliation == "us arctic research commission" ~ "us arctic research commission",
        affiliation == "us global change research program" ~ "us global change research program",
        affiliation == "interagency grizzly bear study team" ~ "interagency grizzly bear study team",
        affiliation == "national endowment for the arts" ~ "nea",
        affiliation == "federal maritime commission" ~ "federal maritime commission",
        affiliation == "us of america" ~ "other",
        affiliation == "us climate variability and predictability project office" ~ "us climate variability and predictability project office",
        affiliation == "interagency special status/sensitive species program" ~ "interagency special status/sensitive species program",
        affiliation == "us federal service" ~ "us federal service",
        affiliation == "national zoological park" ~ "smithsonian",
        affiliation == "national museum of natural history" ~ "smithsonian",
        affiliation == "national zoo" ~ "smithsonian",
        affiliation == "national museum of the american indian" ~ "smithsonian",
        affiliation == "national museum of american history" ~ "smithsonian",
        affiliation == "national museum of asian art" ~ "smithsonian",
        affiliation == "office of the us global aids coordinator" ~ "state",
        affiliation == "us international trade commission" ~ "state",
        affiliation == "us dep of the interior" ~ "interior",
        affiliation == "us international development finance corporation (dfc)" ~ "us international development finance corporation",
        affiliation == "agency for international development" ~ "usaid",
        affiliation == "agency for international development (aid)" ~ "usaid",
        affiliation == "us forest service" ~ "usda",
        affiliation == "us national arboretum" ~ "usda",
        affiliation == "us national poultry research center" ~ "usda",
        affiliation == "us forest products laboratory" ~ "usda",
        affiliation == "us vegetable breeding laboratory" ~ "usda",
        affiliation == "us vegetable laboratory" ~ "usda",
        affiliation == "us pacific basin agricultural research center" ~ "usda",
        affiliation == "national center for cool and coldwater aquaculture" ~ "usda",
        affiliation == "national tropical botanical garden" ~ "usda",
        affiliation == "usad-ars" ~ "usda",
        affiliation == "us forest service international programs wood identification and screening center" ~ "usda",
        affiliation == "us forest servhice" ~ "usda",
        affiliation == "national center for ptsd" ~ "va",
        affiliation == "national center for post-traumatic stress disorder" ~ "va",
        affiliation == "vha" ~ "va",
        affiliation == "captain james a lovell federal health care center" ~ "va",
        affiliation == "va national surgery office" ~ "va",
        affiliation == "national center for rehabilitative auditory research" ~ "va",
        affiliation == "va national center for patient safety" ~ "va",
        affiliation == "va national teleoncology" ~ "va",
        affiliation == "va national expert consultation and specialized services" ~ "va",
        affiliation == "va national pharmacogenomics program" ~ "va",
        str_detect(affiliation, "us geológico survey") ~ "interior",
        str_detect(affiliation, "veteran") & str_detect(affiliation, "health care") ~ "va",
        str_detect(affiliation, "va ") & str_detect(affiliation, "health services") ~ "va",
        country == "usa" & str_detect(affiliation, "national fish hatchery") ~ "interior",
        country == "usa" & str_detect(affiliation, "national forest") ~ "usda",
        country == "usa" & str_detect(affiliation, "us vegetable lab") ~ "usda",
        country == "usa" & str_detect(affiliation, "agency for toxic substance and disease registry") ~ "hhs",
        country == "usa" & str_detect(affiliation, "th medical group") ~ "dod",
        country == "usa" & str_detect(affiliation, "walter reed") ~ "dod",
        country == "usa" & str_detect(affiliation, "us bureau of alcohol") ~ "doj",
        country == "usa" & str_detect(affiliation, "astdr") ~ "cdc",
        country == "usa" & str_detect(affiliation, "national marine sanctuary") ~ "noaa",
        country == "usa" & str_detect(affiliation, "national park") ~ "interior",
        country == "usa" & str_detect(affiliation, "national seashore") ~ "interior",
        country == "usa" & str_detect(affiliation, "national wildlife refuge") ~ "interior",
        country == "usa" & str_detect(affiliation, "us fish and wildlife") ~ "interior",
        country == "usa" & str_detect(affiliation, "national library of medicine") ~ "nih",
        country == "usa" & str_detect(affiliation, "us government") ~ "other",
        country == "usa" & str_detect(affiliation, "james a lovell") ~ "va",
        country == "usa" & str_detect(affiliation, "va national") ~ "va",
        country == "usa" & str_detect(affiliation, "veteran affairs") ~ "va",
        country == "usa" & str_detect(affiliation, "ars usda") ~ "usda",
        country == "usa" & str_detect(affiliation, "arlington national") ~ "dod",
        country == "usa" & str_detect(affiliation, "national monument") ~ "interior",
        country == "usa" & str_detect(affiliation, "frederick national") ~ "nih",
        country == "usa" & str_detect(affiliation, "national estuarine") ~ "interior",
        country == "usa" & str_detect(affiliation, "noaa") ~ "noaa",
        country == "usa" & str_detect(affiliation, "us mission") ~ "state",
        country == "usa" & str_detect(affiliation, "malaria initiative improving malaria") ~ "eop",
        country == "usa" & str_detect(affiliation, "the us president") ~ "eop",
        country == "usa" & str_detect(affiliation, "us aid") ~ "usaid",
        country == "usa" & str_detect(affiliation, "afit") ~ "dod",
        country == "usa" & str_detect(affiliation, "carl r darnall") ~ "dod",
        country == "usa" & str_detect(affiliation, "national defense university") ~ "dod",
        str_detect(affiliation, "va health") ~ "va",
        str_detect(affiliation, "va north texas healthcare system") ~ "va",
        str_detect(affiliation, "va office of information and technology") ~ "va",
        str_detect(affiliation, "va center") ~ "va",
        str_detect(affiliation, "us fish") ~ "interior",
        TRUE ~ as.character(agency_short)
      )
  ) 





affil_ids<-affil_ids %>% 
  mutate(agency_short = case_when(
    str_detect(agency_short, "us public health service") ~ "usphs",
    str_detect(agency_short, "public health service commissioned") ~ "usphs",
    str_detect(agency_short, "office of trade") ~ "eop",
    str_detect(agency_short, "national ice center") ~ "dod",
    agency_short == "ecosystems research" ~ "interior",
    agency_short == "corporation for national and community service" ~ "americorps",
    agency_short == "sea grant" ~ "noaa",
    agency_short == "office of nuclear energy" ~ "doe",
    str_detect(agency_short, "us marine corps") ~ "dod",
    str_detect(agency_short, "us coast guard") ~ "dhs",
    str_detect(agency_short, "us army") ~ "dod",
    str_detect(agency_short, "university corporation for atmospheric research") ~ "nsf",
    is.na(agency_short) & str_detect(affiliation, "cdc ") ~ "cdc",
    is.na(agency_short) & str_detect(affiliation, "nih ") ~ "nih",
    is.na(agency_short) & str_detect(affiliation, "nsf ") ~ "nsf",
    is.na(agency_short) & str_detect(affiliation, "national research council ") ~ "nrc",
    is.na(agency_short) & str_detect(affiliation, "nci ") ~ "nih",
    is.na(agency_short) & str_detect(affiliation, "argonne ") ~ "doe",
    is.na(agency_short) & str_detect(affiliation, "national park service") ~ "interior",
    is.na(agency_short) & str_detect(affiliation, "national cancer institute") ~ "nih",
    is.na(agency_short) & str_detect(affiliation, "nsf-") ~ "nsf",
    is.na(agency_short) & str_detect(affiliation, "nih/") ~ "nih",
    is.na(agency_short) & str_detect(affiliation, "doe ") ~ "doe",
    is.na(agency_short) & str_detect(affiliation, "nist ") ~ "nist",
    is.na(agency_short) & str_detect(affiliation, "forest service ") ~ "usda",
    is.na(agency_short) & str_detect(affiliation, "usaf ") ~ "dod",
    is.na(agency_short) & str_detect(affiliation, "army ") ~ "dod",
    is.na(agency_short) & str_detect(affiliation, "navy ") ~ "dod",
    is.na(agency_short) & str_detect(affiliation, "vha ") ~ "va",
    is.na(agency_short) & str_detect(affiliation, "vha ") ~ "va",
    is.na(agency_short) & str_detect(affiliation, "usgs ") ~ "interior",
    is.na(agency_short) & str_detect(affiliation, "noaa ") ~ "commerce",
    is.na(agency_short) & str_detect(affiliation, "national weather service ") ~ "commerce",
    is.na(agency_short) & str_detect(affiliation, "bureau of land management ") ~ "interior",
    is.na(agency_short) & str_detect(affiliation, "us bureau of land management") ~ "interior",
    is.na(agency_short) & str_detect(affiliation, "los alamos ") ~ "doe",
    is.na(agency_short) & str_detect(affiliation, "sandia ") ~ "doe",
    is.na(agency_short) & str_detect(affiliation, "usfws ") ~ "interior",
    is.na(agency_short) & str_detect(affiliation, "fda ") ~ "fda",
    str_detect(affiliation, "us coast guard ") ~ "dod",
    TRUE ~ as.character(agency_short)
  )) %>%
  mutate(agency_short = if_else(country == "usa" & str_detect(affiliation, "brigade|group|combat|battalion|usmc|regiment|command"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(country == "usa" & str_detect(affiliation, "defence|wing|squadron|division|cavalry| usn |field hospital|medical wing"), "dod", agency_short)) %>%
  mutate(agency_short = case_when(
    affiliation %in% c("inova center of outcomes research") ~ NA,
    TRUE ~ as.character(agency_short)
  )) %>%
  mutate(agency_short = case_when(
    affiliation %in% c("inova center of outcomes research") ~ NA,
    TRUE ~ as.character(agency_short)
  )) %>%
  mutate(agency_short = case_when(
    affil_id %in% c(
      60015482, 100896083, 132194144, 132193788, 123882638, 128754886,
      123214228, 128478576, 125203918, 127864124, 129282179, 126611873,
      127222996, 121302036, 128186702, 122485294
    ) ~ NA,
    TRUE ~ as.character(agency_short)
  )) 



affil_ids<-affil_ids %>% 
  mutate(agency_short = if_else(str_detect(affiliation, "defense|navy|air force|marine corps|army|military|naval"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "brigade|group|combat|battalion|usmc|regiment|command"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "defence|wing|squadron|division|cavalry| usn |field hospital|medical wing"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "1st|4th"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "usfs"), "usda", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "wildlife"), "interior", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "dod-va|va "), "va", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "veteran"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "toxic substances and disease"), "cdc", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national institute"), "nih", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "federal reserve"), "federal reserve system", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "federal deposit insurance corporation"), "fdic", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national wetlands inventory"), "interior", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national risk management research laboratory"), "epa", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "federal transit administration"), "dot", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "federal trade commission"), "ftc", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national nuclear security site"), "doe", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national ecological observation network"), "nsf", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national nuclear security site livermore office"), "doe", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "callaghan federal hospital"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "callaghan federal medical"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "james a lovell federal health care center"), "va", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "safety and work life coast guard"), "dod", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "us president’s malaria initiative"), "usaid", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national weather center"), "noaa", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national institutes of child health and development"), "nih", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national institutes of arthritis"), "nih", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national institute on alcoholism and alcohol abuse"), "nih", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national risk management research laboratory"), "epa", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national weather center research experiences for undergraduates"), "noaa", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national tsunami warning center"), "noaa", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national institute of standards"), "nist", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national homeland security research center"), "epa", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national systematics laboratory of the national oceanic"), "noaa", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national transportation research center"), "dot", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national nuclear security site"), "doe", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national soil resource conservation service"), "usda", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "national historical park|historic site|historical site|national lakeshore"), "interior", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "us department of interior—international technical assistance program"), "interior", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(affiliation, "federal highway administration"), "dot", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "108382367", "usda", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128941071", "usda", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125244852", "usda", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "122988512", "ahrq", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100502481", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100571977", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100822854", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "115752362", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "121212875", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "122286572", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "129973323", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130518547", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130701994", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130892705", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "132182075", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60008492", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100335911", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101792179", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "106453365", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "109347134", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112920600", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112922559", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "120168333", "doe", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123502999", "epa", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100677110", "hhs", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113820147", "hhs", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125383427", "hhs", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128533828", "hhs", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101140478", "interior", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128172361", "interior", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130391142", "interior", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123678203", "nasa", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123865204", "nasa", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "107986893", "nasem", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "122628604", "nasem", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130148658", "nasem", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112602793", "nih", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113013369", "nih", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125340513", "nih", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125381761", "nih", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125755037", "nih", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131324608", "nih", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131324998", "nih", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100316021", "noaa", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101000910", "noaa", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123141776", "noaa", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130557216", "noaa", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60071501", "nsf", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100587655", "nsf", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101394353", "nsf", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "106456794", "nsf", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "108578096", "smithsonian", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "114159234", "smithsonian", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128149737", "smithsonian", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "115539127", "usphs", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123896933", "usphs", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "126381052", "usphs", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "120532431", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128306628", "usda", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131558906", "usda", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125763856", "usda", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125364545", "usda", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113119893", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "118439805", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "124601958", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "107062094", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113951892", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123822264", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128757432", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131466439", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101261119", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101265211", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131511930", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113007889", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "114684115", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "115867250", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112587340", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60012281", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "106548849", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "110545556", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "121897786", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125786644", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131254575", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112949764", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "107980732", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113860189", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "121466434", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "122411044", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60014521", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "106585504", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "109248263", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112905068", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "107089434", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "129839931", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125919619", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113212209", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100364127", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112994777", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112637847", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100332619", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "117974668", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "129449620", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "129132280", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128165491", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101004622", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60002223", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112987892", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60105859", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "126765651", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "107025178", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "115341424", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "118903785", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123301448", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123300875", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131254594", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131254565", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131254588", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131254570", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "126999016", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "129518085", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112843093", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112910426", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "105621128", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113197759", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60105937", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131554680", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "129184086", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131119214", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112587380", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "122995724", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "119743592", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60105918", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60004786", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112234684", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101581898", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130434719", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "114586585", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "109494186", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "109911421", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "114306072", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "115168483", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "105745097", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112656582", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "119639166", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "105333223", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130217062", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112805663", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "105424804", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113069894", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "114786642", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125701945", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100988589", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "128210866", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "111235657", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "118700423", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "112117183", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "131644549", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "130647362", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125155173", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "110646347", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "110271622", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "120477769", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "126413640", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101964606", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "121518184", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "107852606", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "114455318", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "113187369", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "125781615", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "126907053", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "101516366", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "108095235", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "123934640", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60073942", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "126395319", "va", agency_short)) 

affil_ids<-affil_ids %>% 
  mutate(agency_short = if_else(affiliation == "us department of defense", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "us department of the interior", "interior", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "independent", agency_short, agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "national laboratories", "doe", agency_short)) %>%
  mutate(agency_short = if_else(str_detect(agency_short, "us government accountability"), "gao", agency_short)) 
  
affil_ids<-affil_ids %>% 
  mutate(agency_short = if_else(affiliation == "environmental protection agency office of research and development", "epa", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "engineer research and development center", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "naval undersea warfare center", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "voice of america", "voa", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "us postal service", "usps", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "general services administration", "gsa", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "inter-american foundation", "iaf", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "army research, development and engineering command", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "army research laboratory", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "tennessee valley authority", "tva", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "national research council senior fellow", "nrc", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "securities and exchange commission", "sec", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "americorps vista", "americorps", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "army armament research, development and engineering center", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "national research council research associateship program", "nrc", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "social security administration", "ssa", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "national research council (ibfm-cnr)", "nrc", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "us climate variability and predictability project office", "noaa", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "federal maritime commission (fmc)", "federal maritime comission", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "national research council postdoctoral fellow", "nrc", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "interagency special status/sensitive species program", "dod", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "institute of museum and library services", "imls", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "national transportation safety board", "ntsb", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "federal communications commission", "fcc", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "national labor relations board", "nlrb", agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cooperative institute for research in the atmosphere", "noaa",agency_short)) %>%		# 	csu,noaa
  mutate(agency_short = if_else(affiliation == "national high magnetic field laboratory", "nonfed",agency_short)) %>%		# 	fsu,nsf,doe
  mutate(agency_short = if_else(affiliation == "jila", "nist",agency_short)) %>%		# 	ucboulder, nist
  mutate(agency_short = if_else(affiliation == "institute for bioscience and biotechnology research", "multiagency",agency_short)) %>%		# 	UMD,NIST,NIH
  mutate(agency_short = if_else(affiliation == "chicago center for cosmochemistry", "nonfed",agency_short)) %>%		# 	University of Chicago, NASA-affiliated
  mutate(agency_short = if_else(affiliation == "joint institute for the study of the atmosphere and ocean", "noaa",agency_short)) %>%		# 	uw,noaa
  mutate(agency_short = if_else(affiliation == "abernathy fish technology center", "interior",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "advanced light source, berkeley", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "advanced materials laboratory", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "ahrq national resource center for health information technology", "ahrq",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "alamogordo primate facility (apf)", "nih",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "albuquerque forestry sciences laboratory", "usda",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "american international health alliance", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "american international health alliance, moldova", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "animal welfare science centre", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "appalachian laboratory for occupational safety and health", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "arkansas department of energy and environment", "NONFED",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "armed forces research institute of medical sciences, san francisco", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "armstrong flight research center", "nasa",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "arnold afb", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "basic support for institutionalizing child survival", "USAID",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "basic support for institutionalizing child survival, senegal", "USAID",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "benét laboratories", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "bettis atomic power laboratory", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "boise district office", "interior",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "bonneville power administration", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "brookhaven national laboratory biology department", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "brookhaven national laboratory condensed matter physics and materials science department", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "brookhaven national laboratory physics department", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "buck island reef national monument", "interior",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "bureau of economic analysis", "commerce",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "bureau of medicine and surgery", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "camp evans signal laboratory", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "canberra deep space communication complex", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "captain (ret) us public health service", "nphs",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "captain usaf", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "caribbean marine research center", "noaa",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "ccdc", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cdc's office of public health data", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cdc/nceh", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cdc/niosh div of fld stus and eng", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for administrative records research and applications us census bureau", "commerce",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for applied physics & superconducting technologies", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for financing access and cost trends ahrq", "ahrq",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for integrated nanotechnologies", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for isotope geochemistry", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for mental health services", "hhs",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for nonlinear studies", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for robust decision making on climate and energy policy", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for theoretical astrophysics", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for tobacco products (ctp)", "fda",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center for ultra-wide-area resilient electric energy transmission networks", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "center of innovation to accelerate discovery and practice transformation (adapt)", "va",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "centers for disease control and prevention san juan", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "centers for disease control and prevention, kenya", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "centers for disease control and prevention, south africa", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "chelonidata llc", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "chicago materials research center", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "children’s national research institute", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "children's national research institute", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "clinical transformation", "",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "clinical trials transformation initiative", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "coast guard incident management assist team", "dhs",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "coastal engineering research center - fort belvoir", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cold regions research and engineering laboratory", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cold regions research engineering laboratory", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "colorado river fishery project", "interior",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "columbia scientific balloon facility", "NASA",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "columbususa technologies contract at niaid", "nih",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "commonwealth transfusion foundation", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "computing, environment and life sciences", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "comunidad tribu yuke de jayuya", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "conservation and research center (national zoo)", "smithsonian",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "consortium of universities for the advancement of hydrologic science, inc", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "consultant to karna llc/cdc", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "contractor to the usgs", "usgs",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "coordinating center for environmental health and injury prevention (ccehip)", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "coordinating center for health information and service", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "coordinating center for health promotion", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "coordinating center for infectious diseases", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "corporal michael j crescenz vamc", "va",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "counsulting botanist and ecologist", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cramer fish sciences-genidaqs", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "critical zone exploration network", "nsf",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "cyclotron institute", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "daniel k inouye us pacific basin agricultural research center", "usda",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "department of energy - new york", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "dhw consulting and us census bureau", "commerce",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "directorate of education and human resources", "nsf",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "dolphins plus marine mammal responder", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "drug enforcement administration", "dea",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "durham virginia health care system", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "eastern oregon agricultural research center", "usda",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "edwards afb", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "el-erdc", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "elko field office, bureau of land management", "interior",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "embassy of türkiye in ouagadougou", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "environmental epidemiology service", "va",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "environmental laboratory", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "erdc-el", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "erdc-geospatial research laboratory", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "fagatele bay national marine sanctuary", "noaa",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "fbi laboratory", "doj",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "federal department of agriculture", "usda",agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "129987247", "fema",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "federal housing finance agency (fhfa)", "fhfa",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "federal maritime commission (fmc)", "fmc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "feed the future innovation lab for soybean value chain research", "usaid funded",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "fhwa office of policy and governmental affairs", "dot",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "foundation for the nih", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "framingham heart study", "nih",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "geminiobservatory/nsf’snoirlab", "nsf",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "general dynamics information technology (gdit) contractor supporting cdc's covid-19 response", "cdc",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "geotechnical and structures laboratory", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "geriatric research education and clinical center", "va",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "gulf of maine ocean observing system", "noaa",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "harvard college observatory", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "health care financing administration", "hhs",agency_short))

affil_ids<-affil_ids %>% 
  mutate(agency_short = if_else(affiliation == "health transformation and network management", "nonfed",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "hhs office of preparedness and emergency operations", "hhs",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "high temperature materials laboratory", "doe",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "hill afb", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "homeland security investigations", "dhs",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "hrsa’s bureau of primary health care", "hrsa",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "hrsa/ hiv/ aids bureau (hab)", "hrsa",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "hydrologic engineering center", "dod",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "idaho department of health and welfare", "NONFED",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "idaho fishery resource office", "interior",agency_short)) %>%
  mutate(agency_short = if_else(affiliation == "ils/contractor supporting the ntp interagency center for the evaluation of alternative toxicological methods (niceatm)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "inc under contract to the us geological", "usgs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "infection preventionist (retired)", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "innovative transfusion medicine", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "institute for theoretical atomic, molecular and optical physics", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "institute for transformative technologies", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "institute for transfusion medicine", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "institute of geophysics and planetary physics at lawrence livermore national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "institute of ophthalmology ucl & amp; nihr biomedical research centre", "NONFED",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "internal revenue service", "irs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "jefferson lab theory center", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "jet propulsion laboratory’s", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint base andrews", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint base elmendorf-richardson", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint base pearl harbor–hickam", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint base san antonio", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint bioenergy institute", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint center for earth systems technology", "NASA",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint institute for advanced materials", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "joint spectrum center", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "kampong of the national tropical botanical garden", "congress",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "kavli institute for particle astrophysics and cosmology", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "kentucky research station", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "knolls atomic power laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "l sakvarelidze national center for disease control and public health", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "landstuhl regional medical center", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "laser interferometer gravitational-wave observatory", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "lawrence bekeley national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "lawrence berkekely national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "lawrence j ellison institute for transformative medicine", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "lawrencelivermore national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "lawrnece livermore national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "ligo hanford", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "ligo livingston", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "lincoln laboratory", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "lt gen (ret) douglas robb is affiliated with the uniformed services university for health sciences", "Uniformed Services University of the Health Sciences",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "mansfield family practice", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "marine mammals management", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "maui high performance computing center", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "mns institute for sustainable urban transformations", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "natick soldier systems center", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national academy of medicine of korea", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national academy of sciences nrc senior research associateship", "nasem",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national aeronautics and space administration/goddard space flight center", "nasa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national astronomy and ionosphere center, new york", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national atmospheric deposition program (nadp)", "nasa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for advancing translational science", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for advancing translational sciences (ncats)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for biotechnology information (ncbi)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for complementary and alternative medicine (nccam)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for complementary and integrative health (nccih)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for environmental economics", "epa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for genome resources", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for health marketing", "cdc",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for ptsd (116b-3)", "va",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for research resources (ncrr)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national center for teacher residencies", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national centers for coastal ocean science (nccos)", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national centers for environmental prediction (ncep)", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national clonal germplasm repository for citrus and dates", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national elk refuge", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national energy technology laboratory (netl)", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national energy technology laboratory (netl) support contractor", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national energy technology laboratory, albany", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national energy technology laboratory, morgantown", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national energy technology laboratory, pittsburgh", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national energy technology laboratory/netl support contractor", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national eye institute (nei)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national geophysical data center", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national human genome research institute (nhgri)", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national intelligence council", "odni",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national invasive species council", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national invasive species council (nisc) staff", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national invasive species council staff", "interagency",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national laboratory center", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national laboratory for genetic resources preservation", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national museum of natural history research collaborator", "smithsonian",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national museum of the american indian (nmai)", "smithsonian",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national radio astronomy observatory socorro", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national radio astronomy observatory tucson", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national radio astronomy observatory very large array", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national renewable energy laboratory (doe)", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national renewable energy laboratory (nrel)", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national renewable energy laboratory’s (nrel)", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national renewable energy laboratory’s national wind technology center", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national security agency/central security service", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national security council", "nsc",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national severe storms laboratory norman", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national solar observatory (nso)", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national solar observatory sacramento peak", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national synchrotron light source ii brookhaven national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national toxicology program (ntp)", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national toxicology program interagency center for the evaluation of alternative toxicological methods", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national toxicology program laboratory", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national weather service, eastern region headquarters", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national weather service/lower mississippi river forecast center", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "national weather service/short-term prediction research and transition center", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "natl research council associate", "nrc",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "ncbi/nlm", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nci-frederick laboratory animal sciences program", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nctr/us fda", "fda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nevada department of health and human services", "NONFED",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nevada national security sites", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nevada national security sites (nnss)", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "new brunswick laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "new york sea grant", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nhgri advanced imaging & analysis core facility", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nhlbi laboratory of biochemical genetics", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nhlbi laboratory of kidney and electrolyte metabolism", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nia gerontology research center", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nichd", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nida addiction research center", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nida drug abuse treatment clinical trials network", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nida intramural research program", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "niddk/nih", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nimh clinical training program", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "niosh office of extramural programs", "cdc",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nnss ‐ nevada national security site", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "noaa/nws weather forecast office", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "normative aging study", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "north atlantic area (naa)", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "north pacific fishery management council", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "northern navajo medical center", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "northwest network", "va",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nrao, green bank", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nsf’s graduate research fellowship program in alexandria", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "nsf’s noir lab", "nsf",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "ntp interagency center for the evaluation of alternative toxicological methods", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "oak ridge institute for science and education", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "oak ridge lab", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "oak ridge national laboratoy", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "oak ridge office of environmental management", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of air quality planning and standards", "epa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of electricity", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of environmental management", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of fossil energy and carbon management", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of indian energy policy and programs", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of legacy management", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of public affairs", "",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of research, development and technology", "dot",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of river protection", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of science", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of security", "",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of special education and rehabilitative services", "education",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of special education programs", "education",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of tax analysis", "treasury",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "office of the surgeon general", "usphs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "oglala national grassland", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "ohio aerospace institute", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "orbusneich medical, inc", "NONFED",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "overseeing us nuclear weapons policies and programs", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "pacific northwes national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "pacific northwest national laboratory (jgcri-pnnl)", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "pacific northwest national laboratory (retired)", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "paciÔ¨Åc northwest national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "palo alto health care system", "va",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "perry institute for marine science", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "peterson afb", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "phoenix indian medical center", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "pnsn/usgs", "usgs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "portsmouth/paducah project office", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "program for climate model diagnosis and intercomparison", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "protagonist therapeutics", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "providence boston cfar-ccerc community engaged research council and vice chair n3c-cfar national cab coalition", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "radiological and environmental sciences laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "renewable and sustainable energy institute", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "research institute for advanced computer science", "NASA",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "retired us public health service", "usphs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "retired usfws", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "richard l roudebush vamc", "va",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "riken bnl research center", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "risq, inc", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "rmoms hrsa grant", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "robins afb", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "sackler centre for consciousness science", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "savannah river site", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "school of social transformation", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "scientific resource center for the ahrq epc program", "ahrq",agency_short)) %>%

mutate(agency_short = if_else(affiliation == "scientific resource center for the ahrq evidence-based practice center program", "ahrq",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "scott afb", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "sec's advisory committee on national health promotion and disease prevention objectives for 2030", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "shull wollan center: a joint institute for neutron sciences", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "sierra pacific network", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "soldier center (ccdc-sc)", "cdc",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "southern institute forest genetics", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "southwest watershed research center", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "space telescope science institute", "nasa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "spallation neutron source", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "stanford synchrotron radiation lightsource", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "substance abuse and mental health services administration", "hhs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "suncat center for interface science and catalysis", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "sustainability institute for regional transformations", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "systematic entomology laboratory c/o national museum of natural history", "smithsonian",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the advanced photon source", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the department of energy", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the ischemic heart disease queri", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the joint institute for computational sciences", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the kampong of the national tropical botanical garden", "congress",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the mental illness research, education and clinical centers", "va",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the office of high energy physics", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "the thomas young centre", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "theoretical physics department", "NONFED",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "tinker afb", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "transformer, inc", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "transfusion medicine", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "tripler regional med center", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "ucla joint institute for regional earth system science and engineering", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "uniformed services university camp lejeune site", "Uniformed Services University of the Health Sciences",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "uniformed services university jacksonville site", "Uniformed Services University of the Health Sciences",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "uniformed services university school of medicine", "Uniformed Services University of the Health Sciences",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "uniformed services university-southern region", "Uniformed Services University of the Health Sciences",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "united state department of agriculture", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "united state department of agriculture northwest climate hub", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "united state environmental protection agency", "epa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us aid lestari-indonesia", "usaid funded",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us consulate general jeddah, saudi arabia", "state",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us dairy export council", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us department of agricultural research service", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us department of agricultural- agricultural research service", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us deprescribing research network (usden) stakeholder engagement council", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us dot volpe center", "dot",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us general services administration (gsa)", "gsa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us geologic survey (usgs) geology", "usgs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us geological surve", "usgs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us green building council", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us medical center for federal prisoners, springfield", "doj",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us mission to uganda", "state",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us national laboratory", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us nih’s national heart", "nih",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us nuclear regulatory commision", "nrc",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us nws caribbean tsunami warning program", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us office of personnel management (opm)", "opm",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us office of personnel management center for leadership development", "opm",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us pac basin agric res center", "usda",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us patent patant & trademark office", "commerce",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us public health service (usphs)", "usphs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us small business administration", "sba",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us space force (rtd", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us surgeon general of the us public health service (usphs)", "usphs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "us virgin islands department of agriculture", "NONFED",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "usace erdc", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "usafr", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "usdi-bureau of land management", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "usfws/lower mississippi valley joint venture", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "usphs hospital - seattle", "usphs",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "valkyrie emergency blood transfusion training pro-gram", "nonfed",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "viral and rickettsial zoonoses br", "cdc",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "virgin islands national park and virgin islands coral reef national monument", "interior",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "virginia national center for ptsd", "va",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "volpe national transportation system center", "dot",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "washington sea grant", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "west desert test center", "dod",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "western area power administration", "doe",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "western regional climate center, nevada", "noaa",agency_short)) %>%
mutate(agency_short = if_else(affiliation == "wilford hall ambulatory surgical center", "dod",agency_short)) %>% 
mutate(agency_short = if_else(affiliation == "paciﬁc northwest national laboratory", "doe",agency_short)) %>% 
  mutate(agency_short = if_else(affil_id == "128609279", "nonfed", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "60016716", "nonfed", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "100536084", "nonfed", agency_short)) %>%
  
  mutate(agency_short = if_else(affil_id == "112685121", "va", agency_short)) %>%
  mutate(agency_short = if_else(affil_id == "105477331", "dhs", agency_short)) 
  


affil_ids<-affil_ids %>% 
  
  mutate(federal = if_else(!is.na(agency_short), TRUE, FALSE)) %>%
  distinct()


missing_agency_short<-
affil_ids %>% 
  filter(is.na(agency_short)) %>% 
  select(affil_id,affiliation) %>% 
  distinct()

write_csv(missing_agency_short,"./data_raw/missing_agency_short.csv")

affil_ids<-affil_ids %>% 
  mutate(agency_short=tolower(agency_short)) %>% 
  mutate(federal=if_else(agency_short=="nonfed",FALSE,federal)) %>% 
  mutate(agency_short=if_else(agency_short=="nonfed",NA,agency_short)) %>% 
  mutate(agency_short=if_else(str_detect(affiliation,"coast guard"),"dhs",agency_short)) %>% 
  select(-affil_check)


affil_ids<-affil_ids %>% 
  filter(federal==TRUE)
write_csv(affil_ids,"./data_clean/fed_affils_07102025.csv")


# remove from fed ---------------------------------------------------------

nonfed<-c(127078141,
          132488945,
          128609279,
          123764501,
          128145323,
          126369743,
          60106655,
          125117795,
          106562201,
          126149601,
          125136769,
          128064856,
          121848755,
          125377399,
          113727207,
          128853540,
          119493734,
          113727207,
          129556287,
          101869475,
          129349573,
          116261440,
          110141225,
          101967749,
          101967749,
          120290071,
          121302036,
          128186702,
          122485294,
          128186702,
          60096341,
          123794871,
          118462400,
          131383200,
          105428030,
          127687531,
          60096341,
          60023260,
          60004062,
          60162091,
          121302036,
          130623897,
          101967749,
          127677508,
          110075293,
          121502463,
          125125015,
          129587280,
          125868899,
          130790299,
          112784759,
          127056194,
          115858329,
          
131793716,
113044510,
106077252,
105605863,
112663540,
129868510,
112813607,
121153711,
117846194,
127114361,
125585522,
112598262,
108350937,
          127222996,
          126340532,
          113064197,
          117846194,
          107887380,
          
          126243689,
          124235648,
          109520920,
          100536084,
          116499533,
          60276047,
          60010459,
          100506957,
          60028437,
          111965129,
          124053433,
          116394172,
60013443,
116732570,
114398399,
125014439,
122485294,
115973022,
125117795,
121816043,
101967749,
100597450,
112377346,
128058768,
101556712,
131676655,
120290071,
123659193,
121366124,
130701562,
121302036,
127078141,
127905605,
131974087,
13239267,
127206196,
110141225,
126709532,
100320615,
110075293,
105637145,
119493734,
129357068,
117887308,
126369743,
124235648,
109520920,
128609279,
119020150,
131736517,
125336568,
128186702,
60119229,
60028451,
60086597,
60082598,
60090486,
60016716,
60012284,
60118175,
60175885,
60004435,
60091251,
60023942,
60074885,
60009350,
60004062,
60031821,
60002294,
60088290,
60277579,
60029115,
60089351,
60089352,
60118178,
60117925,
60003480,
60103346,
60009109,
60030939,
60016175,
60031471,
60162091,
127687531,
129556287,
121848755,
60106655,
101869475,
105428030,
106562201,
121502463,
116261440,
118462400,
123764501,
123794871,
125125015,
125136769,
125377399,
125868899,
126149601,
127677508,
128064856,
128145323,
128853540,
129349573,
130790299,
131383200,
132488945,
60096341,
130360403,
132113017,
129061813,
128934222,
119795422,
112739618,
60111400,
128268429,
123934887,
130830729,
124141948,
127349629,
120437792,
126831656,
100557977,
118531979,
118862967,
130428619,
100361739,
125916745,
128123214,
123725178,
132475889,
108519048,
116804153,
126962904,
100330046,
121072995,
60082586,
115350562,
116585518,
132056784,
128052398,
127055550,
125577815,
124376601,
122813821,
117045154,
117356744,
126824963,
120288931,
122992184,
121169296,
117047886,
127383794,
109346072,
106121997,
131878970,
132380041,
132380041,
122759474,
121368830,
127023381,
127876668,
129434464,
125383607,
128395598
)


nonfed<-as.data.frame(nonfed) %>% 
  rename(affil_id=nonfed) %>% 
  mutate(federal=FALSE) %>% 
  distinct()

write_csv(nonfed,"./data_clean/NONFED_affils_07102025.csv")