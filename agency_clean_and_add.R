#  fed affils -------------------------------------------------------------

library(tidyverse)
library(janitor)

agency_search <- read_csv("./data_raw/agency_search_terms.csv",locale=locale(encoding="latin1")) %>%
  mutate_all(tolower) %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(search_term=gsub("&amp;","and",search_term)) %>%
  mutate(search_term=gsub("united states ","us ",search_term)) %>% 
  mutate(search_term=gsub("u s ","us ",search_term)) %>% 
  mutate(search_term=gsub("u.s. ","us ",search_term)) %>%
  mutate(search_term=gsub("u. s. ","us ",search_term)) %>%
  mutate(search_term=gsub("Agency fHealthcare Research and Quality","ahrq",search_term)) %>%
  select(-search) %>% 
  mutate(agency = case_when(
    unit == "department of health and human services" ~ "hhs",
    unit == "department of the air force" ~ "dod",
    unit == "department of energy" ~ "doe",
    unit == "department of the army" ~ "dod",
    unit == "department of labor" ~ "labor",
    unit == "department of commerce" ~ "commerce",
    unit == "department of defense" ~ "dod",
    unit == "department of housing and urban development" ~ "hud",
    unit == "department of veterans affairs" ~ "va",
    unit == "environmental protection agency" ~ "epa",
    unit == "department of transportation" ~ "dot",
    unit == "attorney general" ~ "doj",
    unit == "department of housing and urban development" ~ "hud",
    unit == "department of housing and urban development" ~ "hud",
    unit == "department of energy" ~ "doe",
    unit == "department of energy" ~ "doe",
    unit == "national institutes of health" ~ "nih",
    unit == "department of the interior" ~ "interior",
    unit == "department of the interior" ~ "interior",
    unit == "department of agriculture" ~ "usda",
    unit == "national oceanic and atmospheric administration" ~ "noaa",
    unit == "nasa" ~ "nasa",
    unit == "national science foundation" ~ "nsf",
    unit == "department of the navy" ~ "dod",
    unit == "usaid" ~ "usaid",
    unit == "centers for disease control and prevention"~"cdc",
    unit == "national institutes of health"~"nih",
    unit == "department of state"~"state",
    unit == "fish and wildlife service"~"interior",              
    unit == "food and drug administration"~"fda",
    # unit == "united states department of agriculture"~"usda",
    unit == "us department of agriculture"~"usda",
    unit == "lawrence berkeley national laboratory"~"doe",
    unit == "smithsonian institution"~"smithsonian",
    unit == "national park service"~"interior",
    unit == "national science foundation"~"nsf",       
    # unit == "united states department of energy"~"doe",
    # unit == "united states geological survey"~"interior",
    unit == "us department of energy"~"doe",
    unit == "us geological survey"~"interior",
    unit == "department of education" ~"interior",
    unit == "department of homeland security" ~"dhs",
    unit == "department of justice" ~"doj",
    unit == "department of the treasury" ~"treasury",     
    unit == "department of transportation" ~"dot",
    .default = as.character(agency))
  ) %>% 
  drop_na(search_term) %>% 
  distinct() 

unique(agency_search$search_term)

unique(agency_search$agency)

write_csv(agency_search, "./data_clean/agency_search_clean.csv")



# load and prep pubs and authors ------------------------------------------


all_papers <- read_rds("./data_clean/scopus_papers.rds") %>% 
  separate_wider_delim(filename,
                       delim = "_",
                       names = c("agency"),
                       too_many = "drop"
  ) %>% 
  mutate_all(tolower) %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(refID=as.numeric(refID))

names(all_papers)

details <- all_papers %>% 
  select(PY, PM,refID) %>% 
  mutate_all(as.integer) 


fed_authors_original <- read_rds("./data_clean/scopus_authors.rds") %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(refID=as.numeric(refID)) %>% 
  left_join(details, by = "refID") %>% 
  mutate_all(tolower) %>% 
  mutate(affiliation=gsub("united states ","us ", affiliation)) %>% 
  mutate(affiliation=gsub("u s ","us ",affiliation)) %>% 
  mutate(affiliation=gsub("united state ","us ",affiliation)) %>% 
  mutate(affiliation=str_replace_all(affiliation, fixed("."), ""))


setdiff((fed_authors_original$refID),details$refID)
setdiff((fed_authors_original$entry_no_authors),all_papers$entry_no)

rm(details)


search_terms<-agency_search %>% 
  select(search_term) %>% 
  mutate(search_term = case_when(
    search_term == "ars" ~ "ars usda",
    search_term == "nei" ~ "\\(nei\\)",
    search_term == "nia" ~ "\\(nia\\)",
    search_term == "doe" ~ "doe ",
    search_term == "nist" ~ "nist ",
    search_term == "hud" ~ "hud ",
    search_term == "epa" ~ "epa ",
    search_term == "va" ~ " va ",
    search_term == "nist" ~ "nist ",
    search_term == "stri" ~ " stri ",
    .default = as.character(search_term)
  ))

terms_to_add<-data.frame(search_term=c(
  " epa",
" ars "
))

search_terms<-bind_rows(search_terms,terms_to_add) %>% 
  arrange(search_term) %>% 
  mutate(search_term=gsub("united states ","us ",search_term)) %>% 
  mutate(search_term=gsub("u s ","us ",search_term)) 

rm(terms_to_add)
  
search_terms<-paste(search_terms$search_term, collapse = "|" )
 
author_affils <- fed_authors_original %>%
  select(affiliation, city, country) %>%
  unique()



unique(author_affils$country)


fed_affils_usa <-author_affils %>% 
  filter(country=="usa"|
           country=="puerto rico"|
           country=="northern mariana islands"|
           country=="american samoa"|
           country=="guam"|
           country=="virgin islands (u.s.)") %>% 
  mutate(search_term=str_extract(affiliation, search_terms)) %>% 
  mutate_all(trimws,which = c("both")) %>% 
  # DO SOME CORRECTIONS
  mutate(search_term = case_when(
    affiliation == "victus ars inc" ~ NA,
    affiliation == "cto of docbox, inc" ~ NA,
    affiliation == "cto of docbox, inc" ~ NA,
    affiliation == "community-based organization partners (cbop) and community ethics review board (cerb)" ~ NA,
    affiliation == "cdc foundation" ~ NA,
    affiliation == "heilongjiang provincial cdc" ~ NA,
    affiliation == "dpi/fdacs"~NA,
    .default = as.character(search_term)
  ))

usa_affils_not_fed<-fed_affils_usa %>% 
  filter(is.na(search_term))

fed_affils_usa <-fed_affils_usa %>% 
  drop_na(search_term)
  


  
# 
# 
search_terms_intl<-data.frame(search_term=c("niaid/nih international centers for excellence in research",
                                            "usaid",
                                            "us aid ",
                                            "us antarctic program",
                                            "us army",
                                            "us bureau of land management",
                                            "us centers for disease control",
                                            "us cdc",
                                            "us consulate",
                                            "us department of agriculture",
                                            "us department of defense",
                                            "us department of defence",
                                            "us department of veteran",
                                            "us embassy",
                                            "us environmental protection agency",
                                            "us forces",
                                            "us forest service",
                                            "us geological survey",
                                            "us marine corp",
                                            "us military",
                                            "us mission",
                                            "us national institute of allergy and infectious diseases",
                                            "us naval",
                                            "us navy",
                                            "us of america agency for international development",
                                            "us office of naval",
                                            "us president",
                                            "us walter reed",
                                            "usda-ars",
                                            "usda",
                                            "us peace corps",
                                            "us cdc",
                                            "us 8th army",
                                            "us agency for international development"))



search_terms_intl<-paste(search_terms_intl$search_term, collapse = "|" )
# 
# 
# 
fed_affils_intl <-author_affils %>%
  filter(country!="usa" &
           country!="puerto rico" &
           country!="northern mariana islands" &
           country!="american samoa" &
           country!="guam" &
           country!="virgin islands (u.s.)") %>%
mutate(search_term=str_extract(affiliation, search_terms_intl)) %>%
  mutate_all(trimws,which = c("both"))



affils_intl_not_fed<-fed_affils_intl %>% 
  filter(is.na(search_term))

fed_affils_intl <-fed_affils_intl %>% 
  drop_na(search_term)

# bind them up 

# no federal affils (to 2x check)
no_fed_affils<-bind_rows(affils_intl_not_fed,usa_affils_not_fed) %>% 
  unique() 

rm(affils_intl_not_fed,usa_affils_not_fed)


# find any that weren't caught --------------------------------------------


search_vec<-c(
  "national telecommunications and information administration",
  "the us national archives and records administration",
  "national institute for mathematical and biological synthesis",
  "office of national drug control policy",
  "National Center for Preparedness, Detection, and Control of Infectious Diseases",
  "national socio-environmental synthesis center",
  "national evolutionary synthesis center",
  "national ecological observatory network",
  "us department of interior",
  "the us national archives and records administration",
  "us department of agricultural",
  "national historical park",
  "fredrick national laboratory",
  "us geológico survey",
  "us agency for internationl development",
  "national telecommunications and information",
  "us fish &amp; wildlife")
  
pull_to_fed_affils<-no_fed_affils %>% 
  # filter(country=="usa") %>% 
  filter(str_detect(affiliation, pattern = paste(search_vec, collapse = '|'))) %>% 
  distinct() %>% 
  mutate(search_term=str_extract(affiliation, pattern = paste(search_vec, collapse = '|'))) %>% 
  mutate_all(trimws)
pull_to_fed_affils2<-no_fed_affils %>% 
  filter(country=="usa" & str_detect(affiliation, pattern = "national lab")) %>% 
  distinct() %>% 
  mutate(search_term=str_extract(affiliation, pattern = paste(search_vec, collapse = '|'))) %>% 
  mutate_all(trimws) %>% 
  filter(!str_detect(affiliation, pattern = "ihesp")) %>% 
  filter(!str_detect(affiliation, pattern = "iss national")) %>% 
filter(!str_detect(affiliation, pattern = "international space station")) %>% 
  filter(!str_detect(affiliation, pattern = "galveston national laboratory"))

pull_to_fed_affils<-bind_rows(pull_to_fed_affils,pull_to_fed_affils2)
rm(pull_to_fed_affils2)

pull_to_fed_affils<-pull_to_fed_affils %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national lab"),"doe",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national historical park"),"interior",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national monument"),"interior",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "fish &amp; wildlife"),"interior",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "fredrick"),"nih",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "frederick"),"nih",search_term)) %>% 
  
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national telecommunications and information administration"),"commerce",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "the us national archives and records administration"),"eop",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national ecological observatory network"),"nsf",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national institute for mathematical and biological synthesis"),"nsf",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national evolutionary synthesis center"),"nsf",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "national socio-environmental synthesis center"),"nsf",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "office of national drug control policy"),"eop",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "office of national drug control policy"),"eop",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "frederick"),"nih",search_term)) %>% 
  
  mutate(search_term=if_else(str_detect(affiliation, pattern = "us department of interior"),"interior",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "us department of agriculture"),"usda",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "us department of agricultural"),"usda",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "us agency for internationl development"),"usaid",search_term)) %>% 
  mutate(search_term=if_else(str_detect(affiliation, pattern = "geológico survey"),"interior",search_term)) 

pull_to_fed_affils


no_fed_affils<-anti_join(no_fed_affils,pull_to_fed_affils,by="affiliation")

# fed affils (still need to 2x)
fed_affils<-bind_rows(fed_affils_usa,fed_affils_intl,pull_to_fed_affils) %>% 
  unique() %>% 
  left_join(agency_search,by="search_term")

rm(fed_affils_usa,fed_affils_intl)

# 
# foo<-fed_authors %>% filter(affiliation==string_detect(affiliation,"army"))
names(fed_authors_original)
names(fed_affils)
unique(fed_authors_original$PY)

fed_authors<-left_join(fed_authors_original,fed_affils,by=c("affiliation","city","country")) %>%
  arrange(refID,author_order) %>% 
  mutate(fed_author=if_else(is.na(agency),FALSE,TRUE)) %>% 
  mutate(refID=as.numeric(refID))


no_feds<-fed_authors %>% 
  group_by(refID) %>% 
  tally(fed_author) %>%
  filter(n<1) %>% 
  select(refID)

pubs_with_fed<-fed_authors %>% 
  group_by(refID) %>% 
  tally(fed_author) %>%
  filter(n>0) %>% 
  select(refID)

pubs_with_fed<-fed_authors %>% 
  filter(refID%in%pubs_with_fed$refID) 



#  check affiliations -----------------------------------------------------


# Scopus Affiliation numbers
scopus_affils_df<-pubs_with_fed %>% 
  filter(fed_author==TRUE) %>% 
  select(affil_id, affiliation) %>% 
  distinct()


scopus_affils_df %>% 
  group_by(affil_id) %>% 
  tally() %>% 
  arrange(desc(n))


# fixing false positives --------------------------------------------------

# ie labeled as fed but not

library(datasets)

state_names<-state.name %>% 
  tolower() %>% 
  as_tibble()

# Abbreviations tough because in, de, etc. 
# state_abbrev<-state.abb %>% 
#   tolower() %>% 
#   as_tibble() %>% 
#   filter(value!="in") %>% 
#   mutate(value=paste(" ",value," ",sep=""))

false_fed_positives1<-scopus_affils_df %>% 
  filter(str_detect(affiliation, paste(state_names$value, collapse = "|"))) %>% 
  filter(!str_detect(affiliation, "usda")) %>% 
  filter(!str_detect(affiliation, "us ")) %>% 
  filter(!str_detect(affiliation, "army")) %>% 
  filter(!str_detect(affiliation, "army")) %>% 
  filter(!str_detect(affiliation, "noaa")) %>% 
  filter(!str_detect(affiliation, "air force")) %>% 
  filter(!str_detect(affiliation, "bureau of land management")) %>% 
  filter(!str_detect(affiliation, "cdc")) %>% 
  filter(!str_detect(affiliation, "national cancer institute")) %>% 
  filter(!str_detect(affiliation, "usaid")) %>% 
  filter(!str_detect(affiliation, "usgs")) %>% 
  filter(!str_detect(affiliation, "usfws")) %>% 
  filter(!str_detect(affiliation, "veterans administration")) %>% 
  filter(!str_detect(affiliation, "veterans health administration")) %>% 
  filter(!str_detect(affiliation, "vha")) %>% 
  filter(!str_detect(affiliation, "centers for disease control and prevention")) %>% 
  filter(!str_detect(affiliation, "national")) %>% 
  select(affil_id)

  false_fed_positives2<-scopus_affils_df %>% 
  filter(!str_detect(affiliation, "usda")) %>%
  # filter(!str_detect(affiliation, "us ")) %>%
  filter(!str_detect(affiliation, "army")) %>%
  filter(!str_detect(affiliation, "us ")) %>%
  filter(!str_detect(affiliation, "national weather service")) %>%
  filter(!str_detect(affiliation, "usaf")) %>%
  filter(!str_detect(affiliation, "noaa")) %>%
  filter(!str_detect(affiliation, "usepa")) %>%
  filter(!str_detect(affiliation, "air force")) %>%
  filter(!str_detect(affiliation, "veteran affairs health")) %>%
  filter(!str_detect(affiliation, "veteran affairs")) %>%
  filter(!str_detect(affiliation, "82nd airborne")) %>%
  filter(!str_detect(affiliation, "cber")) %>%
  filter(!str_detect(affiliation, "coast guard")) %>%
  filter(!str_detect(affiliation, "ahrq")) %>%
  filter(!str_detect(affiliation, "national academy")) %>%
  filter(!str_detect(affiliation, "center for biologics evaluation")) %>%
  filter(!str_detect(affiliation, "department of defense")) %>%
  filter(!str_detect(affiliation, "national museum")) %>%
  filter(!str_detect(affiliation, "administration")) %>%
  filter(!str_detect(affiliation, "agency for")) %>%
  filter(!str_detect(affiliation, "bureau of land management")) %>%
  filter(!str_detect(affiliation, "smithsonian")) %>%
  filter(!str_detect(affiliation, "military")) %>%
  filter(!str_detect(affiliation, "nasa")) %>%
  filter(!str_detect(affiliation, "national park")) %>%
  filter(!str_detect(affiliation, "national laboratory")) %>%
  filter(!str_detect(affiliation, "national wildlife refuge")) %>%
  filter(!str_detect(affiliation, "cdc")) %>%
  filter(!str_detect(affiliation, "national cancer institute")) %>%
  filter(!str_detect(affiliation, "usaid")) %>%
  filter(!str_detect(affiliation, "usda")) %>%
  filter(!str_detect(affiliation, "usgs")) %>%
  filter(!str_detect(affiliation, "usfws")) %>%
  filter(!str_detect(affiliation, "veterans administration")) %>%
  filter(!str_detect(affiliation, "veterans health administration")) %>%
  filter(!str_detect(affiliation, "vha")) %>%
  filter(!str_detect(affiliation, "us department")) %>%
  filter(!str_detect(affiliation, "federal")) %>%
  filter(!str_detect(affiliation, "agency")) %>%
  filter(!str_detect(affiliation, " national ")) %>%
  filter(!str_detect(affiliation, "uniformed services")) %>%
  filter(!str_detect(affiliation, "nsf")) %>%
  filter(!str_detect(affiliation, "nih")) %>%
  filter(!str_detect(affiliation, "national institute")) %>%
  filter(!str_detect(affiliation, "national research council")) %>%
  filter(!str_detect(affiliation, "national solar")) %>%
  filter(!str_detect(affiliation, "national zoo")) %>%
  filter(!str_detect(affiliation, "fbi ")) %>%
  filter(!str_detect(affiliation, "doe")) %>%
  filter(!str_detect(affiliation, "naval")) %>%
  filter(!str_detect(affiliation, "national library")) %>%
  filter(!str_detect(affiliation, "national museum")) %>%
  filter(!str_detect(affiliation, "national center")) %>%
  filter(!str_detect(affiliation, "medical group")) %>%
  filter(!str_detect(affiliation, "combat")) %>%
  filter(!str_detect(affiliation, "ames laboratory")) %>%
  filter(!str_detect(affiliation, "armed forces")) %>%
  filter(!str_detect(affiliation, "nci ")) %>%
  filter(!str_detect(affiliation, "afit ")) %>%
  filter(!str_detect(affiliation, "wrair")) %>%
  filter(!str_detect(affiliation, "niaaa")) %>%
  filter(!str_detect(affiliation, "national eye institute")) %>%
  filter(!str_detect(affiliation, "national fish and wildlife refuge")) %>%
  filter(!str_detect(affiliation, "nhlbi")) %>%
  filter(!str_detect(affiliation, "niaid")) %>%
  filter(!str_detect(affiliation, "niams")) %>%
  filter(!str_detect(affiliation, "nichd")) %>%
  filter(!str_detect(affiliation, "nida")) %>%
  filter(!str_detect(affiliation, "nimh")) %>%
  filter(!str_detect(affiliation, "niosh")) %>%
  filter(!str_detect(affiliation, "nist")) %>%
  filter(!str_detect(affiliation, "nmrc")) %>%
  filter(!str_detect(affiliation, "oak ridge lab")) %>%
  filter(!str_detect(affiliation, "usace")) %>%
  filter(!str_detect(affiliation, "patuxent wildlife research center")) %>%
  filter(!str_detect(affiliation, "national heart, lung, and blood institute")) %>%
  filter(!str_detect(affiliation, "national high magnetic field laboratory los almos")) %>%
  filter(!str_detect(affiliation, "national human genome research institute")) %>%
  filter(!str_detect(affiliation, "national human genome research institute")) %>%
  filter(!str_detect(affiliation, "nhgri")) %>%
  filter(!str_detect(affiliation, "national hurricane center")) %>%
  filter(!str_detect(affiliation, "national immunization program")) %>%
  filter(!str_detect(affiliation, "national intelligence council")) %>%
  filter(!str_detect(affiliation, "national invasive species council")) %>%
  filter(!str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
  filter(!str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
  filter(!str_detect(affiliation, "national gallery of art")) %>%
  filter(!str_detect(affiliation, "national guard")) %>%
  filter(!str_detect(affiliation, "national radio ")) %>%
  filter(!str_detect(affiliation, "national renewable")) %>%
  filter(!str_detect(affiliation, "national science board")) %>%
  filter(!str_detect(affiliation, "national science foundation")) %>%
  filter(!str_detect(affiliation, "national security council")) %>%
  filter(!str_detect(affiliation, "national severe storms laboratory norman")) %>%
  filter(!str_detect(affiliation, "national socio-environmental synthesis center")) %>%
  filter(!str_detect(affiliation, "sesync")) %>%
  filter(!str_detect(affiliation, "incident command system")) %>%
  filter(!str_detect(affiliation, "national toxicology program")) %>%
  filter(!str_detect(affiliation, "national toxicology program laboratory")) %>%
  filter(!str_detect(affiliation, "national tropical botanical garden")) %>%
  filter(!str_detect(affiliation, "national war college")) %>%
  filter(!str_detect(affiliation, "national wetlands research center")) %>%
  filter(!str_detect(affiliation, "national wetlands research center")) %>%
  filter(!str_detect(affiliation, "natl research council associate")) %>%
  filter(!str_detect(affiliation, "ncbi/nlm")) %>%
  filter(!str_detect(affiliation, "nci-frederick laboratory animal sciences program")) %>%
  filter(!str_detect(affiliation, "nhlbi")) %>%
  filter(!str_detect(affiliation, "nhgri")) %>%
  filter(!str_detect(affiliation, "national health and environmental effects research laboratory")) %>%
  filter(!str_detect(affiliation, "hrsa")) %>%
  filter(!str_detect(affiliation, "hurricane flood risk reduction design branch")) %>%
  filter(!str_detect(affiliation, "jet propulsion laboratory")) %>%
  filter(!str_detect(affiliation, "national atmospheric deposition program")) %>%
  filter(!str_detect(affiliation, "nad")) %>%
  filter(!str_detect(affiliation, "epa region")) %>%
  filter(!str_detect(affiliation, "erdc")) %>%
  filter(!str_detect(affiliation, "national defense")) %>%
  filter(!str_detect(affiliation, "national ecological observatory network")) %>%
  filter(!str_detect(affiliation, "national endowment for the arts")) %>%
  filter(!str_detect(affiliation, "national energy technology laboratory")) %>%
  filter(affil_id!="109507990") %>%
  filter(affil_id!="112891175") %>%
  filter(affil_id!="100312437") %>%
  filter(affil_id!="60013346") %>%
  filter(affil_id!="100475245") %>%
  filter(affil_id!="107901133") %>%
  filter(affil_id!="60151531") %>%
  filter(affil_id!="101344572") %>%
  filter(affil_id!="60076285") %>%
  filter(affil_id!="100332158") %>%
  filter(affil_id!="123502856") %>%
  filter(affil_id!="102033117") %>%
  filter(affil_id!="109562444") %>%
  filter(affil_id!="124744686") %>%
  filter(affil_id!="122676681") %>%
  filter(affil_id!="114003101") %>%
  filter(affil_id!="118416507") %>%
  filter(affil_id!="123523957") %>%
  filter(affil_id!="122461253") %>%
  filter(affil_id!="121968104") %>%
  filter(affil_id!="60090737") %>%
  filter(affil_id!="108038436") %>%
  filter(affil_id!="131447634") %>%
  filter(affil_id!="60029965") %>%
  filter(affil_id!="110325356") %>%
  filter(affil_id!="124101950") %>%
  filter(affil_id!="60013369") %>%
  filter(affil_id!="60001662") %>%
  filter(affil_id!="60020465") %>%
  filter(affil_id!="100689964") %>%
  filter(affil_id!="124534107") %>%
  filter(affil_id!="60031735") %>%
  filter(affil_id!="60021549") %>%
  filter(affil_id!="112985829") %>%
  filter(affil_id!="60029698") %>%
  filter(affil_id!="100489231") %>%
  filter(affil_id!="126924761") %>%
  filter(affil_id!="130780665") %>%
  filter(affil_id!="106700961") %>%
  filter(affil_id!="60008780") %>%
  filter(affil_id!="130475198") %>%
  filter(affil_id!="130475119") %>%
  filter(affil_id!="112700141") %>%
  filter(affil_id!="100966017") %>%
  filter(affil_id!="101356352") %>%
  filter(affil_id!="101619617") %>%
  filter(!str_detect(affiliation, "centers for disease control and prevention")) %>% 
  select(affil_id)

  
false_fed_positives3<-tibble(affil_id=c("129951829",
                                        "60013443",
                                        "101967749",
                                        "120290071",
                                        "109520920",
                                        "110141225",
                                    "132228870",
                                    "106977950",
                                    "112703830",
                                    "131734604",
                                    "131990960",
                                    "119704201",
                                    "129167163",
                                    "128902945",
                                    "60019853",
                                    "114043849",
                                    "101469376",
                                    "100951187",
                                    "126500206",
                                    "107136275",
                                    "122460076",
                                    "60098179",
                                    "122503316",
                                    "131197678",
                                    "60006107",
                                    "129770980",
                                    "130644180",
                                    "100951187",
                                    "128785382",
                                    "130621169",
                                    "112578573",
                                    "123813980",
                                    "100318446",
                                    "113647404",
                                    "120666603",
                                    "60030891",
                                    "60110648",
                                    "130101353",
                                    "120662121"
                                    ))

false_fed_positives4<-scopus_affils_df %>% 
  filter(str_detect(affiliation, ("college|university"))) %>% 
  filter(!str_detect(affiliation, "air force")) %>% 
  filter(!str_detect(affiliation, "war ")) %>% 
  filter(!str_detect(affiliation, "defense university")) %>% 
  filter(!str_detect(affiliation, "uniformed services university")) %>% 
  filter(!str_detect(affiliation, "us army")) %>% 
  filter(!str_detect(affiliation, "us marine corps")) %>% 
  filter(!str_detect(affiliation, "us geological")) %>% 
  filter(!str_detect(affiliation, "noaa")) %>% 
  filter(!str_detect(affiliation, "usaf")) %>% 
  filter(!str_detect(affiliation, "usda")) %>% 
  select(affil_id)

# contract to EPA
# "121898177" "oak ridge association university student services contractor to us environmental protection agency"

false_fed_positives<-bind_rows(false_fed_positives1,
                               false_fed_positives2,
                               false_fed_positives3,
                               false_fed_positives4)


pubs_with_fed <- pubs_with_fed %>% 
  mutate(fed_author=if_else(affil_id%in%false_fed_positives$affil_id,FALSE,fed_author)) %>% 
  mutate(agency=if_else(fed_author==FALSE,NA,agency)) %>% 
  mutate(unit=if_else(fed_author==FALSE,NA,unit)) %>% 
  mutate(str_detect=if_else(fed_author==FALSE,NA,str_detect)) %>% 
  mutate(search_term=if_else(fed_author==FALSE,NA,search_term))



# fixing false negatives --------------------------------------------------

# labeled as not fed but are


# IS fed_authors

fed_affils<-c(
  "108382367",
  "125244852",
  "122988512",
  "100502481",
  "100571977",
  "100822854",
  "115752362",
  "121212875",
  "122286572",
  "129973323",
  "130518547",
  "130701994",
  "130892705",
  "132182075",
  "60008492",
  "100335911",
  "101792179",
  "106453365",
  "109347134",
  "112920600",
  "112922559",
  "120168333",
  "123502999",
  "100677110",
  "113820147",
  "125383427",
  "128533828",
  "101140478",
  "128172361",
  "130391142",
  "123678203",
  "123865204",
  "107986893",
  "122628604",
  "130148658",
  "112602793",
  "113013369",
  "125340513",
  "125381761",
  "125755037",
  "131324608",
  "131324998",
  "100316021",
  "101000910",
  "123141776",
  "130557216",
  "60071501",
  "100587655",
  "101394353",
  "106456794",
  "108578096",
  "114159234",
  "128149737",
  "115539127",
  "123896933",
  "126381052",
  "120532431",
  "109683782",
  "129477382",
  "129282179",
  "112927977",
  "114921497",
  "123871342",
  "60105857",
  "60011058",
  "100349009",
  "101085594",
  "112177733",
  "121624708",
  "128217908",
  "60004768",
  "112883629",
  "60105856",
  "114520379",
  "113057100",
  "107034880",
  "122400567",
  "123807703",
  "124701231",
  "114385940",
  "60009511",
  "122096438",
  "60022556",
  "114983183",
  "100968866",
  "114317889",
  "127833144",
  "112914080",
  "105216627",
  "60085690",
  "107713456",
  "60105938",
  "128137528",
  "112936325",
  "127303149",
  "60014232",
  "118884567",
  "123739119",
  "108788235",
  "106637561",
  "118122905",
  "60023621",
  "60022482",
  "107064832",
  "105557510",
  "110183199",
  "60009004",
  "129777814",
  "109516894",
  "60021513",
  "60105919",
  "112967581",
  "121322284",
  "129171136",
  "105265305",
  "60105916",
  "108109623",
  "126447335",
  "113873823",
  "120943775",
  "122480364",
  "122547667",
  "116873603",
  "120943775",
  "122480364",
  "122547667",
  "116873603",
  "112793106",
  "122565177",
  "126476821",
  "129650313",
  "116422949",
  "130278976",
  "107868390",
  "60079887",
  "114301940",
  "60029074",
  "60000342",
  "124218141",
  "126781328",
  "60028084",
  "112586877",
  "108668766",
  "127726999",
  "117121465",
  "129050707",
  "126413105",
  "60105920",
  "112775938",
  "60105926",
  "60008908",
  "60028851",
  "129304473",
  "60110851",
  "121650040",
  "115337507",
  "100986075",
  "126678908",
  "109412620",
  "113136311",
  "113142943",
  "115242479",
  "127191778",
  "112645758",
  "124221191",
  "114875913",
  "60276988",
  "101014876",
  "113184837",
  "113182535",
  "60031866",
  "112833665",
  "112755184",
  "127279491",
  "123885611",
  "112868925",
  "109959576",
  "122914561",
  "60019743",
  "118159692",
  "106235883",
  "106701315",
  "112689669",
  "100484256",
  "60112315",
  "60018468",
  "60002320",
  "101578321",
  "105747226",
  "126076606",
  "131400196",
  "129855621",
  "60005951",
  "126775312",
  "60029545",
  "130435610",
  "121430097",
  "60020147",
  "125607176",
  "118597046",
  "110112058",
  "124376606",
  "131227607",
  "131607847",
  "130891703",
  "100534504",
  "129047260",
  "100433566",
  "126581661",
  "100526928",
  "130729587",
  "130729535",
  "129703203",
  "101773114",
  "128109779",
  "128064446",
  "123497681",
  "130730801",
  "131607492",
  "127268406",
  "128109745",
  "126965506",
  "128346477",
  "129673523",
  "128109746",
  "101941425",
  "126581146",
  "126249725",
  "127970991",
  "131305851",
  "128872316",
  "124376686",
  "127941504",
  "126665837",
  "130730423",
  "131056078",
  "131607877",
  "124599864",
  "115235109",
  "126553855",
  "122597934",
  "131418733",
  "127308876",
  "128408529",
  "126200849",
  "126911802",
  "127268436",
  "100533221",
  "130702026",
  "120532431",
  "107910127",
  "130518547",
  "60013443",
  "108382367",
  "130759815",
  "106773571",
  "116525328",
  "101394353",
  "101792179",
  "107986893",
  "100316021",
  "106453365",
  "112922559",
  "101140478",
  "109347134",
  "100587655",
  "60008492",
  "106456794",
  "113820147",
  "125244852",
  "113216252",
  "128262913",
  "123865204",
  "112627589",
  "106602485",
  "130557216",
  "60032692",
  "122988512",
  "127062422",
  "121225521",
  "108578096",
  "101000910",
  "112602793",
  "113112328",
  "107573430",
  "115494035",
  "125755037",
  "125381761",
  "123141776",
  "112706380",
  "112625941",
  "123502999",
  "108888250",
  "130892705",
  "115752362",
  "130701994",
  "129973323",
  "124470182",
  "130122371",
  "129289059",
  "131076240",
  "127374290",
  "128395574",
  "131531341",
  "132149803",
  "128906596",
  "114633975",
  "60278767",
  "112740566",
  "112924517",
  "125379120",
  "130723823",
  "117646342",
  "132182075",
  "100335911",
  "131324608",
  "131324998",
  "125340513",
  "100677110",
  "125383427",
  "114159234",
  "128149737",
  "128533828",
  "120168333",
  "113718737",
  "121955902",
  "129049833",
  "100502481",
  "122286572",
  "126395319",
  "121212875",
  "128306628",
  "131558906",
  "125763856",
  "125364545",
  "113119893",
  "118439805",
  "124601958",
  "107062094",
  "113951892",
  "123822264",
  "128757432",
  "131466439",
  "101261119",
  "101265211",
  "131511930",
  "113007889",
  "114684115",
  "115867250",
  "112587340",
  "60012281",
  "106548849",
  "110545556",
  "121897786",
  "125786644",
  "131254575",
  "112949764",
  "107980732",
  "113860189",
  "121466434",
  "122411044",
  "60014521",
  "106585504",
  "109248263",
  "112905068",
  "107089434",
  "129839931",
  "125919619",
  "113212209",
  "100364127",
  "112994777",
  "112637847",
  "100332619",
  "117974668",
  "129449620",
  "129132280",
  "128165491",
  "101004622",
  "60002223",
  "112987892",
  "60105859",
  "126765651",
  "107025178",
  "115341424",
  "118903785",
  "123301448",
  "123300875",
  "131254594",
  "131254565",
  "131254588",
  "131254570",
  "126999016",
  "129518085",
  "112843093",
  "112910426",
  "105621128",
  "113197759",
  "60105937",
  "131554680",
  "129184086",
  "131119214",
  "112587380",
  "122995724",
  "119743592",
  "60105918",
  "60004786",
  "112234684",
  "101581898",
  "130434719",
  "114586585",
  "109494186",
  "109911421",
  "114306072",
  "115168483",
  "105745097",
  "112656582",
  "119639166",
  "105333223",
  "130217062",
  "112805663",
  "105424804",
  "113069894",
  "114786642",
  "125701945",
  "100988589",
  "128210866",
  "111235657",
  "118700423",
  "112117183",
  "131644549",
  "130647362",
  "125155173",
  "110646347",
  "110271622",
  "128941071",
  "120477769",
  "126413640",
  "101964606",
  "121518184",
  "107852606",
  "114455318",
  "113187369",
  "125781615",
  "126907053",
  "101516366",
  "108095235",
  "123934640",
  "60073942",
  "100822854")


fed_affils<-tibble(fed_affils)






# VETERANS
# veterans health care system of the ozarks

fed_affils_name_with_usa<-c(
  "us fish",
  "121 field hospital",
  "121st combat support hospital",
  "marine corps",
  "coast guard",
  "us securities and exchange comission",
  "veterans affairs",
  "va medical center",
  "va medical research center",
  "national biodefense analysis and countermeasures center",
  "va healthcare system",
  "va healthcare center",
  "va health care system",
  "va health care center",
  "biological defense research directorate",
  "defense center of excellence",
  "secretary of defense ",
  "army",
  "navy",
  "naval",
  "space and missile defense command directed energy directorate",
  "va central western ",
  "va clinical operations",
  "va connecticut healthcare system",
  "va cooperative studies program",
  "va eastern colorado",
  "va evidence-based synthesis program",
  "va finger lakes healthcare system",
  "va great lakes veteran health care system",
  "va greater los angeles healthcare system",
  "va health care upstate new york",
  "va health services research and development",
  "va healthcare",
  "va healthcare network",
  "va heartland network",
  "va hlth serv r and d service center",
  "va hsr center for health information and communication",
  "va hsr",
  "va illinana health care system",
  "va informatics and computing infrastructure",
  "va information resource center",
  "va loma linda healthcare system",
  "va long beach healthcare system",
  "va maine healthcare system",
  "va medical center",
  "va mid-atlantic",
  "va midsouth healthcare network",
  "va midwest health care network",
  "va nebraska",
  "va new england",
  "va new jersey healthcare system",
  "va nj health care system",
  "va north texas",
  "va northeast ohio",
  "va northeast program evaluation center",
  "va northern california",
  "va northwest network",
  "va office of health equity",
  "va office of mental health and suicide prevention",
  "va office of patient care",
  "va office of research and development",
  "va office of systems redesign and improvement",
  "va pacific islands health care services community living center",
  "va palliative care quality improvement resource center",
  "va palo alto",
  "va pharmacy benefits management",
  "va pittsburg healthcare system",
  "va portland health care",
  "va providence health care system",
  "va puget sound",
  "va quality enhancement research initiative (queri)",
  "va readjustment counseling service",
  "va rocky mountain mirecc for suicide prevention",
  "va salt lake city healthcare system",
  "va san diego healthcare system",
  "va sierra nevada health care system",
  "va southeast network",
  "va southern nevada healthcare system",
  "va st louis health care system",
  "va texas valley coastal bend health care system",
  "va visn 17 center of excellence for research on returning war veterans",
  "va visn2 behavioral telehealth center",
  "va western new york healthcare system",
  "va western ny healthcare system",
  "veterans affairs va mid-atlantic mirecc workgroup",
  "w g (bill) hefner va healthcare system",
  "washington dc va medical center",
  "west palm beach va healthcare system",
  "western north carolina va health care system",
  "white river junction va medical center",
  "william jennings bryan dorn va medical center",
  "wilmington va medical center",
  "pacific islands fish and wildlife office",
  "oregon department of fish and wildlife",
  "oklahoma fish and wildlife conservation office",
  "office of fish and wildlife health and forensics",
  "washington fish and wildlife office",
  "fish and wildlife service",
  "columbia fish and wildlife conservation office",
  "colorado fish and wildlife conservation office",
  "carterville fish and wildlife conservation office",
  "ashland fish and wildlife conservation office",
  "arizona cooperative fish and wildlife research unit",
  "Office of the Federal Coordinator for Meteorology",
  "callaghan federal medical center",
  "callaghan federal hospital",
  "james a lovell federal health care center",
  "federal transit administration",
  "federal trade commission",
  "federal reserve bank",
  "federal highway administration",
  "ederal deposit insurance corporation",
  "usfs",
  "national wetlands inventory",
  "national weather center",
  "national tsunami warning center",
  "national transportation research center",
  "national systematics laboratory of the national",
  "national soil resource conservation service",
  "national risk management research laboratory",
  "national nuclear security site",
  "national institutes of child health and development",
  "national institutes of arthritis",
  "national institute on alcoholism and alcohol abuse",
  "national homeland security research center",
  "national ecological observation network",
  "national institute of standards"
)


fed_affils_full_noUSA<-c(
  "1st armored division",
"1st battalion",
"1st cavalry division",
"1st fighter wing optimizing the human weapon system",
"1st infantry division",
"1st marine division",
"1st marine division",
"1st marine expeditionary force",
"1st marine expeditionary force",
"1st marine raider battalion",
"1st medical battalion",
"1st medical battalion",
"1st medical brigade",
"1st special forces command",
"1st special forces command",
"1st special forces group (airborne)",
"1st special warfare center and school (airborne)",
"25th special forces group",
"28th bomb wing 2900 doolittle drive",
"2d battalion",
"2d marine raider battalion",
"2nd battalion",
"2nd brigade",
"2nd brigade combat team",
"2nd cavalry regiment",
"2nd medical battalion",
"2nd stryker brigade combat team",
"33d fighter wing aviator performance team",
"352nd civil affairs command",
"375 operational medical readiness squadron",
"3d marine division",
"3d medical battalion",
"3rd marine regiment",
"3rd mcds command surgeon ft gillem",
"3rd medical command (deployment support)",
"3rd medical command fwd",
"47th combat support hospital jblm",
"48th rescue squadron",
"49th wing aviator performance team",
"4th fighter wing optimizing the human weapon system",
"4th id",
"4th infantry division",
"4th infantry division",
"4th infantry division",
"4th marine reconnaissance battalion",
"5-20 infantry battalion",
"512th field hospital",
"559th medical group",
"56th fighter wing human performance team",
"59th medical wing",
"59th medical wing",
"59th medical wing science and technology",
"64th medical detachment",
"737th training group",
"75th ranger regiment",
"75th ranger regiment",
"dod-va",
"75th ranger regiment",
"75th ranger regiment headquarters",
"75th ranger regiment headquarters",
"807th medical command (deployment support)",
"91st civil affairs bn",
"a warrior transition unit",
"brian allgood army community hospital",
"brian d allgood army community hospital",
"eisenhower army medical center",
"u s army combat capabilities development command atlantic",
"us 8th army korea medical simulation training center",
"us walter reed army institute of research/department of defense (wrair/dod)",
"walter reed army institute of research",
"walter reed army institute of research",
"walter reed army institute of research (wrair)",
"walter reed army institute of research africa",
"naval medical readiness and training center (nmrtc)",
"naval medical readiness and training command guam",
"naval medical research center-asia",
"naval medical research unit south",
"naval medical research unit-2",
"naval medical research unit-3 ghana detachment",
"naval postgraduate school",
"naval station guantanamo bay",
"office of naval research global",
"office of naval research-global",
"usaf/afosr european office of aerospace research &am",
"marine corps air station",
"us marine corps",
"usmc",
"rear admiral usn",
"usn",
"usn is an soidc and advanced tactical paramedic",
"usna",
"usnc-tech",
"usnr",
"usns mercy (t-ah 19)",
"the usma at west point",
"national center for genome resources",
"national institute of environmental health science",
"national health and environmental effects research l",
"national agricultural library",
"national academy of medicine",
"national institute of allergy and infectious disease",
"national institute of heath",
"national institute for allergy and infectious diseases",
"national radio astronomy laboratory",
"national energy research scientific computing center",
"national reconnaissance office",
"national renewable laboratory",
"national fisheries research center",
"national research laboratory of berkeley",
"valles caldera national preserve",
"thomas jeerson national accelerator facility",
"national astronomy and ionosphere center",
"national astronomy and ionosphere center",
"national institute of neurological and communicative",
"national defence university",
"national center for veterans studies",
"national agricultural statistical service",
"national bioforensic analysis center",
"iss national lab",
"national institute of allergy and infectious disease",
"national human genomic research institute",
"national estuarine research reserve",
"national center on sleep disorders research",
"national historical park",
"national academy of medicine",
"national institute of arthritis, metabolism and digestiv",
"national institute for diabetes and digestive and kidney diseases",
"national institute of diabetes, digestive and kidney dis",
"national institute of neurological disorders",
"national instutes of health",
"national historic site",
"national military park",
"national lakeshore",
"national oceanic and atmospheric association",
"national oceanic and atmospheric association",
"kitt peak national observatory",
"7th special forces group (airborne)",
"american museum of national history",
"office of assistant secretary",
"office of the secretary of defense",
"office of assistant secretary for indian affairs",
"office of the assistant secretary for preparedness and r",
"agency for toxic substances and disease registry",
"the agency for toxic substances and disease registry",
"agency for toxic substances and disease registry",
"agency for health care research")

# Scopus Affiliation numbers
scopus_affils_df_NOTFED<-pubs_with_fed %>% 
  filter(fed_author==FALSE) %>% 
  select(affil_id, country,affiliation) %>% 
  distinct()



false_fed_negatives1 <- scopus_affils_df_NOTFED %>%
  filter(country=="usa" & str_detect(affiliation, paste(fed_affils_name_with_usa, collapse = "|"))) 


false_fed_negatives2 <- scopus_affils_df_NOTFED %>%
  filter(str_detect(affiliation, paste(fed_affils_full_noUSA, collapse = "|"))) 



false_fed_negatives3 <- scopus_affils_df_NOTFED %>%
  filter(affil_id %in% fed_affils$fed_affils) 

false_fed_negatives<-bind_rows(false_fed_negatives1,
                               false_fed_negatives2,
                               false_fed_negatives3) %>%
  distinct()



pubs_with_fed <- pubs_with_fed %>%
  mutate(fed_author = if_else(affil_id %in% false_fed_negatives$affil_id, TRUE, fed_author)) 

fed_authors_fix <- pubs_with_fed %>%
  filter(fed_author==TRUE & is.na(agency)) %>% 
  select(affil_id,affiliation,agency) %>%
  distinct() %>% 
  mutate(agency = if_else(str_detect(affiliation,"defense|navy|air force|marine corps|army|military|naval"),"dod",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"brigade|group|combat|battalion|usmc|regiment|command"),"dod",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"defence|wing|squadron|division|cavalry| usn |field hospital|medical wing"),"dod",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"1st|4th"),"dod",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"usfs"),"usda",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"wildlife"),"interior",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"dod-va|va "),"va",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"veteran"),"dod",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"toxic substances and disease"),"cdc",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"national institute"),"nih",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"federal reserve"),"federal reserve system",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"federal deposit insurance corporation"),"fdic",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national wetlands inventory"),"interior",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national risk management research laboratory"),"epa",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"federal transit administration"),"dot",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"federal trade commission"),"ftc",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national nuclear security site"),"doe",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national ecological observation network"),"nsf",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national nuclear security site livermore office"),"doe",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"callaghan federal hospital"),"dod",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"callaghan federal medical"),"dod",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"james a lovell federal health care center"),"va",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"safety and work life coast guard"),"dod",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"us president’s malaria initiative"),"usaid",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national weather center"),"noaa",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national institutes of child health and development"),"nih",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national institutes of arthritis"),"nih",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national institute on alcoholism and alcohol abuse"),"nih",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national risk management research laboratory"),"epa",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national weather center research experiences for undergraduates"),"noaa",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national tsunami warning center"),"noaa",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national institute of standards"),"nist",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national homeland security research center"),"epa",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national systematics laboratory of the national oceanic"),"noaa",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national transportation research center"),"dot",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national nuclear security site"),"doe",agency)) %>%
  mutate(agency = if_else(str_detect(affiliation,"national soil resource conservation service"),"usda",agency)) %>%
mutate(agency = if_else(str_detect(affiliation,"national historical park|historic site|historical site|national lakeshore"),"interior",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"us department of interior—international technical assistance program"),"interior",agency)) %>% 
  mutate(agency = if_else(str_detect(affiliation,"federal highway administration"),"dot",agency)) %>% 
  mutate(agency = if_else(affil_id=="108382367","usda",agency)) %>%
  mutate(agency = if_else(affil_id=="128941071","usda",agency)) %>%
  mutate(agency = if_else(affil_id=="125244852","usda",agency)) %>%
  mutate(agency = if_else(affil_id=="122988512","ahrq",agency)) %>%
  mutate(agency = if_else(affil_id=="100502481","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="100571977","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="100822854","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="115752362","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="121212875","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="122286572","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="129973323","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="130518547","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="130701994","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="130892705","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="132182075","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="60008492","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="100335911","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="101792179","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="106453365","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="109347134","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="112920600","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="112922559","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="120168333","doe",agency)) %>%
  mutate(agency = if_else(affil_id=="123502999","epa",agency)) %>%
  mutate(agency = if_else(affil_id=="100677110","hhs",agency)) %>%
  mutate(agency = if_else(affil_id=="113820147","hhs",agency)) %>%
  mutate(agency = if_else(affil_id=="125383427","hhs",agency)) %>%
  mutate(agency = if_else(affil_id=="128533828","hhs",agency)) %>%
  mutate(agency = if_else(affil_id=="101140478","interior",agency)) %>%
  mutate(agency = if_else(affil_id=="128172361","interior",agency)) %>%
  mutate(agency = if_else(affil_id=="130391142","interior",agency)) %>%
  mutate(agency = if_else(affil_id=="123678203","nasa",agency)) %>%
  mutate(agency = if_else(affil_id=="123865204","nasa",agency)) %>%
  mutate(agency = if_else(affil_id=="107986893","nasem",agency)) %>%
  mutate(agency = if_else(affil_id=="122628604","nasem",agency)) %>%
  mutate(agency = if_else(affil_id=="130148658","nasem",agency)) %>%
  mutate(agency = if_else(affil_id=="112602793","nih",agency)) %>%
  mutate(agency = if_else(affil_id=="113013369","nih",agency)) %>%
  mutate(agency = if_else(affil_id=="125340513","nih",agency)) %>%
  mutate(agency = if_else(affil_id=="125381761","nih",agency)) %>%
  mutate(agency = if_else(affil_id=="125755037","nih",agency)) %>%
  mutate(agency = if_else(affil_id=="131324608","nih",agency)) %>%
  mutate(agency = if_else(affil_id=="131324998","nih",agency)) %>%
  mutate(agency = if_else(affil_id=="100316021","noaa",agency)) %>%
  mutate(agency = if_else(affil_id=="101000910","noaa",agency)) %>%
  mutate(agency = if_else(affil_id=="123141776","noaa",agency)) %>%
  mutate(agency = if_else(affil_id=="130557216","noaa",agency)) %>%
  mutate(agency = if_else(affil_id=="60071501","nsf",agency)) %>%
  mutate(agency = if_else(affil_id=="100587655","nsf",agency)) %>%
  mutate(agency = if_else(affil_id=="101394353","nsf",agency)) %>%
  mutate(agency = if_else(affil_id=="106456794","nsf",agency)) %>%
  mutate(agency = if_else(affil_id=="108578096","smithsonian",agency)) %>%
  mutate(agency = if_else(affil_id=="114159234","smithsonian",agency)) %>%
  mutate(agency = if_else(affil_id=="128149737","smithsonian",agency)) %>%
  mutate(agency = if_else(affil_id=="115539127","usphs",agency)) %>%
  mutate(agency = if_else(affil_id=="123896933","usphs",agency)) %>%
  mutate(agency = if_else(affil_id=="126381052","usphs",agency)) %>%
  mutate(agency = if_else(affil_id=="120532431","va",agency)) %>% 
  mutate(agency = if_else(affil_id=="128306628","usda",agency)) %>%
  mutate(agency = if_else(affil_id=="131558906","usda",agency)) %>%
  mutate(agency = if_else(affil_id=="125763856","usda",agency)) %>%
  mutate(agency = if_else(affil_id=="125364545","usda",agency)) %>%
  mutate(agency = if_else(affil_id=="113119893","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="118439805","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="124601958","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="107062094","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="113951892","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="123822264","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="128757432","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="131466439","dod",agency)) %>%
  mutate(agency = if_else(affil_id=="101261119","va",agency)) %>%
  mutate(agency = if_else(affil_id=="101265211","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131511930","va",agency)) %>%
  mutate(agency = if_else(affil_id=="113007889","va",agency)) %>%
  mutate(agency = if_else(affil_id=="114684115","va",agency)) %>%
  mutate(agency = if_else(affil_id=="115867250","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112587340","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60012281","va",agency)) %>%
  mutate(agency = if_else(affil_id=="106548849","va",agency)) %>%
  mutate(agency = if_else(affil_id=="110545556","va",agency)) %>%
  mutate(agency = if_else(affil_id=="121897786","va",agency)) %>%
  mutate(agency = if_else(affil_id=="125786644","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131254575","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112949764","va",agency)) %>%
  mutate(agency = if_else(affil_id=="107980732","va",agency)) %>%
  mutate(agency = if_else(affil_id=="113860189","va",agency)) %>%
  mutate(agency = if_else(affil_id=="121466434","va",agency)) %>%
  mutate(agency = if_else(affil_id=="122411044","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60014521","va",agency)) %>%
  mutate(agency = if_else(affil_id=="106585504","va",agency)) %>%
  mutate(agency = if_else(affil_id=="109248263","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112905068","va",agency)) %>%
  mutate(agency = if_else(affil_id=="107089434","va",agency)) %>%
  mutate(agency = if_else(affil_id=="129839931","va",agency)) %>%
  mutate(agency = if_else(affil_id=="125919619","va",agency)) %>%
  mutate(agency = if_else(affil_id=="113212209","va",agency)) %>%
  mutate(agency = if_else(affil_id=="100364127","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112994777","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112637847","va",agency)) %>%
  mutate(agency = if_else(affil_id=="100332619","va",agency)) %>%
  mutate(agency = if_else(affil_id=="117974668","va",agency)) %>%
  mutate(agency = if_else(affil_id=="129449620","va",agency)) %>%
  mutate(agency = if_else(affil_id=="129132280","va",agency)) %>%
  mutate(agency = if_else(affil_id=="128165491","va",agency)) %>%
  mutate(agency = if_else(affil_id=="101004622","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60002223","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112987892","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60105859","va",agency)) %>%
  mutate(agency = if_else(affil_id=="126765651","va",agency)) %>%
  mutate(agency = if_else(affil_id=="107025178","va",agency)) %>%
  mutate(agency = if_else(affil_id=="115341424","va",agency)) %>%
  mutate(agency = if_else(affil_id=="118903785","va",agency)) %>%
  mutate(agency = if_else(affil_id=="123301448","va",agency)) %>%
  mutate(agency = if_else(affil_id=="123300875","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131254594","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131254565","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131254588","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131254570","va",agency)) %>%
  mutate(agency = if_else(affil_id=="126999016","va",agency)) %>%
  mutate(agency = if_else(affil_id=="129518085","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112843093","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112910426","va",agency)) %>%
  mutate(agency = if_else(affil_id=="105621128","va",agency)) %>%
  mutate(agency = if_else(affil_id=="113197759","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60105937","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131554680","va",agency)) %>%
  mutate(agency = if_else(affil_id=="129184086","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131119214","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112587380","va",agency)) %>%
  mutate(agency = if_else(affil_id=="122995724","va",agency)) %>%
  mutate(agency = if_else(affil_id=="119743592","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60105918","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60004786","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112234684","va",agency)) %>%
  mutate(agency = if_else(affil_id=="101581898","va",agency)) %>%
  mutate(agency = if_else(affil_id=="130434719","va",agency)) %>%
  mutate(agency = if_else(affil_id=="114586585","va",agency)) %>%
  mutate(agency = if_else(affil_id=="109494186","va",agency)) %>%
  mutate(agency = if_else(affil_id=="109911421","va",agency)) %>%
  mutate(agency = if_else(affil_id=="114306072","va",agency)) %>%
  mutate(agency = if_else(affil_id=="115168483","va",agency)) %>%
  mutate(agency = if_else(affil_id=="105745097","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112656582","va",agency)) %>%
  mutate(agency = if_else(affil_id=="119639166","va",agency)) %>%
  mutate(agency = if_else(affil_id=="105333223","va",agency)) %>%
  mutate(agency = if_else(affil_id=="130217062","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112805663","va",agency)) %>%
  mutate(agency = if_else(affil_id=="105424804","va",agency)) %>%
  mutate(agency = if_else(affil_id=="113069894","va",agency)) %>%
  mutate(agency = if_else(affil_id=="114786642","va",agency)) %>%
  mutate(agency = if_else(affil_id=="125701945","va",agency)) %>%
  mutate(agency = if_else(affil_id=="100988589","va",agency)) %>%
  mutate(agency = if_else(affil_id=="128210866","va",agency)) %>%
  mutate(agency = if_else(affil_id=="111235657","va",agency)) %>%
  mutate(agency = if_else(affil_id=="118700423","va",agency)) %>%
  mutate(agency = if_else(affil_id=="112117183","va",agency)) %>%
  mutate(agency = if_else(affil_id=="131644549","va",agency)) %>%
  mutate(agency = if_else(affil_id=="130647362","va",agency)) %>%
  mutate(agency = if_else(affil_id=="125155173","va",agency)) %>%
  mutate(agency = if_else(affil_id=="110646347","va",agency)) %>%
  mutate(agency = if_else(affil_id=="110271622","va",agency)) %>%
  mutate(agency = if_else(affil_id=="120477769","va",agency)) %>%
  mutate(agency = if_else(affil_id=="126413640","va",agency)) %>%
  mutate(agency = if_else(affil_id=="101964606","va",agency)) %>%
  mutate(agency = if_else(affil_id=="121518184","va",agency)) %>%
  mutate(agency = if_else(affil_id=="107852606","va",agency)) %>%
  mutate(agency = if_else(affil_id=="114455318","va",agency)) %>%
  mutate(agency = if_else(affil_id=="113187369","va",agency)) %>%
  mutate(agency = if_else(affil_id=="125781615","va",agency)) %>%
  mutate(agency = if_else(affil_id=="126907053","va",agency)) %>%
  mutate(agency = if_else(affil_id=="101516366","va",agency)) %>%
  mutate(agency = if_else(affil_id=="108095235","va",agency)) %>%
  mutate(agency = if_else(affil_id=="123934640","va",agency)) %>%
  mutate(agency = if_else(affil_id=="60073942","va",agency)) %>%
  mutate(agency = if_else(affil_id=="126395319","va",agency)) %>%
  mutate(agency = if_else(agency=="us department of defense","dod",agency)) %>%
  mutate(agency = if_else(agency=="us department of the interior","interior",agency)) %>% 
  arrange(affiliation)


pubs_with_fed<-full_join(pubs_with_fed,fed_authors_fix,by=c("affil_id","affiliation")) %>% 
  rename(agency=agency.x,
         agency_fixed=agency.y)
pubs_with_fed<-pubs_with_fed %>% 
  mutate(agency=if_else((is.na(agency) & !is.na(agency_fixed)),agency_fixed,agency)) %>% 
  select(-agency_fixed) 

# final change of agency names
pubs_with_fed<-pubs_with_fed %>% 
  mutate(agency = if_else(agency=="agriculture","usda",agency))


# save isolate fed affiliations

fed_affiliations_list<-pubs_with_fed %>% 
  filter(fed_author==TRUE) %>% 
  select(affil_id,affiliation,country,agency) %>% 
  distinct()

NONfed_affiliations_list<-pubs_with_fed %>% 
  filter(fed_author==FALSE) %>% 
  select(affil_id,affiliation,country) %>% 
  distinct()
# 
# ###intermediate
# write_csv(fed_authors_fix,"./data_raw/fed_authors_fix.csv")
# 
# veterans_fix<-NONfed_affiliations_list %>% 
#   filter(country=="usa"|country=="puerto rico"|is.na(country)) %>% 
#   filter(str_detect(affiliation,"veteran|va |vha|nsf |usda|dod ")) %>% 
#   arrange(affiliation)
# write_csv(veterans_fix,"./data_raw/veterans_fix.csv")

# save --------------------------------------------------------------------



write_rds(pubs_with_fed, "./data_clean/analysis_fed_pubs.rds")
write_rds(fed_affiliations_list, "./data_clean/fed_affiliations_list.rds")
write_rds(NONfed_affiliations_list, "./data_clean/NONfed_affiliations_list.rds")


