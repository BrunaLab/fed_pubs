
# list of agencies --------------------------------------------------------
# Federal Research & Development Agencies, many of which also publish under agency name
# https://en.wikipedia.org/wiki/List_of_United_States_federal_research_and_development_agencies
library(janitor)
library(httr)
library(XML)
library(tidyverse)
url <- "https://en.wikipedia.org/wiki/Federally_funded_research_and_development_centers"

r <- GET(url)
federally_funded_centers <- readHTMLTable(
  doc=content(r, "text"))
federally_funded_centers<-federally_funded_centers[[1]]

federally_funded_centers <- federally_funded_centers %>% 
  row_to_names(row_number = 1)

agencies<-read_csv("./data_raw/all_agencies.csv") %>% 
  mutate_all(tolower) %>% 
  separate_wider_delim(
    agency,
    delim = "[",
    names = c("agency", "extra"),
    too_few = "align_start",
    cols_remove = TRUE
  ) %>% 
  select(-extra) %>% 
  arrange(agency) %>% 
  mutate(agency=gsub(" - ","-",agency)) %>% 
  add_count(agency) %>% 
  relocate(n,.before=1) %>% 
  arrange(agency,scopus_affil_id)
  

agencies<-group_by(agencies) %>% 
  mutate(scopus_affil_id=if_else(n>1 & is.na(scopus_affil_id), "DELETE",scopus_affil_id)) %>% 
  arrange(desc(n),agency) 


agencies %>% 
  group_by(scopus_affil_id) %>% 
  tally() %>% 
  arrange(desc(n))


need_affil_id<-agencies %>% filter(is.na(scopus_affil_id))

# Install and load the necessary package
install.packages("rscopus")
library(rscopus)

# Set your Scopus API key
api_key <- "----"
# 
# # # Define the institution name
# # institution_name <- "advanced research projects agency for health"
# 
# # Perform the affiliation search
# affiliation_results <- get_affiliation_info(affil_name = need_affil_id[h,]$agency, api_key = api_key)
# 
# # # Extract the affiliation ID
# # affiliation_id <- affiliation_results$content$`search-results`$entry[[1]]$`affiliation-id`
# 
# # Print the affiliation ID
# agency <- seq_along(need_affil_id$agency)
# 
# datalist = list()
# ####
# #DONT FORGET TO CHANGE THE FILE NAME BELOW!!!!!!!
# #####
# 
# for (h in agency){
#   
#   # Define the institution name
#   
#   
#   # Perform the affiliation search
#   affiliation_results <- get_affiliation_info(affil_name = need_affil_id$agency[h], api_key = api_key)
#   datalist[[h]]<-affiliation_results
#   }

# Ensure need_affil_id exists and contains an 'agency' column
if (!exists("need_affil_id") || !"agency" %in% names(need_affil_id)) {
  stop("Error: `need_affil_id` dataset is missing or does not contain an `agency` column.")
}

# Initialize the list to store results
datalist <- vector("list", length(need_affil_id$agency))

# Loop through each agency and fetch the affiliation info
for (h in seq_along(need_affil_id$agency)) {
  affiliation_name <- need_affil_id$agency[h]
  
  # Wrap the API call in tryCatch to handle errors. If an error or null response occurs,
  # record the search term in datalist[[h]] and move on.
  affiliation_results <- tryCatch({
    get_affiliation_info(affil_name = affiliation_name, api_key = api_key)
  }, error = function(e) {
    message("Error encountered for affiliation '", affiliation_name, "': ", e$message)
    return(NULL)
  })
  
  # If the API call returned NULL, record the search term, otherwise record the results along with the search term.
  if (is.null(affiliation_results)) {
    datalist[[h]] <- list(agency = affiliation_name, results = "NULL response")
  } else {
    datalist[[h]] <- list(agency = affiliation_name, results = affiliation_results)
  }
}

write_rds(datalist,"./data_raw/datalist_from_scopus.rds")



# part II -----------------------------------------------------------------



datalist<-read_rds("./data_raw/datalist_from_scopus.rds")

# Optionally, remove any entries from datalist that are entirely NULL (if any exist)
datalist <- Filter(Negate(is.null), datalist)

# Check the first few entries to verify the output
print(head(datalist))

# Ensure all list elements are dataframes with the same column structure
datalist <- lapply(datalist, function(x) {
  if (is.null(x)) {
    return(data.frame())
  }
  as.data.frame(x)
})

# Find all unique column names across the list
all_columns <- unique(unlist(lapply(datalist, names)))

# Standardize column structure across all dataframes
datalist <- lapply(datalist, function(df) {
  # Convert to data.frame in case it isn't
  df <- as.data.frame(df)
  
  # Determine which columns are missing in this dataframe
  missing_cols <- setdiff(all_columns, names(df))
  
  # Loop over each missing column and add it with NA values
  for (col in missing_cols) {
    df[[col]] <- NA  # NA will be replicated to match nrow(df)
  }
  
  # Reorder the columns to match all_columns
  df <- df[all_columns]
  
  return(df)
})

# Combine all the dataframes into one using dplyr::bind_rows
affiliation_df <- bind_rows(datalist) %>% 
  tibble() %>% select(
    agency,
    affil_name_scopus="results.affiliation.name",
    city="results.city",
    country="results.country",
    parent_affil_id="results.parent.affiliation.id",
    affil_id="results.affil_id",
    affil_name="results.affil_name"
    ) %>% 
  mutate_all(tolower)

affiliation_df<-affiliation_df %>% 
  mutate(affil_name_scopus=if_else(is.na(affil_name_scopus),affil_name,affil_name_scopus))


# Print the first few rows
head(affiliation_df)


affiliation_df_keep<-affiliation_df %>% filter(country=="united states"|
                                                str_detect(affil_name_scopus,"u. s. ")|
                                                str_detect(affil_name_scopus,"usda")|
                                                str_detect(affil_name_scopus,"us department of agriculture")|
                                                str_detect(affil_name_scopus,"peace corps")|
                                                str_detect(affil_name_scopus,"us federal")|
                                                str_detect(affil_name_scopus,"us postal")|
                                                str_detect(affil_name_scopus,"us public")|
                                                str_detect(affil_name_scopus,"usaf")|
                                                 str_detect(affil_name_scopus,"us bureau of consumer financial")|
                                                 str_detect(affil_name_scopus,"medicaid services (cms)")|
                                                 
                                                str_detect(affil_name_scopus,"nih")|
                                                str_detect(affil_name_scopus,"us federal")|
                                                str_detect(affil_name_scopus,"u.s. ")|
                                                str_detect(affil_name_scopus,"united states")) %>% 
  filter(!str_detect(affil_name_scopus," carolina")) %>% 
  filter(!str_detect(affil_name_scopus,"mayor")) %>% 
  filter(!str_detect(affil_name_scopus,"vera institute")) %>% 
  filter(!str_detect(affil_name_scopus,"new york state")) %>% 
  filter(!str_detect(affil_name_scopus,"nc office of state")) %>% 
  filter(!str_detect(affil_name_scopus,"city")) %>% 
  filter(!str_detect(affil_name_scopus,"county")) %>% 
  filter(!str_detect(affil_name_scopus,"wellesley")) %>% 
  filter(!str_detect(affil_name_scopus,"medical college")) %>% 
  filter(!str_detect(affil_name_scopus,"massachusetts")) %>% 
  
  filter(!str_detect(affil_name_scopus,"college and university professional association")) %>% 
  filter(!str_detect(affil_name_scopus,"division of applied sciences harvard university")) %>% 
  filter(!str_detect(agency,"vice president of the united states")) %>% 
  filter(!str_detect(affil_name_scopus,"same university")) %>% 
  filter(!str_detect(affil_name_scopus,"human resources research center")) %>% 

filter(!str_detect(affil_name_scopus,"office of the university provost")) %>% 
  filter(!str_detect(affil_name_scopus,"university health service")) %>% 
  filter(!str_detect(affil_name_scopus,"supreme university")) %>% 
  filter(!str_detect(affil_name_scopus,"state of")) %>% 
  filter(!str_detect(affil_name_scopus,"vermont office")) %>% 
  filter(!str_detect(affil_name_scopus,"oregon state office")) %>% 
  filter(!str_detect(affil_name_scopus,"penn state")) %>% 
  filter(!str_detect(agency,"west center")) %>% 
filter(!str_detect(affil_name_scopus,"atrium health-virtual critical care")) %>% 
filter(!str_detect(affil_name_scopus,"utah office of")) %>% 
filter(!str_detect(affil_name_scopus,"barnstable co")) %>% 
filter(!str_detect(affil_name_scopus,"blood systems laboratories")) %>% 
filter(!str_detect(affil_name_scopus,"bryan cave llp")) %>% 
filter(!str_detect(affil_name_scopus,"brooklyn botanic garden")) %>% 
filter(!str_detect(affil_name_scopus,"california botanic garden")) %>% 
filter(!str_detect(affil_name_scopus,"canadian institute of peace research")) %>% 
filter(!str_detect(affil_name_scopus,"culver military academy")) %>% 
filter(!str_detect(affil_name_scopus,"fairchild tropical botanic garden")) %>% 
filter(!str_detect(affil_name_scopus,"executive secretariat")) %>% 
filter(!str_detect(affil_name_scopus,"executive secretary")) %>% 
filter(!str_detect(affil_name_scopus,"french air force academy")) %>% 
filter(!str_detect(affil_name_scopus,"washington state department ")) %>% 
filter(!str_detect(affil_name_scopus,"georgia department of public health")) %>% 
filter(!str_detect(affil_name_scopus,"governor")) %>% 
filter(!str_detect(affil_name_scopus,"florida legislature")) %>% 
filter(!str_detect(affil_name_scopus," llc")) %>% 
filter(!str_detect(affil_name_scopus,"richardson merrell, inc.")) %>% 
filter(!str_detect(affil_name_scopus,"woods hole marine systems inc.")) %>% 
filter(!str_detect(affil_name_scopus,"houston zoological gardens")) %>% 
filter(!str_detect(affil_name_scopus,"illinois emergency")) %>% 
filter(!str_detect(affil_name_scopus,"illinois environmental")) %>% 
filter(!str_detect(affil_name_scopus,"jpmorgan chase and us cyber command")) %>% 
filter(!str_detect(affil_name_scopus,"juniper level botanic garden")) %>% 
filter(!str_detect(affil_name_scopus,"king and spalding")) %>% 
filter(!str_detect(affil_name_scopus,"korea military academy")) %>% 
filter(!str_detect(affil_name_scopus,"kroc institute of international peace studies")) %>% 
filter(!str_detect(affil_name_scopus,"lektorat mint")) %>% 
filter(!str_detect(affil_name_scopus,"liberty mutual insurance")) %>% 
filter(!str_detect(affil_name_scopus,"louisiana hlth soc. rehab. serv. adm.")) %>% 
filter(!str_detect(affil_name_scopus,"ma office of technical assistance")) %>% 
filter(!str_detect(affil_name_scopus,"maryland")) %>% 
filter(!str_detect(affil_name_scopus,"mesker park zoo and botanic garden")) %>% 
filter(!str_detect(affil_name_scopus,"michigan")) %>% 
filter(!str_detect(affil_name_scopus,"military acad of technology")) %>% 
filter(!str_detect(affil_name_scopus,"louisiana office of tourism")) %>% 
filter(!str_detect(affil_name_scopus,"lakes park botanic garden")) %>% 
filter(!str_detect(affil_name_scopus,"jt. oil anal. prog. tech. support c.")) %>% 
filter(!str_detect(affil_name_scopus,"hunton and williams")) %>% 
filter(!str_detect(affil_name_scopus,"hjf")) %>% 
filter(!str_detect(affil_name_scopus,"gentiva health services")) %>% 
filter(!str_detect(affil_name_scopus,"ganna walska lotusland")) %>% 
filter(!str_detect(affil_name_scopus,"fork union military academy")) %>% 
filter(!str_detect(affil_name_scopus,"foundation for the advancement of military medicine")) %>% 
filter(!str_detect(affil_name_scopus,"el paso center for children inc")) %>% 
filter(!str_detect(affil_name_scopus,"defense acquisition, inc.")) %>% 
filter(!str_detect(affil_name_scopus,"energizer holdings, inc.")) %>% 
filter(!str_detect(affil_name_scopus,"dela-ware office of child care licensing")) %>% 
filter(!str_detect(affil_name_scopus,"dr corum")) %>% 
filter(!str_detect(affil_name_scopus,"mint color")) %>% 
filter(!str_detect(affil_name_scopus,"mint health + drugs: meridian")) %>% 
filter(!str_detect(affil_name_scopus,"mint hill")) %>% 
filter(!str_detect(affil_name_scopus,"mint labs inc.")) %>% 
filter(!str_detect(affil_name_scopus,"mississippi")) %>% 
filter(!str_detect(affil_name_scopus,"missouri")) %>% 
filter(!str_detect(affil_name_scopus,"museum and gardens library")) %>% 
filter(!str_detect(affil_name_scopus,"museum of jewish heritage — a living memorial to the holocaust")) %>% 
filter(!str_detect(affil_name_scopus,"national fish and wildlife foundation")) %>% 
filter(!str_detect(affil_name_scopus,"national lead company")) %>% 
filter(!str_detect(affil_name_scopus,"national surgical adjuvant breast and bowel")) %>% 
filter(!str_detect(affil_name_scopus,"naval academy athletic association")) %>% 
filter(!str_detect(affil_name_scopus,"novant health mint hill ob/gyn")) %>% 
filter(!str_detect(affil_name_scopus,"yukon kuskokwim health corporation")) %>% 
filter(!str_detect(affil_name_scopus,"warnell school of forestry and natural resources")) %>% 
filter(!str_detect(affil_name_scopus,"women's bureau")) %>% 
filter(!str_detect(affil_name_scopus,"utah")) %>% 
filter(!str_detect(affil_name_scopus,"yu. a. gagarin air force academy")) %>% 
filter(!str_detect(affil_name_scopus,"naval academy preparatory school")) %>% 
filter(!str_detect(affil_name_scopus,"national space development agency of japan")) %>% 
filter(!str_detect(affil_name_scopus,"national park foundation")) %>% 
filter(!str_detect(affil_name_scopus,"national parkinson foundation")) %>% 
filter(!str_detect(affil_name_scopus,"national industries for the blind")) %>% 
filter(!str_detect(affil_name_scopus,"mint museum of art")) %>% 
filter(!str_detect(affil_name_scopus,"minnesota")) %>% 
filter(!str_detect(affil_name_scopus,"duke family support program")) %>% 
filter(!str_detect(affil_name_scopus,"chicago botanical garden")) %>% 
filter(!str_detect(affil_name_scopus,"chicano secret service")) %>% 
filter(!str_detect(affil_name_scopus,"chicago office")) %>% 
filter(!str_detect(affil_name_scopus,"child welfare league of america")) %>% 
#   # military academy: check all of these
# chief of staff?
     filter(!str_detect(affil_name_scopus,"kentucky state administrative office of the courts")) %>% 
     filter(!str_detect(affil_name_scopus,"california administrative office of the courts")) %>% 
     filter(!str_detect(affil_name_scopus,"carolina administrative office of the courts")) %>% 
     filter(!str_detect(affil_name_scopus,"administrative office of the illinois courts")) %>% 
     filter(!str_detect(affil_name_scopus,"sohail inayatullah is at the office of the administrative director of the courts")) %>% 
     filter(!str_detect(affil_name_scopus,"california administrative office of the courts’ center for families")) %>% 
     filter(!str_detect(affil_name_scopus,"utah administrative office of the courts")) %>% 
     filter(!str_detect(affil_name_scopus,"african wildlife foundation")) %>% 
     filter(!str_detect(affil_name_scopus,"new jersey department of agriculture")) %>% 
     filter(!str_detect(affil_name_scopus,"california association of sanitation agencies")) %>% 
     filter(!str_detect(affil_name_scopus,"western association of fish and wildlife agencies")) %>% 
     filter(!str_detect(affil_name_scopus,"association of fish and wildlife agencies")) %>% 
     filter(!str_detect(affil_name_scopus,"colorado department of regulatory agencies")) %>% 
     filter(!str_detect(affil_name_scopus,"association of fish and wildlife agencies")) %>% 
     filter(!str_detect(affil_name_scopus,"hillside family of agencies")) %>% 
     filter(!str_detect(affil_name_scopus,"chicago research center, inc.")) %>% 
     filter(!str_detect(affil_name_scopus,"aerospace medical association")) %>% 
     filter(!str_detect(affil_name_scopus,"american institutes for research")) %>% 
     filter(!str_detect(affil_name_scopus,"airesearch manufacturing co")) %>% 
     filter(!str_detect(affil_name_scopus,"center of effort (coe) winery")) %>% 
     filter(!str_detect(affil_name_scopus,"arroyo chamiso pediatric center")) %>% 
     filter(!str_detect(affil_name_scopus,"bureau of economics")) %>% 
     filter(!str_detect(affil_name_scopus,"bureau of economic analysis")) %>% 
  filter(!str_detect(affil_name_scopus,"canadian ambassador ")) %>% 
  filter(!str_detect(affil_name_scopus,"massachusetts bureau of statistics of labor")) %>% 
  filter(!str_detect(affil_name_scopus,"scratch foundation")) %>% 
  filter(!str_detect(affil_name_scopus,"provost")) %>% 
  filter(!str_detect(affil_name_scopus," dean ")) %>% 
filter(!str_detect(affil_name_scopus,"military medical academy")) %>% 
filter(!str_detect(affil_name_scopus,"royal military academy")) %>% 
filter(!str_detect(affil_name_scopus,"medical academy")) %>% 
filter(!str_detect(affil_name_scopus,"military academy of lithuania")) %>% 
filter(!str_detect(affil_name_scopus,"s.m. kirov military medical academy")) %>% 
filter(!str_detect(affil_name_scopus,"military medical academy")) %>% 
filter(!str_detect(affil_name_scopus,"military technical academy")) %>% 
filter(!str_detect(affil_name_scopus,"the military academy")) %>% 
filter(!str_detect(affil_name_scopus,"sarasota military academy")) %>% 
filter(!str_detect(affil_name_scopus,"hellenic")) %>% 
filter(!str_detect(affil_name_scopus,"polish")) %>% 
filter(!str_detect(affil_name_scopus,"st. thomas military academy")) %>% 
filter(!str_detect(affil_name_scopus,"marine military academy")) %>% 
filter(!str_detect(affil_name_scopus,"hargrave military academy")) %>% 
  
  filter(!str_detect(affil_name_scopus,"chief of staff office")) %>% 
  filter(!str_detect(affil_name_scopus,"bureau for the economic analysis of development (bread)")) %>% 
  filter(agency!="children's bureau") %>% 
  filter(agency!="civil division") %>% 
  filter(agency!="civil rights division") %>% 
  filter(agency!="criminal division") %>% 
  filter(agency!="division of emergency operations") %>% 
  filter(agency!="office of legal counsel") %>% 
  filter(agency!="office of legal policy") %>% 
  filter(agency!="gallaudet university") %>% 
  filter(agency!="east–west center") %>% 
  filter(agency!="chief medical officer") %>% 
  filter(affil_name_scopus!="bureau of preventable diseases") %>% 
  filter(affil_name_scopus!="bureau of maternal") %>% 
  filter(affil_name_scopus!="bureau of food and drugs") %>% 
  filter(affil_name_scopus!="population reference bureau") %>% 
  filter(agency!="bureau of medical services") %>% 
  filter(agency!="chief administrative office") %>% 
filter(agency!="office of homeland security") %>% 
  filter(agency!="climate change research") %>% 
  filter(agency!="center for global health") %>%
  filter(agency!="human resources university") %>%
  filter(agency!="legal services corporation") %>%
  filter(agency!="agencies")
  
  affiliation_df_keep2<-affiliation_df %>% filter(affil_name_scopus=="u.s. army criminal investigation command"|
                                                    affil_name_scopus=="u.s. secret service"|
                                                    str_detect(affil_name_scopus,"air national guard")|
                                                    str_detect(affil_name_scopus,"west point military academy")|
                                                    str_detect(affil_name_scopus,"us military academy (west point)")|
                                                    str_detect(affil_name_scopus,"united stated military academy")|
                                                    str_detect(affil_name_scopus,"the u.s. military academy at west point")|
                                                    str_detect(affil_name_scopus,"keller army hospital military academy west point")|
                                                    str_detect(affil_name_scopus,"civil engineering department u.s. military academy west point"))
  
  
  affiliation_df_keep<-bind_rows(affiliation_df_keep2,affiliation_df_keep)
rm(affiliation_df_keep2)

  
  
  


affiliation_df_keep<-
affiliation_df_keep %>% rowwise() %>% 
  mutate(exactmatch=affil_name_scopus==agency,
         match=str_detect(affil_name_scopus,agency)) %>% 
  relocate(exactmatch,.before=1) %>%
  relocate(match,.after=1) %>% 
  arrange(desc(exactmatch),desc(match))


# foo<-affiliation_df_keep %>% 
#   select(agency) %>% 
#   distinct() 





                   
affiliation_df_NA_country<-anti_join(affiliation_df,affiliation_df_keep) %>%
  filter(is.na(country)) %>% 
filter(!str_detect(affil_name_scopus,"netherlands")) %>% 
  filter(!str_detect(affil_name_scopus,"company tax division")) %>% 
filter(!str_detect(affil_name_scopus,"alliance for coastal technologies")) %>% 
filter(!str_detect(affil_name_scopus,"texas transportation commission")) %>% 
filter(!str_detect(affil_name_scopus,"civ. eng. and mar. constr. division")) %>% 
filter(!str_detect(affil_name_scopus,"east-west center")) %>% 
filter(!str_detect(affil_name_scopus,"executive secretariat of the council of ministers of health of central america and the dominican republic (se-comisca)")) %>% 
filter(!str_detect(affil_name_scopus,"global legal information network (glin) foundation")) %>% 
filter(!str_detect(affil_name_scopus,"senior research fellow in the center for the study")) %>% 
filter(!str_detect(affil_name_scopus,"a director of the cost estimates and tax analysis division at the national assembly budget office")) %>% 
filter(!str_detect(affil_name_scopus,"apa office of ethics")) %>% 
filter(!str_detect(affil_name_scopus,"massachusetts")) %>%
  filter(!str_detect(affil_name_scopus,"planning staff")) %>% 
  filter(!str_detect(affil_name_scopus,"agricultural credit and policy council")) %>% 
filter(agency!="policy planning staff") %>% 
filter(agency!="privacy office") %>% 
filter(agency!="private sector office") %>% 
filter(agency!="public company accounting oversight board") %>% 
filter(agency!="risk management service") %>% 
filter(agency!="vera institute of justice")
       




affiliation_df_keep<-bind_rows(affiliation_df_NA_country,affiliation_df_keep)
affiliation_df_keep <-affiliation_df_keep %>% 
  select(-affil_name,
         -exactmatch,
         -match,
         -parent_affil_id) %>% 
  rename(scopus_affil_id=affil_id)
         


affiliation_df_NOT_usa<-anti_join(affiliation_df,affiliation_df_keep) %>% 
  filter(!is.na(country))

# This was the original one based on search by names
fed_affiliations_list<-read_rds("./data_clean/fed_affiliations_list.rds") %>% 
  tibble() %>% 
  rename(scopus_affil_id=affil_id) %>% 
  mutate(source="scopus_term_search")


# TRhis is the one based on searching scopus after crafting a list from the wikliupedia site
affiliation_df_keep<-affiliation_df_keep %>% 
  mutate(country=if_else(country=="united states","usa",country)) %>% 
  rename(agency_full=agency) %>% 
  mutate(source="wp_scopus") %>% 
  mutate(affil_name_scopus=gsub("u. s. ","us ", affil_name_scopus)) %>% 
  mutate(affil_name_scopus=gsub("u.s. ","us ", affil_name_scopus)) %>% 
  mutate(affil_name_scopus=gsub("united states ","us ", affil_name_scopus)) %>% 
  mutate(affil_name_scopus=gsub("[.]","", affil_name_scopus))

names(fed_affiliations_list)
names(affiliation_df_keep)

# These are only ones need o search

left_to_search<-anti_join(affiliation_df_keep,fed_affiliations_list,by="scopus_affil_id") %>% 
  distinct(scopus_affil_id) %>% 
  mutate(scopus_affil_id=paste("\'",scopus_affil_id,"\'",",",sep=''))

write_delim(left_to_search,"./data_raw/left_to_search.txt")



complete_affil_list<-full_join(fed_affiliations_list,affiliation_df_keep,by="scopus_affil_id") %>% 
  mutate(country.y=if_else(country.x==country.y,NA,country.y)) %>% 
  mutate(affil_name_scopus=if_else(affil_name_scopus==affiliation,NA,affil_name_scopus)) %>% 
  select(-country.y) %>% 
  rename(country=country.x)



fill_missing<-need_affil_id %>% select(cat1, cat2,agency_full=agency, acronym)
complete_affil_list<-left_join(complete_affil_list,fill_missing)





complete_affil_list<-complete_affil_list %>% 
  mutate(agency_full=gsub("united states", "us",agency_full)) %>% 
  mutate(agency_full=gsub("[.]", "",agency_full)) %>% 
  mutate(agency_full=gsub("inter-american", "interamerican",agency_full)) %>% 
  # mutate(agency_full=gsub("&nbsp;", "",agency_full)) %>% 
  # mutate(agency_full=gsub("&nbsp;<span class="searchMatch">", " ",agency_full)) %>% 
  filter(agency_full!="vietnam education foundation") %>% 
  filter(agency_full!="northwest power planning council") %>% 
  filter(agency_full!="north american electric reliability corporation") %>% 
  filter(agency_full!="northwest power and conservation council") %>% 
  filter(agency_full!="ecosystems research")  %>% 
  filter(scopus_affil_id!=115199897)  %>% 
  filter(scopus_affil_id!=115199897)  %>% 
  filter(scopus_affil_id!=105311009)  %>% 
  filter(scopus_affil_id!=60003954)  %>% 
  filter(scopus_affil_id!=100346728)  %>% 
  
filter(scopus_affil_id!=116673115)  %>% 
filter(scopus_affil_id!=60002753)  %>% 
filter(scopus_affil_id!=116673115)  %>% 
filter(scopus_affil_id!=112792726)  %>% 
filter(scopus_affil_id!=60010695)  %>% 
filter(scopus_affil_id!=127814663)  %>% 
filter(scopus_affil_id!=114377074)  %>% 
filter(scopus_affil_id!=119098994)  %>% 
filter(scopus_affil_id!=123712881)  %>% 
filter(scopus_affil_id!=127814663)  %>% 
filter(scopus_affil_id!=110945231)  %>% 
filter(scopus_affil_id!=100354036)  %>% 
filter(scopus_affil_id!=60001559)  %>%
  filter(scopus_affil_id!=100401363) %>% 
  filter(scopus_affil_id!=100366327)  %>% 
  mutate(agency_full=
           case_when(
             scopus_affil_id == 100505945 ~ "USFWS",
             scopus_affil_id == 100335975 ~ "USFWS",
             scopus_affil_id == 102024109 ~ "USFWS",
             scopus_affil_id == 100842382 ~ "USFWS",
             scopus_affil_id == 60023281 ~ "USFWS Sea Lamprey Management Program",
             scopus_affil_id == 60071536 ~ "USDA Forest Service Palmer",
             scopus_affil_id == 60074837 ~ "USFWS Columbia Ecological Services Office",
             scopus_affil_id == 109179491 ~ "Veterans Administration",
             scopus_affil_id == 112569590 ~ "Veterans Administration",
             scopus_affil_id == 107563119 ~ "Veterans Administration",
             scopus_affil_id == 124744472 ~ "Veterans Administration",
             scopus_affil_id == 100526739 ~ "NASA",
             scopus_affil_id == 109937768 ~ "NOAA",
             .default = as.character(agency_full)
           )
  ) %>% 
mutate(cat1=
         case_when(
           cat1=="independent" & agency_full == "engineer research and development center" ~ "dod",
           agency == "epa" ~ "epa",
           cat1=="independent" & agency_full == "medicaid and chip payment and access commission" ~ "congress",
           cat1=="independent" & str_detect(agency_full,"naval") ~ "dod",
           cat1=="independent" & str_detect(agency_full,"army") ~ "dod",
           cat1=="independent" & agency_full == "advanced research projects agency for health" ~ "hhs",
           cat1=="independent" & agency_full == "engineer research and development center" ~ "dod",
           cat1=="independent" & agency_full == "medicare payment advisory commission" ~ "congress",
           cat1=="independent" & agency_full == "university corporation for atmospheric research" ~ "nsf",
           str_detect(agency_full,"coast guard") ~ "dhs",
           str_detect(agency_full,"agency_full office of the chief signal officer") ~ "dod",
           str_detect(agency_full,"office of national drug control policy") ~ "eop",
           
           
           cat1=="independent" & str_detect(agency_full,"harry s. truman scholarship foundation") ~ "congress",
           .default = as.character(cat1)
       )
) %>%
  mutate(agency_full=
           case_when(
             scopus_affil_id== 60011972 ~ "Air Force Research Laboratory Information Directorate",
             scopus_affil_id== 60011390 ~ "Air Force Office of Scientific Research",
             scopus_affil_id== 60021080 ~ "Naval Air Warfare Center Weapons Division",
             scopus_affil_id== 60019228 ~ "Air Force Research Laboratory",
             scopus_affil_id== 60073971. ~ "Air Force Cambridge Research Laboratories",
             scopus_affil_id== 60021630 ~ "Joint Base Langley-Eustis",
             scopus_affil_id== 60001247 ~ "Hanscom AFB",
             scopus_affil_id== 60029473 ~ "Tyndall Air Force Base",
             scopus_affil_id== 60021888 ~ "NOAA Air Resources Laboratory - Field Research Division",
             scopus_affil_id== 60076904 ~ "Air Force Materiel Command",
             .default = as.character(agency_full)
           )
  ) %>%   
  mutate(agency=
           case_when(
             scopus_affil_id== 60011972 ~ "dod",
             scopus_affil_id== 60011390 ~ "dod",
             scopus_affil_id== 60021080 ~ "dod",
             scopus_affil_id== 60019228 ~ "dod",
             scopus_affil_id== 60073971. ~ "dod",
             scopus_affil_id== 60021630 ~ "dod",
             scopus_affil_id== 60001247 ~ "dod",
             scopus_affil_id== 60029473 ~ "dod",
             scopus_affil_id== 60021888 ~ "noaa",
             scopus_affil_id== 100378779 ~ "judiciary",
      
             scopus_affil_id== 60076904 ~ "dod",
             str_detect(agency_full,"us forest service")~"usda",
             str_detect(agency_full,"army research lab")~"dod",
             
             str_detect(agency_full,"international institute of tropical forestry")~"usda",
             str_detect(agency_full,"government publishing office congress")~"congress",
             
             str_detect(agency_full,"national geodetic survey")~"noaa",
             str_detect(agency_full,"national marine fisheries service")~"noaa",
             str_detect(agency_full,"government publishing office congress")~"congress",
             str_detect(agency_full,"us postal service")~"us postal service",
             str_detect(agency_full,"national heart, lung, and blood institute")~"nih",
            str_detect(agency_full,"national heart, lung, and blood institute")~"nih",
            str_detect(agency_full,"national institute of arthritis and musculoskeletal and skin diseases")~"nih",
            str_detect(agency_full,"national institute of general medical sciences")~"nih",
            str_detect(agency_full,"national institute on deafness and other")~"nih",
             str_detect(agency_full,"rocky mountain laboratories")~"nih",
             str_detect(agency_full,"university corporation for atmospheric research")~"nsf",
             str_detect(agency_full,"us army futures command")~"dod",
             str_detect(agency_full,"us environmental protection agency")~"epa",
             str_detect(agency_full,"us secret service")~"dhs",
             str_detect(agency_full,"us environmental protection agency")~"epa",
             str_detect(agency_full,"consumer product safety commission")~"cpsc",
            str_detect(agency_full,"bureau of indian affairs")~"interior",
            str_detect(agency_full,"general services administration")~"gsa",
            str_detect(agency_full,"federal communications commission")~"fcc",
            str_detect(agency_full,"securities and exchange commission")~"sec",
            str_detect(agency_full,"institute of museum and library services")~"imls",
            str_detect(agency_full,"consumer financial protection bureau")~"cfpb",
            str_detect(agency_full,"frederick national laboratory for cancer research")~"nih",
            str_detect(agency_full,"epidemic intelligence service")~"cdc",
            str_detect(agency_full,"environmental protection agency")~"epa",
            str_detect(agency_full,"us marine corps warfighting laboratory")~"dod",
            str_detect(agency_full,"usps office of the inspector general")~"us postal service",
            
             
             .default = as.character(agency)
           )
  ) %>%   
  mutate(agency=
           case_when(
             str_detect(agency_full,"government accountability office") ~ "gao",
             str_detect(agency_full,"coast guard") ~ "dhs",
             .default = as.character(agency)
           )
  ) %>%  
  mutate(affiliation=if_else(is.na(affiliation),agency_full,affiliation)) %>% 
  mutate(affiliation=if_else(is.na(agency),cat1,agency)) %>% 
  mutate(agency=if_else(is.na(agency),cat1,agency)) %>% 
  mutate(agency=if_else(agency=="independent",agency_full,agency)) %>% 
  mutate(agency=if_else(agency=="other",agency_full,agency)) %>% 
  mutate(agency=if_else(agency=="other_and_quasi",agency_full,agency)) %>% 
  mutate(agency=if_else(agency=="doi","interior",agency))


names(complete_affil_list)
dup<-complete_affil_list %>% group_by(agency_full) %>% summarize(n=n_distinct(agency)) %>% arrange(desc(n)) %>% filter(n>1)






complete_affil_list<-complete_affil_list %>% select(-affiliation,-affil_name_scopus,-cat1,-cat2)

# save complete_affil_list -----------------------------------------------





write_csv(complete_affil_list,"./data_clean/complete_affil_list.csv")

complete_affil_list %>% 
  group_by(scopus_affil_id) %>% 
  tally() %>% 
  filter(n==1) %>% 
  arrange(desc(n))


unique_affils<-complete_affil_list %>% 
  distinct(scopus_affil_id) %>% 
  mutate(scopus_affil_id=paste("\'",scopus_affil_id,"\'",",",sep=''))

write_delim(unique_affils,"./data_clean/unique_affils.txt")

oiginal_search<-anti_join(unique_affils,left_to_search,)
write_delim(oiginal_search,"./data_clean/oiginal_search.txt")

# DID NOT INCLUDE:
# University_Affiliated_Research_Center
# https://en.wikipedia.org/wiki/University_Affiliated_Research_Center