

# rscopus package and vignettes -------------------------------------------
# install.packages("devtools")
# devtools::install_github("muschellij2/rscopus")

# vignette: https://johnmuschelli.com/rscopus/

# SCOPUS INFO
# https://dev.elsevier.com/academic_research_scopus.html
# API LIMITS: https://dev.elsevier.com/api_key_settings.html
# Phrases/Operators: https://dev.elsevier.com/sc_search_tips.html

# Scopus subject classificationc codes/Subject Terms: 
#   https://service.elsevier.com/app/answers/detail/a_id/15181/supporthub/scopus/

  

# load libraries ----------------------------------------------------------

library(rscopus)
library(tidyverse)
rscopus::set_api_key("38c1ea28aed25f40f11034d20557ccde")



# set source --------------------------------------------------------------

source<-c("biotropica")
# set date range ----------------------------------------------------------

date_range<-seq(2008,2025)

"SRCTITLE(revista AND de AND biologia AND tropical) AND 
PUBYEAR('1968-2022') AND 
DOCTYPE(ar) OR DOCTYPE(no) OR DOCTYPE(re) OR DOCTYPE(dp)"

# Need to make sure don't pull in eg jpl - china
# AFFILORG() AND AFFILCOUNTRY( united AND states* ) 

# AFFIL ( nih ) AND PUBYEAR = 2024 AND ( LIMIT-TO ( DOCTYPE , "ar" ) 
# OR LIMIT-TO ( DOCTYPE , "re" ) OR LIMIT-TO ( DOCTYPE , "ch" ) 
# OR LIMIT-TO ( DOCTYPE , "no" ) OR LIMIT-TO ( DOCTYPE , "ed" ) 
# OR LIMIT-TO ( DOCTYPE , "le" ) OR LIMIT-TO ( DOCTYPE , "dp" ) )


# query_string<-"AFFILCOUNTRY(united AND states* ) AND (DOCTYPE(ar) OR DOCTYPE(re) 
# OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no)) AND 
# (PUBYEAR = 2023) AND (AFFILORG(NIH))"
# api_key <- "eef216ea695006c2039f8da2a1c27c09"
# s = generic_elsevier_api(query = query_string,
#                          type = "search", 
#                          search_type = "scopus",
#                          api_key = "38c1ea28aed25f40f11034d20557ccde",
#                          verbose = FALSE)
# s$content$`search-results`[[1]]

# https://aurelien-goutsmedt.com/post/extracting-biblio-data-1/
query_string<-"AFFILCOUNTRY(united AND states* ) AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no)) AND (PUBYEAR = 2019) AND (AFFILORG(NIH))"
scopus_data <- rscopus::scopus_search(query_string, 
                                      max_count=5000,
                                     view = "COMPLETE",
                                     api_key = "eef216ea695006c2039f8da2a1c27c09")
scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
# saveRDS(scopus_data_raw, file = "./data_clean/rbt/rbt_scopus.rds")
papers <- scopus_data_raw$df
write_csv(papers, "./data_raw/nih_scopus_papers_2019.csv")
affiliations <- scopus_data_raw$affiliation 
names(scopus_data_raw)
write_csv(affiliations, "./data_raw/nih_scopus_affiliations_2019.csv")
authors <- scopus_data_raw$author
write_csv(authors, "./data_raw/nih_scopus_authors_2019.csv")



# query N pubs per year from scopus with api ------------------------------
# searches for Articles, Data papers, Notes, and Reviews


# set up a dataframe to store data
pub_count<- data.frame(country=NA,
                       year=NA,
                       count=NA)

# Set up the function to query sequence of years for each country
geo<-seq_along(countries)
year<-seq_along(date_range)

for(i in geo) {
  for(j in year) {
    a<-"AFFILCOUNTRY("
    b<-") AND PUBYEAR = "
    c<-" AND DOCTYPE(ar) OR DOCTYPE(no) OR DOCTYPE(re) OR DOCTYPE(dp)"
    
    query_string<-paste(a, countries[i], b, date_range[j], c, sep="")
    s = generic_elsevier_api(query = query_string,
                             type = "search", search_type = "scopus",
                             api_key = "38c1ea28aed25f40f11034d20557ccde",
                             verbose = FALSE)
    api_return<- data.frame(country=NA,
                            year=NA,
                            count=NA)
    
    api_return$country<-countries[i]
    api_return$year<-date_range[j]
    api_return$count<-s$content$`search-results`[[1]]
    # names(uni_results_match)==names(api_return)
    pub_count<-bind_rows(pub_count,api_return)
    
  }
}
head(pub_count)
str(pub_count)
pub_count<-drop_na(pub_count)

# years<-paste(min(date_range),max(date_range),sep="_")
# save the results --------------------------------------------------------

# appends the years surveyed to the filename
write_csv(pub_count,"./data_raw/total_pubs_all_fields.csv")




# query_string = "AFFILCOUNTRY(argentina) AND 
# PUBYEAR > 2014 AND 
# PUBYEAR < 2020 AND 
# DOCTYPE(ar) OR 
# DOCTYPE(no) OR 
# DOCTYPE(re) OR 
# DOCTYPE(dp) AND 
# SUBJTERMS(1105)"

# code(AGRI)
# subject-classification code SUBJTERMS()
# 1105	Ecology, Evolution, Behavior and Systematics
# 2303	Ecology
# 2309	Nature and Landscape Conservation
# 1107	Forestry

# ecology journals in both AGRI and ENVI

# ISSN(00129658) = Ecology

# EXACTSRCTITLE ' any journal with Ecology in title'
# Biotropica: Agricultural and Biological Sciences: Ecology, Evolution, Behavior and Systematics
# J App Ecol: Environmental Science: Ecology
# Use affiliation query

s = generic_elsevier_api(query = query_string,
                      type = "search", search_type = "scopus",
                         api_key = "38c1ea28aed25f40f11034d20557ccde",
                         verbose = FALSE)

s$content$`search-results`[1]

names(s$content$`search-results`)
names(s$content$`search-results`$entry)

# s$content$`search-results`$entry[[26]]$`prism:publicationName`
# unique(s$content$`search-results`$entry)
# s$content$`search-results`[1]
# s$content$`search-results`$entry[[12]]$`prism:publicationName`
# 






journals <- 
c('Ecology Letters',
'Annual Review of Ecology, Evolution, and Systematics',
'Global Change Biology',
'Frontiers in Ecology and the Environment',
'Ecological Monographs',
'Global Ecology and Biogeography',
'Journal of Ecology',
'Ecology',
'Journal of Applied Ecology',
'Conservation Biology',
'The American Naturalist',
'Functional Ecology',
'Journal of Animal Ecology',
'Ecography',
'Ecological Applications',
'Journal of Biogeography',
'Diversity and Distributions',
'Ecosystems',
'Oecologia',
'Biological Conservation',
'Biological Invasions',
'Oikos',
'Ecology and Society',
'Microbial Ecology',
'Landscape Ecology',
'Behavioral Ecology',
'Animal Conservation',
'Behavioral Ecology and Sociobiology',
'Marine Ecology Progress Series',
'Journal of Vegetation Science',
'Ecological Modelling',
'Evolutionary Ecology',
'Basic and Applied Ecology',
'Oryx',
'Biotropica',
'Biodiversity and Conservation',
'Forest Ecology and Management',
'Aquatic Conservation',
'Restoration Ecology',
'Plant Ecology',
'Ecological Entomology',
'Austral Ecology',
'Applied Vegetation Science',
'Journal of Zoology',
'Journal of Insect Conservation',
'Ecoscience',
'Environmental Entomology',
'Acta Oecologica',
'Ecology of Freshwater Fish',
'Aquatic Ecology',
'Entomologia Experimentalis et Applicata',
'Journal of Tropical Ecology',
'Ecological Research',
'Marine Ecology',
'European Journal of Wildlife Research'
) 

countries<-c(
'Argentina',
'Bolivia',
'Brazil',
'Chile',
'Colombia', 
'Costa Rica',
'Cuba',
'Dominican Republic',
'Ecuador',
'El Salvador',
'Guatemala',
'Honduras',
'Mexico',
'Nicaragua',
'Panama',
'Paraguay',
'Peru',
'Uruguay',
'Venezuela'
)

date_range<-seq(1991,2021)


pub_count<- data.frame(country=NA,
                       year=NA,
                       count=NA)

geo<-seq_along(countries)
year<-seq_along(date_range)
# i=2
# j=3
for(i in geo) {
  for(j in year) {
a<-"AFFILCOUNTRY("
b<-") AND PUBYEAR = "
c<-" AND DOCTYPE(ar) OR DOCTYPE(no) OR DOCTYPE(re) OR DOCTYPE(dp)"

query_string<-paste(a, countries[i], b, date_range[j], c, sep="")
s = generic_elsevier_api(query = query_string,
                         type = "search", search_type = "scopus",
                         api_key = "38c1ea28aed25f40f11034d20557ccde",
                         verbose = FALSE)
api_return<- data.frame(country=NA,
                       year=NA,
                       count=NA)

api_return$country<-countries[i]
api_return$year<-date_range[j]
api_return$count<-s$content$`search-results`[1]
    # names(uni_results_match)==names(api_return)
pub_count<-bind_rows(pub_count,api_return)

  }
}
pub_count
write_csv(pub_count,"all_fields_of_sci.csv")
