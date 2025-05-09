# load packages -----------------------------------------------------------

library(tidyverse)
library(refsplitr)
library(janitor)


# load and clean affils ---------------------------------------------------

## load affils

raw_affils <- read_csv("./data_raw/fed_affils.csv")

affils_wos <- raw_affils %>%
  distinct(wos_affils) %>%
  drop_na() %>%
  rename(affils = wos_affils) %>%
  mutate(database = "wos") %>%
  mutate(affils = tolower(affils))

affils_scopus <- raw_affils %>%
  distinct(scopus_affils) %>%
  drop_na() %>%
  rename(affils = scopus_affils) %>%
  mutate(database = "scopus") %>%
  mutate(affils = tolower(affils))


eb_agencies <- data.frame(affils = c(
  "afit", "nci", "ncbi", "nei", "nhlbi", "nhgri",
  "ahrq", "ars", "agricultral research service",
  "niosh", "niaid", "niams", "nibib", "nichd", "nidcr",
  "niddk", "niehs", "nimh", "nia", "niaaa", "nidcd",
  "nida", "nlm", "usaf", "cber", "cbo", "dha", "fbi",
  "ncar", "nrao", "nsf", "erdc", "hrsa", "fda",
  "vha", "wrair", "stri",
  "air force research lab", "argonne national laboratory",
  "brookhaven national laboratory", "carl r darnall army medical center",
  "center for nanophase materials sciences", "congressional budget office",
  "defense health agcy", "department of veterans affairs",
  "division of intramural research (niaid)", "faa",
  "federal bureau of investigation", "federal housing finance agency",
  "federal reserve system", "frederick national laboratory for cancer research",
  "idaho national laboratory", "lawrence berkeley national laboratory",
  "lawrence livermore national laboratory", "los alamos national laboratory",
  "madigan army medical center", "national defense university",
  "national energy technology laboratory", "national institute of standards technology",
  "national renewable energy laboratory", "national solar observatory",
  "national institute of environmental health science", "naval medical center san diego",
  "naval medical research center ", "nmrc",
  "naval postgraduate school", "oak ridge national laboratory",
  "pacific northwest national laboratory", "roswell park comprehensive cancer center",
  "san antonio military medical center", "sandia national laboratories",
  "u s army corps of engineers", "uniformed services university ",
  "uniformed services university of the health sciences", "united states air force",
  "united states air force academy", "united states army",
  "united states environmental protection agency", "united states forest service",
  "united states geological survey", "united states military academy",
  "united states naval academy", "united states naval research laboratory",
  "united states navy", "united states public health service",
  "us air force research laboratory", "us army",
  "us bureau of labor statistics", "us census bureau",
  "us department of housing urban development", "us fish and wildlife service",
  "us national park service", "us naval observatory",
  "us naval research lab", "us navy", "national institute of biomedical imaging and bioengineering",
  "va caribbean healthcare system", "walter reed national military medical center"
))

eb_agencies <- eb_agencies %>% mutate(
  database = "scopus_eb",
  agency = affils
)

affils_all <- bind_rows(affils_scopus, affils_wos, eb_agencies) %>%
  mutate(agency = if_else(str_detect(affils, "doe"), "doe", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "noaa"), "noaa", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "usda"), "usda", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nih"), "nih", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "usgs"), "usgs", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nasa"), "nasa", agency)) %>%
  mutate(agency = if_else(str_detect(affils, " epa"), "epa", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "noaa"), "noaa", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "smithsonian"), "smithsonian", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "usda ars"), "usda", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "hhs"), "hhs", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "usfs"), "usfs", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "afit"), "afit", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nci"), "nci", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "ncbi"), "ncbi", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nei"), "nei", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nhlbi"), "nhlbi", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nhgri"), "nhgri", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "ahrq"), "ahrq", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "niosh"), "niosh", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "niaid"), "niaid", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "niams"), "niams", agency)) %>%
  mutate(agency = if_else(str_detect(affils, " nibib"), "nibib", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nichd"), "nichd", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nidcr"), "nidcr", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "niddk"), "niddk", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "niehs"), "niehs", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nimh"), "nimh", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nia"), "nia", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "niaaa"), "niaaa", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nidcd"), "nidcd", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nida"), "nida", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nlm"), "nlm", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "usaf"), "usaf", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "cber"), "cber", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "cbo"), "cbo", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "fbi"), "fbi", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "ncar"), "ncar", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nrao"), "nrao", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nsf"), "nsf", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "erdc"), "erdc", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "usaf"), "usaf", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "hrsa"), "hrsa", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "fda"), "fda", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "vha"), "vha", agency)) %>%
  mutate(agency = if_else(str_detect(affils, " wrair"), "wrair", agency)) %>%
  mutate(agency = if_else(str_detect(affils, "nibib"), "nibib", agency)) %>%
  mutate(agency = if_else(database != "wos" & is.na(agency), affils, agency))



# Delete non-agency

affils_all <- affils_all %>%
  filter(affils != "nc state university") %>%
  filter(affils != "institut pierre-simon laplace")





write_csv(affils_wos, "./data_clean/fed_affils_wos.csv")
write_csv(affils_scopus, "./data_clean/fed_affils_scopus.csv")
write_csv(affils_all, "./data_clean/fed_affils_all.csv")


scopus_search <- affils_all %>%
  filter(database != "wos") %>%
  distinct(agency) %>%
  arrange(agency)

write_csv(scopus_search, "./data_clean/scopus_search_affils.csv")



#
# search_pattern_wos <- affils_wos %>%
#   pull(wos_affils) %>%
#   str_c(collapse = " OR ")


search_pattern_scopus <- scopus_search %>%
  pull(agency) %>%
  str_c(collapse = ")) OR (AFFIL(")
search_pattern_scopus <- paste("(AFFIL(", search_pattern_scopus, "))", sep = "")

search_pattern_scopus1 <- scopus_search %>%
  slice_head(n = 40) %>%
  pull(agency) %>%
  str_c(collapse = ")) OR (AFFIL(")

search_pattern_scopus2a <- scopus_search %>%
  slice(41:60) %>%
  pull(agency) %>%
  str_c(collapse = ")) OR (AFFIL(")

search_pattern_scopus2b <- scopus_search %>%
  slice(61:80) %>%
  pull(agency) %>%
  str_c(collapse = ")) OR (AFFIL(")

search_pattern_scopus3 <- scopus_search %>%
  slice(81:123) %>%
  pull(agency) %>%
  str_c(collapse = ")) OR (AFFIL(")


search_pattern_scopus1 <- paste("(AFFIL(", search_pattern_scopus1, "))", sep = "")
search_pattern_scopus2a <- paste("(AFFIL(", search_pattern_scopus2a, "))", sep = "")
search_pattern_scopus2b <- paste("(AFFIL(", search_pattern_scopus2b, "))", sep = "")
search_pattern_scopus3 <- paste("(AFFIL(", search_pattern_scopus3, "))", sep = "")

# AND PY=(2007-2025)) AND ((DT==("MEETING ABSTRACT" OR "LETTER" OR "DATA PAPER"
# OR "EDITORIAL MATERIAL" OR "REVIEW" OR "ARTICLE"))
# AND (PY==("2007" OR "2008" OR "2009" OR "2010" OR "2011" OR "2012" OR "2013" OR "2014" OR "2015" OR "2016" OR "2017" OR "2018" OR "2019" OR "2020" OR "2021" OR "2022" OR "2023" OR "2024" OR "2025"))
# AND DOP="2023-01-01/2023-04-15")

# (OG=(AFIT Graduate School of Engineering and Management OR Air Force Institute of Technology OR Argonne National Laboratory OR Brookhaven National Laboratory OR Centers for Disease Control and Prevention OR DOE Agile BioFoundry OR DOE Bioenergy Research Centers OR DOE Center for Advanced Bioenergy and Bioproducts Innovation OR Fermi National Accelerator Laboratory OR Harvard-Smithsonian Center for Astrophysics OR Institut Pierre-Simon Laplace OR Jean Mayer USDA Human Nutrition Research Center on Aging OR Jet Propulsion Laboratory OR Lawrence Berkeley National Laboratory OR Lawrence Livermore National Laboratory OR Los Alamos National Laboratory OR MSU-DOE Plant Research Laboratory OR NASA Ames Research Center OR NASA Glenn Research Center OR NASA Goddard Space Flight Center OR NASA Johnson Space Center OR NASA Langley Research Center OR NASA Marshall Space Flight Center OR National Aeronautics and Space Administration OR National Cancer Institute (NCI) OR National Cancer Institute at Frederick OR National Center for Atmospheric Research OR National Center for Biotechnology Information (NCBI) OR National Center for Environmental Prediction OR National Eye Institute (NEI) OR National Heart, Lung, and Blood Institute (NHLBI) OR National Human Genome Research Institute (NHGRI) OR National Institute for Occupational Safety and Health OR National Institute of Allergy and Infectious Diseases (NIAID) OR National Institute of Arthritis and Musculoskeletal and Skin Diseases (NIAMS) OR National Institute of Biomedical Imaging and Bioengineering (NIBIB) OR National Institute of Child Health and Human Development (NICHD) OR National Institute of Dental and Craniofacial Research (NIDCR) OR National Institute of Diabetes and Digestive and Kidney Diseases (NIDDK) OR National Institute of Environmental Health Sciences (NIEHS) OR National Institute of Mental Health OR National Institute of Neurological Disorders and Stroke OR National Institute of Standards and Technology OR National Institute of Water and Atmospheric Research OR National Institute on Aging (NIA) OR National Institute on Alcohol Abuse and Alcoholism (NIAAA) OR National Institute on Deafness and Other Communication Disorders (NIDCD) OR National Institute on Drug Abuse (NIDA) OR National Institutes of Health (NIH) OR National Library of Medicine (NLM) OR National Oceanic and Atmospheric Administration OR NC State University OR NIAID Rocky Mountain Laboratories OR NIH Clinical Center (CC) OR NOAA Alaska Fisheries Science Center OR NOAA National Marine Fisheries Service OR NOAA National Marine Fisheries Service Northeast Fisheries Science Center OR NOAA National Marine Fisheries Service Northwest Regional Office OR NOAA National Marine Fisheries Service Southwest Regional Office OR NOAA National Severe Storms Laboratory OR NOAA Northwest Fisheries Science Center OR NOAA Pacific Islands Fisheries Science Center OR NOAA Pacific Marine Environmental Laboratory OR NOAA Southeast Fisheries Science Center OR NOAA Southwest Fisheries Science Center OR NOAA's Atlantic Oceanographic and Meteorological Laboratory OR NOAA's National Environmental Satellite, Data, and Information Service OR Oak Ridge National Laboratory OR Pacific Northwest National Laboratory OR Patuxent Wildlife Research Center OR SLAC National Accelerator Laboratory OR Smithsonian Institution OR Smithsonian National Museum of Natural History OR Stanford-USGS Ion Microprobe Laboratory OR U.S. Department of Energy Joint Genome Institute OR UCLA-DOE Institute for Genomics and Proteomics OR United States Department of Agriculture OR United States Department of Energy OR United States Environmental Protection Agency OR United States Geological Survey OR United States Geological Survey Central Region OR United States Geological Survey Western Region OR US EPA National Health and Environmental Effects Research Laboratory OR USDA Agricultural Research Service OR USDA Agricultural Research Service - Lincoln, Nebraska OR USDA Agricultural Research Service, Cereal Disease Laboratory OR USDA Agricultural Research Service, Gainesville OR USDA Agricultural Research Service, Stoneville OR USDA Animal and Plant Health Inspection Service (APHIS) OR USDA APHIS National Wildlife Research Center OR USDA ARS Appalachian Fruit Research Station OR USDA ARS Aquatic Animal Health Research Laboratory OR USDA ARS Avian Disease and Oncology Laboratory OR USDA ARS Beltsville Agricultural Research Center OR USDA ARS Beltsville Human Nutrition Research Center OR USDA ARS Beneficial Insects Introduction Research Unit OR USDA ARS Carl Hayden Bee Research Center OR USDA ARS Center for Grain and Animal Health Research OR USDA ARS Children's Nutrition Research Center OR USDA ARS Conservation and Production Research Laboratory OR USDA ARS Corvallis Forestry Sciences Laboratory OR USDA ARS Crop Science Research Laboratory OR USDA ARS Dale Bumpers Small Farms Research Center OR USDA ARS Eastern Regional Research Center OR USDA ARS Forage and Range Research Laboratory OR USDA ARS Grand Forks Human Nutrition Research Center OR USDA ARS Grazinglands Research Laboratory OR USDA ARS Horticultural Crops Research Unit OR USDA ARS Invasive Plant Research Laboratory OR USDA ARS Kika de la Garza Subtropical Agricultural Research Center OR USDA ARS Moscow Forestry Sciences Lab OR USDA ARS National Animal Disease Center OR USDA ARS National Center for Agricultural Utilization Research OR USDA ARS National Clonal Germplasm Repository OR USDA ARS National Laboratory for Agriculture and the Environment OR USDA ARS National Peanut Research Laboratory OR USDA ARS National Sedimentation Laboratory OR USDA ARS National Soil Erosion Research Laboratory OR USDA ARS Natural Products Utilization Research Unit OR USDA ARS North Carolina State University OR USDA ARS North Central Agricultural Research Laboratory OR USDA ARS Northwest Irrigation and Soils Research Laboratory OR USDA ARS Northwest Watershed Research Center OR USDA ARS Plum Island Animal Disease Center OR USDA ARS Poisonous Plant Research Laboratory OR USDA ARS Pullman OR USDA ARS Robert W. Holley Center for Agriculture and Health OR USDA ARS Rocky Mountain Research Station OR USDA ARS Roman L. Hruska U.S. Meat Animal Research Center OR USDA ARS Russell Research Center (RRC) OR USDA ARS Salinity Laboratory OR USDA ARS Soil Drainage Research Unit OR USDA ARS Southeast Watershed Research Laboratory OR USDA ARS Southern Plains Agricultural Research Center OR USDA ARS Southern Regional Research Center OR USDA ARS Stillwater OR USDA ARS U.S. Arid-Land Agricultural Research Center OR USDA ARS U.S. Dairy Forage Research Center OR USDA ARS U.S. Horticultural Research Laboratory OR USDA ARS U.S. Vegetable Laboratory OR USDA ARS US Water Conservation Lab OR USDA ARS Western Regional Research Center (WRRC) OR USDA Economic Research Service OR USDA Forest Products Laboratory OR USDA Forest Service OR USDA Forest Service Aldo Leopold Wilderness Research Institute OR USDA Forest Service Northeastern Research Station Morgantown OR USDA Forest Service Pacific Northwest Research Station OR USDA Forest Service Pacific Southwest Research Station OR USDA Forest Service Rocky Mountain Research Station OR USDA Natural Resources Conservation Service OR USDA Plant Gene Expression Center OR USGS Earth Resources Observation and Science Center OR USGS Forest and Rangeland Ecosystems Science Center OR USGS National Map Liaison OR USGS National Wildlife Health Center OR USGS Wyoming-Montana Water Science Center)
# AND (DT==("MEETING ABSTRACT" OR "LETTER" OR "DATA PAPER" OR "EDITORIAL MATERIAL" OR "REVIEW" OR "ARTICLE"))
# AND PY=(2007-2025))
# AND DOP="2023-01-01/2023-04-15"))
# AND DOP=("2008-11-01/2009-01-31"))
# AND DOP="2009-02-01/2009-04-30"))

# 2008 Pres
# AND DOP=("2008-11-01/2009-01-31"))
# AND DOP=("2009-02-01/2009-04-30"))

# 09-10
# AND DOP=("2009-11-01/2010-01-31"))
# AND DOP=("2010-02-01/2010-04-30"))

# 10-11
# AND DOP=("2010-11-01/2011-01-31"))
# AND DOP=("2011-02-01/2011-04-30"))

# 11-12
# AND DOP=("2011-11-01/2012-01-31"))
# AND DOP=("2012-02-01/2012-04-30"))


# 2012 Pres
# AND DOP=("2012-11-01/2013-01-31"))
# AND DOP=("2013-02-01/2013-04-30"))

# 13-14
# AND DOP=("2013-11-01/2014-01-31"))
# AND DOP=("2014-02-01/2014-04-30"))

# 14-15
# AND DOP=("2014-11-01/2015-01-31"))
# AND DOP=("2015-02-01/2015-04-30"))

# 15-16
# AND DOP=("2015-11-01/2016-01-31"))
# AND DOP=("2016-02-01/2016-04-30"))

# 2016 election
# AND DOP=("2016-11-01/2017-01-31"))
# AND DOP=("2017-02-01/2017-04-30"))

# 17-18
# AND DOP=("2017-11-01/2018-01-31"))
# AND DOP=("2018-02-01/2018-04-30"))

# 18-19
# AND DOP=("2018-11-01/2019-01-31"))
# AND DOP=("2019-02-01/2019-04-30"))

# 19-20
# AND DOP=("2019-11-09/2020-01-31"))
# AND DOP=("2020-02-01/2020-04-30"))

# 2020 election
# AND DOP=("2020-11-01/2021-01-31"))
# AND DOP=("2021-02-01/2021-04-30"))   = 6918


# 21-22
# AND DOP=("2021-11-01/2022-01-31"))
# AND DOP=("2022-02-01/2022-04-30"))

# 22-23
# AND DOP=("2022-11-01/2023-01-31"))
# AND DOP=("2023-02-01/2023-04-30"))

# 23-24
# AND DOP=("2023-11-09/2024-01-31"))
# AND DOP=("2024-02-01/2024-04-30"))


# 2024 election
# AND DOP=("2024-11-01/2025-01-31"))
# AND DOP=("2025-02-01/2025-04-30"))


transition_pubs <- read_csv("./data_raw/transition_pubs.csv") %>%
  mutate(
    change = post - pre,
    perc_diff = change / pre * 100,
    election = as.factor(election)
  )


transition_pubs %>%
  ggplot(aes(x = year, y = perc_diff, fill = election)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(y = "% difference\n (3 mo window pre- vs. post-inaug)") +
  scale_y_continuous(n.breaks = 10, limits = c(-40, 20)) +
  scale_fill_manual(values = c(
    "darkgreen",
    "gray"
  ))


transition_pubs %>%
  select(year, election, pre, post) %>%
  pivot_longer(cols = pre:post, values_to = "pubs", names_to = "period") %>%
  mutate(period = fct_relevel(
    period,
    "pre",
    "post"
  )) %>%
  ggplot(aes(period, pubs, colour = election, group = year)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(y = "No. of pubs (3 month window)") +
  scale_y_continuous(n.breaks = 15, limits = c(0, 8500))


transition_pubs %>%
  ggplot(aes(x = election, y = perc_diff)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(y = "% difference\n (3 mo window pre- vs. post-inaug)") +
  scale_y_continuous(n.breaks = 10, limits = c(-40, 0))


# )

# Look at months before and after presidential transitons

pub_counts <- read_csv("./data_raw/pubs_per_year.csv") %>%
  mutate(months = rep(12, nrow(pub_counts))) %>%
  mutate(months = if_else(yr == 2025, 4.5, months)) %>%
  mutate(pubs_per_month = pub_count / months)


pub_counts %>%
  filter(yr != 2025) %>%
  ggplot(aes(x = yr, y = pub_count)) +
  geom_line() +
  expand_limits(y = 0) +
  theme_classic()



# on april 16, 2025


pub_summary <-
  pub_summary %>%
  mutate(perc_of_previous_monthly = per_month / lag(per_month) * 100) %>%
  mutate(perc_change = perc_of_previous_monthly - 100) %>%
  mutate(perc_of_previous_4mo = pubs_jan_april / lag(pubs_jan_april) * 100) %>%
  mutate(perc_change_april = perc_of_previous_4mo - 100)








# NIH 2019-2025 -----------------------------------------------------------


# combined_data_papers$PM
#
# combined_data_papers$PM<-gsub(" ,","",combined_data_papers$PM)
# combined_data_papers$PM<-gsub("Februaryy","February",combined_data_papers$PM)
#
# combined_data_papers <-combined_data_papers %>%
#   separate(PM, c("PM","PM-remainder", remove=FALSE,extra="drop"))
#
# combined_data_papers <-combined_data_papers %>%
#   mutate(PM=
#   case_when(
#     PM == "Sep" ~ "September",
#     PM == "Mar" ~ "March",
#     PM == "Aug" ~ "August",
#     PM == "Jul" ~ "July",
#     PM == "Feb" ~ "February",
#     PM == "Winter" ~ "December",
#     PM == "Spring" ~ "March",
#     PM == "Summer" ~ "June",
#     PM == "Autumn" ~ "September",
#     PM == "" ~ NA,
#     .default = as.character(PM)
#   )
#   )
#
# combined_data_papers$PM<-ordered(combined_data_papers$PM,
#                                  levels = c("January",
#                                             "February",
#                                             "March",
#                                             "April",
#                                             "May",
#                                             "June",
#                                             "July",
#                                             "August",
#                                             "September",
#                                             "October",
#                                             "November",
#                                             "December"))
#
#
#

combined_data_papers <- all_scopus_api[[1]]
names(combined_data_papers)
combined_data_authors <- all_scopus_api[[2]]
names(combined_data_authors)

unique(as.factor(combined_data_papers$PM))

combined_data_papers %>% tally()

combined_data_papers %>%
  group_by(PY) %>%
  tally()

combined_data_papers <- combined_data_papers %>%
  separate_wider_delim(filename, delim = "_", names = c("agency"), too_many = "drop")

summary <-
  combined_data_papers %>%
  group_by(PY, PM) %>%
  tally()

all_trim <- combined_data_papers
all_trim_summary <-
  all_trim %>%
  # filter(PY>2018) %>%
  filter(PY > 2018) %>%
  group_by(PY, PM) %>%
  tally()

unique(as.factor(all_trim$agency))

library(gghighlight)


library(gghighlight)
all_trim_summary %>%
  # filter(PM>4) %>%
  filter(PY < 2025) %>%
  ggplot(aes(x = PM, y = n, group = PY, color = PY)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  theme_classic() +
  scale_x_continuous(n.breaks = 12, limits = c(0, 12)) +
  scale_y_continuous(n.breaks = 20, limits = c(0, 8000)) +
  # gghighlight(min(n) < 50)
  gghighlight(PY == 2024)


all_trim_summary %>%
  filter(PM < 5) %>%
  ggplot(aes(x = PM, y = n, group = PY, color = PY)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  theme_classic() +
  # scale_x_continuous(n.breaks = 5, limits = c(0, 4))+
  scale_y_continuous(n.breaks = 15, limits = c(0, 9000)) +
  # gghighlight(min(n) < 50)
  gghighlight(PY == 2025)



summary_all_yr_to_date <-
  all_trim %>%
  filter(PY > 2018) %>%
  mutate(PM = if_else(PY == 2025, 5, PM)) %>% ######## THIS IS KEY
  filter(PM < 6) %>%
  group_by(PY) %>%
  tally()
summary_all_yr_to_date

n_2024 <- summary_all_yr_to_date %>%
  filter(PY == 2024) %>%
  select(n)
n_2025 <- summary_all_yr_to_date %>%
  filter(PY == 2025) %>%
  select(n)
perc_2425 <- as.numeric((n_2025 - n_2024) / n_2024 * 100)

summary_all_yr_to_date %>%
  ggplot(aes(x = PY, y = n)) +
  geom_bar(stat = "identity") +
  expand_limits(y = 0) +
  theme_classic() +
  annotate(
    geom = "text", x = 2024, y = max(summary_all_yr_to_date$n) - 700, label = n_2024,
    color = "navyblue"
  ) +
  annotate(
    geom = "text", x = 2025, y = max(summary_all_yr_to_date$n) - 700, label = n_2025,
    color = "navyblue"
  ) +
  annotate(
    geom = "text", x = 2025, y = max(summary_all_yr_to_date$n) - 1600, label = paste("(", round(perc_2425, 2), "%)", sep = ""),
    color = "red"
  ) +
  scale_y_continuous(n.breaks = 20, limits = c(0, 20000)) +
  scale_x_continuous(breaks = seq(2019, 2025, by = 1)) +
  gghighlight(PY == 2025)



after_april_25 <-
  all_trim %>%
  filter(PM > 5) %>%
  filter(PY == 2025)


before_april_25 <-
  all_trim %>%
  filter(PM < 6) %>%
  filter(PY == 2025)

all_trim %>%
  filter(PY == 2025) %>%
  group_by(PM) %>%
  tally()



# agency specific ---------------------------------------------------------


combined_data_papers %>%
  filter(PY > 2023) %>%
  mutate(PM = if_else(PY == 2025, 5, PM)) %>%
  filter(PM < 6) %>%
  group_by(agency, PY) %>%
  tally() %>%
  # mutate(perc_of_previous_yr=n/lag(n)*100) %>%
  # mutate(perc_change=perc_of_previous_yr-100) %>%
  # drop_na() %>%
  # ggplot(aes(x=agency,y=perc_change)) +
  ggplot(aes(x = PY, y = n, group = agency)) +
  geom_point() +
  geom_line() +
  # ggplot(aes(x=PY,y=perc_change,group=agency)) +
  # geom_bar(stat="identity",position="dodge")+

  # geom_bar(stat="identity",position="dodge")+
  # facet_wrap(vars(agency))+
  # gghighlight(PY == 2025)






  combined_data_papers %>%
  # filter(PY>2018) %>%
  filter(PY > 2023) %>%
  # filter(PY>2018) %>%
  mutate(PM = if_else(PY == 2025, 5, PM)) %>%
  filter(PM < 6) %>%
  # filter(PY>2023) %>%
  group_by(agency, PY) %>%
  tally() %>%
  mutate(perc_of_previous_yr = n / lag(n) * 100) %>%
  mutate(perc_change = perc_of_previous_yr - 100) %>%
  drop_na() %>%
  ggplot(aes(x = agency, y = perc_change)) +
  # ggplot(aes(x=PY,y=perc_change,group=agency)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_bar(stat="identity",position="dodge")+
  # facet_wrap(vars(agency))+
  gghighlight(PY == 2025)



combined_data_papers %>%
  # filter(PY>2018) %>%
  filter(PY > 2022 & PY < 2025) %>%
  # filter(PY>2018) %>%
  # mutate(PM=if_else(PY==2025,5,PM)) %>%
  filter(PM < 6) %>%
  # filter(PY>2023) %>%
  group_by(agency, PY) %>%
  tally() %>%
  mutate(perc_of_previous_yr = n / lag(n) * 100) %>%
  mutate(perc_change = perc_of_previous_yr - 100) %>%
  drop_na() %>%
  ggplot(aes(x = agency, y = perc_change)) +
  # ggplot(aes(x=PY,y=perc_change,group=agency)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_bar(stat="identity",position="dodge")+
  # facet_wrap(vars(agency))+
  gghighlight(PY == 2024)



ungroup() %>%
  # filter(PM>4) %>%
  # filter(PY<2025) %>%
  ggplot(aes(x = PY, y = n, group = agency, color = agency)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  theme_classic() +
  # facet_wrap(vars(agency))+
  gghighlight(PY == 2025)
# scale_x_continuous(n.breaks = 5, limits = c(5, 12))+
# scale_y_continuous(n.breaks = 20, limits = c(0, 2000))+
# gghighlight(min(n) < 50)




unique(fed_agencies$agency)


# adding egencies ---------------------------------------------------------












fed_papers <- read_rds("./data_clean/scopus_papers.rds")
fed_papers <- fed_papers %>%
  separate_wider_delim(filename,
    delim = "_",
    names = c("agency"),
    too_many = "drop"
  )

details <- fed_papers %>% select(PY, refID)
fed_authors <- read_rds("./data_clean/scopus_authors.rds") %>%
  left_join(details, by = "refID")

rm(details)


fed_authors_affils <- fed_authors %>%
  select(affiliation, city, country) %>%
  unique() %>%
  mutate_all(tolower)

head(fed_authors_affils)

search_affils <- data.frame(affiliation = c("department of veterans affairs", "epa"))

inner_join(search_affils, fed_authors_affils, by = "affiliation")
