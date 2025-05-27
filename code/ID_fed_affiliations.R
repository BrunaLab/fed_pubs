ID_fed_affiliations <- function(affils_df) {

  
  
  # standardize COUNTRIES -----------------------------------------------
  levels(as.factor(affils_df$country))
  
  affils_df$country[affils_df$country == "united states"] <- "usa"
  affils_df$country[affils_df$country == "virgin islands (u.s.)"] <- "us virgin islands"
  affils_df$country[affils_df$country == "viet nam"] <- "vietnam"
  affils_df$country[affils_df$country == "cote d'ivoire"] <- "ivory coast"
  
  
  
  # 2x all operations center
  # cava de' tirreni aou s giovanni di dio e ruggiero d'arago
  # u s president’s malaria initiative evolve project nigeria
  
    affil_list <- read_csv("./data_clean/complete_affil_list.csv") %>%
    rename(affil_id = scopus_affil_id) %>%
    select(-source.x, -source.y) %>%
    mutate(federal = TRUE) %>%
    # select(-agency_full) %>%
    distinct(affil_id, agency, city, agency_full, .keep_all = TRUE) %>%
    select(-acronym)
  
  affils_df$affil_id <- as.numeric(affils_df$affil_id)
  
  fed_affils_scopus <- read_csv("./data_clean/results_datasetv1/fed_affils_scopus.csv") %>%
    select(
      affil_id = scopus_affil_id,
      agency_full = affiliation,
      agency,
      country
    )
  
  
  all_fed_affils <- full_join(fed_affils_scopus, affil_list, by = "affil_id") %>%
    mutate_all(tolower) %>%
    relocate(agency.y, .after = "agency.x") %>%
    relocate(agency_full.y, .after = "agency_full.x") %>%
    mutate(federal = TRUE) %>%
    group_by(affil_id, agency_full.x, agency.y, agency_full.y, agency.x) %>%
    distinct() %>%
    mutate(
      agency.x =
        case_when(
          agency.x == "other" ~ agency_full.x,
          agency.x == "other_and_quasi" ~ agency.y,
          agency.y == "gao" ~ agency.y,
          agency.y == "nih" ~ agency.y,
          agency.y == "noaa" ~ agency.y,
          agency.y == "judiciary" ~ agency.y,
          agency.y == "us postal service" ~ agency.y,
          agency.x == "doi" ~ "interior",
          TRUE ~ as.character(agency.x)
        )
    ) %>%
    mutate(agency_full.y = if_else(agency_full.y == agency_full.x, NA, agency_full.y)) %>%
    mutate(agency.y = if_else(agency.y == agency.x, NA, agency.y)) %>%
    mutate(affil_id = as.numeric(affil_id))
  
  
  affils_df <- left_join(affils_df, all_fed_affils, by = "affil_id") %>%
    select(
      -agency_full.y,
      -agency.y,
      -country.x,
      -country.y,
      -city.y
    ) %>%
    rename(
      city = city.x,
      agency = agency.x
    ) %>%
    mutate(affiliation = gsub("&amp;", "and", agency)) %>%
    mutate(affiliation = gsub("u s ", "us ", agency)) %>%
    mutate(affiliation = gsub("united states ", "us ", agency)) %>%
    mutate(affiliation = gsub("u.s. ", "us ", affiliation)) %>%
    mutate(affiliation = gsub("u. s. ", "us ", affiliation)) %>%
    mutate(affiliation = gsub("[.]", "", affiliation)) %>%
    mutate(agency_full.x = if_else(agency_full.x == affiliation, NA, agency_full.x)) %>%
    relocate(federal, .before = 1) %>%
    relocate(agency_full.x, .after = "affiliation") %>%
    relocate(agency, .after = "agency_full.x") %>%
    mutate(country = case_when(
      affil_id %in% c(116412197, 117758211, 130163250, 120790199, 117717065, 126483911) ~ "usa",
      TRUE ~ as.character(country)
    )) %>%
    mutate(
      agency =
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
          TRUE ~ as.character(agency)
        )
    ) %>%
    mutate(agency = case_when(
      str_detect(agency_full.x, "us public health service") ~ "usphs",
      str_detect(agency_full.x, "public health service commissioned") ~ "usphs",
      str_detect(agency_full.x, "office of trade") ~ "eop",
      str_detect(agency_full.x, "national ice center") ~ "dod",
      agency_full.x == "ecosystems research" ~ "interior",
      agency_full.x == "corporation for national and community service" ~ "americorps",
      agency_full.x == "sea grant" ~ "noaa",
      agency_full.x == "office of nuclear energy" ~ "doe",
      str_detect(agency_full.x, "us marine corps") ~ "dod",
      str_detect(agency_full.x, "us coast guard") ~ "dhs",
      str_detect(agency_full.x, "us army") ~ "dod",
      str_detect(agency_full.x, "university corporation for atmospheric research") ~ "nsf",
      TRUE ~ as.character(agency)
    )) %>%
    mutate(agency = if_else(country == "usa" & str_detect(affiliation, "brigade|group|combat|battalion|usmc|regiment|command"), "dod", agency)) %>%
    mutate(agency = if_else(country == "usa" & str_detect(affiliation, "defence|wing|squadron|division|cavalry| usn |field hospital|medical wing"), "dod", agency)) %>%
    mutate(agency = case_when(
      affiliation %in% c("inova center of outcomes research") ~ NA,
      TRUE ~ as.character(agency)
    )) %>%
    mutate(agency = case_when(
      affiliation %in% c("inova center of outcomes research") ~ NA,
      TRUE ~ as.character(agency)
    )) %>%
    mutate(agency = case_when(
      affil_id %in% c(
        60015482, 100896083, 132194144, 132193788, 123882638, 128754886,
        123214228, 128478576, 125203918, 127864124, 129282179, 126611873,
        127222996, 105477331, 121302036, 128186702, 122485294, 100536084
      ) ~ NA,
      TRUE ~ as.character(agency)
    )) %>%
    mutate(agency = if_else(str_detect(affiliation, "defense|navy|air force|marine corps|army|military|naval"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "brigade|group|combat|battalion|usmc|regiment|command"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "defence|wing|squadron|division|cavalry| usn |field hospital|medical wing"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "1st|4th"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "usfs"), "usda", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "wildlife"), "interior", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "dod-va|va "), "va", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "veteran"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "toxic substances and disease"), "cdc", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national institute"), "nih", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "federal reserve"), "federal reserve system", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "federal deposit insurance corporation"), "fdic", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national wetlands inventory"), "interior", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national risk management research laboratory"), "epa", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "federal transit administration"), "dot", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "federal trade commission"), "ftc", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national nuclear security site"), "doe", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national ecological observation network"), "nsf", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national nuclear security site livermore office"), "doe", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "callaghan federal hospital"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "callaghan federal medical"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "james a lovell federal health care center"), "va", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "safety and work life coast guard"), "dod", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "us president’s malaria initiative"), "usaid", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national weather center"), "noaa", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national institutes of child health and development"), "nih", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national institutes of arthritis"), "nih", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national institute on alcoholism and alcohol abuse"), "nih", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national risk management research laboratory"), "epa", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national weather center research experiences for undergraduates"), "noaa", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national tsunami warning center"), "noaa", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national institute of standards"), "nist", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national homeland security research center"), "epa", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national systematics laboratory of the national oceanic"), "noaa", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national transportation research center"), "dot", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national nuclear security site"), "doe", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national soil resource conservation service"), "usda", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "national historical park|historic site|historical site|national lakeshore"), "interior", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "us department of interior—international technical assistance program"), "interior", agency)) %>%
    mutate(agency = if_else(str_detect(affiliation, "federal highway administration"), "dot", agency)) %>%
    mutate(agency = if_else(affil_id == "108382367", "usda", agency)) %>%
    mutate(agency = if_else(affil_id == "128941071", "usda", agency)) %>%
    mutate(agency = if_else(affil_id == "125244852", "usda", agency)) %>%
    mutate(agency = if_else(affil_id == "122988512", "ahrq", agency)) %>%
    mutate(agency = if_else(affil_id == "100502481", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "100571977", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "100822854", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "115752362", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "121212875", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "122286572", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "129973323", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "130518547", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "130701994", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "130892705", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "132182075", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "60008492", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "100335911", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "101792179", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "106453365", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "109347134", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "112920600", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "112922559", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "120168333", "doe", agency)) %>%
    mutate(agency = if_else(affil_id == "123502999", "epa", agency)) %>%
    mutate(agency = if_else(affil_id == "100677110", "hhs", agency)) %>%
    mutate(agency = if_else(affil_id == "113820147", "hhs", agency)) %>%
    mutate(agency = if_else(affil_id == "125383427", "hhs", agency)) %>%
    mutate(agency = if_else(affil_id == "128533828", "hhs", agency)) %>%
    mutate(agency = if_else(affil_id == "101140478", "interior", agency)) %>%
    mutate(agency = if_else(affil_id == "128172361", "interior", agency)) %>%
    mutate(agency = if_else(affil_id == "130391142", "interior", agency)) %>%
    mutate(agency = if_else(affil_id == "123678203", "nasa", agency)) %>%
    mutate(agency = if_else(affil_id == "123865204", "nasa", agency)) %>%
    mutate(agency = if_else(affil_id == "107986893", "nasem", agency)) %>%
    mutate(agency = if_else(affil_id == "122628604", "nasem", agency)) %>%
    mutate(agency = if_else(affil_id == "130148658", "nasem", agency)) %>%
    mutate(agency = if_else(affil_id == "112602793", "nih", agency)) %>%
    mutate(agency = if_else(affil_id == "113013369", "nih", agency)) %>%
    mutate(agency = if_else(affil_id == "125340513", "nih", agency)) %>%
    mutate(agency = if_else(affil_id == "125381761", "nih", agency)) %>%
    mutate(agency = if_else(affil_id == "125755037", "nih", agency)) %>%
    mutate(agency = if_else(affil_id == "131324608", "nih", agency)) %>%
    mutate(agency = if_else(affil_id == "131324998", "nih", agency)) %>%
    mutate(agency = if_else(affil_id == "100316021", "noaa", agency)) %>%
    mutate(agency = if_else(affil_id == "101000910", "noaa", agency)) %>%
    mutate(agency = if_else(affil_id == "123141776", "noaa", agency)) %>%
    mutate(agency = if_else(affil_id == "130557216", "noaa", agency)) %>%
    mutate(agency = if_else(affil_id == "60071501", "nsf", agency)) %>%
    mutate(agency = if_else(affil_id == "100587655", "nsf", agency)) %>%
    mutate(agency = if_else(affil_id == "101394353", "nsf", agency)) %>%
    mutate(agency = if_else(affil_id == "106456794", "nsf", agency)) %>%
    mutate(agency = if_else(affil_id == "108578096", "smithsonian", agency)) %>%
    mutate(agency = if_else(affil_id == "114159234", "smithsonian", agency)) %>%
    mutate(agency = if_else(affil_id == "128149737", "smithsonian", agency)) %>%
    mutate(agency = if_else(affil_id == "115539127", "usphs", agency)) %>%
    mutate(agency = if_else(affil_id == "123896933", "usphs", agency)) %>%
    mutate(agency = if_else(affil_id == "126381052", "usphs", agency)) %>%
    mutate(agency = if_else(affil_id == "120532431", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "128306628", "usda", agency)) %>%
    mutate(agency = if_else(affil_id == "131558906", "usda", agency)) %>%
    mutate(agency = if_else(affil_id == "125763856", "usda", agency)) %>%
    mutate(agency = if_else(affil_id == "125364545", "usda", agency)) %>%
    mutate(agency = if_else(affil_id == "113119893", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "118439805", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "124601958", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "107062094", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "113951892", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "123822264", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "128757432", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "131466439", "dod", agency)) %>%
    mutate(agency = if_else(affil_id == "101261119", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "101265211", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131511930", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "113007889", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "114684115", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "115867250", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112587340", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60012281", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "106548849", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "110545556", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "121897786", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "125786644", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131254575", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112949764", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "107980732", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "113860189", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "121466434", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "122411044", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60014521", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "106585504", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "109248263", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112905068", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "107089434", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "129839931", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "125919619", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "113212209", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "100364127", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112994777", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112637847", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "100332619", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "117974668", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "129449620", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "129132280", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "128165491", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "101004622", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60002223", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112987892", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60105859", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "126765651", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "107025178", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "115341424", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "118903785", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "123301448", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "123300875", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131254594", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131254565", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131254588", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131254570", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "126999016", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "129518085", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112843093", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112910426", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "105621128", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "113197759", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60105937", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131554680", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "129184086", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131119214", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112587380", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "122995724", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "119743592", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60105918", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60004786", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112234684", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "101581898", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "130434719", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "114586585", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "109494186", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "109911421", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "114306072", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "115168483", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "105745097", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112656582", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "119639166", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "105333223", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "130217062", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112805663", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "105424804", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "113069894", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "114786642", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "125701945", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "100988589", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "128210866", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "111235657", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "118700423", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "112117183", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "131644549", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "130647362", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "125155173", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "110646347", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "110271622", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "120477769", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "126413640", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "101964606", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "121518184", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "107852606", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "114455318", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "113187369", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "125781615", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "126907053", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "101516366", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "108095235", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "123934640", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "60073942", "va", agency)) %>%
    mutate(agency = if_else(affil_id == "126395319", "va", agency)) %>%
    mutate(agency = if_else(affiliation == "us department of defense", "dod", agency)) %>%
    mutate(agency = if_else(affiliation == "us department of the interior", "interior", agency)) %>%
    mutate(agency = if_else(agency == "independent", agency_full.x, agency)) %>%
    mutate(agency = if_else(agency == "national laboratories", "doe", agency)) %>%
    mutate(agency = if_else(str_detect(agency, "us government accountability"), "gao", agency)) %>%
    mutate(agency = if_else(agency == "environmental protection agency office of research and development", "epa", agency)) %>%
    mutate(agency = if_else(agency == "engineer research and development center", "dod", agency)) %>%
    mutate(agency = if_else(agency == "naval undersea warfare center", "dod", agency)) %>%
    mutate(agency = if_else(agency == "voice of america", "voa", agency)) %>%
    mutate(agency = if_else(agency == "us postal service", "usps", agency)) %>%
    mutate(agency = if_else(agency == "general services administration", "gsa", agency)) %>%
    mutate(agency = if_else(agency == "inter-american foundation", "iaf", agency)) %>%
    mutate(agency = if_else(agency == "army research, development and engineering command", "dod", agency)) %>%
    mutate(agency = if_else(agency == "army research laboratory", "dod", agency)) %>%
    mutate(agency = if_else(agency == "tennessee valley authority", "tva", agency)) %>%
    mutate(agency = if_else(agency == "national research council senior fellow", "nrc", agency)) %>%
    mutate(agency = if_else(agency == "securities and exchange commission", "sec", agency)) %>%
    mutate(agency = if_else(agency == "americorps vista", "americorps", agency)) %>%
    mutate(agency = if_else(agency == "army armament research, development and engineering center", "dod", agency)) %>%
    mutate(agency = if_else(agency == "national research council research associateship program", "nrc", agency)) %>%
    mutate(agency = if_else(agency == "social security administration", "ssa", agency)) %>%
    mutate(agency = if_else(agency == "national research council (ibfm-cnr)", "nrc", agency)) %>%
    mutate(agency = if_else(agency == "us climate variability and predictability project office", "noaa", agency)) %>%
    mutate(agency = if_else(agency == "federal maritime commission (fmc)", "federal maritime comission", agency)) %>%
    mutate(agency = if_else(agency == "national research council postdoctoral fellow", "nrc", agency)) %>%
    mutate(agency = if_else(agency == "interagency special status/sensitive species program", "dod", agency)) %>%
    mutate(agency = if_else(agency == "institute of museum and library services", "imls", agency)) %>%
    mutate(agency = if_else(agency == "national transportation safety board", "ntsb", agency)) %>%
    mutate(agency = if_else(agency == "federal communications commission", "fcc", agency)) %>%
    mutate(agency = if_else(agency == "national labor relations board", "nlrb", agency)) %>%
    mutate(federal = if_else(!is.na(agency), TRUE, FALSE)) %>%
    distinct()
  
  affils_df<-affils_df %>% arrange(desc(federal),agency)
  
  return(affils_df)
}





# unique(affils_df2$agency)
# 
# 
# # final change of affiliation names
# pubs_with_fed <- pubs_with_fed %>%
#   mutate(affiliation = if_else(affiliation == "agriculture", "usda", affiliation))


# save isolate fed affiliations
#
# fed_affiliations_list<-pubs_with_fed %>%
#   filter(fed_author==TRUE) %>%
#   select(affil_id,affiliation,country,affiliation) %>%
#   distinct()
#
# NONfed_affiliations_list<-pubs_with_fed %>%
#   filter(fed_author==FALSE) %>%
#   select(affil_id,affiliation,country) %>%
#   distinct()
#
# # save --------------------------------------------------------------------
#
#
# write_rds(no_feds, "./data_clean/pubs_with_no_feds.rds")
# write_rds(pubs_with_fed, "./data_clean/analysis_fed_pubs.rds")
# write_rds(fed_affiliations_list, "./data_clean/fed_affiliations_list.rds")
# write_rds(NONfed_affiliations_list, "./data_clean/NONfed_affiliations_list.rds")









#
#
# agency.x=="independent"&is.na(agency.y)~ agency.y,
#
#   mutate(agency.x=if_else(agency.x=="independent"&!is.na(agency.y),agency.y,agency.x)) %>%
#   mutate(agency.x=if_else(agency.y=="dhs",agency.y,agency.x)) %>%
#
#   mutate(agency.y=if_else(agency.y==agency.x,NA,agency.y)) %>%
#   select(-country.y,
#          -acronym) %>%
#   rename(country=country.x) %>%
#   mutate(agency.y=if_else(agency.x==agency.y,NA,agency.y)) %>%
#
#
#   mutate(agency.x=if_else(agency_full.y=="the secretary of defense","dod",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.y=="air national guard","dod",agency.x))   %>%
# mutate(agency.x=if_else(agency_full.y=="office of the under secretary of defense","dod",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.y=="the deputy secretary of defense","dod",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.y=="us space command","dod",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.y=="us public health service commissioned corps","usphs",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.y=="public health service commissioned corps","usphs",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x)) %>%
#
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
# mutate(agency.x=if_else(agency_full.y=="us coast guard","dhs",agency.x))
#
# office of the assistant secretary for health
# us air force
# us army medical command
# us special operations command
# us environmental protection agency
# americorps
# office of the secretary of the army
# us army corps of engineers
# army research, development and engineering command
#
#   mutate(agency.x=if_else(agency_full.x=="the deputy secretary of defense ","dod",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.x=="defense health agency","dha",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.x=="us public health service commissioned corps","usphs",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.x=="us public health service commissioned corps","usphs",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.x=="us public health service commissioned corps","usphs",agency.x)) %>%
#   mutate(agency.x=if_else(agency_full.x=="us public health service commissioned corps","usphs",agency.x)) %>%
#
#
# all_fed_affils %>% filter(is.na(agency.x)) %>% group_by(agency_full.y) %>% tally() %>% arrange(desc(n))
#
# %>%


#
#
# affils_df2_to_fix<-affils_df2 %>%
#   filter(federal==FALSE) %>%
#   select(affil_id,affiliation,agency_full.x,agency,country)








#
#
#
#
#
#
#
#
#
#
#
#
# utate(agency=gsub("us department of defense", "dod",agency)) %>%
#   mutate(agency=gsub("us department of the interior", "interior",agency)) %>%
#   mutate(agency=gsub("federal reserve system", "frs",agency)) %>%
# mutate(agency=if_else(str_detect(affiliation,"ccdc"),"dod",agency)) %>%
#   mutate(affiliation=gsub("united state ","us ",agency)) %>%
#   mutate(affiliation=str_replace_all(affiliation, fixed("."), "")) %>%
#   mutate(agency=
#            case_when(
#            str_detect(affiliation, "agency for healthcare research and quality") ~ "ahrq",
#            str_detect(affiliation, "us department of health and human services") ~ "hhs",
#     str_detect(affiliation,"us army") ~ "dod",
#     country=="usa" & str_detect(affiliation,"department of the air force") ~ "dod",
#     str_detect(affiliation,"smithsonian") ~ "smithsonian",
#     str_detect(affiliation,"us department of energy") ~ "doe",
#     str_detect(affiliation,"us epa") ~ "epa",
#     str_detect(affiliation,"usepa") ~ "epa",
#     str_detect(affiliation,"epa region") ~ "epa",
#     str_detect(affiliation,"department of the army") ~ "dod",
#     str_detect(affiliation,"usda") ~ "usda",
#     str_detect(affiliation,"us department of labor") ~ "labor",
#     country=="usa" & str_detect(affiliation,"naval") ~ "dod",
#     country=="usa" & str_detect(affiliation,"veteran ") ~ "va",
#     country=="puerto rico" & str_detect(affiliation,"veteran") ~ "va",
#     country=="usa" & str_detect(affiliation,"veterans ") ~ "va",
#     country=="usa" & str_detect(affiliation,"veteran's ") ~ "va",
#     str_detect(affiliation,"us department of commerce") ~ "commerce",
#     str_detect(affiliation,"us department of defense") ~ "dod",
#     str_detect(affiliation,"us department of housing and urban development") ~ "hud",
#     # str_detect(affiliation,"us department of veterans affairs") ~ "va",
#     # str_detect(affiliation,"us department of veteran affairs") ~ "va",
#     str_detect(affiliation,"us environmental protection agency") ~ "epa",
#     str_detect(affiliation,"us department of transportation") ~ "dot",
#     str_detect(affiliation,"us attorney general") ~ "doj",
#     country=="usa" & str_detect(affiliation,"national institutes of health") ~ "nih",
#     country=="usa" & str_detect(affiliation,"army ") ~ "dod",
#     str_detect(affiliation,"us department of the interior") ~ "interior",
#     str_detect(affiliation,"us department of agriculture") ~ "usda",
#     str_detect(affiliation,"national oceanic and atmospheric administration") ~ "noaa",
#     country=="usa" & str_detect(affiliation,"nasa") ~ "nasa",
#     country=="usa" & str_detect(affiliation,"national science foundation") ~ "nsf",
#     country=="usa" & str_detect(affiliation,"navy ") ~ "dod",
#     str_detect(affiliation,"us department of the navy") ~ "dod",
#     str_detect(affiliation,"us nav") ~ "dod",
#     str_detect(affiliation,"us air force") ~ "dod",
#     str_detect(affiliation,"us marine corps") ~ "dod",
#     str_detect(affiliation,"us air force") ~ "dod",
#     str_detect(affiliation,"usaid") ~ "usaid",
#     country=="usa" & str_detect(affiliation,"centers for disease control and prevention")~"cdc",
#     country=="usa" & str_detect(affiliation,"national institutes of health")~"nih",
#     str_detect(affiliation,"us department of state")~"state",
#     str_detect(affiliation,"us fish and wildlife service")~"interior",
#     country=="usa" & str_detect(affiliation,"food and drug administration")~"fda",
#     country=="usa" & str_detect(affiliation,"us department of agriculture")~"usda",
#     str_detect(affiliation,"lawrence berkeley national laboratory")~"doe",
#     str_detect(affiliation,"smithsonian")~"smithsonian",
#     country=="usa" & str_detect(affiliation,"national park service")~"interior",
#     country=="usa" & str_detect(affiliation,"veterans")~"va",
#     country=="usa" & str_detect(affiliation,"national science foundation")~"nsf",
#     str_detect(affiliation,"us department of energy")~"doe",
#     str_detect(affiliation,"us geological survey")~"interior",
#     str_detect(affiliation,"us department of education") ~"interior",
#     str_detect(affiliation,"us department of homeland security") ~"dhs",
#     str_detect(affiliation,"us department of justice") ~"doj",
#     str_detect(affiliation,"us department of the treasury") ~"treasury",
#    str_detect(affiliation,"us department of transportation")~"dot",
#
#   str_detect(affiliation,"us cdc")~"cdc",
#   str_detect(affiliation,"us embassy")~"state",
#   str_detect(affiliation,"us peace corps")~"state",
#   str_detect(affiliation,"us office of naval")~"dod",
#   str_detect(affiliation,"us 8th army")~"dod",
#   str_detect(affiliation,"us president")~"eop",
#   str_detect(affiliation,"us forces")~"dod",
#   str_detect(affiliation,"us military")~"dod",
#    str_detect(affiliation,"us national institute of allergy and infectious diseases")~"nih",
#   str_detect(affiliation," us forest service")~"usda",
#   str_detect(affiliation," us aid")~"state",
#   str_detect(affiliation," us department of defence")~"dod",
#   str_detect(affiliation," us walter reed")~"dod",
#   str_detect(affiliation," us mission")~"state",
#   str_detect(affiliation," us bureau of land management")~"interior",
#   str_detect(affiliation," us antarctic program")~"nsf",
#   str_detect(affiliation," niaid/nih international centers for excellence in ")~"nih",
#              str_detect(affiliation," us centers for disease control")~"cdc",
#   str_detect(affiliation,"uniformed services university of the health sciences")~"dod",
#   str_detect(affiliation,"walter reed")~"dod",
#   str_detect(affiliation,"us agency for international development")~"usaid",
#
#   country=="usa" & str_detect(affiliation,"national park")~"interior",
#   country=="usa"& str_detect(affiliation,"national park")~"interior",
#   country=="usa"& str_detect(affiliation,"national seashore")~"interior",
#
#   country=="puerto rico"& str_detect(affiliation,"national park")~"interior",
#     country=="northern mariana islands"& str_detect(affiliation,"national park")~"interior",
#     country=="american samoa"& str_detect(affiliation,"national park")~"interior",
#     country=="guam"& str_detect(affiliation,"national park")~"interior",
#     country=="virgin islands (u.s.)" & str_detect(affiliation,"national park")~"interior",
#
#
# str_detect(affiliation,"national telecommunications and information administration")~"commerce",
# str_detect(affiliation,"us national archives and records administration")~"national archives",
# str_detect(affiliation,"national institute for mathematical and biological synthesis")~"nsf",
# str_detect(affiliation,"office of national drug control policy")~"eop",
# str_detect(affiliation,"national center for preparedness, detection, and control of infectious diseases")~"cdc",
# str_detect(affiliation,"national socio-environmental synthesis center")~"nsf",
# str_detect(affiliation,"national evolutionary synthesis center")~"nsf",
# str_detect(affiliation,"national ecological observatory network")~"nsf",
# str_detect(affiliation,"us department of interior")~"interior",
# str_detect(affiliation,"national historical park")~"interior",
# str_detect(affiliation,"fredrick national laboratory")~"nih",
#
# str_detect(affiliation,"national park service social science program")~"interior",
# affiliation=="national park service social science program"~"interior",
#
# affiliation=="us preventive services task force"~"ahrq",
# affiliation=="ahrq"~"ahrq",
# affiliation=="congressional budget office"~"cbo",
# affiliation=="national center for injury prevention and control"~"cdc",
# affiliation=="national center for emerging and zoonotic infectious diseases"~"cdc",
# affiliation=="national center for health statistics"~"cdc",
# affiliation=="national center for chronic disease prevention and health promotion"~"cdc",
# affiliation=="national center for birth defects and developmental disabilities"~"cdc",
# affiliation=="national center for immunization and respiratory diseases"~"cdc",
# affiliation=="cdc"~"cdc",
# affiliation=="national center for environmental health"~"cdc",
# affiliation=="national center for disease control and public health"~"cdc",
# affiliation=="agency for toxic substance and disease registry"~"cdc",
# affiliation=="contracting agency to the division of viral diseases"~"cdc",
# affiliation=="central intelligence agency"~"cia",
# affiliation=="us patent and trademark office"~"commerce",
# affiliation=="us house of representatives"~"congress",
# affiliation=="us senate"~"congress",
# affiliation=="us botanic garden"~"congress",
# affiliation=="national gallery of art"~"congress",
# affiliation=="national museum of health and medicine"~"dha",
# affiliation=="defense health agency"~"dod",
# affiliation=="national research institute"~"dod",
# affiliation=="defense threat reduction agency"~"dod",
# affiliation=="defense advanced research projects agency"~"dod",
# affiliation=="us pacific fleet"~"dod",
# affiliation=="defense logistics agency"~"dod",
# affiliation=="national geospatial-intelligence agency"~"dod",
# affiliation=="san antonio military medical center"~"dod",
# affiliation=="air force institute of technology"~"dod",
# affiliation=="82nd airborne division"~"dod",
# affiliation=="us fleet forces command"~"dod",
# affiliation=="national guard bureau"~"dod",
# affiliation=="96th medical group"~"dod",
# affiliation=="us dep of the navy"~"dod",
# affiliation=="defense nuclear agency"~"dod",
# affiliation=="nmrc"~"dod",
# affiliation=="institute of infectious diseases"~"dod",
# affiliation=="wrair"~"dod",
# affiliation=="us combat casualty care research program"~"dod",
# affiliation=="defense intelligence agency"~"dod",
# affiliation=="national defense university"~"dod",
# affiliation=="northwest national laboratory"~"dod",
# affiliation=="99th medical group"~"dod",
# affiliation=="88th medical group"~"dod",
# affiliation=="national center for telehealth and technology"~"dod",
# affiliation=="us department of army"~"dod",
# affiliation=="national strategic research institute"~"dod",
# affiliation=="us air war college"~"dod",
# affiliation=="us baylor military graduate program in nutrition"~"dod",
# affiliation=="defense information systems agency"~"dod",
# affiliation=="air force"~"dod",
# affiliation=="national war college"~"dod",
# affiliation=="us armed forces health surveillance division"~"dod",
# affiliation=="us dep of the army"~"dod",
# affiliation=="us marine"~"dod",
# affiliation=="erdc"~"dod",
# affiliation=="us marine forces cyberspace command"~"dod",
# affiliation=="us armed services blood program office"~"dod",
# affiliation=="us coast guard"~"dod",
# affiliation=="military vaccine agency"~"dod",
# affiliation=="defence health agency"~"dod",
# affiliation=="air combat command"~"dod",
# affiliation=="hurricane flood risk reduction design branch"~"dod",
# affiliation=="97th military police battalion"~"dod",
# affiliation=="us second fleet"~"dod",
# affiliation=="defense pow/mia accounting agency"~"dod",
# affiliation=="national immunization program"~"dod",
# affiliation=="lawrence livermore national laboratory"~"doe",
# affiliation=="doe"~"doe",
# affiliation=="pacific northwest national laboratory"~"doe",
# affiliation=="oak ridge national laboratory"~"doe",
# affiliation=="brookhaven national laboratory"~"doe",
# affiliation=="national renewable energy laboratory"~"doe",
# affiliation=="national center for electron microscopy"~"doe",
# affiliation=="los alamos national laboratory"~"doe",
# affiliation=="argonne national laboratory"~"doe",
# affiliation=="jet propulsion laboratory"~"doe",
# affiliation=="princeton plasma physics laboratory"~"doe",
# affiliation=="us iter project office"~"doe",
# affiliation=="fermi national accelerator laboratory"~"doe",
# affiliation=="nevada national security site"~"doe",
# affiliation=="slac national accelerator laboratory"~"doe",
# affiliation=="national energy technology laboratory"~"doe",
# affiliation=="savannah river national laboratory"~"doe",
# affiliation=="federal energy regulatory commission"~"doe",
# affiliation=="idaho national laboratory"~"doe",
# affiliation=="oak ridge"~"doe",
# affiliation=="advanced research projects agency - energy"~"doe",
# affiliation=="thomas jefferson national accelerator facility"~"doe",
# affiliation=="national nuclear security administration"~"doe",
# affiliation=="ames laboratory"~"doe",
# affiliation=="national high magnetic field laboratory los almos"~"doe",
# affiliation=="federal bureau of investigation"~"doj",
# affiliation=="federal medical center, rochester"~"doj",
# affiliation=="us bureau of alcohol"~"doj",
# affiliation=="national institute of justice"~"doj",
# affiliation=="fbi"~"doj",
# affiliation=="federal highway administration"~"dot",
# affiliation=="national highway traffic safety administration"~"dot",
# affiliation=="federal railroad administration"~"dot",
# affiliation=="us national security council"~"eop",
# affiliation=="us national security advisor"~"eop",
# affiliation=="national health and environmental effects research laboratory"~"epa",
# affiliation=="us research laboratory"~"epa",
# affiliation=="enivronmental protection agency"~"epa",
# affiliation=="federal aviation administration"~"faa",
# affiliation=="national center for toxicological research"~"fda",
# affiliation=="us food and drug admnistration"~"fda",
# affiliation=="center for biologics evaluation and research"~"fda",
# affiliation=="federal emergency management agency"~"fema",
# affiliation=="federal housing finance agency"~"fhfa",
# affiliation=="federal reserve system"~"frs",
# affiliation=="denver federal center"~"gsa",
# affiliation=="hrsa"~"hhs",
# affiliation=="health resources and services administration"~"hhs",
# affiliation=="department of housing and urban development"~"hud",
# affiliation=="patuxent wildlife research center"~"interior",
# affiliation=="bureau of land management"~"interior",
# affiliation=="national wetlands research center"~"interior",
# affiliation=="us fish and wildlife national forensics laboratory"~"interior",
# affiliation=="fish and wildlife service"~"interior",
# affiliation=="usfws"~"interior",
# affiliation=="usfs medicine bow/routt national forests and thunder basin national grassland"~"interior",
# affiliation=="us geoheritage and geoparks advisory group"~"interior",
# affiliation=="us geologic survey"~"interior",
# affiliation=="national fish and wildlife refuge"~"interior",
# affiliation=="usgs"~"interior",
# affiliation=="national aeronautics and space administration"~"nasa",
# affiliation=="national academy of sciences"~"nasem",
# affiliation=="national academy of engineering"~"nasem",
# affiliation=="national credit union administration"~"ncua",
# affiliation=="national institute for occupational safety and health"~"nih",
# affiliation=="national institute of mental health"~"nih",
# affiliation=="national center for complementary and integrative health"~"nih",
# affiliation=="nci"~"nih",
# affiliation=="national cancer institute"~"nih",
# affiliation=="national library of medicine"~"nih",
# affiliation=="national institute on aging"~"nih",
# affiliation=="national institute of neurological disorders and stroke"~"nih",
# affiliation=="national institute on drug abuse"~"nih",
# affiliation=="nhgri"~"nih",
# affiliation=="nhlbi"~"nih",
# affiliation=="national institute of allergy and infectious diseases"~"nih",
# affiliation=="national institute of dental and craniofacial research"~"nih",
# affiliation=="us national library of medicine"~"nih",
# affiliation=="national institute of child health and human development"~"nih",
# affiliation=="nida"~"nih",
# affiliation=="niaaa"~"nih",
# affiliation=="national institute on alcohol abuse and alcoholism"~"nih",
# affiliation=="national eye institute"~"nih",
# affiliation=="national institute of biomedical imaging and bioengineering"~"nih",
# affiliation=="national human genome research institute"~"nih",
# affiliation=="national institute of diabetes and digestive and kidney diseases"~"nih",
# affiliation=="nia"~"nih",
# affiliation=="national center for advancing translational sciences"~"nih",
# affiliation=="nimhd"~"nih",
# affiliation=="niddk"~"nih",
# affiliation=="fogarty international center"~"nih",
# affiliation=="niams"~"nih",
# affiliation=="national institute on minority health and health disparities"~"nih",
# affiliation=="national institute of nursing research"~"nih",
# affiliation=="national center for infectious diseases"~"nih",
# affiliation=="us nih"~"nih",
# affiliation=="national institute of standards and technology"~"nist",
# affiliation=="national oceanic/atmospheric admin"~"noaa",
# affiliation=="nat oceanic atmospheric adm"~"noaa",
# affiliation=="national center for environmental prediction"~"noaa",
# affiliation=="national weather service"~"noaa",
# affiliation=="national oceanographic and atmospheric administration"~"noaa",
# affiliation=="national centers for coastal ocean science"~"noaa",
# affiliation=="national hurricane center"~"noaa",
# affiliation=="us integrated ocean observing system"~"noaa",
# affiliation=="central pacific hurricane center"~"noaa",
# affiliation=="national centers for environmental information"~"noaa",
# affiliation=="national estuarine research reserve"~"noaa",
# affiliation=="national atmospheric and oceanic administration fisheries"~"noaa",
# affiliation=="national centers for environmental prediction"~"noaa",
# affiliation=="national center for atmospheric research"~"nsf",
# affiliation=="national radio astronomy observatory"~"nsf",
# affiliation=="national science board"~"nsf",
# affiliation=="nsf"~"nsf",
# affiliation=="national solar observatory"~"nsf",
# affiliation=="national center for science and engineering statistics"~"nsf",
# affiliation=="us office of personnel management"~"opm",
# affiliation=="national research council"~"nrc",
# affiliation=="us government"~"other",
# affiliation=="us arctic research commission"~"us arctic research commission",
# affiliation=="us global change research program"~"us global change research program",
# affiliation=="interagency grizzly bear study team"~"interagency grizzly bear study team",
# affiliation=="national endowment for the arts"~"nea",
# affiliation=="federal maritime commission"~"federal maritime commission",
# affiliation=="us of america"~"other",
# affiliation=="us climate variability and predictability project office"~"us climate variability and predictability project office",
# affiliation=="interagency special status/sensitive species program"~"interagency special status/sensitive species program",
# affiliation=="us federal service"~"us federal service",
# affiliation=="national zoological park"~"smithsonian",
# affiliation=="national museum of natural history"~"smithsonian",
# affiliation=="national zoo"~"smithsonian",
# affiliation=="national museum of the american indian"~"smithsonian",
# affiliation=="national museum of american history"~"smithsonian",
# affiliation=="national museum of asian art"~"smithsonian",
# affiliation=="office of the us global aids coordinator"~"state",
# affiliation=="us international trade commission"~"state",
# affiliation=="us dep of the interior"~"interior",
# affiliation=="us international development finance corporation (dfc)"~"us international development finance corporation",
# affiliation=="agency for international development"~"usaid",
# affiliation=="agency for international development (aid)"~"usaid",
# affiliation=="us forest service"~"usda",
# affiliation=="us national arboretum"~"usda",
# affiliation=="us national poultry research center"~"usda",
# affiliation=="us forest products laboratory"~"usda",
# affiliation=="us vegetable breeding laboratory"~"usda",
# affiliation=="us vegetable laboratory"~"usda",
# affiliation=="us pacific basin agricultural research center"~"usda",
# affiliation=="national center for cool and coldwater aquaculture"~"usda",
# affiliation=="national tropical botanical garden"~"usda",
# affiliation=="usad-ars"~"usda",
# affiliation=="us forest service international programs wood identification and screening center"~"usda",
# affiliation=="us forest servhice"~"usda",
# affiliation=="national center for ptsd"~"va",
# affiliation=="national center for post-traumatic stress disorder"~"va",
# affiliation=="vha"~"va",
# affiliation=="captain james a lovell federal health care center"~"va",
# affiliation=="va national surgery office"~"va",
# affiliation=="national center for rehabilitative auditory research"~"va",
# affiliation=="va national center for patient safety"~"va",
# affiliation=="va national teleoncology"~"va",
# affiliation=="va national expert consultation and specialized services"~"va",
# affiliation=="va national pharmacogenomics program"~"va",
# str_detect(affiliation,"us geológico survey")~"interior",
# str_detect(affiliation,"veteran") & str_detect(affiliation,"health care") ~ "va",
# str_detect(affiliation,"va ") & str_detect(affiliation,"health services") ~ "va",
#
#
#
# country=="usa" & str_detect(affiliation,"national fish hatchery")~"interior",
# country=="usa" & str_detect(affiliation,"national forest")~"usda",
# country=="usa" & str_detect(affiliation,"us vegetable lab")~"usda",
# country=="usa" & str_detect(affiliation,"agency for toxic substance and disease registry")~"hhs",
# country=="usa" & str_detect(affiliation,"th medical group")~"dod",
# country=="usa" & str_detect(affiliation,"walter reed")~"dod",
# country=="usa" & str_detect(affiliation,"us bureau of alcohol")~"doj",
# country=="usa" & str_detect(affiliation,"astdr")~"cdc",
# country=="usa" & str_detect(affiliation,"national marine sanctuary")~"noaa",
# country=="usa" & str_detect(affiliation,"national park")~"interior",
# country=="usa" & str_detect(affiliation,"national seashore")~"interior",
# country=="usa" & str_detect(affiliation,"national wildlife refuge")~"interior",
# country=="usa" & str_detect(affiliation,"us fish and wildlife")~"interior",
# country=="usa" & str_detect(affiliation,"national library of medicine")~"nih",
# country=="usa" & str_detect(affiliation,"us government")~"other",
# country=="usa" & str_detect(affiliation,"james a lovell")~"va",
# country=="usa" & str_detect(affiliation,"va national")~"va",
# country=="usa" & str_detect(affiliation,"veteran affairs")~"va",
# country=="usa" & str_detect(affiliation,"ars usda")~"usda",
# country=="usa" & str_detect(affiliation,"arlington national")~"dod",
# country=="usa" & str_detect(affiliation,"national monument")~"interior",
# country=="usa" & str_detect(affiliation,"frederick national")~"nih",
# country=="usa" & str_detect(affiliation,"national estuarine")~"interior",
# country=="usa" & str_detect(affiliation,"noaa")~"noaa",
# country=="usa" & str_detect(affiliation,"us mission")~"state",
# country=="usa" & str_detect(affiliation,"malaria initiative improving malaria")~"eop",
# country=="usa" & str_detect(affiliation,"the us president")~"eop",
# country=="usa" & str_detect(affiliation,"us aid")~"usaid",
# country=="usa" & str_detect(affiliation,"afit")~"dod",
# country=="usa" & str_detect(affiliation,"carl r darnall")~"dod",
# country=="usa" & str_detect(affiliation,"national defense university")~"dod",
#
#
#
# TRUE ~ as.character(agency)))
#
#
#
#
# agency_search<-read_csv("./data_clean/results_datasetv1/agency_search_clean.csv") %>%
#   mutate(agency=if_else(agency=="agriculture", "usda",agency)) %>%
#   rename(affiliation=search_term)
#
#
#
#   affils_df2_to_fix <- affils_df2_to_fix %>%
#   mutate(agency = if_else(is.na(agency) & country == "usa" & affiliation %in% agency_search$affiliation, "MATCH", agency))
#   foo<-affils_df2_to_fix %>% filter(agency=="MATCH") %>% select(affiliation) %>% distinct() %>% left_join(agency_search)
#   write_csv(foo,"./data_raw/foo.csv")
#
#
#   foo<-agency_search %>% filter(!is.na(str_detect)) %>% select(str_detect)
#
#
# us centers for disease control
#
#
# FALSE
# office of information technology
# office of ethics
# office of human resources
#
#
# filter(str_detect(affiliation, paste(state_names$value, collapse = "|"))) %>%
#
# foo<-affils_df2_to_fix %>%
#   filter(str_detect(affiliation, "us ")) %>%
#   filter(str_detect(affiliation, "army")) %>%
#   filter(str_detect(affiliation, "army")) %>%
#   filter(str_detect(affiliation, "noaa")) %>%
#   filter(str_detect(affiliation, "air force")) %>%
#   filter(str_detect(affiliation, "bureau of land management")) %>%
#   filter(str_detect(affiliation, "cdc")) %>%
#   filter(str_detect(affiliation, "national cancer institute")) %>%
#   filter(str_detect(affiliation, "usaid")) %>%
#   filter(str_detect(affiliation, "usgs")) %>%
#   filter(str_detect(affiliation, "usfws")) %>%
#   filter(str_detect(affiliation, "veterans administration")) %>%
#   filter(str_detect(affiliation, "veterans health administration")) %>%
#   filter(str_detect(affiliation, "vha")) %>%
#   filter(str_detect(affiliation, "centers for disease control and prevention")) %>%
#   filter(str_detect(affiliation, "national")) %>%
#   select(affil_id)
#
# false_fed_positives2<-scopus_affils_df %>%
#   filter(str_detect(affiliation, "usda")) %>%
#   # filter(str_detect(affiliation, "us ")) %>%
#   filter(str_detect(affiliation, "army")) %>%
#   filter(str_detect(affiliation, "us ")) %>%
#   filter(str_detect(affiliation, "national weather service")) %>%
#   filter(str_detect(affiliation, "usaf")) %>%
#   filter(str_detect(affiliation, "noaa")) %>%
#   filter(str_detect(affiliation, "usepa")) %>%
#   filter(str_detect(affiliation, "air force")) %>%
#   filter(str_detect(affiliation, "veteran affairs health")) %>%
#   filter(str_detect(affiliation, "veteran affairs")) %>%
#   filter(str_detect(affiliation, "82nd airborne")) %>%
#   filter(str_detect(affiliation, "cber")) %>%
#   filter(str_detect(affiliation, "coast guard")) %>%
#   filter(str_detect(affiliation, "ahrq")) %>%
#   filter(str_detect(affiliation, "national academy")) %>%
#   filter(str_detect(affiliation, "center for biologics evaluation")) %>%
#   filter(str_detect(affiliation, "department of defense")) %>%
#   filter(str_detect(affiliation, "national museum")) %>%
#   filter(str_detect(affiliation, "administration")) %>%
#   filter(str_detect(affiliation, "affiliation for")) %>%
#   filter(str_detect(affiliation, "bureau of land management")) %>%
#   filter(str_detect(affiliation, "smithsonian")) %>%
#   filter(str_detect(affiliation, "military")) %>%
#   filter(str_detect(affiliation, "nasa")) %>%
#   filter(str_detect(affiliation, "national park")) %>%
#   filter(str_detect(affiliation, "national laboratory")) %>%
#   filter(str_detect(affiliation, "national wildlife refuge")) %>%
#   filter(str_detect(affiliation, "cdc")) %>%
#   filter(str_detect(affiliation, "national cancer institute")) %>%
#   filter(str_detect(affiliation, "usaid")) %>%
#   filter(str_detect(affiliation, "usda")) %>%
#   filter(str_detect(affiliation, "usgs")) %>%
#   filter(str_detect(affiliation, "usfws")) %>%
#   filter(str_detect(affiliation, "veterans administration")) %>%
#   filter(str_detect(affiliation, "veterans health administration")) %>%
#   filter(str_detect(affiliation, "vha")) %>%
#   filter(str_detect(affiliation, "us department")) %>%
#   filter(str_detect(affiliation, "federal")) %>%
#   filter(str_detect(affiliation, "affiliation")) %>%
#   filter(str_detect(affiliation, " national ")) %>%
#   filter(str_detect(affiliation, "uniformed services")) %>%
#   filter(str_detect(affiliation, "nsf")) %>%
#   filter(str_detect(affiliation, "nih")) %>%
#   filter(str_detect(affiliation, "national institute")) %>%
#   filter(str_detect(affiliation, "national research council")) %>%
#   filter(str_detect(affiliation, "national solar")) %>%
#   filter(str_detect(affiliation, "national zoo")) %>%
#   filter(str_detect(affiliation, "fbi ")) %>%
#   filter(str_detect(affiliation, "doe")) %>%
#   filter(str_detect(affiliation, "naval")) %>%
#   filter(str_detect(affiliation, "national library")) %>%
#   filter(str_detect(affiliation, "national museum")) %>%
#   filter(str_detect(affiliation, "national center")) %>%
#   filter(str_detect(affiliation, "medical group")) %>%
#   filter(str_detect(affiliation, "combat")) %>%
#   filter(str_detect(affiliation, "ames laboratory")) %>%
#   filter(str_detect(affiliation, "armed forces")) %>%
#   filter(str_detect(affiliation, "nci ")) %>%
#   filter(str_detect(affiliation, "afit ")) %>%
#   filter(str_detect(affiliation, "wrair")) %>%
#   filter(str_detect(affiliation, "niaaa")) %>%
#   filter(str_detect(affiliation, "national eye institute")) %>%
#   filter(str_detect(affiliation, "national fish and wildlife refuge")) %>%
#   filter(str_detect(affiliation, "nhlbi")) %>%
#   filter(str_detect(affiliation, "niaid")) %>%
#   filter(str_detect(affiliation, "niams")) %>%
#   filter(str_detect(affiliation, "nichd")) %>%
#   filter(str_detect(affiliation, "nida")) %>%
#   filter(str_detect(affiliation, "nimh")) %>%
#   filter(str_detect(affiliation, "niosh")) %>%
#   filter(str_detect(affiliation, "nist")) %>%
#   filter(str_detect(affiliation, "nmrc")) %>%
#   filter(str_detect(affiliation, "oak ridge lab")) %>%
#   filter(str_detect(affiliation, "usace")) %>%
#   filter(str_detect(affiliation, "patuxent wildlife research center")) %>%
#   filter(str_detect(affiliation, "national heart, lung, and blood institute")) %>%
#   filter(str_detect(affiliation, "national high magnetic field laboratory los almos")) %>%
#   filter(str_detect(affiliation, "national human genome research institute")) %>%
#   filter(str_detect(affiliation, "national human genome research institute")) %>%
#   filter(str_detect(affiliation, "nhgri")) %>%
#   filter(str_detect(affiliation, "national hurricane center")) %>%
#   filter(str_detect(affiliation, "national immunization program")) %>%
#   filter(str_detect(affiliation, "national intelligence council")) %>%
#   filter(str_detect(affiliation, "national invasive species council")) %>%
#   filter(str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
#   filter(str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
#   filter(str_detect(affiliation, "national gallery of art")) %>%
#   filter(str_detect(affiliation, "national guard")) %>%
#   filter(str_detect(affiliation, "national radio ")) %>%
#   filter(str_detect(affiliation, "national renewable")) %>%
#   filter(str_detect(affiliation, "national science board")) %>%
#   filter(str_detect(affiliation, "national science foundation")) %>%
#   filter(str_detect(affiliation, "national security council")) %>%
#   filter(str_detect(affiliation, "national severe storms laboratory norman")) %>%
#   filter(str_detect(affiliation, "national socio-environmental synthesis center")) %>%
#   filter(str_detect(affiliation, "sesync")) %>%
#   filter(str_detect(affiliation, "incident command system")) %>%
#   filter(str_detect(affiliation, "national toxicology program")) %>%
#   filter(str_detect(affiliation, "national toxicology program laboratory")) %>%
#   filter(str_detect(affiliation, "national tropical botanical garden")) %>%
#   filter(str_detect(affiliation, "national war college")) %>%
#   filter(str_detect(affiliation, "national wetlands research center")) %>%
#   filter(str_detect(affiliation, "national wetlands research center")) %>%
#   filter(str_detect(affiliation, "natl research council associate")) %>%
#   filter(str_detect(affiliation, "ncbi/nlm")) %>%
#   filter(str_detect(affiliation, "nci-frederick laboratory animal sciences program")) %>%
#   filter(str_detect(affiliation, "nhlbi")) %>%
#   filter(str_detect(affiliation, "nhgri")) %>%
#   filter(str_detect(affiliation, "national health and environmental effects research laboratory")) %>%
#   filter(str_detect(affiliation, "hrsa")) %>%
#   filter(str_detect(affiliation, "hurricane flood risk reduction design branch")) %>%
#   filter(str_detect(affiliation, "jet propulsion laboratory")) %>%
#   filter(str_detect(affiliation, "national atmospheric deposition program")) %>%
#   filter(str_detect(affiliation, "nad")) %>%
#   filter(str_detect(affiliation, "epa region")) %>%
#   filter(str_detect(affiliation, "erdc")) %>%
#   filter(str_detect(affiliation, "national defense")) %>%
#   filter(str_detect(affiliation, "national ecological observatory network")) %>%
#   filter(str_detect(affiliation, "national endowment for the arts")) %>%
#   filter(str_detect(affiliation, "national energy technology laboratory")) %>%
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# search_terms_intl<-data.frame(search_term=c("niaid/nih international centers for excellence in research",
#                                             "usaid",
#                                             "us aid ",
#                                             "us antarctic program",
#                                             "us army",
#                                             "us bureau of land management",
#                                             "us centers for disease control",
#                                             "us cdc",
#                                             "us consulate",
#                                             "us department of agriculture",
#                                             "us department of defense",
#                                             "us department of defence",
#                                             "us department of veteran",
#                                             "us embassy",
#                                             "us environmental protection affiliation",
#                                             "us forces",
#                                             "us forest service",
#                                             "us geological survey",
#                                             "us marine corp",
#                                             "us military",
#                                             "us mission",
#                                             "us national institute of allergy and infectious diseases",
#                                             "us naval",
#                                             "us navy",
#                                             "us of america affiliation for international development",
#                                             "us office of naval",
#                                             "us president",
#                                             "us walter reed",
#                                             "usda-ars",
#                                             "usda",
#                                             "us peace corps",
#                                             "us cdc",
#                                             "us 8th army",
#                                             "us affiliation for international development"))
#
#
#
# search_terms_intl<-paste(search_terms_intl$search_term, collapse = "|" )
#
# affils_df2_to_fix <-affils_df2_to_fix %>%
#   mutate(search_term=str_extract(affiliation, search_terms_intl)) %>%
#   filter(country!="usa" &
#            country!="puerto rico" &
#            country!="northern mariana islands" &
#            country!="american samoa" &
#            country!="guam" &
#            country!="virgin islands (u.s.)") %>%
#   mutate(search_term=str_extract(affiliation, search_terms_intl)) %>%
#   mutate_all(trimws,which = c("both")) %>%
#   mutate(agency=
#            case_when(
#              search_term=="us cdc"~"cdc",
#              search_term=="us embassy"~"state",
# search_term=="us peace corps"~"state",
# search_term=="us office of naval"~"dod",
# search_term=="us 8th army"~"dod",
# search_term=="us president"~"eop",
# search_term=="us forces"~"dod",
# search_term=="us military"~"dod",
# search_term=="us national institute of allergy and infectious diseases"~"nih",
# search_term==" us forest service"~"usda",
# search_term==" us aid"~"state",
# search_term==" us department of defence"~"dod",
# search_term==" us walter reed"~"dod",
# search_term==" us mission"~"state",
# search_term==" us bureau of land management"~"interior",
# search_term==" us antarctic program"~"nsf",
# search_term==" niaid/nih international centers for excellence in "~"nih",
# search_term==" us centers for disease control"~"cdc",
# .default = as.character(agency))
# )
#
#
#
#
#
#
#
# str_detect(affiliation, "usda")) %>%
#   str_detect(affiliation, "us ")) %>%
#   str_detect(affiliation, "army")) %>%
#   str_detect(affiliation, "army")) %>%
#   str_detect(affiliation, "noaa")) %>%
#   str_detect(affiliation, "air force")) %>%
#   str_detect(affiliation, "bureau of land management")) %>%
#   str_detect(affiliation, "cdc")) %>%
#   str_detect(affiliation, "national cancer institute")) %>%
#   str_detect(affiliation, "usaid")) %>%
#   str_detect(affiliation, "usgs")) %>%
#   str_detect(affiliation, "usfws")) %>%
#   str_detect(affiliation, "veterans administration")) %>%
#   str_detect(affiliation, "veterans health administration")) %>%
#   str_detect(affiliation, "vha")) %>%
#   str_detect(affiliation, "centers for disease control and prevention")) %>%
#   str_detect(affiliation, "national")) %>%
#   select(affil_id)
#
# false_fed_positives2<-scopus_affils_df %>%
#   str_detect(affiliation, "usda")) %>%
#   # str_detect(affiliation, "us ")) %>%
#   str_detect(affiliation, "army")) %>%
#   str_detect(affiliation, "us ")) %>%
#   str_detect(affiliation, "national weather service")) %>%
#   str_detect(affiliation, "usaf")) %>%
#   str_detect(affiliation, "noaa")) %>%
#   str_detect(affiliation, "usepa")) %>%
#   str_detect(affiliation, "air force")) %>%
#   str_detect(affiliation, "veteran affairs health")) %>%
#   str_detect(affiliation, "veteran affairs")) %>%
#   str_detect(affiliation, "82nd airborne")) %>%
#   filter(!str_detect(affiliation, "cber")) %>%
#   filter(!str_detect(affiliation, "coast guard")) %>%
#   filter(!str_detect(affiliation, "ahrq")) %>%
#   filter(!str_detect(affiliation, "national academy")) %>%
#   filter(!str_detect(affiliation, "center for biologics evaluation")) %>%
#   filter(!str_detect(affiliation, "department of defense")) %>%
#   filter(!str_detect(affiliation, "national museum")) %>%
#   filter(!str_detect(affiliation, "administration")) %>%
#   filter(!str_detect(affiliation, "affiliation for")) %>%
#   filter(!str_detect(affiliation, "bureau of land management")) %>%
#   filter(!str_detect(affiliation, "smithsonian")) %>%
#   filter(!str_detect(affiliation, "military")) %>%
#   filter(!str_detect(affiliation, "nasa")) %>%
#   filter(!str_detect(affiliation, "national park")) %>%
#   filter(!str_detect(affiliation, "national laboratory")) %>%
#   filter(!str_detect(affiliation, "national wildlife refuge")) %>%
#   filter(!str_detect(affiliation, "cdc")) %>%
#   filter(!str_detect(affiliation, "national cancer institute")) %>%
#   filter(!str_detect(affiliation, "usaid")) %>%
#   filter(!str_detect(affiliation, "usda")) %>%
#   filter(!str_detect(affiliation, "usgs")) %>%
#   filter(!str_detect(affiliation, "usfws")) %>%
#   filter(!str_detect(affiliation, "veterans administration")) %>%
#   filter(!str_detect(affiliation, "veterans health administration")) %>%
#   filter(!str_detect(affiliation, "vha")) %>%
#   filter(!str_detect(affiliation, "us department")) %>%
#   filter(!str_detect(affiliation, "federal")) %>%
#   filter(!str_detect(affiliation, "affiliation")) %>%
#   filter(!str_detect(affiliation, " national ")) %>%
#   filter(!str_detect(affiliation, "uniformed services")) %>%
#   filter(!str_detect(affiliation, "nsf")) %>%
#   filter(!str_detect(affiliation, "nih")) %>%
#   filter(!str_detect(affiliation, "national institute")) %>%
#   filter(!str_detect(affiliation, "national research council")) %>%
#   filter(!str_detect(affiliation, "national solar")) %>%
#   filter(!str_detect(affiliation, "national zoo")) %>%
#   filter(!str_detect(affiliation, "fbi ")) %>%
#   filter(!str_detect(affiliation, "doe")) %>%
#   filter(!str_detect(affiliation, "naval")) %>%
#   filter(!str_detect(affiliation, "national library")) %>%
#   filter(!str_detect(affiliation, "national museum")) %>%
#   filter(!str_detect(affiliation, "national center")) %>%
#   filter(!str_detect(affiliation, "medical group")) %>%
#   filter(!str_detect(affiliation, "combat")) %>%
#   filter(!str_detect(affiliation, "ames laboratory")) %>%
#   filter(!str_detect(affiliation, "armed forces")) %>%
#   filter(!str_detect(affiliation, "nci ")) %>%
#   filter(!str_detect(affiliation, "afit ")) %>%
#   filter(!str_detect(affiliation, "wrair")) %>%
#   filter(!str_detect(affiliation, "niaaa")) %>%
#   filter(!str_detect(affiliation, "national eye institute")) %>%
#   filter(!str_detect(affiliation, "national fish and wildlife refuge")) %>%
#   filter(!str_detect(affiliation, "nhlbi")) %>%
#   filter(!str_detect(affiliation, "niaid")) %>%
#   filter(!str_detect(affiliation, "niams")) %>%
#   filter(!str_detect(affiliation, "nichd")) %>%
#   filter(!str_detect(affiliation, "nida")) %>%
#   filter(!str_detect(affiliation, "nimh")) %>%
#   filter(!str_detect(affiliation, "niosh")) %>%
#   filter(!str_detect(affiliation, "nist")) %>%
#   filter(!str_detect(affiliation, "nmrc")) %>%
#   filter(!str_detect(affiliation, "oak ridge lab")) %>%
#   filter(!str_detect(affiliation, "usace")) %>%
#   filter(!str_detect(affiliation, "patuxent wildlife research center")) %>%
#   filter(!str_detect(affiliation, "national heart, lung, and blood institute")) %>%
#   filter(!str_detect(affiliation, "national high magnetic field laboratory los almos")) %>%
#   filter(!str_detect(affiliation, "national human genome research institute")) %>%
#   filter(!str_detect(affiliation, "national human genome research institute")) %>%
#   filter(!str_detect(affiliation, "nhgri")) %>%
#   filter(!str_detect(affiliation, "national hurricane center")) %>%
#   filter(!str_detect(affiliation, "national immunization program")) %>%
#   filter(!str_detect(affiliation, "national intelligence council")) %>%
#   filter(!str_detect(affiliation, "national invasive species council")) %>%
#   filter(!str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
#   filter(!str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
#   filter(!str_detect(affiliation, "national gallery of art")) %>%
#   filter(!str_detect(affiliation, "national guard")) %>%
#   filter(!str_detect(affiliation, "national radio ")) %>%
#   filter(!str_detect(affiliation, "national renewable")) %>%
#   filter(!str_detect(affiliation, "national science board")) %>%
#   filter(!str_detect(affiliation, "national science foundation")) %>%
#   filter(!str_detect(affiliation, "national security council")) %>%
#   filter(!str_detect(affiliation, "national severe storms laboratory norman")) %>%
#   filter(!str_detect(affiliation, "national socio-environmental synthesis center")) %>%
#   filter(!str_detect(affiliation, "sesync")) %>%
#   filter(!str_detect(affiliation, "incident command system")) %>%
#   filter(!str_detect(affiliation, "national toxicology program")) %>%
#   filter(!str_detect(affiliation, "national toxicology program laboratory")) %>%
#   filter(!str_detect(affiliation, "national tropical botanical garden")) %>%
#   filter(!str_detect(affiliation, "national war college")) %>%
#   filter(!str_detect(affiliation, "national wetlands research center")) %>%
#   filter(!str_detect(affiliation, "national wetlands research center")) %>%
#   filter(!str_detect(affiliation, "natl research council associate")) %>%
#   filter(!str_detect(affiliation, "ncbi/nlm")) %>%
#   filter(!str_detect(affiliation, "nci-frederick laboratory animal sciences program")) %>%
#   filter(!str_detect(affiliation, "nhlbi")) %>%
#   filter(!str_detect(affiliation, "nhgri")) %>%
#   filter(!str_detect(affiliation, "national health and environmental effects research laboratory")) %>%
#   filter(!str_detect(affiliation, "hrsa")) %>%
#   filter(!str_detect(affiliation, "hurricane flood risk reduction design branch")) %>%
#   filter(!str_detect(affiliation, "jet propulsion laboratory")) %>%
#   filter(!str_detect(affiliation, "national atmospheric deposition program")) %>%
#   filter(!str_detect(affiliation, "nad")) %>%
#   filter(!str_detect(affiliation, "epa region")) %>%
#   filter(!str_detect(affiliation, "erdc")) %>%
#   filter(!str_detect(affiliation, "national defense")) %>%
#   filter(!str_detect(affiliation, "national ecological observatory network")) %>%
#   filter(!str_detect(affiliation, "national endowment for the arts")) %>%
#   filter(!str_detect(affiliation, "national energy technology laboratory")) %>%
#
# mutate(agency=if_else(str_detect(affiliation,"army research laboratory"),"dod",agency))
#
#
#
#
#
# filter(scopus_affil_id!="125727849")
# 60017395
# 60011638
# 106181164
# 101592717
#
#
#
#
#   # DO SOME CORRECTIONS
#   mutate(search_term = case_when(
#     affiliation == "victus ars inc" ~ NA,
#     affiliation == "cto of docbox, inc" ~ NA,
#     affiliation == "cto of docbox, inc" ~ NA,
#     affiliation == "community-based organization partners (cbop) and community ethics review board (cerb)" ~ NA,
#     affiliation == "cdc foundation" ~ NA,
#     affiliation == "heilongjiang provincial cdc" ~ NA,
#     affiliation == "dpi/fdacs"~NA,
#     .default = as.character(search_term)
#   ))
#
# #
# search_terms_intl<-data.frame(search_term=c("niaid/nih international centers for excellence in research",
#                                             "usaid",
#                                             "us aid ",
#                                             "us antarctic program",
#                                             "us army",
#                                             "us bureau of land management",
#                                             "us centers for disease control",
#                                             "us cdc",
#                                             "us consulate",
#                                             "us department of agriculture",
#                                             "us department of defense",
#                                             "us department of defence",
#                                             "us department of veteran",
#                                             "us embassy",
#                                             "us environmental protection affiliation",
#                                             "us forces",
#                                             "us forest service",
#                                             "us geological survey",
#                                             "us marine corp",
#                                             "us military",
#                                             "us mission",
#                                             "us national institute of allergy and infectious diseases",
#                                             "us naval",
#                                             "us navy",
#                                             "us of america affiliation for international development",
#                                             "us office of naval",
#                                             "us president",
#                                             "us walter reed",
#                                             "usda-ars",
#                                             "usda",
#                                             "us peace corps",
#                                             "us cdc",
#                                             "us 8th army",
#                                             "us affiliation for international development"))
#
#   filter(!str_detect(affiliation, pattern = "ihesp")) %>%
#   filter(!str_detect(affiliation, pattern = "iss national")) %>%
# filter(!str_detect(affiliation, pattern = "international space station")) %>%
#   filter(!str_detect(affiliation, pattern = "galveston national laboratory"))
#
# pull_to_fed_affils<-bind_rows(pull_to_fed_affils,pull_to_fed_affils2)
# rm(pull_to_fed_affils2)
#
# pull_to_fed_affils<-pull_to_fed_affils %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national lab"),"doe",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national historical park"),"interior",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national monument"),"interior",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "fish &amp; wildlife"),"interior",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "fredrick"),"nih",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "frederick"),"nih",search_term)) %>%
#
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national telecommunications and information administration"),"commerce",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "the us national archives and records administration"),"eop",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national ecological observatory network"),"nsf",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national institute for mathematical and biological synthesis"),"nsf",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national evolutionary synthesis center"),"nsf",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "national socio-environmental synthesis center"),"nsf",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "office of national drug control policy"),"eop",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "office of national drug control policy"),"eop",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "frederick"),"nih",search_term)) %>%
#
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "us department of interior"),"interior",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "us department of agriculture"),"usda",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "us department of agricultural"),"usda",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "us affiliation for internationl development"),"usaid",search_term)) %>%
#   mutate(search_term=if_else(str_detect(affiliation, pattern = "geológico survey"),"interior",search_term))
#
# affils_df2_to_fix %>%
# false_fed_positives1<-scopus_affils_df %>%
#   filter(str_detect(affiliation, paste(state_names$value, collapse = "|"))) %>%
#   filter(!str_detect(affiliation, "usda")) %>%
#   filter(!str_detect(affiliation, "us ")) %>%
#   filter(!str_detect(affiliation, "army")) %>%
#   filter(!str_detect(affiliation, "army")) %>%
#   filter(!str_detect(affiliation, "noaa")) %>%
#   filter(!str_detect(affiliation, "air force")) %>%
#   filter(!str_detect(affiliation, "bureau of land management")) %>%
#   filter(!str_detect(affiliation, "cdc")) %>%
#   filter(!str_detect(affiliation, "national cancer institute")) %>%
#   filter(!str_detect(affiliation, "usaid")) %>%
#   filter(!str_detect(affiliation, "usgs")) %>%
#   filter(!str_detect(affiliation, "usfws")) %>%
#   filter(!str_detect(affiliation, "veterans administration")) %>%
#   filter(!str_detect(affiliation, "veterans health administration")) %>%
#   filter(!str_detect(affiliation, "vha")) %>%
#   filter(!str_detect(affiliation, "centers for disease control and prevention")) %>%
#   filter(!str_detect(affiliation, "national")) %>%
#   select(affil_id)
#
#   false_fed_positives2<-scopus_affils_df %>%
#   filter(!str_detect(affiliation, "usda")) %>%
#   # filter(!str_detect(affiliation, "us ")) %>%
#   filter(!str_detect(affiliation, "army")) %>%
#   filter(!str_detect(affiliation, "us ")) %>%
#   filter(!str_detect(affiliation, "national weather service")) %>%
#   filter(!str_detect(affiliation, "usaf")) %>%
#   filter(!str_detect(affiliation, "noaa")) %>%
#   filter(!str_detect(affiliation, "usepa")) %>%
#   filter(!str_detect(affiliation, "air force")) %>%
#   filter(!str_detect(affiliation, "veteran affairs health")) %>%
#   filter(!str_detect(affiliation, "veteran affairs")) %>%
#   filter(!str_detect(affiliation, "82nd airborne")) %>%
#   filter(!str_detect(affiliation, "cber")) %>%
#   filter(!str_detect(affiliation, "coast guard")) %>%
#   filter(!str_detect(affiliation, "ahrq")) %>%
#   filter(!str_detect(affiliation, "national academy")) %>%
#   filter(!str_detect(affiliation, "center for biologics evaluation")) %>%
#   filter(!str_detect(affiliation, "department of defense")) %>%
#   filter(!str_detect(affiliation, "national museum")) %>%
#   filter(!str_detect(affiliation, "administration")) %>%
#   filter(!str_detect(affiliation, "affiliation for")) %>%
#   filter(!str_detect(affiliation, "bureau of land management")) %>%
#   filter(!str_detect(affiliation, "smithsonian")) %>%
#   filter(!str_detect(affiliation, "military")) %>%
#   filter(!str_detect(affiliation, "nasa")) %>%
#   filter(!str_detect(affiliation, "national park")) %>%
#   filter(!str_detect(affiliation, "national laboratory")) %>%
#   filter(!str_detect(affiliation, "national wildlife refuge")) %>%
#   filter(!str_detect(affiliation, "cdc")) %>%
#   filter(!str_detect(affiliation, "national cancer institute")) %>%
#   filter(!str_detect(affiliation, "usaid")) %>%
#   filter(!str_detect(affiliation, "usda")) %>%
#   filter(!str_detect(affiliation, "usgs")) %>%
#   filter(!str_detect(affiliation, "usfws")) %>%
#   filter(!str_detect(affiliation, "veterans administration")) %>%
#   filter(!str_detect(affiliation, "veterans health administration")) %>%
#   filter(!str_detect(affiliation, "vha")) %>%
#   filter(!str_detect(affiliation, "us department")) %>%
#   filter(!str_detect(affiliation, "federal")) %>%
#   filter(!str_detect(affiliation, "affiliation")) %>%
#   filter(!str_detect(affiliation, " national ")) %>%
#   filter(!str_detect(affiliation, "uniformed services")) %>%
#   filter(!str_detect(affiliation, "nsf")) %>%
#   filter(!str_detect(affiliation, "nih")) %>%
#   filter(!str_detect(affiliation, "national institute")) %>%
#   filter(!str_detect(affiliation, "national research council")) %>%
#   filter(!str_detect(affiliation, "national solar")) %>%
#   filter(!str_detect(affiliation, "national zoo")) %>%
#   filter(!str_detect(affiliation, "fbi ")) %>%
#   filter(!str_detect(affiliation, "doe")) %>%
#   filter(!str_detect(affiliation, "naval")) %>%
#   filter(!str_detect(affiliation, "national library")) %>%
#   filter(!str_detect(affiliation, "national museum")) %>%
#   filter(!str_detect(affiliation, "national center")) %>%
#   filter(!str_detect(affiliation, "medical group")) %>%
#   filter(!str_detect(affiliation, "combat")) %>%
#   filter(!str_detect(affiliation, "ames laboratory")) %>%
#   filter(!str_detect(affiliation, "armed forces")) %>%
#   filter(!str_detect(affiliation, "nci ")) %>%
#   filter(!str_detect(affiliation, "afit ")) %>%
#   filter(!str_detect(affiliation, "wrair")) %>%
#   filter(!str_detect(affiliation, "niaaa")) %>%
#   filter(!str_detect(affiliation, "national eye institute")) %>%
#   filter(!str_detect(affiliation, "national fish and wildlife refuge")) %>%
#   filter(!str_detect(affiliation, "nhlbi")) %>%
#   filter(!str_detect(affiliation, "niaid")) %>%
#   filter(!str_detect(affiliation, "niams")) %>%
#   filter(!str_detect(affiliation, "nichd")) %>%
#   filter(!str_detect(affiliation, "nida")) %>%
#   filter(!str_detect(affiliation, "nimh")) %>%
#   filter(!str_detect(affiliation, "niosh")) %>%
#   filter(!str_detect(affiliation, "nist")) %>%
#   filter(!str_detect(affiliation, "nmrc")) %>%
#   filter(!str_detect(affiliation, "oak ridge lab")) %>%
#   filter(!str_detect(affiliation, "usace")) %>%
#   filter(!str_detect(affiliation, "patuxent wildlife research center")) %>%
#   filter(!str_detect(affiliation, "national heart, lung, and blood institute")) %>%
#   filter(!str_detect(affiliation, "national high magnetic field laboratory los almos")) %>%
#   filter(!str_detect(affiliation, "national human genome research institute")) %>%
#   filter(!str_detect(affiliation, "national human genome research institute")) %>%
#   filter(!str_detect(affiliation, "nhgri")) %>%
#   filter(!str_detect(affiliation, "national hurricane center")) %>%
#   filter(!str_detect(affiliation, "national immunization program")) %>%
#   filter(!str_detect(affiliation, "national intelligence council")) %>%
#   filter(!str_detect(affiliation, "national invasive species council")) %>%
#   filter(!str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
#   filter(!str_detect(affiliation, "national oceanic/atmospheric admin")) %>%
#   filter(!str_detect(affiliation, "national gallery of art")) %>%
#   filter(!str_detect(affiliation, "national guard")) %>%
#   filter(!str_detect(affiliation, "national radio ")) %>%
#   filter(!str_detect(affiliation, "national renewable")) %>%
#   filter(!str_detect(affiliation, "national science board")) %>%
#   filter(!str_detect(affiliation, "national science foundation")) %>%
#   filter(!str_detect(affiliation, "national security council")) %>%
#   filter(!str_detect(affiliation, "national severe storms laboratory norman")) %>%
#   filter(!str_detect(affiliation, "national socio-environmental synthesis center")) %>%
#   filter(!str_detect(affiliation, "sesync")) %>%
#   filter(!str_detect(affiliation, "incident command system")) %>%
#   filter(!str_detect(affiliation, "national toxicology program")) %>%
#   filter(!str_detect(affiliation, "national toxicology program laboratory")) %>%
#   filter(!str_detect(affiliation, "national tropical botanical garden")) %>%
#   filter(!str_detect(affiliation, "national war college")) %>%
#   filter(!str_detect(affiliation, "national wetlands research center")) %>%
#   filter(!str_detect(affiliation, "national wetlands research center")) %>%
#   filter(!str_detect(affiliation, "natl research council associate")) %>%
#   filter(!str_detect(affiliation, "ncbi/nlm")) %>%
#   filter(!str_detect(affiliation, "nci-frederick laboratory animal sciences program")) %>%
#   filter(!str_detect(affiliation, "nhlbi")) %>%
#   filter(!str_detect(affiliation, "nhgri")) %>%
#   filter(!str_detect(affiliation, "national health and environmental effects research laboratory")) %>%
#   filter(!str_detect(affiliation, "hrsa")) %>%
#   filter(!str_detect(affiliation, "hurricane flood risk reduction design branch")) %>%
#   filter(!str_detect(affiliation, "jet propulsion laboratory")) %>%
#   filter(!str_detect(affiliation, "national atmospheric deposition program")) %>%
#   filter(!str_detect(affiliation, "nad")) %>%
#   filter(!str_detect(affiliation, "epa region")) %>%
#   filter(!str_detect(affiliation, "erdc")) %>%
#   filter(!str_detect(affiliation, "national defense")) %>%
#   filter(!str_detect(affiliation, "national ecological observatory network")) %>%
#   filter(!str_detect(affiliation, "national endowment for the arts")) %>%
#   filter(!str_detect(affiliation, "national energy technology laboratory")) %>%
#   filter(affil_id!="109507990") %>%
#   filter(affil_id!="112891175") %>%
#   filter(affil_id!="100312437") %>%
#   filter(affil_id!="60013346") %>%
#   filter(affil_id!="100475245") %>%
#   filter(affil_id!="107901133") %>%
#   filter(affil_id!="60151531") %>%
#   filter(affil_id!="101344572") %>%
#   filter(affil_id!="60076285") %>%
#   filter(affil_id!="100332158") %>%
#   filter(affil_id!="123502856") %>%
#   filter(affil_id!="102033117") %>%
#   filter(affil_id!="109562444") %>%
#   filter(affil_id!="124744686") %>%
#   filter(affil_id!="122676681") %>%
#   filter(affil_id!="114003101") %>%
#   filter(affil_id!="118416507") %>%
#   filter(affil_id!="123523957") %>%
#   filter(affil_id!="122461253") %>%
#   filter(affil_id!="121968104") %>%
#   filter(affil_id!="60090737") %>%
#   filter(affil_id!="108038436") %>%
#   filter(affil_id!="131447634") %>%
#   filter(affil_id!="60029965") %>%
#   filter(affil_id!="110325356") %>%
#   filter(affil_id!="124101950") %>%
#   filter(affil_id!="60013369") %>%
#   filter(affil_id!="60001662") %>%
#   filter(affil_id!="60020465") %>%
#   filter(affil_id!="100689964") %>%
#   filter(affil_id!="124534107") %>%
#   filter(affil_id!="60031735") %>%
#   filter(affil_id!="60021549") %>%
#   filter(affil_id!="112985829") %>%
#   filter(affil_id!="60029698") %>%
#   filter(affil_id!="100489231") %>%
#   filter(affil_id!="126924761") %>%
#   filter(affil_id!="130780665") %>%
#   filter(affil_id!="106700961") %>%
#   filter(affil_id!="60008780") %>%
#   filter(affil_id!="130475198") %>%
#   filter(affil_id!="130475119") %>%
#   filter(affil_id!="112700141") %>%
#   filter(affil_id!="100966017") %>%
#   filter(affil_id!="101356352") %>%
#   filter(affil_id!="101619617") %>%
#   filter(!str_detect(affiliation, "centers for disease control and prevention")) %>%
#   select(affil_id)
#
#
# false_fed_positives3<-tibble(affil_id=c("129951829",
#                                         "60013443",
#                                         "101967749",
#                                         "120290071",
#                                         "109520920",
#                                         "110141225",
#                                     "132228870",
#                                     "106977950",
#                                     "112703830",
#                                     "131734604",
#                                     "131990960",
#                                     "119704201",
#                                     "129167163",
#                                     "128902945",
#                                     "60019853",
#                                     "114043849",
#                                     "101469376",
#                                     "100951187",
#                                     "126500206",
#                                     "107136275",
#                                     "122460076",
#                                     "60098179",
#                                     "122503316",
#                                     "131197678",
#                                     "60006107",
#                                     "129770980",
#                                     "130644180",
#                                     "100951187",
#                                     "128785382",
#                                     "130621169",
#                                     "112578573",
#                                     "123813980",
#                                     "100318446",
#                                     "113647404",
#                                     "120666603",
#                                     "60030891",
#                                     "60110648",
#                                     "130101353",
#                                     "120662121"
#                                     ))
#
# false_fed_positives4<-scopus_affils_df %>%
#   filter(str_detect(affiliation, ("college|university"))) %>%
#   filter(!str_detect(affiliation, "air force")) %>%
#   filter(!str_detect(affiliation, "war ")) %>%
#   filter(!str_detect(affiliation, "defense university")) %>%
#   filter(!str_detect(affiliation, "uniformed services university")) %>%
#   filter(!str_detect(affiliation, "us army")) %>%
#   filter(!str_detect(affiliation, "us marine corps")) %>%
#   filter(!str_detect(affiliation, "us geological")) %>%
#   filter(!str_detect(affiliation, "noaa")) %>%
#   filter(!str_detect(affiliation, "usaf")) %>%
#   filter(!str_detect(affiliation, "usda")) %>%
#   select(affil_id)
#
# # contract to EPA
# # "121898177" "oak ridge association university student services contractor to us environmental protection affiliation"
#
# false_fed_positives<-bind_rows(false_fed_positives1,
#                                false_fed_positives2,
#                                false_fed_positives3,
#                                false_fed_positives4)
#
#
# pubs_with_fed <- pubs_with_fed %>%
#   mutate(fed_author=if_else(affil_id%in%false_fed_positives$affil_id,FALSE,fed_author)) %>%
#   mutate(affiliation=if_else(fed_author==FALSE,NA,agency)) %>%
#   mutate(unit=if_else(fed_author==FALSE,NA,unit)) %>%
#   mutate(str_detect=if_else(fed_author==FALSE,NA,str_detect)) %>%
#   mutate(search_term=if_else(fed_author==FALSE,NA,search_term))
#
#
#
# # fixing false negatives --------------------------------------------------
#
# # labeled as not fed but are
#
#
# # IS fed_authors
#
# fed_affils<-c(
#   "108382367",
#   "125244852",
#   "122988512",
#   "100502481",
#   "100571977",
#   "100822854",
#   "115752362",
#   "121212875",
#   "122286572",
#   "129973323",
#   "130518547",
#   "130701994",
#   "130892705",
#   "132182075",
#   "60008492",
#   "100335911",
#   "101792179",
#   "106453365",
#   "109347134",
#   "112920600",
#   "112922559",
#   "120168333",
#   "123502999",
#   "100677110",
#   "113820147",
#   "125383427",
#   "128533828",
#   "101140478",
#   "128172361",
#   "130391142",
#   "123678203",
#   "123865204",
#   "107986893",
#   "122628604",
#   "130148658",
#   "112602793",
#   "113013369",
#   "125340513",
#   "125381761",
#   "125755037",
#   "131324608",
#   "131324998",
#   "100316021",
#   "101000910",
#   "123141776",
#   "130557216",
#   "60071501",
#   "100587655",
#   "101394353",
#   "106456794",
#   "108578096",
#   "114159234",
#   "128149737",
#   "115539127",
#   "123896933",
#   "126381052",
#   "120532431",
#   "109683782",
#   "129477382",
#   "129282179",
#   "112927977",
#   "114921497",
#   "123871342",
#   "60105857",
#   "60011058",
#   "100349009",
#   "101085594",
#   "112177733",
#   "121624708",
#   "128217908",
#   "60004768",
#   "112883629",
#   "60105856",
#   "114520379",
#   "113057100",
#   "107034880",
#   "122400567",
#   "123807703",
#   "124701231",
#   "114385940",
#   "60009511",
#   "122096438",
#   "60022556",
#   "114983183",
#   "100968866",
#   "114317889",
#   "127833144",
#   "112914080",
#   "105216627",
#   "60085690",
#   "107713456",
#   "60105938",
#   "128137528",
#   "112936325",
#   "127303149",
#   "60014232",
#   "118884567",
#   "123739119",
#   "108788235",
#   "106637561",
#   "118122905",
#   "60023621",
#   "60022482",
#   "107064832",
#   "105557510",
#   "110183199",
#   "60009004",
#   "129777814",
#   "109516894",
#   "60021513",
#   "60105919",
#   "112967581",
#   "121322284",
#   "129171136",
#   "105265305",
#   "60105916",
#   "108109623",
#   "126447335",
#   "113873823",
#   "120943775",
#   "122480364",
#   "122547667",
#   "116873603",
#   "120943775",
#   "122480364",
#   "122547667",
#   "116873603",
#   "112793106",
#   "122565177",
#   "126476821",
#   "129650313",
#   "116422949",
#   "130278976",
#   "107868390",
#   "60079887",
#   "114301940",
#   "60029074",
#   "60000342",
#   "124218141",
#   "126781328",
#   "60028084",
#   "112586877",
#   "108668766",
#   "127726999",
#   "117121465",
#   "129050707",
#   "126413105",
#   "60105920",
#   "112775938",
#   "60105926",
#   "60008908",
#   "60028851",
#   "129304473",
#   "60110851",
#   "121650040",
#   "115337507",
#   "100986075",
#   "126678908",
#   "109412620",
#   "113136311",
#   "113142943",
#   "115242479",
#   "127191778",
#   "112645758",
#   "124221191",
#   "114875913",
#   "60276988",
#   "101014876",
#   "113184837",
#   "113182535",
#   "60031866",
#   "112833665",
#   "112755184",
#   "127279491",
#   "123885611",
#   "112868925",
#   "109959576",
#   "122914561",
#   "60019743",
#   "118159692",
#   "106235883",
#   "106701315",
#   "112689669",
#   "100484256",
#   "60112315",
#   "60018468",
#   "60002320",
#   "101578321",
#   "105747226",
#   "126076606",
#   "131400196",
#   "129855621",
#   "60005951",
#   "126775312",
#   "60029545",
#   "130435610",
#   "121430097",
#   "60020147",
#   "125607176",
#   "118597046",
#   "110112058",
#   "124376606",
#   "131227607",
#   "131607847",
#   "130891703",
#   "100534504",
#   "129047260",
#   "100433566",
#   "126581661",
#   "100526928",
#   "130729587",
#   "130729535",
#   "129703203",
#   "101773114",
#   "128109779",
#   "128064446",
#   "123497681",
#   "130730801",
#   "131607492",
#   "127268406",
#   "128109745",
#   "126965506",
#   "128346477",
#   "129673523",
#   "128109746",
#   "101941425",
#   "126581146",
#   "126249725",
#   "127970991",
#   "131305851",
#   "128872316",
#   "124376686",
#   "127941504",
#   "126665837",
#   "130730423",
#   "131056078",
#   "131607877",
#   "124599864",
#   "115235109",
#   "126553855",
#   "122597934",
#   "131418733",
#   "127308876",
#   "128408529",
#   "126200849",
#   "126911802",
#   "127268436",
#   "100533221",
#   "130702026",
#   "120532431",
#   "107910127",
#   "130518547",
#   "60013443",
#   "108382367",
#   "130759815",
#   "106773571",
#   "116525328",
#   "101394353",
#   "101792179",
#   "107986893",
#   "100316021",
#   "106453365",
#   "112922559",
#   "101140478",
#   "109347134",
#   "100587655",
#   "60008492",
#   "106456794",
#   "113820147",
#   "125244852",
#   "113216252",
#   "128262913",
#   "123865204",
#   "112627589",
#   "106602485",
#   "130557216",
#   "60032692",
#   "122988512",
#   "127062422",
#   "121225521",
#   "108578096",
#   "101000910",
#   "112602793",
#   "113112328",
#   "107573430",
#   "115494035",
#   "125755037",
#   "125381761",
#   "123141776",
#   "112706380",
#   "112625941",
#   "123502999",
#   "108888250",
#   "130892705",
#   "115752362",
#   "130701994",
#   "129973323",
#   "124470182",
#   "130122371",
#   "129289059",
#   "131076240",
#   "127374290",
#   "128395574",
#   "131531341",
#   "132149803",
#   "128906596",
#   "114633975",
#   "60278767",
#   "112740566",
#   "112924517",
#   "125379120",
#   "130723823",
#   "117646342",
#   "132182075",
#   "100335911",
#   "131324608",
#   "131324998",
#   "125340513",
#   "100677110",
#   "125383427",
#   "114159234",
#   "128149737",
#   "128533828",
#   "120168333",
#   "113718737",
#   "121955902",
#   "129049833",
#   "100502481",
#   "122286572",
#   "126395319",
#   "121212875",
#   "128306628",
#   "131558906",
#   "125763856",
#   "125364545",
#   "113119893",
#   "118439805",
#   "124601958",
#   "107062094",
#   "113951892",
#   "123822264",
#   "128757432",
#   "131466439",
#   "101261119",
#   "101265211",
#   "131511930",
#   "113007889",
#   "114684115",
#   "115867250",
#   "112587340",
#   "60012281",
#   "106548849",
#   "110545556",
#   "121897786",
#   "125786644",
#   "131254575",
#   "112949764",
#   "107980732",
#   "113860189",
#   "121466434",
#   "122411044",
#   "60014521",
#   "106585504",
#   "109248263",
#   "112905068",
#   "107089434",
#   "129839931",
#   "125919619",
#   "113212209",
#   "100364127",
#   "112994777",
#   "112637847",
#   "100332619",
#   "117974668",
#   "129449620",
#   "129132280",
#   "128165491",
#   "101004622",
#   "60002223",
#   "112987892",
#   "60105859",
#   "126765651",
#   "107025178",
#   "115341424",
#   "118903785",
#   "123301448",
#   "123300875",
#   "131254594",
#   "131254565",
#   "131254588",
#   "131254570",
#   "126999016",
#   "129518085",
#   "112843093",
#   "112910426",
#   "105621128",
#   "113197759",
#   "60105937",
#   "131554680",
#   "129184086",
#   "131119214",
#   "112587380",
#   "122995724",
#   "119743592",
#   "60105918",
#   "60004786",
#   "112234684",
#   "101581898",
#   "130434719",
#   "114586585",
#   "109494186",
#   "109911421",
#   "114306072",
#   "115168483",
#   "105745097",
#   "112656582",
#   "119639166",
#   "105333223",
#   "130217062",
#   "112805663",
#   "105424804",
#   "113069894",
#   "114786642",
#   "125701945",
#   "100988589",
#   "128210866",
#   "111235657",
#   "118700423",
#   "112117183",
#   "131644549",
#   "130647362",
#   "125155173",
#   "110646347",
#   "110271622",
#   "128941071",
#   "120477769",
#   "126413640",
#   "101964606",
#   "121518184",
#   "107852606",
#   "114455318",
#   "113187369",
#   "125781615",
#   "126907053",
#   "101516366",
#   "108095235",
#   "123934640",
#   "60073942",
#   "100822854")
#
#
# fed_affils<-tibble(fed_affils)
#
#
#
#
#
#
# # VETERANS
# # veterans health care system of the ozarks
#
#
#
# # Scopus affiliation numbers
# scopus_affils_df_NOTFED<-pubs_with_fed %>%
#   filter(fed_author==FALSE) %>%
#   select(affil_id, country,affiliation) %>%
#   distinct()
#
#
#
# false_fed_negatives1 <- scopus_affils_df_NOTFED %>%
#   filter(country=="usa" & str_detect(affiliation, paste(fed_affils_name_with_usa, collapse = "|")))
#
#
# false_fed_negatives2 <- scopus_affils_df_NOTFED %>%
#   filter(str_detect(affiliation, paste(fed_affils_full_noUSA, collapse = "|")))
#
#
#
# false_fed_negatives3 <- scopus_affils_df_NOTFED %>%
#   filter(affil_id %in% fed_affils$fed_affils)
#
# false_fed_negatives<-bind_rows(false_fed_negatives1,
#                                false_fed_negatives2,
#                                false_fed_negatives3) %>%
#   distinct()
#
#
#
# pubs_with_fed <- pubs_with_fed %>%
#   mutate(fed_author = if_else(affil_id %in% false_fed_negatives$affil_id, TRUE, fed_author))
