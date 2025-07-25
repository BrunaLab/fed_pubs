ID_fed_affiliations <- function(affils_df) {
  # standardize COUNTRIES -----------------------------------------------
  levels(as.factor(affils_df$country))
 
 

  affils_df <- affils_df %>%
    mutate(country = recode(country,
      "united states" = "usa",
      "virgin islands (u.s.)" = "us virgin islands",
      "viet nam" = "vietnam",
      "cote d'ivoire" = "ivory coast",
      "timor-leste" = "east timor"
    ))


  # 2x all operations center
  # cava de' tirreni aou s giovanni di dio e ruggiero d'arago
  # u s president’s malaria initiative evolve project nigeria
  
  # affil_dupes<-fed_affils %>% group_by(affil_id,agency_short) %>% tally() %>% arrange(desc(n)) %>% filter(n>1) 
  # affil_dupes_all<-fed_affils %>% filter(affil_id%in%affil_dupes$affil_id) %>% distinct(affil_id, agency_short,.keep_all = TRUE)
  # 
  fed_affils <- read_csv("./data_clean/fed_affils_07102025.csv") %>%
    filter(!(affil_id==100413214 & agency_short=="hhs")) %>% 
    filter(!(affil_id==60093272 & agency_short=="dhs")) %>% 
    filter(!(affil_id==100413073 & (agency_short=="judiciary"|agency_short=="va"))) %>% 
    filter(!(affil_id==121770345 & agency_short=="judiciary")) %>% 
    
    filter(!(affil_id==114750014 & (agency_short=="hhs"|agency_short=="interior"))) %>% 
    filter(!(affil_id==122535121 & affiliation=="office of the assistant secretary for planning and evaluation")) %>% 
     
    mutate(affiliation=
             case_when(affil_id==100413214~"office of the assistant secretary of defense",
                       affil_id==60093272~"office of the assistant secretary of defense (health affairs)",
                       affil_id==100444200~"office of the deputy undersecretary of the army",
                       affil_id==122559931~"office of the national coordinator for health information technology",
                       affil_id==100727841~"office of the assistant secretary of defense (health affairs)",
                       affil_id==100594372~"office of the assistant secretary for planning and evaluation",
                       affil_id==122535121~"office of the assistant secretary for planning and evaluation",
                       affil_id==60000459~"office of management and budget",
                       affil_id==60033081~"office of policy analysis",
                       
                       .default = as.character(affiliation))) %>% 
    mutate(agency_short=
             case_when(affil_id==60028217~"epa",
                       affil_id==60093272~"dod",
                       affil_id==100727841~"dod",
                       affil_id==122535121~"hhs",
                       affil_id==60000459~"hhs",
                       affil_id==60033081~"interior",
                       
                       .default = as.character(agency_short))) %>% 
    
    
    
    
    
    arrange(affil_id, affiliation,agency_short, city, country, acronym) %>% 
    distinct(affil_id, agency_short,.keep_all = TRUE) 
    
  fed_affils<-fed_affils %>% select(affil_id,agency_short,city,country,federal)
  
  # fed_affils %>% group_by(affil_id,agency_short) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)
  
  nonfed_affils <- read_csv("./data_clean/NONFED_affils_07102025.csv") %>% 
    distinct()
  
  affils_df$affil_id <- as.numeric(affils_df$affil_id)
 
 
  affils_df$affil_id <- as.character(affils_df$affil_id)
  fed_affils$affil_id <- as.character(fed_affils$affil_id)
  affils_df <- left_join(affils_df, fed_affils, by = "affil_id")



  affils_df$affil_check <- affils_df$country.x == affils_df$country.y

  affils_df <- affils_df %>%
    mutate(country.y = if_else((affil_check == FALSE | is.na(affil_check)), country.y, NA)) %>%
    mutate(country.x = if_else(is.na(country.x), country.y, country.x)) %>%
    select(-country.y)

  affils_df$affil_check <- affils_df$city.x == affils_df$city.y

  affils_df <- affils_df %>%
    mutate(city.y = if_else((affil_check == FALSE | is.na(affil_check)), city.y, NA)) %>%
    mutate(city.x = if_else(is.na(city.x), city.y, city.x)) %>%
    select(-city.y)

  

  library(dplyr)
  library(stringr)

  # Define all replacements in a named vector
  replacements <- c(
    "&amp;amp;" = "and",
    "u s " = "us ",
    "united states " = "us ",
    "americorps vista" = "americorps",
    "u\\.s\\. " = "us ",
    "u\\. s\\. " = "us ",
    "\\." = "" # remove all periods
  )

  # Apply all replacements in one go
  affils_df <- affils_df %>%
    mutate(affiliation = str_replace_all(affiliation, replacements))


# 
#   affils_df$affil_check <- affils_df$affiliation.x == affils_df$affiliation.y
# 
#   affils_df <- affils_df %>%
#     mutate(affiliation.y = if_else((affil_check == FALSE | is.na(affil_check)), affiliation.y, NA)) %>%
#     mutate(affiliation.x = if_else(is.na(affiliation.x), affiliation.y, affiliation.x)) %>%
#     select(-affiliation.y)

  affils_df <- affils_df %>%
    rename(
      # affiliation = affiliation.x,
      country = country.x,
      city = city.x
      
    )

  affils_df <- affils_df %>%
    select(-affil_check)




  affils_df <- affils_df %>%
    mutate(agency_short = case_when(
      affil_id %in% c(
        112775938,131119053,118935998,123885611,128217908,130150084,118330540,
        115228508,116423594,131357366,129449620, 128137569, 120040598, 
        105736912, 128316397, 125952897, 112805663, 121072296, 112730052, 
        105477399, 100968866, 123254863, 121472110, 121382041,118575532, 
        118211212, 117458915, 113044510, 112967639, 112887996, 109557356, 
        101708480, 101532374, 127337699, 112893603, 101522669, 122233128, 
        113221153,122565152, 112568591, 101619277, 118206330, 128848258, 
        131254563, 131254559, 132421887, 122667304, 127632018, 115220797, 
        125807927, 112704723, 132434964, 132434964, 109505309, 109505309, 
        115385473, 113184770, 122498102, 109516894, 106929327, 106585504,
        124221191, 119639166, 114301940, 109911421, 113538038, 100886222, 
        112813607, 107070864, 114523236, 123693818, 114322561, 113018271, 
        126381674, 129979905, 106580709, 112669335, 117410704, 114456193, 
        125862540, 123665217, 114654204, 101972100, 121430097, 113171964, 
        101232419, 125849010, 126672045, 112234684, 125302977, 119847200, 
        108145124, 100764674, 130308845, 117356276, 123693690, 128684090, 
        128123367, 118657357, 112833665, 112670485, 131506759, 112868925, 
        130536578, 122243495, 116280683, 112911890, 124409139, 108181713, 
        126791257, 112955278, 112955278, 112967581, 115209092, 107773789, 
        128306669, 115003090, 113384350, 113345733, 114320094, 114270246, 
        129674417, 118657132, 129243419, 113197759, 114674838, 112647481, 
        106923974, 127864166, 105146112, 121989107, 128182625, 116593767, 
        117453904, 113007703, 114000323, 124285483, 116422949, 114442747, 
        127726999, 127726903, 127726892, 127726815, 127726735, 127726717,
        127726694, 117121465, 131478202, 121489108, 101641629, 118122905, 
        120135959, 105437969, 132383244, 106998694, 112994936, 130650522, 
        108699047, 101536146, 124532433, 128306891, 128123526, 108214510, 
        130621744, 123338971, 123338953, 113149283, 106707030, 129327605, 
        129318198, 113200931, 113023651, 130974414, 122221036, 130090966,
        129215697, 110525158, 128983928, 112587243, 109994213, 129650313, 
        121153711, 132533172, 131344873, 100678433, 119310297, 119310297, 
        120168487, 128241149,128726575, 100372859,128588933,
        119086596, 116522307, 118637327, 129236469, 113211184, 131950071, 
        112912153, 132475090, 102070450, 109640198, 127018884, 110031881, 
        106656485, 100387381,112941552, 120338449,116697494, 125868776,
        127580461, 106663664, 132431795, 106621898, 113114472, 112752240, 
        118022065, 107253064, 126426809, 129188352, 129865395, 127123981,
        131588681, 106379948, 128403318, 128403318, 131380914, 130095884, 
        127814565, 108171312, 121099507, 121773398, 100725217, 106984557,
        112648306, 126273729, 125628855, 106341010, 128444166, 129423473, 
        130718131, 129457503, 113144135, 110245040, 126626246, 112582935,
        106661173, 123341752, 112626061, 112635512, 120096391, 121697128, 
        112952363, 101823272, 107413093, 108341947, 120452456, 113823270, 
        118423712, 100620080,105885121, 114699239,113468202, 123897005,
        115972055, 125461327, 122875627, 113848867, 125520925, 125101833, 
        130285399, 101701694, 110915458, 112627278, 116452049, 112900864,
        114406258, 109168654, 116775161, 113000532, 112614222, 130132540, 
        128347842, 119221332, 124333534, 128497769, 123795863, 101069107,
        123924359, 122934559, 127905519, 127904778, 124122851, 128092351, 
        128084398, 122704640, 127013764, 112688333, 108062319, 101490818, 
        127657130, 126684569,112665895, 114721270,122040486, 130158995,
        130085442, 130084925, 118806977, 118661466, 102043815, 101786025, 
        112694640, 112702309, 108243819, 126672057, 121943653, 127418326, 
        112912020, 131329650, 131179893, 100533555, 113296020, 105478672, 
        113130842, 130837812, 100477351, 106969174, 101576389, 132184908,
        132434184, 132417343, 132417343, 113206441, 130454573, 107673059,
        105605863, 129587609, 131246092, 123625235, 123615351, 120076617, 
        121988696, 106657223,106644942, 132434184,119943502, 125582716,
        129963166, 121267960, 120575516, 115956000, 107323255, 112366927, 
        129899603, 128177152, 132524792, 126765566, 108468409, 112836578, 
        107061891, 120248788, 126647356, 113060643, 127310335, 108338060, 
        129973711, 127089255, 127631504, 112456046, 129646779, 123108259, 
        113261742, 122513041, 101480320, 126418410, 125336447, 125253311, 
        132305240, 132122123, 127586974, 128065395, 112930872, 128051819, 
        128387888, 125791023,109859472, 131642253,108444951, 112445788,
        129160431, 112940072, 132389249, 132389249, 112834500, 112834500,
        115560703, 126820526, 122958104, 122466475, 127637878, 117688024, 
        112363151, 111166218, 113081538, 129045481, 131948470, 132229247,
        131341658, 100538844, 109505181, 123123752, 125545011, 113198685, 
        128491187, 106709234, 106587413, 122597158, 132389675, 132389675, 
        120185586, 122864212, 129556304, 112746892, 112617455, 112760468,
        127828413, 122233128,112627278, 107064152,113333922, 127965231,
        113007703, 132670425, 131344873, 108062319, 131950071, 102070450, 
        132670499, 132618487, 100558018, 112886523, 115803040, 112991515, 
        130454573, 114320094, 121989107, 112936183, 127631504, 121390655
      ) ~ "va",
      TRUE ~ as.character(agency_short)
    ))

  # Modify the ones we know to be non-fed

  affils_df <- affils_df %>%
    mutate(federal = if_else(affil_id %in% nonfed_affils$affil_id, FALSE, federal))


  # Correct country

  affils_df <- affils_df %>%
    mutate(country = case_when(
      affil_id %in% c(
        116412197,
        117758211,
        130163250,
        120790199,
        117717065,
        126483911
      ) ~ "usa",
      TRUE ~ as.character(country)
    ))

  affils_df <- affils_df %>%
    mutate(federal = case_when(
      affil_id %in% c(
        112838889,
        60006762,
        128561286,
        128464417,
        100641183,
        112591249,
        124526765
      ) ~ TRUE,
      TRUE ~ federal
    ))
  

  affils_df <- affils_df %>%
    mutate(federal = case_when(
      affil_id %in% c(
        113727207,
        126243689,
        129587280
      ) ~ TRUE,
      TRUE ~ federal
    ))


  affils_df <- affils_df %>%
    mutate(agency_short = case_when(
      affiliation == "nuclear regulatory commission" ~ "nrc",
      TRUE ~ agency_short
    ))



  affils_df <- affils_df %>%
    mutate(agency_short = case_when(
      affil_id == 112838889 ~ "doj",
      affil_id == 128561286 ~ "federal reserve system",
      affil_id == 128464417 ~ "federal reserve system",
      affil_id == 100641183 ~ "dot",
      affil_id == 112591249 ~ "va",
      affil_id == 124526765 ~ "dot",
      TRUE ~ agency_short
    ))
  
  affils_df <- affils_df %>%
    mutate(
      federal = case_when(
        str_detect(affiliation, " llc") ~ FALSE,
        str_ends(affiliation, " inc") ~ FALSE,
        str_ends(affiliation, " ltd") ~ FALSE,
        str_ends(affiliation, " , usa| , uk| , austria| , australia| , france| , germany| , mexico| , mexico city") ~ FALSE,
        str_detect(affiliation, ", inc") ~ FALSE,
        str_detect(affiliation, " corporation") ~ FALSE,
        str_detect(affiliation, "california department") ~ FALSE,
        str_detect(affiliation, "corteva agriscience") ~ FALSE,
        str_detect(affiliation, "inova ") ~ FALSE,
        TRUE ~ federal
      ),
      agency_short = case_when(
        str_detect(affiliation, "california department") ~ NA_character_,
        str_detect(affiliation, "uniformed services university ") ~ "dod",
        TRUE ~ agency_short
      )
    )



  # now search the ones with is.na(federal) to assign


  # territories
  # american samoa
  # guam
  # puerto rico
  # northern mariana islands
  # us virgin islands




  fed_status_ok <- affils_df %>%
    filter(!is.na(federal))


  fed_status_fix <- affils_df %>%
    filter(is.na(federal))


  fed_status_fix <- fed_status_fix %>%
    mutate(
      agency_short =
        case_when(
          str_detect(affiliation, "agency_shortfor healthcare research and quality") ~ "ahrq",
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
          str_detect(affiliation, "national marine sanctuary of american samoa") ~ "interior",
          str_detect(affiliation, "naval hospital guam") ~ "dod",
          country == "guam" & str_detect(affiliation, "naval hospital") ~ "dod",
          str_detect(affiliation, "navy medicine readiness and training command guam") ~ "dod",
          str_detect(affiliation, "navfac marianas") ~ "dod",
          str_detect(affiliation, "va caribbean health care system") ~ "usda",
          country == "usa" & str_detect(affiliation, "naval") ~ "dod",
          country == "usa" & str_detect(affiliation, "veteran ") ~ "va",
          country == "puerto rico" & str_detect(affiliation, "veteran") ~ "va",
          # country == "usa" & str_detect(affiliation, "veterans ") ~ "va",
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
          # str_detect(affiliation, " us department of defence") ~ "dod",
          str_detect(affiliation, " us walter reed") ~ "dod",
          str_detect(affiliation, " us mission") ~ "state",
          str_detect(affiliation, " us bureau of land management") ~ "interior",
          str_detect(affiliation, " us antarctic program") ~ "nsf",
          str_detect(affiliation, " niaid/nih international centers for excellence in ") ~ "nih",
          str_detect(affiliation, " us centers for disease control") ~ "cdc",
          
          str_detect(affiliation, "walter reed") ~ "dod",
          str_detect(affiliation, "us agency_shortfor international development") ~ "usaid",
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
          affiliation == "agency_shortfor toxic substance and disease registry" ~ "cdc",
          affiliation == "contracting agency_shortto the division of viral diseases" ~ "cdc",
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
          # affiliation == "national immunization program" ~ "dod",
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
          affiliation == "advanced research projects agency_short- energy" ~ "doe",
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
          affiliation == "interagency_shortgrizzly bear study team" ~ "interagency_shortgrizzly bear study team",
          affiliation == "national endowment for the arts" ~ "nea",
          affiliation == "federal maritime commission" ~ "federal maritime commission",
          affiliation == "us of america" ~ "other",
          affiliation == "us climate variability and predictability project office" ~ "us climate variability and predictability project office",
          affiliation == "interagency_shortspecial status/sensitive species program" ~ "interagency_shortspecial status/sensitive species program",
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
          affiliation == "agency_shortfor international development" ~ "usaid",
          affiliation == "agency_shortfor international development (aid)" ~ "usaid",
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
          str_detect(affiliation, "4th medical group") ~ "dod",
          str_detect(affiliation, "5th medical group") ~ "dod",
          str_detect(affiliation, "6th medical group") ~ "dod",
          str_detect(affiliation, "8th medical group") ~ "dod",
          str_detect(affiliation, "9th medical group") ~ "dod",
          country == "usa" & str_detect(affiliation, "agency_shortfor toxic substance and disease registry") ~ "hhs",
          # country == "usa" & str_detect(affiliation, "th medical group") ~ "dod",
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
          country == "usa" & str_detect(affiliation, "defense innovation unit") ~ "dod",
          country == "usa" & str_detect(affiliation, "defense medical ethics center") ~ "usuhs",
          country == "usa" & str_detect(affiliation, "frederick national") ~ "nih",
          country == "usa" & str_detect(affiliation, "national estuarine") ~ "interior",
          country == "usa" & str_detect(affiliation, "noaa") ~ "noaa",
          country == "usa" & str_detect(affiliation, "us mission") ~ "state",
          country == "usa" & str_detect(affiliation, "malaria initiative improving malaria") ~ "eop",
          country == "usa" & str_detect(affiliation, "the us president") ~ "eop",
          country == "usa" & str_detect(affiliation, "us aid") ~ "usaid",
          country == "usa" & str_detect(affiliation, "afit") ~ "dod",
          country == "usa" & str_detect(affiliation, "carl r darnall") ~ "dod",
          str_detect(affiliation, "va health") ~ "va",
          str_detect(affiliation, "va north texas healthcare system") ~ "va",
          str_detect(affiliation, "va office of information and technology") ~ "va",
          str_detect(affiliation, "va center") ~ "va",
          str_detect(affiliation, "us fish") ~ "interior",
          TRUE ~ as.character(agency_short)
        )
    )
  
  
  library(dplyr)
  library(stringr)

  fed_status_fix <- fed_status_fix %>%
    mutate(agency_short = case_when(
      country == "usa" & str_detect(affiliation, "national institute") ~ "nih",
      country == "usa" & str_detect(affiliation, "sea grant") ~ "noaa",
      country == "usa" & str_detect(affiliation, "brigade|combat|battalion|usmc|regiment|command") ~ "dod",
      country == "usa" & str_detect(affiliation, "medical wing|squadron|cavalry| usn |field hospital") ~ "dod",
      str_detect(affiliation, "dod-va") ~ "va",
      str_detect(affiliation, "toxic substances and disease") ~ "cdc",
      str_detect(affiliation, "federal reserve") ~ "federal reserve system",
      str_detect(affiliation, "federal deposit insurance corporation") ~ "fdic",
      str_detect(affiliation, "national wetlands inventory") ~ "interior",
      str_detect(affiliation, "national historical park|historic site|historical site|national lakeshore") ~ "interior",
      str_detect(affiliation, "us department of interior—international technical assistance program") ~ "interior",
      str_detect(affiliation, "usgs |us geol survey") ~ "interior",
      str_detect(affiliation, "national risk management research laboratory") ~ "epa",
      str_detect(affiliation, "federal transit administration") ~ "dot",
      str_detect(affiliation, "national transportation research center") ~ "dot",
      str_detect(affiliation, "federal highway administration") ~ "dot",
      str_detect(affiliation, "federal trade commission") ~ "ftc",
      str_detect(affiliation, "national nuclear security site livermore office") ~ "doe",
      str_detect(affiliation, "national ecological observation network") ~ "nsf",
      str_detect(affiliation, "james a lovell federal health care center") ~ "va",
      str_detect(affiliation, "safety and work life coast guard") ~ "dod",
      str_detect(affiliation, "callaghan federal hospital|callaghan federal medical") ~ "dod",
      str_detect(affiliation, "us defense health agency_shortarmed forces health surveillance division air force satellite") ~ "dod",
      str_detect(affiliation, "defense health agency_shorttbi center of excellence") ~ "dod",
      str_detect(affiliation, "us president’s malaria initiative") ~ "usaid",
      str_detect(affiliation, "us agency_shortfor internationl") ~ "usaid",
      str_detect(affiliation, "national weather center") ~ "noaa",
      str_detect(affiliation, "national systematics laboratory of the national oceanic") ~ "noaa",
      str_detect(affiliation, "national weather center research experiences for undergraduates|national tsunami warning center") ~ "noaa",
      str_detect(affiliation, "national institutes of child health and development|national institutes of arthritis|national institute on alcoholism and alcohol abuse") ~ "nih",
      str_detect(affiliation, "national institute of standards") ~ "nist",
      str_detect(affiliation, "national homeland security research center") ~ "epa",
      str_detect(affiliation, "national soil resource conservation service") ~ "usda",
      str_detect(affiliation, "usfs") ~ "usda",
      str_detect(affiliation, "us general accounting office") ~ "gao",
      str_detect(affiliation, "cybersecurity and infrastructure security agency") ~ "dhs",
      TRUE ~ agency_short # default case
    ))

  summary(is.na(fed_status_fix$agency_short))
  # Mode   FALSE    TRUE
  # logical    7069 115885


  fed_status_fix <- fed_status_fix %>%
    mutate(agency_short = case_when(
      affil_id %in% c(
        "108382367",
        "128941071",
        "125244852",
        "128306628",
        "131558906",
        "125763856",
        "125364545"
      ) ~ "usda",
      affil_id == "122988512" ~ "ahrq",
      affil_id == "60019388" ~ "eop",
      affil_id == "124336178" ~ "cdc",
      affil_id %in% c(
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
        "113119893",
        "118439805",
        "124601958",
        "107062094",
        "113951892",
        "129955677",
        "123822264",
        "128757432",
        "131466439"
      ) ~ "dod",
      affil_id %in% c(
        "60008492",
        "100335911",
        "101792179",
        "106453365",
        "109347134",
        "112920600",
        "112922559",
        "120168333"
      ) ~ "doe",
      affil_id == "123502999" ~ "epa",
      affil_id %in% c(
        "100677110",
        "113820147",
        "125383427",
        "128533828"
      ) ~ "hhs",
      affil_id %in% c(
        "101140478",
        "128172361",
        "130391142"
      ) ~ "interior",
      affil_id %in% c(
        "123678203",
        "123865204"
      ) ~ "nasa",
      affil_id %in% c(
        "107986893",
        "122628604",
        "130148658"
      ) ~ "nasem",
      affil_id %in% c(
        "112602793",
        "113013369",
        "125340513",
        "125381761",
        "125755037",
        "131324608",
        "131324998"
      ) ~ "nih",
      affil_id %in% c(
        "100316021",
        "101000910",
        "123141776",
        "130557216"
      ) ~ "noaa",
      affil_id %in% c(
        "60071501",
        "100587655",
        "101394353",
        "106456794"
      ) ~ "nsf",
      affil_id %in% c(
        "108578096",
        "114159234",
        "128149737"
      ) ~ "smithsonian",
      affil_id %in% c(
        "115539127",
        "123896933",
        "126381052"
      ) ~ "usphs",
      affil_id %in% c(
        "120532431",
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
        "126395319"
      ) ~ "va",
      TRUE ~ agency_short
    ))

  
  
  affils_df <- bind_rows(fed_status_fix, fed_status_ok)

  
  # UNIVERSITY IS KIND OF A PAIN BECAUSE THERE ARE SEVERAL "FEDERAL" UNIVERSITIES
  # so did seperately
  affils_df <- affils_df %>%
    mutate(
      federal =
        case_when(str_detect(affiliation, "university") ~ FALSE,
                  .default = as.logical(federal)
        )
    )   %>%
    mutate(
      agency_short =
        case_when(
          str_detect(affiliation, "marine corps university") ~ "dod",
          str_detect(affiliation, "army university press") ~ "dod",
          country == "usa" & str_detect(affiliation, "us dot university transportation center for") ~ "dot",
          country == "usa" & str_detect(affiliation, "national defense university") ~ "dod",
          country == "usa" & str_detect(affiliation, "usaf air university culture") ~ "dod",
          country == "usa" & str_detect(affiliation, "air force engineering university") ~ "dod",
          country == "usa" & str_detect(affiliation, "national defense university") ~ "dod",
          str_detect(affiliation, "uniformed services university ") ~ "dod",
          str_detect(affiliation, "uniformed services university of the health sciences") ~ "dod",
          TRUE ~ agency_short
        )
    ) %>% 
    mutate(
      federal =
        case_when(
          str_detect(affiliation, "marine corps university") ~ TRUE,
          str_detect(affiliation, "army university press") ~ TRUE,
          country == "usa" & str_detect(affiliation, "us dot university transportation center for") ~ TRUE,
          country == "usa" & str_detect(affiliation, "national defense university") ~ TRUE,
          country == "usa" & str_detect(affiliation, "usaf air university culture") ~ TRUE,
          country == "usa" & str_detect(affiliation, "air force engineering university") ~ TRUE,
          country == "usa" & str_detect(affiliation, "national defense university") ~ TRUE,
          str_detect(affiliation, "uniformed services university ") ~ TRUE,
          str_detect(affiliation, "uniformed services university of the health sciences") ~ TRUE,
          TRUE ~ federal
        )
    ) %>% 
    mutate(
      agency_short =
        case_when(
          # foundation is hard - NSF is gov, but  park and cdc foundations are not. 
          # leave to end of run
          affiliation == "cdc foundation" ~ NA, 
          TRUE ~ agency_short
        )
    )
  # 
  
  
  affils_df<-affils_df %>% 
    mutate(agency=case_when(
      agency=="nphs" & federal==TRUE~"usphs",
      agency=="usgs" & federal==TRUE~"interior",
      .default = as.character(agency)
    )
    ) %>% 
    mutate(agency_primary=agency) %>% 
    mutate(agency_primary=case_when(
      agency_primary=="irs"~"treasury",
      agency_primary=="usphs"~"hhs",
      agency_primary=="nih"~"hhs",
      agency_primary=="cdc"~"hhs",
      agency_primary=="fda"~"hhs",
      agency_primary=="nist"~"commerce",
      agency_primary=="noaa"~"commerce",
      agency_primary=="usaid"~"state",
      agency_primary=="faa"~"dot",
      agency_primary=="dea"~"doj",
      agency_primary=="dha"~"dod",
      agency_primary=="fema"~"dhs",
      agency_primary=="usphs"~"hha",
      .default = as.character(agency_primary)
    )
    )
  
  
  # affils_df %>% filter(federal==FALSE) %>% select(agency_short) %>% distinct()
  # affils_df %>% filter(federal==TRUE) %>% select(agency_short) %>% distinct() %>% arrange(agency_short)
  affils_df<-affils_df %>% 
    rename(agency=agency_short) %>% 
    mutate(federal=if_else(!is.na(agency),TRUE,federal))
  # affils_df %>% filter(federal==FALSE) %>% select(agency) %>% distinct()
  # affils_df %>% filter(federal==TRUE) %>% select(agency) %>% distinct() %>% arrange(agency)
  
  
  return(affils_df)
}

