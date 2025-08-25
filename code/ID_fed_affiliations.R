ID_fed_affiliations <- function(affils_df) {
  # affils_df<-affils_df_original
  affils_df_original<-affils_df
  
  
  
  # STILL TO FIGURE OUT
  
  ## still a bunch of 23th medical wing, medical group, etc.
  
  # 122934618
  # army research laboratory
  # 60277579 - mixed fed hybrid
  # nat cvent veterans studies 113119893?
  # 
  # 101635671 va?
  #   
  #   all these nih?
  #   130518137
  # 115004386
  # 110048947
  # 131757712
  # 132187963
  # 126908603
  # 127328899
  # 126387720
  # 128858664
  # 127864131
  # 129617260
  # 130210855
  # 127316494
  # 
  # state? 129016829
  
  
  # standardize COUNTRIES -----------------------------------------------
  levels(as.factor(affils_df$country))
  # 
  # affils_df <- affils_df %>%
  #   mutate(federal = case_when(
  #     affil_id %in% c(
  #       113727207,
  #       126243689,
  #       105428030
  #     ) ~ NA,
  #     TRUE ~ federal
  #   )
  #   )
  # 
affils_df<-affils_df %>% select(affil_url,affil_id,affiliation,city,country) %>% 
  distinct()




# affils_df %>% group_by(affil_id) %>% tally() %>% filter(n>1) %>% arrange(desc(n))

  affils_df <- affils_df %>%
    mutate(country = recode(country,
      "united states" = "usa",
      "virgin islands (u.s.)" = "us virgin islands",
      "viet nam" = "vietnam",
      "cote d'ivoire" = "ivory coast",
      "timor-leste" = "east timor"
    ))
  
  # affils_df_original<-affils_df
  # affils_df<-affils_df_original
  
  
  
  

  # 2x all operations center
  # cava de' tirreni aou s giovanni di dio e ruggiero d'arago
  # u s president’s malaria initiative evolve project nigeria
  
  # affil_dupes<-fed_affils %>% group_by(affil_id,agency_short) %>% tally() %>% arrange(desc(n)) %>% filter(n>1) 
  # affil_dupes_all<-fed_affils %>% filter(affil_id%in%affil_dupes$affil_id) %>% distinct(affil_id, agency_short,.keep_all = TRUE)
  # 
  
  
  
  fed_affils <- read_csv("./data_clean/fed_affils_07102025.csv") %>%
    
    filter(!(affil_id==113727207)) %>% 
    filter(!(affil_id==126243689)) %>% 
    filter(!(affil_id==105428030)) %>% 
    filter(!(affil_id==60004328 & agency_short=="commerce")) %>% 
    filter(!(affil_id==100413214 & agency_short=="hhs")) %>% 
    filter(!(affil_id==60093272 & agency_short=="dhs")) %>% 
    filter(!(affil_id==100413073 & (agency_short=="judiciary"|agency_short=="va"))) %>% 
    filter(!(affil_id==121770345 & agency_short=="judiciary")) %>% 
    filter(!(affil_id==114750014 & (agency_short=="hhs"|agency_short=="interior"))) %>% 
    filter(!(affil_id==122535121 & affiliation=="office of the assistant secretary for planning and evaluation")) %>% 
    filter(!(affil_id==100345200 & agency_short=="doe")) %>% 
    filter(!(affil_id==112456278 & agency_short=="dod")) %>% 
    filter(!(affil_id==122605840 & agency_short=="dod")) %>% 
    filter(!(affil_id==60012320 & agency_short=="dod")) %>% 
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
                       affil_id==112888033~"veterans affairs puget sound",
                       affil_id==121955104~"atlanta va healthcare system",
                       affil_id==60012320~"us secret service",
                       .default = as.character(affiliation))) %>% 
    
    mutate(agency_short=
             case_when(affil_id==60028217~"epa",
                       affil_id==60093272~"dod",
                       affil_id==100727841~"dod",
                       affil_id==122535121~"hhs",
                       affil_id==60000459~"hhs",
                       affil_id==60033081~"interior",
                       affil_id==113727207~NA,
                       affil_id==126243689~NA,
                       affil_id==105428030~NA,
                          .default = as.character(agency_short))) %>% 
    arrange(affil_id, affiliation,agency_short, city, country, acronym) %>% 
    distinct(affil_id, agency_short,.keep_all = TRUE) 
  
  nonfed_affils <- read_csv("./data_clean/NONFED_affils_07102025.csv") %>% 
    distinct()
  
  # remove the NIH and nat park foundations , etc.
  fed_affils<-fed_affils %>% filter(!affil_id%in%nonfed_affils$affil_id)
  
  fed_affils<-fed_affils %>% select(affil_id,agency_short,city,country,federal) %>% distinct()
  
  # fed_affils %>% group_by(affil_id,agency_short) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)
  
  # fed_affils$affil_id 
  # affils_df$affil_id <- as.numeric(affils_df$affil_id)
  # 
  #  affils_df$affil_id <- as.character(affils_df$affil_id)
  # 
  # 
  fed_affils$affil_id <- as.numeric(fed_affils$affil_id)
  affils_df <- left_join(affils_df, fed_affils, by = c("affil_id"))

  affils_df %>% filter(!is.na(federal)) %>% filter(is.na(agency_short))
  affils_df %>% filter(is.na(federal)) %>% filter(!is.na(agency_short))
  ##################### 

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

# 
# 
# 
#   affils_df <- affils_df %>%
#     mutate(agency_short = case_when(
#       affil_id %in% c(
#         112775938,131119053,118935998,123885611,128217908,130150084,118330540,
#         115228508,116423594,131357366,129449620, 128137569, 120040598, 
#         105736912, 128316397, 125952897, 112805663, 121072296, 112730052, 
#         105477399, 100968866, 123254863, 121472110, 121382041,118575532, 
#         118211212, 117458915, 113044510, 112967639, 112887996, 109557356, 
#         101708480, 101532374, 127337699, 112893603, 101522669, 122233128, 
#         113221153,122565152, 112568591, 101619277, 118206330, 128848258, 
#         131254563, 131254559, 132421887, 122667304, 127632018, 115220797, 
#         125807927, 112704723, 132434964, 132434964, 109505309, 109505309, 
#         115385473, 113184770, 122498102, 109516894, 106929327, 106585504,
#         124221191, 119639166, 114301940, 109911421, 113538038, 100886222, 
#         112813607, 107070864, 114523236, 123693818, 114322561, 113018271, 
#         126381674, 129979905, 106580709, 112669335, 117410704, 114456193, 
#         125862540, 123665217, 114654204, 101972100, 121430097, 113171964, 
#         101232419, 125849010, 126672045, 112234684, 125302977, 119847200, 
#         108145124, 100764674, 130308845, 117356276, 123693690, 128684090, 
#         128123367, 118657357, 112833665, 112670485, 131506759, 112868925, 
#         130536578, 122243495, 116280683, 112911890, 124409139, 108181713, 
#         126791257, 112955278, 112955278, 112967581, 115209092, 107773789, 
#         128306669, 115003090, 113384350, 113345733, 114320094, 114270246, 
#         129674417, 118657132, 129243419, 113197759, 114674838, 112647481, 
#         106923974, 127864166, 105146112, 121989107, 128182625, 116593767, 
#         117453904, 113007703, 114000323, 124285483, 116422949, 114442747, 
#         127726999, 127726903, 127726892, 127726815, 127726735, 127726717,
#         127726694, 117121465, 131478202, 121489108, 101641629, 118122905, 
#         120135959, 105437969, 132383244, 106998694, 112994936, 130650522, 
#         108699047, 101536146, 124532433, 128306891, 128123526, 108214510, 
#         130621744, 123338971, 123338953, 113149283, 106707030, 129327605, 
#         129318198, 113200931, 113023651, 130974414, 122221036, 130090966,
#         129215697, 110525158, 128983928, 112587243, 109994213, 129650313, 
#         121153711, 132533172, 131344873, 100678433, 119310297, 119310297, 
#         120168487, 128241149,128726575, 100372859,128588933,
#         119086596, 116522307, 118637327, 129236469, 113211184, 131950071, 
#         112912153, 132475090, 102070450, 109640198, 127018884, 110031881, 
#         106656485, 100387381,112941552, 120338449,116697494, 125868776,
#         127580461, 106663664, 132431795, 106621898, 113114472, 112752240, 
#         118022065, 107253064, 126426809, 129188352, 129865395, 127123981,
#         131588681, 106379948, 128403318, 128403318, 131380914, 130095884, 
#         127814565, 108171312, 121099507, 121773398, 100725217, 106984557,
#         112648306, 126273729, 125628855, 106341010, 128444166, 129423473, 
#         130718131, 129457503, 113144135, 110245040, 126626246, 112582935,
#         106661173, 123341752, 112626061, 112635512, 120096391, 121697128, 
#         112952363, 101823272, 107413093, 108341947, 120452456, 113823270, 
#         118423712, 100620080,105885121, 114699239,113468202, 123897005,
#         115972055, 125461327, 122875627, 113848867, 125520925, 125101833, 
#         130285399, 101701694, 110915458, 112627278, 116452049, 112900864,
#         114406258, 109168654, 116775161, 113000532, 112614222, 130132540, 
#         128347842, 119221332, 124333534, 128497769, 123795863, 101069107,
#         123924359, 122934559, 127905519, 127904778, 124122851, 128092351, 
#         128084398, 122704640, 127013764, 112688333, 108062319, 101490818, 
#         127657130, 126684569,112665895, 114721270,122040486, 130158995,
#         130085442, 130084925, 118806977, 118661466, 102043815, 101786025, 
#         112694640, 112702309, 108243819, 126672057, 121943653, 127418326, 
#         112912020, 131329650, 131179893, 100533555, 113296020, 105478672, 
#         113130842, 130837812, 100477351, 106969174, 101576389, 132184908,
#         132434184, 132417343, 132417343, 113206441, 130454573, 107673059,
#         105605863, 129587609, 131246092, 123625235, 123615351, 120076617, 
#         121988696, 106657223,106644942, 132434184,119943502, 125582716,
#         129963166, 121267960, 120575516, 115956000, 107323255, 112366927, 
#         129899603, 128177152, 132524792, 126765566, 108468409, 112836578, 
#         107061891, 120248788, 126647356, 113060643, 127310335, 108338060, 
#         129973711, 127089255, 127631504, 112456046, 129646779, 123108259, 
#         113261742, 122513041, 101480320, 126418410, 125336447, 125253311, 
#         132305240, 132122123, 127586974, 128065395, 112930872, 128051819, 
#         128387888, 125791023,109859472, 131642253,108444951, 112445788,
#         129160431, 112940072, 132389249, 132389249, 112834500, 112834500,
#         115560703, 126820526, 122958104, 122466475, 127637878, 117688024, 
#         112363151, 111166218, 113081538, 129045481, 131948470, 132229247,
#         131341658, 100538844, 109505181, 123123752, 125545011, 113198685, 
#         128491187, 106709234, 106587413, 122597158, 132389675, 132389675, 
#         120185586, 122864212, 129556304, 112746892, 112617455, 112760468,
#         127828413, 122233128,112627278, 107064152,113333922, 127965231,
#         113007703, 132670425, 131344873, 108062319, 131950071, 102070450, 
#         132670499, 132618487, 100558018, 112886523, 115803040, 112991515, 
#         130454573, 114320094, 121989107, 112936183, 127631504, 121390655
#       ) ~ "va",
#       TRUE ~ as.character(agency_short)
#     ))

  # Modify the ones we know to be non-fed

  # affils_df <- affils_df %>%
  #   mutate(federal = if_else(affil_id %in% nonfed_affils$affil_id, FALSE, federal))
  # 

  # Correct country

  affils_df <- affils_df %>%
    mutate(country = case_when(
      affil_id %in% c(
        116412197,
        117758211,
        130163250,
        120790199,
        117717065,
        126483911,
        127857555,
        126868980,
        128418759
      ) ~ "usa",
      TRUE ~ as.character(country)
    ))

  # 
  # 
  # affils_df <- affils_df %>%
  #   mutate(federal = case_when(
  #     affil_id %in% c(
  #       112838889,
  #       60006762,
  #       128561286,
  #       128464417,
  #       100641183,
  #       112591249,
  #       124526765,
  #       113727207,
  #       126243689,
  #       129587280
  #     ) ~ TRUE,
  #     TRUE ~ federal
  #   ))
  
# 
#   affils_df <- affils_df %>%
#     mutate(federal = case_when(
#       affil_id %in% c(
#         113727207,
#         126243689,
#         129587280
#       ) ~ TRUE,
#       TRUE ~ federal
#     ))
# 
# 
#   affils_df <- affils_df %>%
#     mutate(agency_short = case_when(
#       affiliation == "nuclear regulatory commission" ~ "nrc",
#       TRUE ~ agency_short
#     ))

# 
# 
#   affils_df <- affils_df %>%
#     mutate(agency_short = case_when(
#       affil_id == 112838889 ~ "doj",
#       affil_id == 128561286 ~ "federal reserve system",
#       affil_id == 128464417 ~ "federal reserve system",
#       affil_id == 100641183 ~ "dot",
#       affil_id == 112591249 ~ "va",
#       affil_id == 124526765 ~ "dot",
#       TRUE ~ agency_short
#     ))
  
  # affils_df <- affils_df %>%
  #   mutate(
  #     federal = case_when(
  #       str_detect(affiliation, " llc") ~ FALSE,
  #       str_ends(affiliation, " inc") ~ FALSE,
  #       str_ends(affiliation, " ltd") ~ FALSE,
  #       str_ends(affiliation, " , usa| , uk| , austria| , australia| , france| , germany| , mexico| , mexico city") ~ FALSE,
  #       str_detect(affiliation, ", inc") ~ FALSE,
  #       str_detect(affiliation, " corporation") ~ FALSE,
  #       str_detect(affiliation, "california department") ~ FALSE,
  #       str_detect(affiliation, "corteva agriscience") ~ FALSE,
  #       str_detect(affiliation, "inova ") ~ FALSE,
  #       TRUE ~ federal
  #     ),
  #     agency_short = case_when(
  #       str_detect(affiliation, "california department") ~ NA_character_,
  #       str_detect(affiliation, "uniformed services university ") ~ "dod",
  #       TRUE ~ agency_short
  #     )
  #   )



  # now search the ones with is.na(federal) to assign


  # territories
  # american samoa
  # guam
  # puerto rico
  # northern mariana islands
  # us virgin islands


  
  
  
  affils_df <- affils_df %>%
    mutate(federal = case_when(
      affil_id %in% c(
        112838889,
        60006762,
        128561286,
        128464417,
        100641183,
        112591249,
        124526765,
        # 113727207,
        # 126243689,
        129587280
      ) ~ TRUE,
      TRUE ~ federal
    )
    )
  
  
  
  
  affils_df <- affils_df %>%
    
    mutate(agency_short = case_when(
      
      agency_short == "usgs" ~ "interior",
      agency_short == "usaid funded" ~ "usaid",
      affil_id == "125807845" ~ "usaid",
      affil_id == "112988162" ~ "usaid",
      
      agency_short == "nphs" ~ "usphs",
      affil_id == "122988512" ~ "ahrq",
      
      
      
    
      affil_id == "112838889" ~ "doj",
      affil_id == "124336178" ~ "cdc",
      affil_id == "123502999" ~ "epa",
    
      affil_id %in% c(
        "60012320",
        "129587280"
      ) ~ "dhs",
      
      
      affil_id %in% c(
        "124526765",
        "100641183"
      ) ~ "dot",
      
      affil_id %in% c(
        "108382367",
        "128941071",
        "125244852",
        "128306628",
        "131558906",
        "125763856",
        "125364545"
      ) ~ "usda",
      
      affil_id %in% c(
        "128450129"
      )~ "fda",
      
      affil_id %in% c(
        "128561286",
        "105191288",
        "128464417"
      )~ "federal reserve system",
      
      affil_id %in% c(
        "60019388",
        "100345200"
      )~ "eop",
      
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
        "131466439",
        "60003959",
        "112456278"
      ) ~ "dod",
      
      affil_id %in% c(
        "60008492",
        "100335911",
        "101792179",
        "106453365",
        "109347134",
        "112920600",
        "112922559",
        "60004328",
        "120168333"
      ) ~ "doe",
      
      affil_id %in% c(
        "100677110",
        "113820147",
        "125383427",
        "128533828",
        "122605840"
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
        "125755512",
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
        "112591249",
        "126395319",
        "131513956",
        "131997553",
        "112888033"
      ) ~ "va",
      
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
        130454573, 114320094, 121989107, 112936183, 127631504, 121390655,
        120305441,122903836,112842411,123720575,125203918,106077252,
        123882638,129946937,130647354,127864124,128101244,126671995,126868980,
        128418759,129646493,126299689,116522483,128565778,129286600,108201705,
        123720575,107962105,127827363,109030680,110168190,129688435,126446713,
        126457262,125759628,125785988,112928373, 123149707, 128478576, 127864124,
        129846776, 111505024, 122903836, 128478576, 105477766, 112678032, 
        128064936, 112671825, 125336568,129125631,132208416, 108062034, 108928269,
        107875046,123985164,118654374,100479614,123145084,124475407,112687253,
        126981536,128754886,100317364,132413735,112677640,100479614,122411378,
        118654415,122981065,112648370,123145084,112649013,124475407,124376601,
        112946911,112687253,127347050,106077452,126483911,128116947,129231656,
        129231656,112906884,132010539,118654196,126981536,128754886,100317364,
        132413735,132228479,132623540,112906884,122111615, 130213698,125108842,
        123861380,112746677,108502296,123536086,130095995,125884412,127991848,
        124841698,129832151,132073152,132475883,132782404,132421577,112612124,
        101581722,101106154,115858329,131793716,130982688,127569623,116069117,
        130707843,131534170,131968872,114761681,126140998,106915186,112830535,
        117529542,131940290,128312894,122229514,101767082,128408661,132421860,
        132421344,128408855,132714087,128409203,132570113,122434728,130163250,
        123149610,110426483,118911845,112768796,115003811,130214020,132552524,
        119174436,132550315,101735330,132222426,119591807,123748020,130796795,
        127618307,127622360,127352990,128814775,131444371
        ) ~ "va",
      
      TRUE ~ agency_short
    ))
  
  
  # brought from doc --------------------------------------------------------
  
  
  
  
  affils_df <- affils_df %>%
    mutate(affiliation = gsub("[.]", "", affiliation)) %>%
    mutate(affiliation = gsub("[/]", " ", affiliation)) %>%
    mutate(affiliation = gsub(",", "", affiliation)) %>%
    mutate(affiliation = gsub("- ", "-", affiliation)) %>%
    mutate(affiliation = gsub(" -", "-", affiliation)) %>%
    mutate(affiliation = gsub("indo-pacific", "indopacific", affiliation)) %>%
    mutate(affiliation = gsub("aphisppq", "aphis ppq", affiliation)) %>%
    mutate(affiliation = gsub("-", " ", affiliation)) %>%
    mutate(affiliation = gsub("united states ", "us ", affiliation)) %>%
    mutate(affiliation = gsub("veterans administration", "va", affiliation)) %>%
    mutate(affiliation = gsub("us agency for international development", "usaid", affiliation)) %>%
    mutate(affiliation = gsub("usaid's", "usaid", affiliation)) %>%
    mutate(affiliation = gsub("us department of agriculture", "usda", affiliation))%>%
    # mutate(affiliation = gsub("US FOREST SERVICE", "US Forest Service", affiliation))%>%
    mutate(affiliation = gsub(" servhice", " service", affiliation))%>%
    # mutate(affiliation = gsub("Us ", "US ", affiliation))%>%
    mutate(affiliation = gsub("usda forest service", "us forest service", affiliation))%>%
    mutate(affiliation = gsub("environmental protection agency", "epa", affiliation))%>%
    mutate(affiliation = gsub("us epa", "epa", affiliation))%>%
    mutate(affiliation = gsub("usepa", "epa", affiliation))%>%
    mutate(affiliation = gsub("usfws", "us fish and wildlife service", affiliation))%>%
    mutate(affiliation = gsub("veteran's ", "veterans ", affiliation))%>%
    mutate(affiliation = gsub("epa's ", "epa ", affiliation))%>%
    mutate(affiliation = gsub("nasa's ", "nasa ", affiliation))%>%
    mutate(affiliation = gsub("usda's ", "usda ", affiliation))%>%
    mutate(affiliation = gsub("cdc's ", "cdc ", affiliation))%>%
    mutate(affiliation = gsub("energy's ", "energy ", affiliation))%>%
    mutate(affiliation = gsub("guard's ", "guard ", affiliation))%>%
    mutate(affiliation = gsub("institute's ", "institute ", affiliation))%>%
    mutate(affiliation = gsub("smithsonian's ", "smithsonian ", affiliation))%>%
    mutate(affiliation = gsub("army's ", "army ", affiliation))%>%
    mutate(affiliation = gsub("college's ", "college ", affiliation))%>%
    mutate(affiliation = gsub("engineer's ", "engineer ", affiliation))%>%
    mutate(affiliation = gsub("us geological survey", "usgs", affiliation))%>%
    mutate(affiliation = gsub("us dep of ", "department of ", affiliation))%>%
    mutate(affiliation = gsub("us department of ", "department of ", affiliation))%>%
    mutate(affiliation = gsub("us forest service (usda)", "us forest service", affiliation))%>%
    mutate(affiliation = gsub("us forest service (usfs)", "us forest service", affiliation))%>%
    mutate(affiliation = gsub("noaa's", "noaa", affiliation))%>%
    mutate(affiliation = gsub("national oceanic and atmospheric administration", "noaa", affiliation))%>%
    mutate(affiliation = gsub("[(]retired)", "retired", affiliation))%>%
    mutate(affiliation = gsub("national weather service", "nws", affiliation)) %>% 
    mutate(affiliation = gsub("agency for international development (aid)", "usaid", affiliation)) %>% 
    mutate(affiliation = gsub("united agency for international development (usaid)", "usaid", affiliation)) %>% 
    mutate(affiliation = gsub("agency for international development", "usaid", affiliation)) %>% 
    mutate(affiliation = gsub("us of america agency for international development (usaid)", "usaid", affiliation)) %>% 
    mutate(affiliation = str_replace(affiliation, "^the ", "")) 
  
  affils_df<-affils_df %>% 
    
    mutate(affiliation = 
             case_when(
               affiliation == "animal and plant health inspection service usda"~"usda animal and plant health inspection service",
               affiliation == "european biological control laboratory (usda-ars)"~"usda-ars european biological control laboratory",
               affiliation == "retired us forest service"~"us forest service retired",
               affiliation == "retired usda-ars"~"usda-ars retired",
               affiliation == "us forest service (retired)"~"us forest service retired",
               affiliation == "us forest service (usda)"~"us forest service",
               affiliation == "usda animal and plant health inspection service"~"usda animal and plant health inspection service",
               affiliation == "usda us forest service"~"us forest service",
               affiliation == "usda-ars (retired)"~"usda-ars retired",
               affiliation == "usda-ars european biological control laboratory"~"usda-ars european biological control laboratory",
               affiliation == "affiliate noaa"~"noaa affiliate",
               affiliation == "atlantic oceanographic and meteorological laboratory (noaa)"~"noaa atlantic oceanographic and meteorological laboratory",
               affiliation == "climate program office (noaa)"~"noaa climate program office",
               affiliation == "earth system research laboratory (noaa)"~"noaa earth system research laboratory",
               affiliation == "national environmental satellite data and information service (noaa)"~"noaa national environmental satellite data and information service",
               affiliation == "nmfs noaa"~"noaa nmfs",
               affiliation == "noaa (noaa) restoration center"~"noaa restoration center",
               affiliation == "noaa affiliate"~"noaa affiliate",
               affiliation == "noaa atlantic oceanographic and meteorological laboratory"~"noaa atlantic oceanographic and meteorological laboratory",
               affiliation == "noaa climate program office"~"noaa climate program office",
               affiliation == "noaa earth system research laboratory"~"noaa earth system research laboratory",
               affiliation == "noaa national environmental satellite data and information service"~"noaa national environmental satellite data and information service",
               affiliation == "noaa nmfs"~"noaa nmfs",
               affiliation == "noaa pacific islands fisheries science center"~"noaa pacific islands fisheries science center",
               affiliation == "noaa restoration center"~"noaa restoration center",
               affiliation == "northeast fisheries science center (noaa)"~"noaa northeast fisheries science center",
               affiliation == "northwest fisheries science center (noaa)"~"noaa northwest fisheries science center",
               affiliation == "nws (nws)"~"nws",
               affiliation == "pacific islands fisheries science center (noaa)"~"noaa pacific islands fisheries science center",
               affiliation == "southwest fisheries science center (noaa)"~"noaa southwest fisheries science center",
               affiliation == "albuquerque va medical center"~"albuquerque va medical center",
               affiliation == "army ccdc army research laboratory"~"us army ccdc research laboratory",
               affiliation == "ccdc army research laboratory"~"us army ccdc research laboratory",
               affiliation == "ccdc us army research laboratory"~"us army ccdc research laboratory",
               affiliation == "center for combat and battlefield (combat) research"~"center for combat and battlefield research",
               affiliation == "center for combat and battlefield research"~"center for combat and battlefield research",
               affiliation == "medical corps us air force"~"us air force medical corps",
               affiliation == "medical department activity-korea 65th medical brigade"~"medical department activity-korea  65th medical brigade",
               affiliation == "naval medical training and readiness command"~"naval medical readiness and training command",
               affiliation == "navy medicine readiness and training command"~"navy medicine and readiness training command",
               affiliation == "office of the assistant secretary of defense (health affairs"~"office of the assistant secretary of defense health affairs",
               affiliation == "office of the assistant secretary of defense (health affairs)"~"office of the assistant secretary of defense health affairs",
               affiliation == "san antonio military medical center;"~"san antonio military medical center",
               affiliation == "us air force medical corps"~"us air force medical corps",
               affiliation == "us army (retired)"~"us army retired",
               affiliation == "us army ccdc army research laboratory"~"us army ccdc research laboratory",
               affiliation == "us army devcom army research laboratory"~"us army devcom research laboratory",
               affiliation == "us army institute of surgical re-search"~"us army institute of surgical research",
               affiliation == "us army institute of surgical research"~"us army institute of surgical research",
               affiliation == "us army intelligence threat and analysis center"~"us army intelligence and threat analysis center",
               affiliation == "us army retired"~"us army retired",
               affiliation == "us bureau of navy medicine and surgery"~"us navy bureau of medicine and surgery",
               affiliation == "us devcom army research laboratory"~"us army devcom research laboratory",
               affiliation == "us navy bureau of medicine and surgery"~"us navy bureau of medicine and surgery",
               affiliation == "us veterans' health administration"~"us veterans health administration",
               affiliation == "veterans affairs boston healthcare system"~"boston va healthcare system",
               affiliation == "walter reed national military med-ical center"~"walter reed national military medical center",
               affiliation == "walter reed national military medical center"~"walter reed national military medical center",
               affiliation == "womack army med-ical center"~"womack army medical center",
               affiliation == "womack army medical center"~"womack army medical center",
               affiliation == "los alamos national laboratory (retired)"~"los alamos national laboratory retired",
               affiliation == "national energy technology laboratory (netl) support contractor"~"national energy technology laboratory support contractor",
               affiliation == "national energy technology laboratory netl support contractor"~"national energy technology laboratory support contractor",
               affiliation == "retired los alamos national laboratory"~"los alamos national laboratory retired",
               affiliation == "national institute of diabetes and digestive and kidney diseases"~"national institute of diabetes digestive and kidney diseases",
               affiliation == "national institute of diabetes digestive and kidney diseases"~"national institute of diabetes digestive and kidney diseases",
               affiliation == "national institute on alcoholism and alcohol abuse"~"national institute on alcohol abuse and alcoholism",
               affiliation == "nibib nih center for engineering complex tissues"~"nih nibib center for engineering complex tissues",
               affiliation == "nih nibib center for engineering complex tissues"~"nih nibib center for engineering complex tissues",
               affiliation == "usaid (usaid)"~"usaid",
               affiliation == "boston va health care system"~"boston va healthcare system",
               affiliation == "boston veterans affairs healthcare system"~"boston va healthcare system",
               affiliation == "military and veteran microbiome consortium for research and education (mvm-core)"~"military and veteran microbiome consortium for research and education",
               affiliation == "military and veteran microbiome: consortium for research and education"~"military and veteran microbiome consortium for research and education",
               affiliation == "military and veteran microbiome: consortium for research and education (mvm-core)"~"military and veteran microbiome consortium for research and education",
               affiliation == "va albuquerque medical center"~"albuquerque va medical center",
               affiliation == "va ann arbor health system"~"ann arbor va health system",
               affiliation == "va boston cooperative studies program"~"boston va cooperative studies program",
               affiliation == "va boston health care system"~"boston va healthcare system",
               affiliation == "va boston healthcare system"~"boston va healthcare system",
               affiliation == "va butler healthcare"~"butler va healthcare",
               affiliation == "va center for healthcare organization and implementation research (choir"~"va center for healthcare organization and implementation research",
               affiliation == "va center for healthcare organization and implementation research (choir)"~"va center for healthcare organization and implementation research",
               affiliation == "va finger lakes healthcare system"~"finger lakes va healthcare system",
               affiliation == "va hsr&d"~"va hsrd",
               affiliation == "va long beach healthcare system"~"long beach va healthcare system",
               affiliation == "va manchester medical center"~"manchester va medical center",
               affiliation == "va medical center new orleans"~"new orleans va medical center",
               affiliation == "va montana health care system"~"montana va health care system",
               affiliation == "va northwest network (visn 20)"~"va northwest network 20",
               affiliation == "va palo alto health system"~"palo alto va health system",
               affiliation == "va portland health care system"~"portland va health care system",
               affiliation == "va puget sound health care system"~"puget sound va health care system",
               affiliation == "va va hospital"~"va hospital",
               affiliation == "va visn 20 northwest network"~"va northwest network 20",
               affiliation == "va washington dc health care system"~"va washington dc health care system",
               affiliation == "veterans ' integrated service network (visn) 7"~"veterans ' integrated service network 7",
               affiliation == "veterans affairs nebraska-western iowa health care system"~"nebraska-western iowa veterans affairs health care system",
               affiliation == "veterans integrated service network visn 7"~"veterans ' integrated service network 7",
               affiliation == "veterans' integrated service network (visn) 7"~"veterans ' integrated service network 7",
               affiliation == "washington dc va health care system"~"va washington dc health care system",
               affiliation == "department of the interior bureau land management"~"bureau land management",
               affiliation == "department of the interior bureau of land management"~"bureau land management",
               affiliation == "kaua'i national wildlife refuge complex"~"kauai national wildlife refuge complex",
               affiliation == "kauai national wildlife refuge complex"~"kauai national wildlife refuge complex",
               affiliation == "retired us fish and wildlife service"~"us fish and wildlife service retired",
               affiliation == "retired usgs"~"usgs retired",
               affiliation == "us fish and wildlife service (retired)"~"us fish and wildlife service retired",
               affiliation == "usgs (emeritus)"~"usgs emeritus",
               affiliation == "usgs (retired)"~"usgs retired",
               affiliation == "usgs (usgs)"~"usgs",
               affiliation == "usgs emeritus"~"usgs emeritus",
               affiliation == "usgs retired"~"usgs retired",
               affiliation == "epa (epa)"~"epa",
               affiliation == "epa (orise)"~"epa orise",
               affiliation == "epa (retired"~"epa retired",
               affiliation == "epa (retired)"~"epa retired",
               affiliation == "orise epa"~"epa orise",
               affiliation == "goddard institute for space studies (nasa)"~"nasa goddard institute for space studies",
               affiliation == "nasa goddard institute for space studies"~"nasa goddard institute for space studies",
               affiliation == "american museum of national history"~"national museum of american history",
               affiliation == "national museum of american history"~"national museum of american history",
               affiliation == "smithsonian center for folklife and cultural heritage"~"smithsonian center for folklife and cultural heritage",
               affiliation == "smithsonian center for folklife cultural heritage and folklife center"~"smithsonian center for folklife and cultural heritage",
               affiliation == "cibola national forest & national grasslands"~"cibola national forest",
               affiliation == "daniel boone national forest redbird ranger district 91"~"daniel boone national forest",
               affiliation == "department of agricultural agricultural research service"~"usda ars",
               affiliation == "department of agricultural research service"~"usda ars",
               affiliation == "european biological control laboratory (usda ars)"~"usda ars european biological control laboratory",
               affiliation == "formerly usda ars nlae"~"formerly usda ars",
               affiliation == "nez perce clearwater national forests"~"nez perce clearwater national forest",
               affiliation == "retired usda ars"~"usda ars retired",
               affiliation == "usad ars"~"usda ars",
               affiliation == "usda ars (retired)"~"usda ars retired",
               affiliation == "usda ars aquatic animal health research center (aahrc)"~"usda ars aquatic animal health research center",
               affiliation == "usda ars aquatic animal health research laboratory"~"usda ars aquatic animal health research center",
               affiliation == "usda ars coastal plains soil"~"usda ars coastal plains soil water and plant research center",
               affiliation == "usda ars genetics and sustainable agriculture research unit"~"usda ars genetics and precision agriculture research unit",
               affiliation == "usda ars swcrl"~"usda ars screwworm research laboratory",
               affiliation == "usda veterinary services"~"usda veterinary sciences",
               affiliation == "va caribbean health care system"~"caribbean va health care system",
               affiliation == "1national institute of standards and technology"~"national institute of standards and technology",
               affiliation == "connecticut sea grant (noaa)"~"connecticut sea grant",
               affiliation == "connecticut sea grant college program"~"connecticut sea grant",
               affiliation == "illinois indiana sea grant college program"~"illinois indiana sea grant",
               affiliation == "maryland sea grant college"~"maryland sea grant",
               affiliation == "maryland sea grant extension"~"maryland sea grant",
               affiliation == "michigan sea grant extension"~"michigan sea grant",
               affiliation == "national oceanic and atmopsheric administration (noaa)"~"national oceanic and atmospheric association",
               affiliation == "national oceanographic and atmospheric administration"~"noaa fisheries",
               affiliation == "national oceanographic and atmospheric administration (noaa) fisheries"~"noaa fisheries",
               affiliation == "noaa epp earth system sciences and remote sensing scholar"~"noaa epp earth system science and remote sensing technologies scholar",
               affiliation == "noaa jpss program science office"~"noaa jpss program office",
               affiliation == "noaa national environmental satellite"~"noaa national environmental satellite data and information service",
               affiliation == "noaa national severe storms laboratory"~"noaa national severe storm laboratory",
               affiliation == "noaa nesdis center for satellite applications and research (star)"~"noaa nesdis center for satellite applications and research",
               affiliation == "noaa office of marine and aviation operations (omao)"~"noaa office of marine and aviation operations",
               affiliation == "noaa office of ocean exploration and research"~"noaa office of ocean exploration",
               affiliation == "noaa office of ocean exploration and research (oer)"~"noaa office of ocean exploration",
               affiliation == "noaa pacific islands fisheries center"~"noaa pacific islands fisheries science center",
               affiliation == "75th ranger regiment headquarters"~"75th ranger regiment",
               affiliation == "air force institution of technology"~"air force institute of technology",
               affiliation == "air force life cycle management center"~"air force lifecycle management center",
               affiliation == "air force technical applications center aftac"~"air force technical applications center",
               affiliation == "ann arbor va health system"~"ann arbor va healthcare system",
               affiliation == "army public health center (provisional)"~"army public health center",
               affiliation == "atlanta veterans affairs health care system"~"atlanta va affairs healthcare system",
               affiliation == "bay pines veterans affairs healthcare system"~"bay pines va healthcare system",
               affiliation == "birmingham va health care services"~"birmingham va healthcare system",
               affiliation == "boston veterans health administration"~"boston va healthcare system",
               affiliation == "brook army medical center"~"brooke army medical center",
               affiliation == "brooke armymedical center"~"brooke army medical center",
               affiliation == "carl r darnall medical center"~"carl r darnall army medical center",
               affiliation == "carl r darnall military treatment facility"~"carl r darnall army medical center",
               affiliation == "carl r darnell army medical center"~"carl r darnall army medical center",
               affiliation == "central virginia veterans affairs health care system"~"central virginia va health care system",
               affiliation == "cold regions research engineering laboratory"~"cold regions research and engineering laboratory",
               affiliation == "combat trauma research group"~"us navy combat trauma research group",
               affiliation == "combat trauma research group us navy"~"us navy combat trauma research group",
               affiliation == "combating terrorism center"~"combating terrorism center at west point",
               affiliation == "combating terrorism center at west po"~"combating terrorism center at west point",
               affiliation == "defence health agency"~"defense health agency",
               affiliation == "defense health agency (j3)"~"defense health agency",
               affiliation == "defense health agency research and development (j 9)"~"defense health agency research and development",
               affiliation == "el erdc"~"erdc el",
               affiliation == "engineer research and development center (erdc)"~"engineer research and development center",
               affiliation == "iowa city veterans affairs healthcare system"~"iowa city veterans affairs health center",
               affiliation == "james a haley veterans affairs"~"james a hale va healthcare system",
               affiliation == "james a haley veterans affairs healthcare system"~"james a hale va healthcare system",
               affiliation == "james a haley veterans' hospital"~"james a hale va healthcare system",
               affiliation == "james j peters veterans affairs medical center"~"james j peters va",
               affiliation == "keller army hospital military academy west point"~"keller army hospital",
               affiliation == "lexington va medical center"~"lexington va healthcare system",
               affiliation == "marine corps system command"~"marine corps systems command",
               affiliation == "mcdonald army health center fort eustis"~"mcdonald army health center",
               affiliation == "medical department activity korea  65th medical brigade"~"medical department activity korea 65th medical brigade",
               affiliation == "medical research and materiel command"~"medical research and development command",
               affiliation == "mike o'callaghan federal hospital"~"mike o'callaghan federal medical center",
               affiliation == "minneapolis va health care system"~"minneapolis va healthcare system",
               affiliation == "minneapolis veterans affairs"~"minneapolis va healthcare system",
               affiliation == "minneapolis veterans affairs health care system"~"minneapolis va healthcare system",
               affiliation == "national defence university"~"national defense university",
               affiliation == "naval aerospace medicine institute"~"naval aerospace medical research laboratory",
               affiliation == "naval center for combat and operational stress control"~"naval center for combat & operational stress control",
               affiliation == "naval nedical center portsmouth"~"naval medical center portsmouth",
               affiliation == "naval surface forces pacific"~"naval surface force pacific fleet",
               affiliation == "navy experimental diving unit"~"navy experimental dive unit",
               affiliation == "new mexico veterans affairs health care system"~"new mexico va affairs healthcare system",
               affiliation == "noaa office of ocean exploration and research engagement division"~"noaa office of ocean exploration",
               affiliation == "northport veterans affairs medical center"~"northport va medical center",
               affiliation == "oklahoma city department of veterans affairs medical center"~"oklahoma city va healthcare system",
               affiliation == "phoenix va health care system"~"phoenix va healthcare system",
               affiliation == "rocky mountain regional veterans affairs medical center"~"rocky mountain regional va healthcare system",
               affiliation == "salem veterans affairs health care system"~"salem va healthcare system",
               affiliation == "san antonio military medical center texas"~"san antonio military medical center",
               affiliation == "san francisco veterans affairs health care system"~"san francisco va healthcare system",
               affiliation == "u s army natural resources program on o'ahu"~"us army natural resources program oahu",
               affiliation == "u s naval hospital"~"us naval hospital",
               affiliation == "uniformed services university of the health science"~"uniformed services university of the health sciences",
               affiliation == "us army aeromedical research laboratory"~"us army aeromedical activity",
               affiliation == "us army aeromedical research laboratory (usaarl)"~"us army aeromedical activity",
               affiliation == "us army armaments research development and engineering center"~"us army armament research and development center",
               affiliation == "us army ccdc army research laboratory (arl)"~"us army ccdc research laboratory",
               affiliation == "us army ccdc sc"~"us army ccdc center",
               affiliation == "us army ccdc soldier center"~"us army ccdc center",
               affiliation == "us army combat capabilities development com "~"us army combat capabilities development com",
               affiliation == "us army combat capabilities development command army research laboratory"~"us army ccdc research laboratory",
               affiliation == "us army combat capabilities development command chemical biological center"~"us army ccdc chemical biological center",
               affiliation == "us army criminal investigation laboratory"~"us army criminal investigation command",
               affiliation == "us army criminal investigation laboratorydefense forensic science center"~"us army criminal investigation command",
               affiliation == "us army institute of surgical re search"~"us army institute of surgical research",
               affiliation == "us army maneuver center of excellence (mcoe)"~"us army maneuver center of excellence",
               affiliation == "us army medical materiel development activity"~"us army medical material development activity",
               affiliation == "us army natick research"~"us army natick rd&e cent",
               affiliation == "us army natick research and development command"~"us army natick rd&e cent",
               affiliation == "us army natural resources program on o'ahu"~"us army natural resources program oahu",
               affiliation == "us army rdecom ardec"~"us army rde command",
               affiliation == "us army reserve"~"us army reserves",
               affiliation == "us baylor military graduate program in nutrition"~"us military baylor graduate program in nutrition",
               affiliation == "us combat capabilities development command army research laboratory"~"us army ccdc research laboratory",
               affiliation == "us naval academy (md)"~"us naval academy",
               affiliation == "us navy bureau of medicine and surgery (bumed)"~"us navy bureau of medicine and surgery",
               affiliation == "usaf dental evaluation and consultation service (decs)"~"usaf dental evaluation and consultation service",
               affiliation == "usaf reserve"~"usaf reserves",
               affiliation == "usaf school of aerospace medicine (sme)"~"usaf school of aerospace medicine",
               affiliation == "usarmy combat capabilities development command chemical biological center"~"us army ccdc chemical biological center",
               affiliation == "va caribbean healthcare system"~"caribbean va health care system",
               affiliation == "va connecticut healthcare system"~"connecticut va healthcare system",
               affiliation == "va health care system"~"va healthcare system",
               affiliation == "va north texas healthcare system"~"north texas va healthcare system",
               affiliation == "veterans affairs northern california healthcare system"~"northern california va healthcare system",
               affiliation == "veterans affairs tennessee valley health care system"~"tennessee valley va healthcare system",
               affiliation == "vha office of veterans access to care"~"va office of veterans access to care",
               affiliation == "walter reed national military med ical center"~"walter reed national military medical center",
               affiliation == "white river junction veterans affairs medical center"~"white river junction va medical center",
               affiliation == "womack army med ical center"~"womack army medical center",
               affiliation == "argonne national laboratory"~"argonne national accelerator laboratory",
               affiliation == "argonne national labpratory"~"argonne national accelerator laboratory",
               affiliation == "jet propulsion laboratory's"~"jet propulsion laboratory",
               affiliation == "lawrencelivermore national laboratory"~"lawrence livermore national laboratory",
               affiliation == "lawrnece livermore national laboratory"~"lawrence livermore national laboratory",
               affiliation == "national energy research scientific computing center (nersc)"~"national energy research scientific computing center",
               affiliation == "nevada national security sites"~"nevada national security site",
               affiliation == "nevada national security sites (nnss)"~"nevada national security site",
               affiliation == "oak ridge national laboratoy"~"oak ridge national laboratory",
               affiliation == "agency for healthcare research and quality"~"agency for health care research & quality",
               affiliation == "agency for healthcare research and quality (ahrq)"~"agency for health care research & quality",
               affiliation == "agency for healthcare research and quality evidence based practice centers program"~"agency for health care research & quality",
               affiliation == "agency for toxic substances and disease registry (atsdr)"~"agency for toxic substances and disease registry",
               affiliation == "cdc covid 19 emergency response team"~"cdc covid 19 emergency response",
               affiliation == "epidemic intelligence service cdc"~"epidemic intelligence service",
               affiliation == "epidemic intelligence service officer"~"epidemic intelligence service",
               affiliation == "national human genome research institute (nhgri)"~"national human genome research institute",
               affiliation == "national human genomic research institute"~"national human genome research institute",
               affiliation == "national institute on alcohol abuse and alcoholism (niaaa)"~"national institute on alcohol abuse and alcoholism",
               affiliation == "network of the national library of medicine region 4"~"network of the national library of medicine",
               affiliation == "pacific cancer research consortium national cancer institute community oncology research program (ncorp)"~"pacific cancer research consortium nci community oncology research program",
               affiliation == "public health service commissioned corps"~"public health service",
               affiliation == "us public health service (usphs)"~"us public health service",
               affiliation == "us public health service commissioned corps"~"us public health service",
               affiliation == "usa national library of medicine"~"us national library of medicine",
               affiliation == "office of the us global aids coordinator and health diplomacy"~"office of the us global aids coordinator",
               affiliation == "sauti program|usaid grantee"~"sauti program usaid grantee",
               affiliation == "usaid regional development mission for asia"~"usaid regional development mission asia",
               affiliation == "1va salt lake city health care system"~"salt lake city va healthcare system",
               affiliation == "ann arbor va healthcare"~"ann arbor va healthcare system",
               affiliation == "atlanta va geriatric research education and clinical center (grecc)"~"atlanta va geriatric research education and clinical center",
               affiliation == "atlanta va health care system"~"atlanta va healthcare system",
               affiliation == "atlanta veterans affairs healthcare system"~"atlanta va affairs healthcare system",
               affiliation == "baltimore veterans affairs hospital"~"baltimore va hospital",
               affiliation == "baltimore veterans affairs medical center"~"baltimore va hospital",
               affiliation == "bay pines veterans health system"~"bay pines va healthcare system",
               affiliation == "birmingham va health care system"~"birmingham va healthcare system",
               affiliation == "birmingham veterans affairs healthcare system (bvahs)"~"birmingham veterans affairs health care system",
               affiliation == "birmingham veterans affairs medical center"~"birmingham veterans affairs health care system",
               affiliation == "boston veteran administration healthcare system"~"boston va healthcare system",
               affiliation == "boston veterans administration healthcare"~"boston va healthcare system",
               affiliation == "butler va health care system"~"butler va healthcare system",
               affiliation == "butler va healthcare"~"butler va healthcare system",
               affiliation == "center for veterans research and education"~"center for veterans research & education",
               affiliation == "central arkansas va health care system"~"central arkansas va healthcare system",
               affiliation == "central texas va health care system"~"central texas va healthcare system",
               affiliation == "central texas veterans health care system"~"central texas va healthcare system",
               affiliation == "central texas veterans health system"~"central texas va healthcare system",
               affiliation == "central texas veterans healthcare system"~"central texas va healthcare system",
               affiliation == "central virginia veterans healthcare system"~"central virginia va health care system",
               affiliation == "columbia va health care system"~"columbia va healthcare system",
               affiliation == "connecticut veteran health system"~"connecticut va healthcare system",
               affiliation == "connecticut veteran healthcare system"~"connecticut va healthcare system",
               affiliation == "connecticut veterans legal center"~"connecticut va healthcare system",
               affiliation == "corporal michael crescenz vha medical center"~"corporal michael crescenz va medical center",
               affiliation == "corporal michael j crescenz va medical center"~"corporal michael crescenz va medical center",
               affiliation == "corporal michael j crescenz vamc"~"corporal michael crescenz va medical center",
               affiliation == "corporal michael j crescenz veterans affairs medical center"~"corporal michael crescenz va medical center",
               affiliation == "dod va extremity trauma and amputation center of excellence"~"dod va extremity trauma & amputation center of excellence",
               affiliation == "durham va health care system"~"durham va healthcare system",
               affiliation == "durham va healthcare system duram"~"durham va healthcare system",
               affiliation == "eastern kansas veteran affair healthcare system"~"eastern kansas va healthcare system",
               affiliation == "eastern kansas veteran healthcare system"~"eastern kansas va healthcare system",
               affiliation == "edward hines jr veterans affairs medical center"~"edward hines jr va hospital",
               affiliation == "edward hines jr veterans hospital"~"edward hines jr va hospital",
               affiliation == "finger lakes va health care system"~"finger lakes va healthcare system",
               affiliation == "greater los angeles va health system"~"greater los angeles va healthcare system",
               affiliation == "greater los angeles veterans healthcare system"~"greater los angeles va healthcare system",
               affiliation == "greenville va health care center"~"greenville va healthcare center",
               affiliation == "gulf coast veterans health care system"~"gulf coast va healthcare system",
               affiliation == "gulf coast veterans healthcare system in biloxi"~"gulf coast va healthcare system",
               affiliation == "hwashington dc veterans affairs medical center"~"washington dc va healthcare system",
               affiliation == "iowa city va health care system"~"iowa city va healthcare system",
               affiliation == "iowa city veterans health care system"~"iowa city veterans affairs health center",
               affiliation == "james a hale veterans f hospital"~"james a hale va healthcare system",
               affiliation == "james a haley va hospital and clinics"~"james a haley va hospital",
               affiliation == "james a haley veterans administration"~"james a hale va healthcare system",
               affiliation == "james j peters va medical research center"~"james j peters va",
               affiliation == "kansas city veterans affair medical center"~"kansas city va healthcare system",
               affiliation == "kansas city veterans affairs healthcare system"~"kansas city va healthcare system",
               affiliation == "lexington va health care system"~"lexington va healthcare system",
               affiliation == "lexington va healthcare"~"lexington va healthcare system",
               affiliation == "lexington veterans affairs healthcare system"~"lexington va healthcare system",
               affiliation == "louis stokes cleveland va medical centers"~"louis stokes cleveland va medical center",
               affiliation == "louis stokes cleveland va medical centre"~"louis stokes cleveland va medical center",
               affiliation == "manhattan veterans affairs"~"montana va healthcare system",
               affiliation == "massachusetts veterans epidemiology and information center"~"massachusetts veterans epidemiological research and information center",
               affiliation == "massachusetts veterans epidemiology research and information center"~"massachusetts veterans epidemiological research and information center",
               affiliation == "massachusetts veterans epidemiology research and information center (maveric)"~"massachusetts veterans epidemiological research and information center",
               affiliation == "memphis veteran administration hospital"~"memphis va hospital",
               affiliation == "memphis veterans affairs center"~"memphis va hospital",
               affiliation == "michael e debakey va medican center"~"michael e debakey va medical center",
               affiliation == "michael e debakey veterans affairs medical center"~"michael e debakey veterans affairs hospital",
               affiliation == "military and veteran microbiome consortium for research and education (mvm core)"~"military and veteran microbiome consortium for research and education",
               affiliation == "military and veteran microbiome: consortium for research and education (mvm core)"~"military and veteran microbiome consortium for research and education",
               affiliation == "minneapolis va health care center"~"minneapolis va healthcare system",
               affiliation == "montana va health care system"~"montana va healthcare system",
               affiliation == "montana veteran affairs health care system"~"montana va healthcare system",
               affiliation == "nebraska western iowa veterans affairs health care system"~"nebraska western iowa va health care system",
               affiliation == "new mexico va health care services"~"new mexico va healthcare system",
               affiliation == "new mexico veterans affairs healthcare system"~"new mexico va affairs healthcare system",
               affiliation == "new york harbor veterans health affairs"~"new york harbour va healthcare system",
               affiliation == "new york harbour veterans affairs healthcare system new york city"~"new york harbour va healthcare system",
               affiliation == "northport veterans affairs hospital"~"northport va medical center",
               affiliation == "oklahoma city department of veterans health care system"~"oklahoma city va healthcare system",
               affiliation == "oklahoma city va health care system"~"oklahoma city va healthcare system",
               affiliation == "oklahoma city veterans affairs health care system"~"oklahoma city va healthcare system",
               affiliation == "oklahoma city veterans affairs medical centercity veterans affairs medical center"~"oklahoma city va healthcare system",
               affiliation == "patton veterans project"~"patton veterans film project",
               affiliation == "phoenix va health care center"~"phoenix va healthcare system",
               affiliation == "portland va health care system"~"portland va healthcare system",
               affiliation == "ralph h johnson va medical center"~"ralph h johnson va health care system",
               affiliation == "robley rex veterans medical center"~"robley rex veterans affairs medical center",
               affiliation == "rocky mountain regional va medical system va"~"rocky mountain regional va healthcare system",
               affiliation == "salem va health care system"~"salem va healthcare system",
               affiliation == "salem veterans affair medical center"~"salem va healthcare system",
               affiliation == "salisbury va healthcare system in salisbury"~"salisbury va health care system",
               affiliation == "salt lake city va health care system"~"salt lake city va healthcare system",
               affiliation == "san francisco va health care system"~"san francisco va healthcare system",
               affiliation == "san francisco veterans affairs healthcare system"~"san francisco va healthcare system",
               affiliation == "south texas veterans hcs"~"south texas veterans healthcare system",
               affiliation == "south texas veterans health care system"~"south texas veterans healthcare system",
               affiliation == "south texas veterans health san antonio"~"south texas veterans healthcare system",
               affiliation == "south texas veterans healthcare administration"~"south texas veterans healthcare system",
               affiliation == "southeast louisiana veterans affairs healthcare system"~"southeast louisiana va healthcare system",
               affiliation == "southeast louisiana veterans health care"~"southeast louisiana va healthcare system",
               affiliation == "southeast louisiana veterans health care system"~"southeast louisiana va healthcare system",
               affiliation == "southern arizona va health care system"~"southern arizona va healthcare system",
               affiliation == "southern arizona va health system"~"southern arizona va healthcare system",
               affiliation == "tennessee valley veterans affairs geriatric research education clinical center"~"tennessee valley va geriatric research education clinical center",
               affiliation == "tennessee valley veterans affairs geriatric research education clinical center (grecc)"~"tennessee valley va geriatric research education clinical center",
               affiliation == "texas valley coastal bend veterans health care system"~"texas valley coastal bend va healthcare system",
               affiliation == "texas valley costal bend veterans affairs (va)"~"texas valley coastal bend va healthcare system",
               affiliation == "va ann arbor center for clinical management research health services research and development center of innovation"~"va ann arbor center for clinical management research",
               affiliation == "va ann arbor health care system"~"ann arbor va healthcare system",
               affiliation == "va ann arbor healthcare system"~"ann arbor va healthcare system",
               affiliation == "va capitol health care network"~"capitol va healthcare network",
               affiliation == "va capitol health care system"~"capitol va healthcare network",
               affiliation == "va capitol healthcare network mental illness research education and clinical center"~"capitol va healthcare network",
               affiliation == "va collaborative evaluation center (vace)"~"va collaborative evaluation center",
               affiliation == "va connecticut health system"~"connecticut va healthcare system",
               affiliation == "va connicticut healthcare system"~"connecticut va healthcare system",
               affiliation == "va eastern colorado health care system"~"eastern colorado va healthcare system",
               affiliation == "va eastern colorado healthcare system"~"eastern colorado va healthcare system",
               affiliation == "va eastern kansas healthcare system"~"eastern kansas va healthcare system",
               affiliation == "va greater los angeles healthcare system"~"greater los angeles va healthcare system",
               affiliation == "va loma linda health care system"~"loma linda va healthcare system",
               affiliation == "va loma linda healthcare system"~"loma linda va healthcare system",
               affiliation == "va montana healthcare system"~"montana va healthcare system",
               affiliation == "va national teleoncology program"~"va national teleoncology",
               affiliation == "va nebraska western iowa health care system"~"nebraska western iowa va health care system",
               affiliation == "va nebraska western iowa health care system omaha division"~"nebraska western iowa va health care system",
               affiliation == "va nebraska western iowa healthcare system"~"nebraska western iowa va health care system",
               affiliation == "va new england health care system"~"new england va healthcare system",
               affiliation == "va new england healthcare system"~"new england va healthcare system",
               affiliation == "va new england mental illness research and education center"~"va new england mental illness",
               affiliation == "va new england mirecc"~"va new england mental illness",
               affiliation == "va new york harbor healthcare"~"va new york harbor health care system",
               affiliation == "va nj health care system"~"nj va healthcare system",
               affiliation == "va nj healthcare system"~"nj va healthcare system",
               affiliation == "va north texas health care system"~"north texas va healthcare system",
               affiliation == "va northeast ohio health care"~"northeast ohio va healthcare system",
               affiliation == "va northeast ohio healthcare system"~"northeast ohio va healthcare system",
               affiliation == "va northern ca health care system"~"northern ca va healthcare system",
               affiliation == "va northern california"~"northern ca va healthcare system",
               affiliation == "va northern california health care system"~"northern california va healthcare system",
               affiliation == "va northern california healthcare system"~"northern california va healthcare system",
               affiliation == "va office of analytics and business intelligence"~"va office of analytics and business informatics (oabi)",
               affiliation == "va office of patient care"~"vha office of patient centered care & cultural transformation",
               affiliation == "va office of patient care services"~"vha office of patient centered care & cultural transformation",
               affiliation == "va office of patient centered care and cultural transformation"~"vha office of patient centered care & cultural transformation",
               affiliation == "va office of research & development (ord)"~"va office of research and development",
               affiliation == "va office of specialty care services (scs)"~"va office of specialty care services",
               affiliation == "va pacific islands health care services community living center"~"pacific islands va healthcare system",
               affiliation == "va pacific islands healthcare system"~"pacific islands va healthcare system",
               affiliation == "va palo alto health care system"~"palo alto va healthcare system",
               affiliation == "va palo alto healthcare system"~"palo alto va healthcare system",
               affiliation == "va pittsburgh health care system"~"va pittsburg healthcare system",
               affiliation == "va pittsburgh health services"~"va pittsburg healthcare system",
               affiliation == "va pittsburgh healthcare center"~"va pittsburg healthcare system",
               affiliation == "va providence health care system"~"providence va healthcare system",
               affiliation == "va providence healthcare"~"providence va healthcare system",
               affiliation == "va puget sound health care system and visn 20 clinical resource hub"~"va puget sound health care center",
               affiliation == "va puget sound health services research and development"~"va puget sound health care center",
               affiliation == "va puget sound health system"~"va puget sound health care center",
               affiliation == "va puget sound healthcare system"~"va puget sound health care center",
               affiliation == "va salt lake city healthcare system"~"salt lake city va healthcare system",
               affiliation == "va san diego healthcare system"~"san diego va healthcare system",
               affiliation == "va sand diego healthcare system"~"san diego va healthcare system",
               affiliation == "va serious mental illness treatment resource and evaluation center (smitrec)"~"va serious mental illness treatment resource and evaluation center",
               affiliation == "va south central mental illness research education and clinic center"~"va south central mental illness research",
               affiliation == "va south central mental illness research education and clinical center"~"va south central mental illness research",
               affiliation == "va st louis health care system"~"st louis va healthcare system",
               affiliation == "va st louis healthcare system"~"st louis va healthcare system",
               affiliation == "va tennessee valley healthcare"~"va tennessee valley health system",
               affiliation == "va tennessee valley healthcare system"~"tennessee valley va healthcare system",
               affiliation == "va texas valley coastal bend health care system"~"texas valley coastal bend va healthcare system",
               affiliation == "va texas valley coastal bend healthcare system"~"texas valley coastal bend va healthcare system",
               affiliation == "va washington dc health care system"~"washington dc va healthcare system",
               affiliation == "va washington dc healthcare system"~"washington dc va healthcare system",
               affiliation == "va western new york health care system"~"western new york va healthcare system",
               affiliation == "va western new york healthcare system"~"western new york va healthcare system",
               affiliation == "veteran affairs ann arbor health care system"~"ann arbor va healthcare system",
               affiliation == "veteran affairs ann arbor healthcare system"~"ann arbor va healthcare system",
               affiliation == "veteran consulting and research"~"veteran consulting and research corp",
               affiliation == "veterans afairs medical center"~"veterans affairs medical center",
               affiliation == "veterans affairs center for integrated health care"~"veterans affairs center for integrated healthcare",
               affiliation == "veterans affairs nebraska western iowa health care system"~"nebraska western iowa va health care system",
               affiliation == "veterans affairs northern california health care system"~"northern california va healthcare system",
               affiliation == "veterans affairs palo alto health care system"~"palo alto va healthcare system",
               affiliation == "veterans affairs palo alto healthcare system"~"palo alto va healthcare system",
               affiliation == "veterans affairs quality scholarship program"~"veterans affairs quality scholars program",
               affiliation == "veterans affairs tennessee valley healthcare system"~"tennessee valley va healthcare system",
               affiliation == "veterans emergency management evaluation center (vemec)"~"veterans emergency management evaluation center",
               affiliation == "veterans engineering resource center (verc)"~"veterans engineering resource center",
               affiliation == "vha"~"va",
               affiliation == "vha health services research and development"~"va health services research and development",
               affiliation == "vha national center for ethics in health care"~"va national center for ethics in health care",
               affiliation == "vha national center for health promotion and disease prevention"~"va national center for health promotion and disease prevention",
               affiliation == "vha national center for patient safety"~"va national center for patient safety",
               affiliation == "vha national surgery office"~"va national surgery office",
               affiliation == "vha office of health equity"~"va office of health equity",
               affiliation == "vha office of mental health and suicide prevention"~"va office of mental health and suicide prevention",
               affiliation == "vha pharmacy benefits management services"~"va pharmacy benefits management services",
               affiliation == "visn 17 center for research on returning veterans"~"visn 17 center of excellence for research on returning war veterans",
               affiliation == "washington d c veterans affairs medical center and heart center"~"washington dc va healthcare system",
               affiliation == "washington dc va medical center"~"washington dc va healthcare system",
               affiliation == "washington dc veterans medical center"~"washington dc va healthcare system",
               affiliation == "west palm beach veterans affairs health care system"~"west palm beach va healthcare system",
               affiliation == "william s middleton memorial veterans hospital"~"william s middleton va hospital",
               affiliation == "william s middleton veterans health administration hospital"~"william s middleton va hospital",
               affiliation == "biscayne national parknational park servicedepartment of the interior"~"biscayne national park",
               affiliation == "national invasive species council (nisc) staff"~"national invasive species council",
               affiliation == "national park of american samoa (npsa)"~"national park of american samoa",
               affiliation == "national parks service"~"national park service",
               affiliation == "north cascades national park service complex"~"north cascades national park",
               affiliation == "office of assistant secretary indian affairs"~"office of assistant secretary for indian affairs",
               affiliation == "sacramento national wildlife refuge complex"~"sacramento national wildlife refuge",
               affiliation == "u s fish and wildlife service"~"us fish and wildlife service",
               affiliation == "us geologic survey (usgs) geology"~"us geologic survey",
               affiliation == "us geologico survey"~"us geologic survey",
               affiliation == "usdi national park service"~"national park service",
               affiliation == "usdoi national park service"~"national park service",
               affiliation == "usgs wyoming montana water science center in helena"~"usgs wyoming montana water science center",
               affiliation == "national health and environmental effects research lab (nheerl)"~"national health and environmental effects research laboratory",
               affiliation == "nasa godard space flight center"~"nasa goddard spaceflight center",
               affiliation == "nasa goddard space flight center"~"nasa goddard spaceflight center",
               affiliation == "nasa goddard space flight center (gsfc"~"nasa goddard spaceflight center",
               affiliation == "nasa hubble fellowship program sagan fellow"~"nasa hubble fellowship program",
               affiliation == "nasa interdisciplinary consortia for astrobiology research (icar)"~"nasa interdisciplinary consortia for astrobiology research",
               affiliation == "nasa nexus for exoplanet system science (nexss)"~"nasa nexus for exoplanet system science",
               affiliation == "nasa nexus for exoplanetary system science: earths in other solar systems team"~"nasa nexus for exoplanet system science",
               affiliation == "nasa postdoctoral program"~"nasa postdoctoral fellow",
               affiliation == "nasa postdoctoral program (npp) fellow"~"nasa postdoctoral fellow",
               affiliation == "nasa postdoctoral program fellow"~"nasa postdoctoral fellow",
               affiliation == "nasa virtual planetary laboratory lead team"~"nasa virtual planetary laboratory",
               affiliation == "nsf nasa center of chemical evolution"~"nsf nasa center for chemical evolution",
               affiliation == "geminiobservatory nsf'snoirlab"~"gemini observatory",
               affiliation == "national ecological observation network (neon)"~"national ecological observatory network",
               affiliation == "national radio astronomy observatory"~"national radio astronomy laboratory",
               affiliation == "national radio astronomy observatory socorro"~"national radio astronomy laboratory",
               affiliation == "national solar observatory (nso)"~"national solar observatory",
               affiliation == "national solar observatory sacramento peak"~"national solar observatory",
               affiliation == "nsf nanosystems engineering research center for nanotechnology enabled water treatment (newt)"~"nsf nanosystems engineering research center on nanotechnology enabled water treatment",
               affiliation == "federal emergency management agency's (fema)"~"federal emergency management agency",
               affiliation == "federal housing finance agency (fhfa)"~"federal housing finance agency",
               affiliation == "national invasive species council staff"~"national invasive species council",
               affiliation == "national snow and ice data center (nsidc)"~"national snow and ice data center",
               affiliation == "us general services administration (gsa)"~"us general services administration",
               affiliation == "national museum of the american indian (nmai)"~"national museum of the american indian",
               affiliation == "smithsonian conservation biology institute center for conservation education & sustainability (cces)"~"smithsonian conservation biology institute",
               affiliation == "smithsonian conservation biology unit"~"smithsonian conservation biology institute",
               affiliation == "smithsonian environmental research center (si)"~"smithsonian environmental research center",
               affiliation == "smithsonian environmental research station"~"smithsonian environmental research center",
               affiliation == "smithsonian tropical institute"~"smithsonian tropical research institute",
               .default = as.character(affiliation)
             ))
  
  # 
  # pull<-pull %>% 
  #   mutate(agency_primary = case_when(
  #     affil_id == "112988162" ~ "state",
  #     .default = as.character(agency_primary)
  #   )) %>%
    # mutate(agency = case_when(
    #   affil_id == "112988162" ~ "usaid",
    #   .default = as.character(agency)
    # )) 
    # 
  # pull$affiliation
  # # Extract strings with parentheses
  # matches <- str_extract_all(pull$affiliation, "\\([^()]+\\)")
  # 
  # # Flatten the list and remove NULLs
  # matches <- unlist(matches)
  # matches <- matches[matches != ""]
  # 
  
  
  # parentheses <- pull %>%
  #   mutate(parentheses_text = str_extract(affiliation, "\\([^()]+\\)")) %>% 
  #   filter(!is.na(parentheses_text))
  # 
  # parentheses$parentheses_text
  # pull2<-pull %>% 
  #   mutate(str_remove(affiliation,"[(]jrs[)]"))
  affils_df<-affils_df %>% 
    mutate(affiliation = gsub("[(]nhlbi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nih[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]airborne[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]wrrc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]niaid[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]niehs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nlm[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]ret[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]aphis[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]retired[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]grecc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nia[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nadp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nidcd[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nci[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncep[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nichd[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]national zoo[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]erdc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nida[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cindrr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]hsr and d[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nimhd[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usda ars[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]noaa[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncep[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ctp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nmfs[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]va desert pacific healthcare network[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]socom[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]sesync[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]niddk[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncats[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]r[)]","(retired)",affiliation)) %>% 
    mutate(affiliation = gsub("[(]mu ncorp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]aid[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]c stars[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]contractor to the usgs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usda ars[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncorp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]star[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]grecc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nswc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cc[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]retired[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]onr science and technology 101unit[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]mcsp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]west point[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ret)","(retired)",affiliation)) %>% 
    mutate(affiliation = gsub("[(]hpw[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nih[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]formerly the institute for transfusion medicine[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nmaahc[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]army futures command[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ussouthcom[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]fmc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]erdc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]netl[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usphs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nzp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nifs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]apf[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]sonny[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nei[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncbi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nidcr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]rrc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cnpp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ntp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]niams[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nibib[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ninr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cit[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nigms[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nccih[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]warun[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]od[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]csr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]coeci[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]afosi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]niimbl[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ussocom[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]hs&rd[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ideas[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]quirc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]grecc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]va[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]va[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn 6[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]installations and logistics[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]airborne[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]namru 2[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ma mirecc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]atlanta[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nam[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]adapt[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]perc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]roc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]erdc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usaid[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]niceatm[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncei[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]afmoa[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]r[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]government contractor for noaa ncei[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncei[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ccdc sc[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]ret[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]retired[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]r[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]erdc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]niwc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ibfm cnr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]mirecc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]rdrl hrs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]contractor[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]posthumous[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]r[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nserl ars usda[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]mtaps[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]badc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usuhs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usu[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]pmi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]oes[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]va[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]oabi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]pris m[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]airborne[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]deployment support[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]va[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nf[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nsrdec[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]navfac syscom midlant[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]grecc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]mtaps[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]renuwit[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]retired[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]bill[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nia nih[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]a[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usfs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nci[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vrhrc slc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vrhrc ic[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]as 40[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]devcom[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nia[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]arl[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]hab[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]airborne[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]queri[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]the center for environmental sustainability through insect farming nsf i ucrc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]boem[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cmat[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]airborne[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]dcmt[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]dmea[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]irac[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]orwh[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]deployment support[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]tasc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]oabi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]grecc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cg oes 3[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ret[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn 1[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]neptune[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cesamh[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usda ars[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nih cc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]pacific cancer research consortium national cancer institute community oncology research program[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn 19[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cabbi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]afmra[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]dac[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]gecdac[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nwr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncats[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]afrl rwwi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]idds[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]retired[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nwr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]star[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]us army retired[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncis[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]kelly services[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncorp[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nihs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]aifs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cms[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]va echo[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]climb[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]pacdet[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]tradoc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]troop support[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]116b 3[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]eros[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]climb[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usamrd g[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]idds[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]idds[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]t ah 19[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nmrtc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vrhrc gnv[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ssc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usfs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ret[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cimt[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]doe[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]apac[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]wwetac[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]afsec[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usamrd a[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nam[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]dod[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]11spec[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]jgcri pnnl[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]idds[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usvl[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]idds[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]wabiled[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]supporting noaa  nesdis star[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nrel[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]bha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]reserve[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cmat[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]r6[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ncrr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]star[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]va[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]dchv[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]hsr&d[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]osires[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cares[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nccam[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nichq[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cna[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]quirc[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]under contract with noaa southwest fisheries science center[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]eric[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]11g[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]veterans integrated service network 16[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vaqs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nccos[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nia[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]thrive coin[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]gao[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]noaa[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usawc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]opm[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]socom[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usda[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nccih[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]usamrd a[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nlm[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]gdit[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]wrair dod[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]retired[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]founding[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]2300 1810d[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]jal jangal[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]erc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]wpo[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]frfs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]afsoc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cesamh[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]wrair[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nci[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]pact karamoja[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]doe's[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]nrel[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]sfm[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]integrated resilience directorate[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]afosi[)]","",affiliation)) %>% 
    # mutate(affiliation = gsub("[(]noaa nesdis[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]gdvs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]jal jangal[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cber[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]discovvr[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]va[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]finvet[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn 1[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]quail[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]netl[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]dha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]atp bio[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]sea[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ret[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]skai[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]drs trikhacheva and dengler[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]noaa glerl[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]eoard[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]jrs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]doi[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]ccld[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]retired[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]lcluc[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vha[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]edward p boland[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]vetwise lhs[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]cmk[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]aetc psse[)]","",affiliation)) %>% 
    mutate(affiliation = gsub("[(]visn 1)","",affiliation)) %>% 
    mutate(affiliation = gsub("health care","healthcare",affiliation)) %>% 
    
  mutate(affiliation = gsub("natural resources conservation service","nrcs",affiliation)) %>% 
  mutate(affiliation = gsub("animal and plant health inspection service","aphis",affiliation)) %>% 
    mutate(affiliation = gsub("usda agricultural research service","usda ars",affiliation)) %>% 
    
    mutate(affiliation = gsub("u s ","us ",affiliation)) 
  
  
  affils_df<-affils_df %>%
    
    mutate(country = 
             case_when(
               affil_id=="123149707"~"usa",
               affil_id=="128478576"~"usa",
               affil_id=="127864124"~"usa",
               affil_id=="129846776"~"usa",
               affil_id=="111505024"~"usa",
               affil_id=="122903836"~"usa",
               affil_id=="128478576"~"usa",
               affil_id=="118812066"~"usa",
               affil_id=="129282179"~"usa",
               affil_id=="130647354"~"usa",
               affil_id=="125701945"~"usa",
               affil_id=="129946937"~"usa",
               affil_id=="128101244"~"usa",
               
               .default = as.character(country)
             )
    ) %>% 
  mutate(agency_short = 
           case_when(
             str_detect(affiliation,"epa ")& country=="usa"~"epa",
             str_detect(affiliation,"usda")& country=="usa"~"usda",
             str_detect(affiliation,"usda ars")~"usda",
             str_detect(affiliation,"usda aphis")~"usda",
             str_detect(affiliation,"usda nrcs")~"usda",
             str_detect(affiliation,"usda nifa")~"usda",
             str_detect(affiliation,"us forest service")~"usda",
             str_detect(affiliation,"usgs")~"interior",
             str_detect(affiliation,"noaa")~"noaa",
             str_detect(affiliation,"us army")~"dod",
             str_detect(affiliation,"us navy")~"dod",
             str_detect(affiliation,"us naval")~"dod",
             str_detect(affiliation,"ccdc")~"dod",
             str_detect(affiliation,"usaf ")~"dod",
             str_detect(affiliation," va healthcare")~"va",
             str_detect(affiliation,"veterans integrated service network")~"va",
             str_detect(affiliation,"veterans rural health resource center")~"va",
             str_detect(affiliation,"veterans healthcare system")~"va",
             str_detect(affiliation,"veterans healthcare administration")~"va",
             str_detect(affiliation,"vha")& country=="usa"~"va",
             str_detect(affiliation,"veterans health administration")~"va",
             str_detect(affiliation,"veteran affairs")& country=="usa"~"va",
             str_detect(affiliation,"veteran affairs")& country=="puerto rico"~"va",
             str_detect(affiliation,"veterans affairs")& country=="usa"~"va",
             str_detect(affiliation,"veterans affairs")& country=="puerto rico"~"va",
             str_detect(affiliation,"national center for ptsd")~"va",
             str_detect(affiliation,"ucla joint institute for regional earth system science")~"other",
             
             
            
                        .default = as.character(agency_short)
             )
         ) %>% 
    mutate(federal = 
             case_when(
               agency_short=="va"~TRUE,
               agency_short=="tva"~TRUE
               
              # .default = as.character(federal)
             )
    )
     
  
  scopus_id_1<-read_csv("./data_raw/agencies/agencies_redux_clean.csv")
  scopus_id_2<-read_csv("./data_raw/agencies/agencies_orig_clean.csv")
  search_affil_ids<-bind_rows(scopus_id_2,scopus_id_1) %>%  
    distinct(affil_id,.keep_all = TRUE) %>% 
    select(affil_id) 
  
  
  not_feds<-data.frame(affil_id=c(100320615,100597450,105428030,110075293,
              112377346,113727207,115973022,117887308,118462400,
              119020150,120290071,122485294,124235648,125377399,
              126149601,129357068,130701562,60013443,60002294,
              60082598,60091251,60088290,60004435,60012284,
              60175885,60103346,60106655,60029115,60118175,60086597,
              60089351,60090486,60096341,60118178,60162091,106562201,
              110141225,121366124,123659193,123764501,
              123794871,125014439,125117795,131383200,
              60009350,60031821,60016716,60117925,101556712,109520920,
              126243689,126369743,126709532,127078141,127206196,127677508,128145323,
              60004062,127687531,114398399,116261440,121302036,
              121816043,127905605,128058768,128064856,128186702,128609279,
              101869475,101967749,119493734,125136769,128853540,
              129556287,60028451,121848755,116732570,129349573,130790299,
              131676655,131736517,131974087,60023942,60119229,
              105637145,132392674,132488945))
  
  
  affils_df<-affils_df %>% 
    mutate(federal=if_else(affil_id%in%search_affil_ids$affil_id,TRUE,federal)) %>% 
    mutate(federal=if_else(affil_id%in%not_feds$affil_id,FALSE,federal))
  
  rm(scopus_id_1,scopus_id_2,search_affil_ids,not_feds)

   
  affils_df<-affils_df %>% 
    mutate_all(trimws) %>% 
    mutate(affiliation = gsub("  ","",affiliation))   
  
  
  
  
  
  
  # brought from doc end ----------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  affils_df <-affils_df %>% 
    mutate(federal=if_else(((is.na(federal)) & (!is.na(agency_short))),"TRUE",federal))
  
  
  affils_df %>% filter(!is.na(federal)) %>% filter(is.na(agency_short)) 
  affils_df %>% filter(is.na(federal)) %>% filter(!is.na(agency_short)) 
  
  
  affils_fed_ok<-affils_df %>% filter(federal==TRUE)
  affils_fed_check<-affils_df %>% filter(federal==FALSE|is.na(federal)) %>% 
    mutate(agency_short=NA) %>% 
    mutate(federal=as.logical(federal))
  
  
  
  affils_fed_check <- affils_fed_check %>%
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
      )
    )
      

  # 
  # fed_status_ok <- affils_fed_check %>%
  #   filter(!is.na(federal))
  # 
  # 
  # affils_fed_check <- affils_fed_check %>%
  #   filter(is.na(federal))


  affils_fed_check <- affils_fed_check %>%
    mutate(
      agency_short =
        case_when(
          # agency_short=="nphs"~"usphs",
          # agency_short=="usgs"~"interior",
          str_detect(affiliation, "california department") ~ NA_character_,
          str_detect(affiliation, "uniformed services university ") ~ "dod",
          affiliation == "nuclear regulatory commission" ~ "nrc",
          str_detect(affiliation, "agency_shortfor healthcare research and quality") ~ "ahrq",
          str_detect(affiliation, "us department of health and human services") ~ "hhs",
          # str_detect(affiliation, "us army") ~ "dod",
          country == "usa" & str_detect(affiliation, "department of the air force") ~ "dod",
          str_detect(affiliation, "smithsonian") ~ "smithsonian",
          # str_detect(affiliation, "us department of energy") ~ "doe",
          # str_detect(affiliation, "us epa") ~ "epa",
          # str_detect(affiliation, "usepa") ~ "epa",
          str_detect(affiliation, "epa retired") ~ "epa",
          # str_detect(affiliation, "department of the army") ~ "dod",
          # str_detect(affiliation, "usda") ~ "usda",
          # str_detect(affiliation, "us department of labor") ~ "labor",
          str_detect(affiliation, "national marine sanctuary of american samoa") ~ "interior",
          str_detect(affiliation, "naval hospital guam") ~ "dod",
          country == "guam" & str_detect(affiliation, "naval hospital") ~ "dod",
          str_detect(affiliation, "navy medicine readiness and training command guam") ~ "dod",
          str_detect(affiliation, "navfac marianas") ~ "dod",
          # str_detect(affiliation, "va caribbean health care system") ~ "va",
          country == "usa" & str_detect(affiliation, "naval") ~ "dod",
          # country == "usa" & str_detect(affiliation, "veteran ") ~ "va",
          # country == "puerto rico" & str_detect(affiliation, "veteran") ~ "va",
          # country == "usa" & str_detect(affiliation, "veterans ") ~ "va",
          country == "usa" & str_detect(affiliation, "veteran's ") ~ "va",
          # str_detect(affiliation, "us department of commerce") ~ "commerce",
          # str_detect(affiliation, "us department of defense") ~ "dod",
          # str_detect(affiliation, "us department of housing and urban development") ~ "hud",
          # str_detect(affiliation, "us department of veteran") ~ "va",
          # str_detect(affiliation,"us department of veterans affairs") ~ "va",
          # str_detect(affiliation, "veterans affairs medical center") ~ "va",
          # str_detect(affiliation,"us department of veteran affairs") ~ "va",
          # str_detect(affiliation, "us environmental protection agency") ~ "epa",
          # str_detect(affiliation, "us department of transportation") ~ "dot",
          # str_detect(affiliation, "us attorney general") ~ "doj",
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
          # country == "usa" & str_detect(affiliation, "food and drug administration") ~ "fda",
          country == "usa" & str_detect(affiliation, "us department of agriculture") ~ "usda",
          # str_detect(affiliation, "lawrence berkeley national laboratory") ~ "doe",
          str_detect(affiliation, "smithsonian") ~ "smithsonian",
          country == "usa" & str_detect(affiliation, "national park service") ~ "interior",
          str_detect(affiliation, "national park service social science program") ~ "interior",
          
          # country == "usa" & str_detect(affiliation, "veterans") ~ "va",
          country == "usa" & str_detect(affiliation, "national science foundation") ~ "nsf",
          country == "usa" & str_detect(affiliation, "usdi bureau of land management") ~ "interior",
          country == "usa" & str_detect(affiliation, "usdi blm") ~ "interior",
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

  # summary(is.na(affils_fed_check$agency_short))
  # Mode   FALSE    TRUE
  # logical    7069 115885

  
  
  # 
  # 
  # affils_fed_check <- bind_rows(affils_fed_check, fed_status_ok)

  
  # UNIVERSITY IS KIND OF A PAIN BECAUSE THERE ARE SEVERAL "FEDERAL" UNIVERSITIES
  # so did seperately
  affils_fed_check <- affils_fed_check %>%
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
          affil_id== 126969834 ~ TRUE,
          affil_id== 128075102 ~ TRUE,
          affil_id== 122529790 ~ TRUE,
          affil_id== 125585876 ~ TRUE,
          affil_id== 129517741 ~ TRUE,
          affil_id== 120305441 ~ TRUE,
          str_detect(affiliation, "marine corps university") ~ TRUE,
          str_detect(affiliation, "army university press") ~ TRUE,
          country == "usa" & str_detect(affiliation, "us dot university transportation center for") ~ TRUE,
          country == "usa" & str_detect(affiliation, "national defense university") ~ TRUE,
          country == "usa" & str_detect(affiliation, "usaf air university culture") ~ TRUE,
          country == "usa" & str_detect(affiliation, "air force engineering university") ~ TRUE,
          country == "usa" & str_detect(affiliation, "army university") ~ TRUE,
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

    
  # do a last check against these
  # nonfed_affils 
  affils_fed_check<-affils_fed_check %>% 
    mutate(federal=if_else((affil_id%in%nonfed_affils$affil_id),FALSE,federal))
  
  
  affils_fed_check<-affils_fed_check %>% 
    mutate(federal=if_else((is.na(federal) & !is.na(agency_short)),TRUE,federal))
  
  # affils_fed_check %>% filter(!is.na(agency_short)) %>% filter(federal==FALSE)
  
  affils_fed_check<-affils_fed_check %>% 
    mutate(agency_short=if_else(federal==FALSE, NA,agency_short))
  
  affils_fed_ok<-affils_fed_ok %>% 
    mutate(federal=as.logical(federal))
  
  affils_df_corrected<-bind_rows(affils_fed_check,affils_fed_ok)
  
  affils_df_corrected<-affils_df_corrected %>% 
    # mutate(agency=case_when(
    #   agency=="nphs" & federal==TRUE~"usphs",
    #   agency=="usgs" & federal==TRUE~"interior",
    #   .default = as.character(agency)
    # )
    # ) %>% 
    mutate(agency_primary=agency_short) %>% 
    mutate(agency_primary=case_when(
      agency_primary=="irs"~"treasury",
      agency_primary=="usphs"~"hhs",
      agency_primary=="nih"~"hhs",
      agency_primary=="cdc"~"hhs",
      agency_primary=="fda"~"hhs",
      agency_primary=="ahrq"~"hhs",
      agency_primary=="nist"~"commerce",
      agency_primary=="noaa"~"commerce",
      agency_primary=="usaid"~"state",
      agency_primary=="usaid funded"~"state",
      agency_primary=="faa"~"dot",
      agency_primary=="dea"~"doj",
      agency_primary=="dha"~"dod",
      agency_primary=="fema"~"dhs",
      agency_primary=="usphs"~"hha",
      agency_primary=="doj"~"doj",
      agency_primary=="state"~"state",
      agency_primary=="labor"~"labor",
      agency_primary=="hud"~"hud",
      agency_primary=="education"~"education",
      agency_primary=="doe"~"doe",
      agency_primary=="hhs"~"hhs",
      agency_primary=="va"~"va",
      agency_primary=="dod"~"dod",
      agency_primary=="usda"~"usda",
      agency_primary=="nasa"~"nasa",
      agency_primary=="commerce"~"commerce",
      agency_primary=="interior"~"interior",
      agency_primary=="smithsonian"~"smithsonian",
      agency_primary=="nsf"~"nsf",
      agency_primary=="epa"~"epa",
      .default = as.character(agency_primary)
    )
    )
  
  affils_df_corrected <- affils_df_corrected %>%
    mutate(agency_primary = case_when(
      federal == TRUE & !(agency_primary %in% c(
        "dod", "va", "interior", "usda", "commerce",
        "hhs", "state", "doe", "smithsonian", "nasa",
        "nsf", "epa"
      )) ~ "other",
      .default = agency_primary
    ))
  
  
  
  # foo<-affils_df_corrected %>% filter(federal==TRUE) %>% group_by(agency_primary, agency_short) %>% tally()%>% arrange(desc(n))
  
  
  
  
  
  
  affils_df_corrected<-affils_df_corrected %>% 
    distinct()
  # 
  # foo<-full_join(fed_affils_search,affils_df,by="affil_id") %>% 
  #   mutate(federal2=ifelse(search=="original"|search=="redux",TRUE,NA)) %>% 
  #   mutate(agency.y=if_else(is.na(agency.y),agency.x,agency.y))
  #   
  #   
  #   mutate(affiliation.y=ifelse(is.na(affiliation.y),affiliation.x,affiliation.y)) %>% 
  #   mutate(affiliation.y=ifelse(is.na(affiliation.y),affiliation.x,affiliation.y)) %>% 
  # 
  # 
  # 
  # mutate(agency.y=if_else(is.na(agency.y),agency.x,agency.y))
  # 
  # 
  # 
  # [1] "affil_id"       "agency.x"       "affiliation.x"  "city.x"         
  # [7] "affiliation.y"  "country"        "agency.y"       "city.y"         
  # foo2<-foo %>% filter(is.na(search)) %>%
  #   
  #   
  #   
  #   
  #   
  #   select(-agency.x,
  #   -affiliation.x,
  #   -city.x,
  #   -search) %>%
  #   rename(
  #   affiliation=affiliation.y,
  #   agency=agency.y,
  #   city=city.y)
  # # 
  # # 
  # foo %>% filter(!is.na(agency.y)) %>% group_by(federal) %>% tally(federal)
  # foo %>% filter(is.na(agency.y)) %>% group_by(federal) %>% tally(federal)
  # foo %>% filter(is.na(affiliation.x)) %>% filter(!is.na(affiliation.y))
  # foo %>% filter(!is.na(affiliation.x)) %>% filter(is.na(affiliation.y))
  # 
  # foo %>% filter(!is.na(agency.x)) %>% filter(is.na(agency.y))
  # foo %>% filter(!is.na(federal)) %>% group_by(agency.y) %>% tally() %>% arrange(desc(n))
  # foo %>% filter(is.na(federal)) %>% group_by(agency.y) %>% tally() %>% arrange(desc(n))
  # foo %>% filter(is.na(agency.y)) %>% group_by(federal) %>% tally(federal)
  # 
  # 
  # foo2 %>% filter(!is.na(agency)) %>% group_by(federal) %>% tally(federal)
  
  
  
  # affils_df %>% filter(federal==FALSE) %>% select(agency_short) %>% distinct()
  # affils_df %>% filter(federal==TRUE) %>% select(agency_short) %>% distinct() %>% arrange(agency_short)
  
  affils_df_corrected<-affils_df_corrected %>% 
    rename(agency=agency_short) 
  
  affils_df_corrected<-affils_df_corrected %>% 
    mutate(federal=if_else(is.na(federal),FALSE,federal))
  
  # %>% 
  #   mutate(federal=if_else(!is.na(agency),TRUE,federal))
  # 
  affils_df_corrected %>% filter(federal==FALSE) %>% filter(!is.na(agency))
  affils_df_corrected %>% filter(federal==TRUE) %>% filter(is.na(agency))
  affils_df_corrected %>% filter(federal==TRUE) %>% filter(is.na(agency_primary))
  
  # affils_df %>% filter(federal==FALSE) %>% select(agency) %>% distinct()
  # affils_df %>% filter(federal==TRUE) %>% select(agency) %>% distinct() %>% arrange(agency)
  
  
  affils_df_original
  fed_pull<-affils_df_corrected %>% 
    select(affil_id,agency,agency_primary,federal) %>% 
    distinct() %>% 
    mutate(affil_id=as.numeric(affil_id))
  
  # foo<-affils_df_corrected %>% 
  #   group_by(affil_id) %>% 
  # tally() %>% 
  #   filter(n>1) %>% 
  #   arrange(desc(n))
  # 
  # fed_pull %>% filter(affil_id%in%foo$affil_id) %>% filter(federal==TRUE)
  # 
  # 
  
  affils_df_original<-affils_df_original %>% 
    left_join(fed_pull,by="affil_id")
  # 
  # 
  # other_agencies<-affils_df_original %>% 
  #   filter(federal==TRUE) %>% 
  #   group_by(agency_primary) %>% 
  #   tally() %>% 
  #   arrange(desc(n))
  #   
  # 
  # other_agencies$agency_primary
  
  return(affils_df_original)
}




