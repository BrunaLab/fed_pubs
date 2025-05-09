

library(rscopus)
library(tidyverse)


# check api limit ---------------------------------------------------------


library(httr)

# Replace with your Scopus API endpoint and API key
url <- "https://api.elsevier.com/content/search/scopus?query=%28%28COVERDATE%28%272024-01-01%27%29%20OR%20%28%272024-02-01%27%29%29%20AND%20AFFILORG%28%27National%20Institutes%20of%20Health%27%29%20AND%20AFFILCOUNTRY%28united%20AND%20states%2A%20%29%20AND%20%28DOCTYPE%28ar%29%20OR%20DOCTYPE%28re%29%20OR%20DOCTYPE%28ch%29%20OR%20DOCTYPE%28ed%29%20OR%20DOCTYPE%28le%29%20OR%20DOCTYPE%28dp%29%20OR%20DOCTYPE%28no%29%29%20AND%20PUBYEAR%20%3D%202024%29&count=25&start=0&view=COMPLETE"
api_key <- "fdd770dc9a75a591e543f943828703aa"

# Make the API request
response <- GET(url, add_headers(`X-ELS-APIKey` = api_key))

# Print the headers to check the rate limit
headers <- headers(response)
print(headers)

# Extract 'X-RateLimit-Remaining' from the headers
remaining_requests <- headers[["X-RateLimit-Remaining"]]
cat("Remaining requests:", remaining_requests)

limit_reset <- headers[["X-RateLimit-Reset"]]       #Date/Time in Epoch seconds when API quota resets
cat("limit_reset:", limit_reset)
# Example: Convert epoch seconds to a date

date <- as.POSIXct(as.numeric(limit_reset), origin = "1970-01-01", tz = "UTC")
date
# call api ----------------------------------------------------------------
# 
# # REMAINING
# # cber
# # center for biologics evaluation and research
# # frederick national laboratory for cancer research
# # national cancer institute at frederick
# # national institute of mental health
# # nimh
# # national institute of neurological disorders and stroke
# # NCI
# # National Cancer Institute 
# # NEI
# # National Eye Institute
# # NHGRI
# # National Human Genome Research Institute 
# # NHLBI
# # National Heart, Lung, and Blood Institute
# # NIA
# # National Institute on Aging
# # NIAAA
# # National Institute on Alcohol Abuse and Alcoholism
# # NIBIB
# # National Institute of Biomedical Imaging and Bioengineering
# # NICHD
# # National Institute of Child Health and Human Development
# # NIDA
# # National Institute on Drug Abuse
# # NIDCD
# # National Institute on Deafness and Other Communication Disorders
# # NIDCR
# # National Institute of Dental and Craniofacial Research
# # NIDDK
# # National Institute of Diabetes and Digestive and Kidney Disease
# # NIEHS
# # National Institute of Environmental Health Sciences
# # ncbi
# # National Center for Biotechnology Information
# # National Institute for Occupational Safety and Health
# # niosh
# # nlm
# # National Library of Medicine
# # u.s. department of energy joint genome institute
# 
# # National Cancer Institute (NCI)
# # National Eye Institute (NEI)
# # National Heart, Lung, and Blood Institute (NHLBI)
# # National Human Genome Research Institute (NHGRI)
# # National Institute on Aging (NIA)
# # National Institute on Alcohol Abuse and Alcoholism (NIAAA)
# # National Institute of Allergy and Infectious Diseases (NIAID)
# # National Institute of Arthritis and Musculoskeletal and Skin Diseases (NIAMS)
# # National Institute of Biomedical Imaging and Bioengineering (NIBIB)
# # National Institute of Child Health and Human Development (NICHD)
# # National Institute on Deafness and Other Communication Disorders (NIDCD)
# # National Institute of Dental and Craniofacial Research (NIDCR)
# # National Institute of Diabetes and Digestive and Kidney Diseases (NIDDK)
# # National Institute on Drug Abuse (NIDA)
# # National Institute of Environmental Health Sciences (NIEHS)
# # National Institute of General Medical Sciences (NIGMS)
# # National Institute of Mental Health (NIMH)
# # National Institute on Minority Health and Health Disparities (NIMHD)
# # National Institute of Neurological Disorders and Stroke (NINDS)
# # National Institute of Nursing Research (NINR)
# # 82nd airborne division
# # 88th medical group
# # 96th medical group
# # 97th military police battalion
# # 99th medical group
# # united states department of army
# # united states department of defense
# # united states department of health and human services
# # united states department of the army
# # united states department of the navy
# # united states department of the treasury
# # us department of commerce
# # us department of justice
# # us department of labor
# # us department of state
# # us department of state contractor
# # us department of the army
# # us department of the interior
# # us department of the interior bureau land management
# # us department of the interior bureau land management idaho
# # us department of transportation
# # us department of veteran's affairs
# # usu-walter reed department of surgery
# # usu-wrnmmc department of surgery
# # usd a-ars-national sedimentation laboratory
# # national museum of health and medicine
# # 
# # # Department of Housing and Urban Development
# # # Housing and Urban Development
# # "cddc" (us army)
# # "ames laboratory"
# # "ames"
# # 
# # Fogarty International Center - Part of the NIH.
# # National Center for Advancing Translational Sciences (NCATS) - Part of the NIH.
# # National Center for Biotechnology Information (NCBI) - Part of the National Library of Medicine, NIH.
# # National Center for Chronic Disease Prevention and Health Promotion - Part of the CDC.
# # National Center for Complementary and Integrative Health (NCCIH) - Part of the NIH.
# # National Center for Emerging and Zoonotic Infectious Diseases - Part of the CDC.
# # National Center for Environmental Health - Part of the CDC.
# # National Center for Health Statistics - Part of the CDC.
# # National Center for HIV, Viral Hepatitis, STD, and TB Prevention - Part of the CDC.
# # National Center for Immunization and Respiratory Diseases - Part of the CDC.
# # National Center for Injury Prevention and Control - Part of the CDC.
# # National Center for Post-Traumatic Stress Disorder (PTSD) - Part of the Department of Veterans Affairs.
# # National Center for Science and Engineering Statistics - Part of the National Science Foundation.
# # National Center for Toxicological Research - Part of the FDA.
# # 
# # VA National Center for Patient Safety - Part of the Department of Veterans Affairs.
# # National Center for Agricultural Utilization Research - Part of the USDA.
# # National Center for Cool and Coldwater Aquaculture - Part of the USDA.
# # National Center for Disease Control and Public Health - Likely refers to international or non-U.S. entities.
# # National Center for Ecological Analysis and Synthesis - Affiliated with the University of California, Santa Barbara.
# # National Center for Electron Microscopy - Part of the Lawrence Berkeley National Laboratory.
# # National Center for Farmworker Health - A nonprofit organization.
# # National Center for Genome Resources - A nonprofit organization.
# # National Center for Ontological Research - Likely an academic or private entity.
# # National Center for Rehabilitative Auditory Research - Part of the Department of Veterans Affairs.
# # National Center for Telehealth and Technology - Part of the Department of Defense.
# # National Centers for Coastal Ocean Science - Part of NOAA.
# # National Centers for Environmental Information - Part of NOAA.
# # National Centers for Environmental Prediction (NCEP) - Part of NOAA.
# # Woodrow Wilson International Center - A nonpartisan policy forum chartered by Congress but not a federal agency.
# # national research council
# # national research institute
# # national science board
# # national science policy network
# # national severe storms laboratory norman
# # national snow and ice data center
# # national spine and pain centers
# # national sports medicine institute
# # national strategic research institute
# # national toxicology program (ntp)
# # national tribal energy association
# # national tropical botanical garden
# # national university of defense technology
# # national war college
# # national weather center
# # national weather service
# # national wetlands research center
# # national wildlife refuge
# # national zoo
# # national zoological park
# # nevada national security site
# # northwest national laboratory
# # national forest
# # office of the national coordinator for health information technology
# # national park
# # oregon national primate research center
# # national forests
# # national seashore
# # advisory committee on national health promotion and disease prevention
# # national fish and wildlife refuge
# # stellwagen bank national marine sanctuary
# # national museum of natural history
# # national fish hatchery
# # u s geological survey-national wildlife health center
# # united states fish and wildlife service international affairs
# # national wildlife and fish refuge
# # us department of agriculture
# # us fish and wildlife national forensics laboratory
# # us forest service international programs wood identification and screening center
# # us forest service on the flathead national forest
# # us national arboretum
# # us national library of medicine
# # us national poultry research center
# # us national security advisor
# # us national security council
# # usd a-ars-national sedimentation laboratory
# # usfs medicine bow/routt national forests and thunder basin national grassland
# # va national expert consultation &amp; specialized services
# # va national pharmacogenomics program
# # va national surgery office
# # va national teleoncology
# # veteran affairs national telemental health hub
# # veterans administration national center for patient safety
# # veterans affairs national center for patient safety
# # walter reed national military center
# # walter reed national military med-ical center
# # national estuarine research reserve
# # national fish hatchery
# # tulane national primate research center
# # national socio-environmental synthesis center (sesync)
# # southwest national primate research center
# # wisconsin national primate research center
# # washington national primate research center
# # air combat command
# # national nuclear security administration
# # national oceanographic and atmospheric administration
# # national oceanic/atmospheric admin
# # national museum of the american indian
# # national museum of health and medicine
# # national museum of asian art
# # national museum of american history
# # national library of medicine (nlm)
# # national laboratory for agriculture and the environment
# # national inventory and monitoring applications 
# # national institute on minority health and health 
# 
# 
# # "us department of commerce"                                                                                             
# # "defense threat reduction agency"
# # "agency for healthcare research and quality"
# # "defense advanced research projects agency"
# # "us department of transportation"
# # "national geospatial-intelligence agency"
# # "us department of education"
# # "us department of defense"
# # "national interagency fire center"
# # "us department of the interior"
# # "central intelligence agency"
# # "us department of the treasury"
# # "us department of health and human services"
# # "us department of state"
# # "military vaccine agency"
# # "us department of justice"
# # "federal emergency management agency"
# # "us department of the interior"
# # "bureau of land management"
# # "bureau land management"
# # "us department of labor"
# # "us department of homeland security"
# # "us department of the navy"
# # "agency for toxic substances and disease registry"
# # "agency for toxic substance and disease registry"
# # "national security agency"
# # "central security service"
# # "advanced research projects agency - energy"
# # "arpa-e"
# # "defense advanced research projects agency"
# # "darpa"
# # "us department of the army"
# # "defense pow/mia accounting agency"
# # "defense logistics agency"
# # "agency for international development"
# # "federal emergency management agency"
# # "defense nuclear agency"
# # "defense advanced projects research agency"
# # "defense logistics agency"
# # "us department of homeland security"
# # "defense intelligence agency"
# # "national geospatial-intelligence agency""
# # "defense information systems agency"
# # "interagency grizzly bear study team"
# # "us department of the navy"
# # "agency for international development (aid)"
# # "agency for health care research &amp; quality"
# # "us department of state"
# # "us of america"
# # "contracting agency to the division of viral diseases"
# # "us department of the army"
# # "interagency special status/sensitive species program"
# # "us agency for internationl development"
# # "federal emergency management agency"
# # "agency for toxic substances and disease registry (atsdr)"
# # "scientific resource center for the agency for healthcare research and quality"
# # "us department of defense hpc modernization program"
# # "us department of the army"
# # "agency for healthcare research and quality evidence-based practice centers program"
# # "federal emergency management agency"
# # "us department of defense joint trauma system en route combat casualty care"
# # "us department of defense hpc modernization program/gdit"
# # "naval facilities engineering systems command us department of the navy"
# # "enivronmental protection agency"
# # "defence health agency (sfm)"
# # "defence health agency"
# # "us department of the navy"
# # "us of america army medical research institute of infectious diseases"
# # "us department of veterans affairs"
# # "us department of veterans affairs"


# round 2 -----------------------------------------------------------------


# DONE 2019-2025 as usda2
# AFFIL(daniel k inouye us pacific basin agricultural research) OR
# AFFIL(National Center for Agricultural Utilization Research) OR
# AFFIL(National Center for Cool and Coldwater Aquaculture) OR
# AFFIL(national clonal germplasm repository for citrus) OR
# AFFIL(national fish hatchery) OR
# AFFIL(national forest) OR
# AFFIL(national inventory and monitoring applications) OR
# AFFIL(national laboratory for agriculture and the environment) OR
# AFFIL(national sedimentation laboratory) OR
# AFFIL(national tropical botanical garden) OR
# AFFIL(us forest products laboratory) OR
# AFFIL(us forest service international programs) OR
# AFFIL(us forest service international programs wood identification and screening center) OR
# AFFIL(us horticultural laboratory) OR
# AFFIL(us national arboretum) OR
# AFFIL(us national poultry research center) OR
# AFFIL(us pacific basin agricultural research center) OR
# AFFIL(us plant soil and nutrition laboratory) OR
# AFFIL(us potato genebank) OR
# AFFIL(us salinity laboratory) OR
# AFFIL(us vegetable breeding laboratory) OR
# AFFIL(us vegetable lab) OR
# AFFIL(us vegetable laboratory) OR
# AFFIL(national sedimentation laboratory) OR

# SMITHSONIAN DONE as SMITH2 done 2019-2025 
# AFFIL(national museum of american history) OR
# AFFIL(national museum of asian art) OR
# AFFIL(national museum of natural history) OR
# AFFIL(national museum of natural history) OR
# AFFIL(national museum of the american indian) OR
# AFFIL(national zoo) OR
# AFFIL(national zoological park) OR
# AFFIL(smithsonian institution) 


# STATE & TREAS DONE 2019-2025
# office of the us global aids coordinator
# us department of the treasury
# us secret service
# agency for international development
# us presidents malaria initiative

# VA DONE 2019-2025
# "national center for ptsd",
# "captain james a lovell federal health care center",
# "National Center for Post-traumatic stress disorder",
# "National Center for Rehabilitative Auditory Research",
# "VA National Center for Patient Safety",
# "va national pharmacogenomics program",
# "va national surgery office",
# "va national teleoncology",
# "va serious mental illness treatment resource and eval",
# "veteran affairs national telemental health hub",
# "veterans administration national center for patient safety",
# "veterans affairs national center for patient safety",
# "national center on homelessness among veterans"

# MISC3 DONE 2019-2025
# AFFIL(us postal inspection service) OR
# AFFIL(central security service) OR
# AFFIL(national security agency) OR
# AFFIL(National Center for Science and Engineering Statistics) OR
# AFFIL(national science board) OR
# AFFIL(us office of personnel management) OR
# AFFIL(national endowment for the arts) OR
# AFFIL(federal maritime commission) OR
# AFFIL(interagency grizzly bear study team) OR
# AFFIL(interagency special status/sensitive species program) OR
# AFFIL(national interagency fire center) OR
# AFFIL(national research council) 

# MISC4 DONE 2019-2025
# "us arctic research commission",
# "us global change research program",
# "us international development finance corporation",
# "director of the congressional budget office",
# "central intelligence agency",
# "us botanic garden",
# "us house of representatives",
# "us senate",
# "consumer product safety commission",
# "national museum of health and medicine",
# "customs and border protection",
# "department of homeland security"




# DONE 2019-2025

# "us national security advisor",
# "us national security council",
# "federal emergency management agency",
# "director of the federal housing finance agency",
# "chair of the federal reserve",
# "federal reserve",
# "us government accountability office",
# "denver federal center",
# "us general services administration",
# "national academy of engineering",
# "national academy of sciences",
# "national credit union administration"


# NIH FULL DONE 2019-2025
# "national center for infectious diseases",
# "Fogarty International Center",
# "National Cancer Institute",
# "National Center for Advancing Translational Sciences",
# "National Center for Biotechnology Information",
# "National Center for Complementary and Integrative Health",
# "National Eye Institute",
# "National Heart, Lung, and Blood Institute",
# "National Human Genome Research Institute",
# "National Institute for Occupational Safety and Health",
# "National Institute of Allergy and Infectious Diseases",
# "National Institute of Arthritis and Musculoskeletal and Skin Diseases",
# "National Institute of Biomedical Imaging and Bioengineering",
# "National Institute of Child Health and Human Development",
# "National Institute of Dental and Craniofacial Research",
# "National Institute of Diabetes and Digestive and Kidney Diseases",
# "National Institute of Environmental Health Sciences",
# "National Institute of General Medical Sciences",
# "National Institute of Mental Health",
# "National Institute of Neurological Disorders and Stroke",
# "National Institute of Nursing Research",
# "National Institute on Aging",
# "National Institute on Alcohol Abuse and Alcoholism",
# "National Institute on Deafness and Other Communication Disorders",
# "National Institute on Drug Abuse",
# "National Institute on Minority Health and Health Disparities",
# "National Library of Medicine",
# "national toxicology program"

NIH ABBREVIATION
ncbi
NCI
NEI
NHGRI
NHLBI
NIA
NIAAA
NIAID
NIAMS
NIBIB
NICHD
NIDA
NIDCD
NIDCR
NIDDK
NIEHS
NIGMS
NIMH
NIMHD
NINDS
NINR
niosh
nlm
ntp
us national library of medicine
astdr


# NOAA DONE 2016-2019
# "National Centers for Coastal Ocean Science",
# "National Centers for Environmental Information",
# "National Centers for Environmental Prediction",
# "national estuarine research reserve",
# "national hurricane center",
# "central pacific hurricane center",
# "national severe storms laboratory",
# "national weather service",
# "us integrated ocean observing system",
# "us nws caribbean tsunami warning program"


# HHS DONE 2019-2025
# "advisory committee on national health promotion and disease prevention",
# "hhs office of infectious disease and hiv/aids policy",
# "office of the national coordinator for health information technology",
# "us department of health and human services",
# "national institute on disability, independent living, and rehabilitation research"

# HUD DONE 2019-2025
# "department of housing and urban development"


# INTERIOR DIBE 2019-2025
# national park
# national park service


AG
Agricultural Research Service 
National Institute of Food and Agriculture


# LABOR DONE 2019-2025
# us department of labor



# INTERIOR DONE 2019-2025
# "national historical park",
# "bureau of land management",
# "nadp",
# "national atmospheric deposition program",
# "national fish and wildlife refuge",
# "national marine sanctuary"
# "national seashore",
# "national wetlands research center",
# "national wildlife and fish refuge",
# "national wildlife health center",
# "national wildlife refuge",
# "u s geological survey",
# "national wildlife health center",
# "united states fish and wildlife service international affairs",
# "us bureau of ocean energy management",
# "us fish and wildlife national forensics laboratory",
# "us geographical survey"



# CDC 2019-2025 DONE 
# "national center for birth defects and developmental disabilities",
# "agency for toxic substance and disease registry",
# "National Center for Chronic Disease Prevention and Health Promotion",
# "National Center for Disease Control and Public Health",
# "National Center for Emerging and Zoonotic Infectious Diseases",
# "National Center for Environmental Health",
# "National Center for Health Statistics",
# "National Center for HIV, Viral Hepatitis, STD, and TB Prevention",
# "National Center for Immunization and Respiratory Diseases",
# "National Center for Injury Prevention and Control",
# "National Center for Preparedness Detection and Control of Infectious Diseases"

# Commerce DONE 2019-2025
# us department of commerce

# Education DONE 2019-2025
# us department of education


# COMMERCE AND EPA DONE 2019-2025 
# "us bureau of the census",
# "us patent and trademark office",
# "national health and environmental effects research laboratory",
# "national center for environmental economics",
# "cber",
# "center for biologics evaluation and research",
# "Food and Drug Administration",
# "National Center for Toxicological Research",
# "us food and drug admnistration",

# DOD done 2016-2019
# national immunization program
# national guard bureau
# 82nd airborne division
# 88th medical group
# 96th medical group
# 97th military police battalion
# 99th medical group
# air combat command
# air force
# cddc
# chief of naval air training
# Combat Capabilities Development Command
# darpa
# defence health agency
# defense advanced research projects agency
# defense information systems agency
# defense intelligence agency
# defense logistics agency
# defense nuclear agency
# defense pow/mia accounting agency
# defense threat reduction agency
# department of defense
# department of the air force
# department of the army
# department of the navy
# institute of infectious diseases
# madigan army medical center
# martin army community hospital
# military vaccine agency
# National Center for Telehealth and Technology
# national geospatial-intelligence agency
# national strategic research institute # DOD-designated University Affiliated Research Center (UARC) of U.S. Strategic Command and the University of Nebraska System, the National Strategic ...
# national war college

# "naval facilities engineering systems command us department of the navy",
# "navy medical readiness and training command",
# "northwest national laboratory",
# "united states department of army",
# "us 8th army korea medical simulation training center",
# "us air force",
# "us air war college",
# "us armed forces health surveillance division",
# "us armed services blood program office",
# "us baylor military graduate program in nutrition",
# "us central command",
# "us coast guard",
# "us combat casualty care research program",
# "us department of defense",
# "us department of defense hpc modernization program",
# "us department of defense joint trauma system en route combat casualty care",
# "us department of the army",
# "us department of the navy",
# "us deputy assistant secretary of the army",
# "us devcom army research laboratory",
# "us fleet forces command",
# "us indo-pacific command",
# "us marine corps",
# "us marine corps school of infantry-east",
# "us marine corps special operations command",
# "us marine corps university",
# "us marine forces cyberspace command",
# "us military",
# "us military hiv research program",
# "us military-baylor graduate program in nutrition",
# "us naval",
# "us of america army medical research institute of infectious diseases",
# "usarmy health clinic",
# "Walter Reed Army Institute of Research",
# "walter reed national military",
# "walter reed national military center",
# "walter reed national military medical center",
# "wrair",
# "hurricane flood risk reduction design branch"

# us pacific fleet
# us southern command
# us space force
# us special operations command




# DOE DONE 2019-2025
  # "us department of transportation",
  # "federal highway administration",
  # "federal railroad administration",
  # "us department of transportation",
  # "national highway traffic safety administration",
  # "national high magnetic field laboratory los almos",
  # "advanced research projects agency - energy",
  # "arpa-e",
  # "federal energy regulatory commission",
  # "National Center for Electron Microscopy",
  # "National Energy Technology Laboratory",
  # "national nuclear security administration",
  # "nevada national security site",
  # "us department of energy joint genome institute",
  # "us dep of energy great lakes bioenergy research center",
  # "us energy information administration",
  # "us iter project office",
  # "us regulatory commission"
# "national high magnetic field laboratory los almos",
# "advanced research projects agency - energy",
# "arpa-e",
# "federal energy regulatory commission",
# "National Center for Electron Microscopy",
# "National Energy Technology Laboratory",
# "national nuclear security administration",
# "nevada national security site",
# "us department of energy joint genome institute",
# "us dep of energy great lakes bioenergy research center",
# "us energy information administration",
# "us iter project office",
# "us regulatory commission"

# DOJ done 2019-2025
# "national institute of justice",
# "attorney general",
# "department of justice",
# "federal bureau of prisons",
# "federal correctional institution",
# "federal medical center",
# "us bureau of alcohol",
# "us department of justice",
# "us marshalls service",
# "us marshalls service"

# DOT DONE 2019-2025
# federal highway administration
# federal railroad administration
# us department of transportation
# national highway traffic safety administration


# completed searches ------------------------------------------------------


# EPA ---------------------------------------------------------------------


# EPA  2019-2025  DONE
# AFFIL(united states environmental protection agency) OR
# AFFIL(environmental protection agency) OR 
# AFFIL(EPA)


# USDA --------------------------------------------------------------------

# USDA  2009-2018 DONE 
# USDA  2019-2025 DONE
# "United States Department of Agriculture" 2009-2018 
# "United States Department of Agriculture" 2009-2025 DONE


# ARS ---------------------------------------------------------------------

# ARS 2019-2025 DONE
# AFFIL(agricultral research service) OR
# AFFIL(ars)

# NIH ---------------------------------------------------------------------

# NIH 2009-2018 DONE 
# NIH 2019-2025 DONE 
# "National Institutes of Health" 2009-2025 DONE


# SMITHSONIAN -------------------------------------------------------------


# Smithsonian 2009-2018  
# Smithsonian 2019-2025 DONE 


# USGS --------------------------------------------------------------------

# USGS 2019-2025 DONE
# USGS 2009-2018
# (AFFILORG(USGS) OR 
# AFFILORG(United States Geological Survey) OR 
# AFFILORG(US Geological Survey) OR 
# AFFILORG(U.S. Geological Survey)


# CDC ---------------------------------------------------------------------

# CDC 2019-2023 DONE
# CDC 2024-2025 DONE
# CDC 2009-2018
# (AFFILORG(CDC) OR AFFILORG(Centers for Disease Control and Prevention) 
# OR AFFILORG(US Centers for Disease Control and Prevention)  
# OR AFFILORG(U. S. Centers for Disease Control and Prevention)
# TODO: remove "CDC Foundation"

# DOE ---------------------------------------------------------------------


# DOE 2009-2018
# DOE 2019-2025 DONE
# (AFFIL (united states department of energy) OR AFFIL (doe) 
# OR AFFIL (us department of energy) OR AFFIL (u.s. department of energy) 
# AND AFFILCOUNTRY (united states)) 

# NASA --------------------------------------------------------------------



# NASA 2009-2018
# NASA 2019-2025 DONE
# national aeronautics and space administration 2019-2025 DONE

# MISC 1 ------------------------------------------------------------------

# MISC1 2019-2025 DONE
# AFFIL(USAID*) OR
# AFFIL(United States Agency for International Development) OR
# AFFIL(US Agency for International Development) OR
# AFFIL(us bureau of labor statistics) OR
# AFFIL(us census bureau) OR
# AFFIL(STRI) OR
# AFFIL(Smithsonian Tropical Research Institute) 


# MISC 2 ------------------------------------------------------------------


# MISC2 2019-2025 DONE
# AFFIL(nsf) OR
# AFFIL(national science foundation) OR
# AFFIL(hrsa) OR
# AFFIL(Health Resources and Services Administration) OR
# AFFIL(cbo) OR
# AFFIL(congressional budget office) OR
# AFFIL(ahrq) OR
# AFFIL(Agency for Healthcare Research and Quality) OR
# AFFIL(fbi) OR
# AFFIL(federal bureau of investigation) OR
# AFFIL(federal housing finance agency) OR
# AFFIL(federal reserve system)


# MISC 3 ------------------------------------------------------------------


# misc3  2019-2025 DONE
# AFFIL(us department of housing urban development) OR
# AFFIL(hud) OR
# AFFIL(faa) OR
# AFFIL(Federal Aviation Administration) OR
# AFFIL(fda) OR
# AFFIL(Food and Drug Administration) OR
# AFFIL(nrao) OR
# AFFIL(National Radio Astronomy Observatory) OR
# AFFIL(national solar observatory) OR
# AFFIL(patuxent wildlife research center) OR
# AFFIL(uniformed services university* ) OR
# AFFIL(united states forest service) OR
# AFFIL(united states public health service) OR
# AFFIL(us fish and wildlife service) OR
# AFFIL(us national park service)

# MIL 1 -------------------------------------------------------------------

# Mil1 2019-2025 DONE
# AFFIL(united states military academy) OR
# AFFIL(united states naval academy) OR
# AFFIL(us naval observatory) OR
# AFFIL(united states naval observatory) OR
# AFFIL(united states naval research laboratory) OR
# AFFIL(us naval research laboratory) OR
# AFFIL(united states naval research laboratory) OR
# AFFIL(us navy) OR
# AFFIL(united states navy) 


# MIL2 -------------------------------------------------------------------

# Mil2 2019-2025 DONE
# AFFIL(us army) OR
# AFFIL(united states army) OR
# AFFIL(usaf) OR
# AFFIL(united states air force) OR
# AFFIL(united states air force academy) OR
# AFFIL(air force institute of technology) OR
# AFFIL(afit) OR
# AFFIL(air force research lab)

# MIL 3 -------------------------------------------------------------------

# mil3  2019-2025 DONE
# AFFIL(nmrc) OR
# AFFIL(Naval Medical Research Command) OR
# AFFIL(san antonio military medical center) OR
# AFFIL(walter reed national military medical center) OR
# AFFIL(wrair) OR
# AFFIL(Walter Reed Army Institute of Research) OR
# AFFIL(u s army corps of engineers) OR
# AFFIL(madigan army medical center) OR
# AFFIL(defense health agency) OR
# AFFIL(dha) OR
# AFFIL(carl r darnall army medical center) OR
# AFFIL(national defense university) OR
# AFFIL(naval medical center san diego) OR
# AFFIL(naval medical research center ) OR
# AFFIL(naval postgraduate school) OR
# AFFIL(erdc) OR
# AFFIL(U.S. Army Engineer Research and Development Center)


# VETS --------------------------------------------------------------------

# vets  2019-2025 DONE
# AFFIL(va caribbean healthcare system) OR
# AFFIL(vha) OR
# AFFIL(department of veterans affairs) OR
# AFFIL(Veterans Health Administration)

# NIST 2009-2018
# NIST 2019-2025 DONE
# National Institute of Standards and Technology
# (AFFIL(NIST) OR 
# AFFIL(National Institute of Standards and Technology)


# NOOA --------------------------------------------------------------------

# noaa 
# AFFIL(noaa*) OR 
# AFFIL(national center for atmospheric research) OR
# AFFIL(national center for environmental prediction ) OR
# AFFIL(national institute of water and atmospheric research) OR
# AFFIL(national oceanic and atmospheric administration)


# NATIONAL LABS -----------------------------------------------------------

# Lawrence Berkeley National Laboratory DONE 2019-2025
# Oak Ridge National Laboratory DONE 2019-2025
# Argonne National Laboratory DONE 2019-2025
# Pacific Northwest National Laboratory DONE 2019-2025
# Idaho National Laboratory DONE 2019-205
# Ames National Laboratory DONE 2019-2025
# AFFIL(Brookhaven National Laboratory) DONE 2019-2025
# AFFIL(Princeton Plasma Physics Laboratory) DONE 2019-2025
# AFFIL(National Energy Technology Laboratory) DONE 2019-2025
# AFFIL(SLAC National Accelerator Laboratory) DONE 2019-2025
# AFFIL(Fermi National Accelerator Laboratory) DONE 2019-2025
# AFFIL(Thomas Jefferson National Accelerator)  DONE 2019-2025
# AFFIL(National Renewable Energy Laboratory) DONE 2019-2025
# AFFIL(Sandia National Laboratories) DONE 2019-2025
# AFFIL(Savannah River National Laboratory)  DONE 2019-2025
# AFFIL(National Energy Technology Laboratory) DONE 2019-2025
# AFFIL(Lawrence Livermore National Laboratory) DONE 2019-2025
# AFFIL(Los Alamos National Laboratory) 
# AFFIL(Jet Propulsion Laboratory) 















# API CALL ----------------------------------------------------------------

yr1=2019
yr2=2025
date_range <- seq(yr1,yr2)
year <- seq_along(date_range)

####
#DONT FORGET TO CHANGE THE FILE NAME BELOW!!!!!!!
#####

for (h in term){

for (j in year) {
  
  a<-"((AFFIL(us postal inspection service) OR AFFIL(central security service) OR AFFIL(national security agency) OR AFFIL(National Center for Science and Engineering Statistics) OR AFFIL(national science board) OR AFFIL(us office of personnel management) OR AFFIL(national endowment for the arts) OR AFFIL(federal maritime commission) OR AFFIL(interagency grizzly bear study team) OR AFFIL(interagency special status/sensitive species program) OR AFFIL(national interagency fire center) OR AFFIL(national research council) AND AFFILCOUNTRY('united states')) AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))"
  
  ###  DONT FORGET TO CHANGE THE FILE NAME BELOW!!!!!!! ###
  
  
  c <- " AND PUBYEAR = "
  
  query_string <-paste0(a, c, date_range[j],")",sep = "")
  # query_string <-paste0(a, b, c, date_range[j],sep = "")
  # query_string <-paste(a, b, c, date_range[j], ')',sep = "")
  # api1: 38c1ea28aed25f40f11034d20557ccde
  # 8d8d7b628fae6e1a5a04db969b0bca93
  # api2: 8e204bc721cb41c0251c8846351342b0
  # api3: c253aa47dd592442b1d5ad7ded7b0514
  # api4: 8d8d7b628fae6e1a5a04db969b0bca93
  # 
  scopus_data <- rscopus::scopus_search(query_string,
    max_count=8000,
    view = "COMPLETE",
    api_key = "8d8d7b628fae6e1a5a04db969b0bca93")
  
  
  # query_string <- paste0("eid(2-s2.0-0024266051)")
  
  scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
  scopus_papers <- scopus_data_raw$df
  # jae_papers$`prism:publicationName`
  papers <- paste("./data_raw/scopus_api/papers/misc3_scopus_papers_", date_range[j], ".csv", sep = "")
  write_csv(scopus_papers, papers)

  scopus_affiliations <- scopus_data_raw$affiliation
  affils <- paste("./data_raw/scopus_api/affils/misc3_scopus_affils_", date_range[j], ".csv", sep = "")
  write_csv(scopus_affiliations, affils)

  scopus_authors <- scopus_data_raw$author
  authors <- paste("./data_raw/scopus_api/authors/misc3_scopus_authors_", date_range[j], ".csv",sep = "")
  write_csv(scopus_authors, authors)
  
}


  

# long loop ---------------------------------------------------------------
  
  yr1=2024
  yr2=2025
  
  
  # "us naval", need 2024,2025
  # "walter reed national military",need 2024,2025
  
  
  
  search_term<-fed_affiliations_list %>% 
    select(affil_id) %>% 
    tibble() %>% 
    distinct() %>% 
    slice(1:5)
  
  search_term<-c(


    
    
       )
  
  
  date_range <- seq(yr1,yr2)
  str(search_term)
  
  
  year <- seq_along(date_range)
  term <- seq_along(search_term)
  
  for (h in term){
    
    for (j in year) {
      
      a<-paste("((AF-ID('",search_term$affil_id[h],"')"," AND AFFILCOUNTRY('united states')) AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
      
      # a<-paste("((AFFIL(",search_term[h],")"," AND AFFILCOUNTRY('united states')) AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
      
      
      ###  DONT FORGET TO CHANGE THE FILE NAME BELOW!!!!!!! ###
      
      
      c <- " AND PUBYEAR = "
      
      query_string <-paste0(a, c, date_range[j],")",sep = "")
      
      # query_string <-paste0(a, b, c, date_range[j],sep = "")
      # query_string <-paste(a, b, c, date_range[j], ')',sep = "")
      # api1: 38c1ea28aed25f40f11034d20557ccde
      # 8d8d7b628fae6e1a5a04db969b0bca93
      # api2: 8e204bc721cb41c0251c8846351342b0
      # api3: c253aa47dd592442b1d5ad7ded7b0514
      # api4: 8d8d7b628fae6e1a5a04db969b0bca93
      # 
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=8000,
                                            view = "COMPLETE",
                                            api_key = "c253aa47dd592442b1d5ad7ded7b0514")
      
      nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3
      # query_string <- paste0("eid(2-s2.0-0024266051)")
      scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
      if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
        next
      }else{
      scopus_papers <- scopus_data_raw$df
      # jae_papers$`prism:publicationName`
      term_for_file<-gsub(" ","-",search_term[h])
      papers <- paste("./data_raw/scopus_api/papers/",term_for_file,"_scopus_papers_", date_range[j], ".csv", sep = "")
      write_csv(scopus_papers, papers)
      
      scopus_affiliations <- scopus_data_raw$affiliation
      
      affils <- paste("./data_raw/scopus_api/affils/",term_for_file,"_scopus_affils_", date_range[j], ".csv", sep = "")
      write_csv(scopus_affiliations, affils)
      
      scopus_authors <- scopus_data_raw$author
      authors <- paste("./data_raw/scopus_api/authors/",term_for_file,"_scopus_authors_", date_range[j], ".csv",sep = "")
      write_csv(scopus_authors, authors)
      }
    }
}














# api loop for keywords by DOI --------------------------------------------

library(rscopus)
library(tidyverse)
DOI<-read.csv("./bibliometrics/data_raw/scopus_no_kw.csv")
DOI<-as.vector(DOI$DI)
kw_list<-list()
for (j in 1:length(DOI)) {
  a <- "DOI("
  b<-")"
  #   DOI<-"10.1111/1365-2745.12194"
  # j=1   j=2     j=3
  
  query_string <-paste0(a, DOI[j],b,sep = "")
  # 
  scopus_data <- rscopus::scopus_search(query_string,
                                        view = "COMPLETE",
                                        # max_count = 5,
                                        api_key = "eef216ea695006c2039f8da2a1c27c09")

  scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
  scopus_papers <- scopus_data_raw$df
   if (is.null(scopus_data[[1]][[1]]$authkeywords)){
  kw_output<-NA
     }else{
  kw_output<- scopus_data[[1]][[1]]$authkeywords
   }
  
  dat <- data.frame(kw_output)
  dat$DOI <- DOI[j]  # maybe you want to keep track of which iteration produced it?
  kw_list[[j]] <- dat # add it to your list
  
} 
# big_data <- dplyr::bind_rows(kw_list)
big_data = do.call(rbind, kw_list)   
write_csv(big_data,"./bibliometrics/data_clean/scopus_no_kw_recovery.csv")

names(big_data)
final_kw<-big_data %>% filter(!is.na(kw_output))
write_csv(final_kw,"./bibliometrics/data_clean/final_kw.csv")






# api loop for keywords by TI and PY  --------------------------------------------

library(rscopus)
library(tidyverse)
DOI<-read.csv("./bibliometrics/data_raw/refs_no_db.csv")
DOI<-as.vector(DOI$ti5)
# DOI<-DOI
SD<-list()
for (j in 1:length(DOI)) {
  a <- ""
  b<-""
  
# DOI<-"DOI(10.1111/1365-2745.12194)"
#   #   DOI <- "DOI(10.1111/1365-2745.12194) and PUBYEAR=1991\""
#   DOI <- "ISSN(00063606) AND PUBYEAR>1991 AND PUBYEAR<1991"
#   DOI <- "KEY(\"cryoelectron microscopy\") AND PUBYEAR > 2005 AND PUBYEAR < 2016\""
#   DOI<-"TITLE(\"short-term activity cycle in ants\") AND PUBYEAR = 1991"
# DOI<-"TITLE(\"short-term activity cycles in ants\") and PUBYEAR = 1991 AND SRCTITLE(\"American Naturalist\")"
# j=1   j=2     j=3
#   
  query_string <-paste0(a, DOI[j],b,sep = "")
  # 
  scopus_data <- rscopus::scopus_search(query_string,
                                        view = "COMPLETE",
                                        api_key = "35605c9cbb3dfa13435f1336d10ff4e3")
  
  
  # 
  # 
  dat <- gen_entries_to_df(scopus_data$entries)
  # scopus_papers <- scopus_data_raw$df

  
  dat$item <- j  # maybe you want to keep track of which iteration produced it?
  SD[[j]] <- dat # add it to your list

} 

write_rds(SD,"./bibliometrics/data_raw/SD.rds")
rm(SD_authors,SD_authors2)

SD_refs<-tibble()
SD_refs2<-tibble()
SD_authors<-tibble()
SD_authors2<-tibble()
SD_affils<-tibble()
SD_affils2<-tibble()
for (j in 1:length(SD)) {
  # j=1
  # j=2
  
  
  
  SD_refs<-bind_rows(SD[j][[1]]$df)
  SD_refs2<-bind_rows(SD_refs,SD_refs2)
  
  SD_authors<-bind_rows(SD[j][[1]]$author)
  SD_authors2<-bind_rows(SD_authors,SD_authors2)
  
  SD_affils<-bind_rows(SD[j][[1]]$affiliation)
  SD_affils2<-bind_rows(SD_affils,SD_affils2)
} 

as.data.frame(do.call(cbind, SD))
# big_data <- dplyr::bind_rows(SD)
big_data = do.call(rbind, SD)   
write_csv(big_data,"./bibliometrics/data_clean/scopus_no_kw_recovery.csv")

names(big_data)
final_kw<-big_data %>% filter(!is.na(kw_output))
write_csv(final_kw,"./bibliometrics/data_clean/final_kw.csv")







# ISSN EVOL
# 15585646
# 00143820

# a <- "EXACTSRCTITLE(journal of tropical ecology)"
# a <- 'ISSN(00347744)'
# a <- 'ISSN(The American Naturalist)'
# a <- "EXACTSRCTITLE(American Naturalist)"
# a <- "EXACTSRCTITLE(Ecology)"
# b <- " AND DOCTYPE(ar OR no OR re OR dp)"
# b <- " AND (DOCTYPE(ar) OR  DOCTYPE(no) OR DOCTYPE(re) OR  DOCTYPE(dp))"
# b <- " AND DOCTYPE(ar OR re)"
# query_string <- paste0("EXACTSRCTITLE(Biotropica) ", 
#                 # "AND DOCTYPE(ar)")
#                 "AND DOCTYPE(ar) ",
#                 "AND PUBYEAR = 1999")
#   