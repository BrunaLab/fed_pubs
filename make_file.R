

###########################################################################
###########################################################################

# process federal records -------------------------------------------------

## choose downloads to process --------------------------------------------

# cat<-"fed"
# date<-"20250901"
# scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-09-01.csv")
# PM_max<-8

# cat<-"fed"
# date<-"20251010"
# scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv")
# PM_max<-8


# process the usgs publication file downloaded from their website ---------

# source("./code/process_usgs.R")
# process_usgs(---,---)


# within year: bind csvs for the scopus IDs -------------------------------

source("./code/csv_binder_within_year.R")
csv_binder_within_year(cat,date)

# across years: bind the annual csvs --------------------------------------

source("./code/csv_binder_across_years.R")
csv_binder_across_years(cat,date)

# clean up the composite files --------------------------------------------

source("./code/clean_fed.R")
clean_fed(cat, date,scopus_ids_searched)

# figures and summaries of results ----------------------------------------

source("./code/make_figs_fed.R")
make_figs_fed(cat,date,PM_max)

# bootstrapping -----------------------------------------------------------

source("./code/bootstrap_npubs_fed.R")
bootstrap_npubs_fed(cat, date,PM_max)

###########################################################################
###########################################################################

# process university records ----------------------------------------------

## select the year and category to process --------------------------------

cat<-"uni"
date<-"20250901"
PM_max<-8


# cat<-"uni"
# date<-"20251010"
# PM_max<-8

# within year: bind csvs for the scopus IDs -------------------------------

source("./code/csv_binder_within_year.R")
csv_binder_within_year(cat,date)

# across years: bind the annual csvs --------------------------------------

source("./code/csv_binder_across_years.R")
csv_binder_across_years(cat,date)

# clean up the composite files --------------------------------------------

source("./code/clean_uni.R")
clean_uni(cat,date)
gc()
# figures and summaries of results ----------------------------------------

source("./code/make_figs_uni.R")
make_figs_uni(cat,date,PM_max)
gc()
# bootstrapping -----------------------------------------------------------

source("./code/bootstrap_npubs_uni.R")
bootstrap_npubs_uni(cat, date,PM_max)
gc()


