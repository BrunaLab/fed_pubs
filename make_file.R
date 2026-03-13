
# SCRAPE DATA FROM SCOPUS 
# 
# `01a_scopus_API_fed.R`
# `01b_scopus_API_uni.R`

# DOWNLOAD DATA FROM USGS PUBLICATIONS WAREHOUSE

# Process with `02_process_usgs.R`

 
###########################################################################
###########################################################################

# to fix: 60002746_2017_04-1

# chose data download date and max month  ---------------------------------


date<-"20260101"
PM_max<-12
PY_max<-2025
PY_min<-2020
author_position<-"first"
# author_position<-"anywhere"
# process federal records -------------------------------------------------

# process the usgs publication file downloaded from their website ---------

# source("./code/process_usgs.R")
# process_usgs(---,---)

# within year: bind csvs for the scopus IDs -------------------------------

cat<-"fed"
source("./code/csv_binder_within_year.R")
csv_binder_within_year(cat,date)

cat<-"uni"
source("./code/csv_binder_within_year.R")
csv_binder_within_year(cat,date)

# across years: bind the annual csvs --------------------------------------

cat<-"fed"
source("./code/csv_binder_across_years.R")
csv_binder_across_years(cat,date)

cat<-"uni"
source("./code/csv_binder_across_years.R")
csv_binder_across_years(cat,date)

# clean up the composite files --------------------------------------------

##### Need to update fix_usgs_affils to get most recent usgs


# Fed Files
source("./code/clean_fed.R")
clean_fed(date)

# Uni Files
source("./code/clean_uni.R")
clean_uni(date)


# prep datasets for analyses and make overall summaries

# NOTE THE FOLLOWING ARE EXCLUDING *ONLY* BOOK CHAPTERS 
# IF YOU WANT BOOK CHAPTERS NEED TO UNCOMMENT
# AND ALSO
# NOT EXCLUDING ANY BY FLAG WORDS

source("./code/prep_analysis_datasets_fed.R")
prep_analysis_datasets_fed(date, PM_max,PY_min,PY_max)

source("./code/prep_analysis_datasets_uni.R")
prep_analysis_datasets_uni(date, PM_max, PY_min, PY_max)



# figures and summaries of results ----------------------------------------

source("./code/make_figs_fed.R")
make_figs_fed(date,PM_max,PY_max,author_position)

source("./code/make_figs_uni.R")
make_figs_uni(date,PM_max,PY_max,author_position)



# bootstrapping -----------------------------------------------------------

source("./code/bootstrap_npubs_fed.R")
bootstrap_npubs_fed(date,PM_max,author_position)

source("./code/bootstrap_npubs_uni.R")
bootstrap_npubs_uni(date,PM_max,author_position)

# render MS ^ Supplementary Information File -------------------------------


# rmarkdown::render('./docs/Bruna_MS.Rmd'),params=list(args = myarg))

# rmarkdown::render('./docs/Bruna_Supporting_Info.Rmd'),params=list(args = myarg))


###########################################################################
###########################################################################


# lag test ----------------------------------------------------------------



# lag test ----------------------------------------------------------------

source("code/lag_test.R")
lag_test_output<-lag_test()
lag_data<-as.data.frame(lag_test_output[1])
lag_fig<-lag_test_output[2]
lag_fig<-lag_fig[[1]]
sum(lag_data$n_lag)
# date1<-"20250901"
# date2<-"20251010"
# 
# 
# source("./code/lag_test_fed.R")
# lag_test_uni(date1, date2,PM_max)
# 
# 
# source("./code/lag_test_uni.R")
# lag_test_uni(cat, date,PM_max)

