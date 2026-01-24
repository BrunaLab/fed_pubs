
# SCRAPE DATA FROM SCOPUS 
# 
# `01a_scopus_API_fed.R`
# `01b_scopus_API_uni.R`

# DOWNLOAD DATA FROM USGS PUBLICATIONS WAREHOUSE

# Process with `02_process_usgs.R`

 
###########################################################################
###########################################################################


# chose data download date and max month  ---------------------------------

date<-"20251210"
PM_max<-12
PY_max<-2025
author_position<-"first"
# author_position<-"anywhere"
# process federal records -------------------------------------------------

# process the usgs publication file downloaded from their website ---------

# source("./code/process_usgs.R")
# process_usgs(---,---)

# within year: bind csvs for the scopus IDs -------------------------------

cat<-"fed"
# cat<-"uni"
source("./code/csv_binder_within_year.R")
csv_binder_within_year(cat,date)

# across years: bind the annual csvs --------------------------------------

# cat<-"fed"
# cat<-"uni"
source("./code/csv_binder_across_years.R")
csv_binder_across_years(cat,date)

# clean up the composite files --------------------------------------------

# Fed Files
source("./code/clean_fed.R")
clean_fed(date)

# Uni Files
source("./code/clean_uni.R")
clean_uni(date)


# prep datasets for analyses and make overall summaries

source("./code/prep_analysis_datasets_fed.R")
prep_analysis_datasets_fed(date, PM_max,PY_max)

source("./code/prep_analysis_datasets_uni.R")
prep_analysis_datasets_uni(date, PM_max,PY_max)


# bootstrapping -----------------------------------------------------------

source("./code/bootstrap_npubs_fed.R")
bootstrap_npubs_fed(date,PM_max,author_position)

source("./code/bootstrap_npubs_uni.R")
bootstrap_npubs_uni(date,PM_max,author_position)


# figures and summaries of results ----------------------------------------

source("./code/make_figs_fed.R")
make_figs_fed(date,PM_max,PY_max,author_position)

source("./code/make_figs_uni.R")
make_figs_uni(date,PM_max,PY_max,author_position)


# render MS ^ Supplementary Information File -------------------------------


# rmarkdown::render('./docs/Bruna_MS.Rmd'),params=list(args = myarg))

# rmarkdown::render('./docs/Bruna_Supporting_Info.Rmd'),params=list(args = myarg))


###########################################################################
###########################################################################


# lag test ----------------------------------------------------------------



# lag test ----------------------------------------------------------------

date1<-"20250901"
date2<-"20251010"


source("./code/lag_test_fed.R")
lag_test_uni(date1, date2,PM_max)


source("./code/lag_test_uni.R")
lag_test_uni(cat, date,PM_max)

