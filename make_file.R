

###########################################################################
###########################################################################


# chose data download date and max month  ---------------------------------


date<-"20251010"
PM_max<-10

# process federal records -------------------------------------------------

## choose downloads to process --------------------------------------------


# date<-"20250901"
# PM_max<-8
# cat<-"fed"

# date<-"20251010"
# PM_max<-9
# cat<-"fed"

cat<-"fed"

author_position<-"first"
# author_position<-"anywhere"

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
clean_fed(cat, date)

# prep datasets for analyses and make overall summaries

source("./code/prep_analysis_datasets_fed.R")
prep_analysis_datasets_fed(cat, date, PM_max)


# figures and summaries of results ----------------------------------------


source("./code/make_figs_fed.R")
make_figs_fed(cat,date,PM_max,author_position)

# bootstrapping -----------------------------------------------------------

source("./code/bootstrap_npubs_fed.R")
bootstrap_npubs_fed(cat, date,PM_max,author_position)


# render MS ---------------------------------------------------------------


# rmarkdown::render('./docs/Bruna_MS.Rmd'),params=list(args = myarg))

# render supp information file --------------------------------------------

# rmarkdown::render('./docs/Bruna_Supporting_Info.Rmd'),params=list(args = myarg))


###########################################################################
###########################################################################

# process university records ----------------------------------------------

## select the year and category to process --------------------------------

# cat<-"uni"
# date<-"20250901"
# PM_max<-8

# cat<-"uni"
# date<-"20251010"
# PM_max<-9
# 


cat<-"uni"

# within year: bind csvs for the scopus IDs -------------------------------

source("./code/csv_binder_within_year.R")
csv_binder_within_year(cat,date)

# across years: bind the annual csvs --------------------------------------

source("./code/csv_binder_across_years.R")
csv_binder_across_years(cat,date)

# clean up the composite files --------------------------------------------

source("./code/clean_uni.R")
clean_uni(cat,date)


# prep datasets for analyses and make overall summaries. --------------

source("./code/prep_analysis_datasets_uni.R")
prep_analysis_datasets_uni(cat, date, PM_max)




# figures and summaries of results ----------------------------------------

source("./code/make_figs_uni.R")

make_figs_uni(cat,date,PM_max,author_position)

# bootstrapping -----------------------------------------------------------

source("./code/bootstrap_npubs_uni.R")
bootstrap_npubs_uni(cat, date,PM_max)










# lag test ----------------------------------------------------------------



# lag test ----------------------------------------------------------------

date1<-"20250901"
date2<-"20251010"


source("./code/lag_test_fed.R")
lag_test_uni(date1, date2,PM_max)


source("./code/lag_test_uni.R")
lag_test_uni(cat, date,PM_max)

