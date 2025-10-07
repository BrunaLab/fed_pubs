# Publications by US Federal Govt Authors 2019-2025

## Repository Overview

This repository is for the cleanup, organization, and archiving of data collected as part of the _FedPubs_ Project. These procedures are carried out by executing two R scripts (see _Workflow_, below). 

An overview of the data and associated metadata will be archived in the Dryad Digital Repository upon acceptance of the resulting publications.

**This repository includes the following:**

1. **R Code** used to:
    - download bibliographic records from Scopus using the API [(`code/---`)](code/---)
      
      > [(`02_scopus_API_search.R`)](02_scopus_API_search.R)
      > [(`02_scopus_API_search_unis.R`)](02_scopus_API_search_unis.R)
    
    - process raw data files for a given year:
      
      > [(`03_csv_binder.R`)](03_csv_binder.R)
      > [(`03a_clean_usgs_csv.R`)](03a_clean_usgs_csv.R)
      > [(`03c_csv_binder_unis.R`)](03c_csv_binder_unis.R)
      
    - process bind data files accross years:
      
      > [(`04_csv_binder_cross_years.R`)](04_csv_binder_cross_years.R)
  
    - correct / make changes to author, affiliation, and publication records  (uses several functions in the `code`,`code\figs`, `code\figs_uni` folders):
      
      > [(`05_merge_usgs_and_final_clean.R`)](05_merge_usgs_and_final_clean.R)
      > [(`05a_clean_uni_affils.R`)](05a_clean_uni_affils.R)
      > [(`05b_final_clean_unis.R`)](05b_final_clean_unis.R)
    
    - generate and save data summaries, make figures (uses several functions in the `code`,`code\figs`, `code\figs_uni` folders):
      
      > [(`06_make_file.R`)](06_make_file.R)
      > [(`06_make_file_uni.R`)](06_make_file_uni.R) 
      
    - Bootstrap analysis
    
      > [(`code/bootstrap_npubs.R`)](code/bootstrap_npubs.R)
      > [(`code/bootstrap_npubs_uni.R`)](code/bootstrap_npubs_uni.R)
    
2. Rmd files used to prepare the manuscript:   

    - [(`docs/`)](docs/) folder
    
3. **Data:** will be available upon manuscript acceptance (files are too large to store on Github)
    
  
4. [**A log of updates and corrections**](NEWS.md).

## Workflow


## Improvements, Suggestions, & Questions

We welcome any suggestions for package improvement or ideas for features to include in future versions. If you have Issues, Feature Requests and Pull Requests, [here is how to contribute](CONTRIBUTING.md). We expect everyone contributing to the package to abide by our [Code of Conduct](CODE_OF_CONDUCT.md).

## Contributors

-   [Emilio M. Bruna](https://github.com/embruna), University of Florida


## Citation

Until a manuscript is accepted and data are archived at Dryad, please cite [this preprint](https://osf.io/preprints/metaarxiv/4hfe9_v1) when using these data for research, publications, teaching, etc.


If you wish to cite this repository, please cite as follows:


@misc{BrunaSurveys2023,  
  author = {Bruna, E.M.},  
  title = {FedPub Project},  
  year = {2025},  
  publisher = {GitHub},  
  journal = {GitHub repository},  
  note = {data v0.9.0.},  
  url={https://github.com/BrunaLab/fed_pubs}  
}  

