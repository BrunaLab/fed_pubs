# Publications by US Federal Govt Authors 2019-2025

## Repository Overview

This repository is for the cleanup, organization, and archiving of data 
collected as part of the _FedPubs_ Project. These procedures are carried out by 
executing two R scripts (see _Workflow_, below). 

An overview of the data and associated metadata will be archived in the 
Dryad Digital Repository upon acceptance of the resulting publications.

**This repository includes the following:**

1. **Data used in analyses/publications:** 
    - Data will be archived at Dryad upon manuscript acceptance (files are too 
    large to store on Github). However, the list of Scopus Affiliation ID codes 
    used for searches of university and US federal agency productivity 
    (Supplementary Tables 2 & 3) are available for download as `.csv` files.
    
    > [(`fed_scopus_codes.csv`)](data_archive/fed_scopus_codes.csv)  
    > [(`uni_scopus_codes.csv`)](data_archive/uni_scopus_codes.csv)  
    
    
2. **R Code** used to:
    - download bibliographic records from Scopus using the API
      
      > [(`01a_scopus_API_fed.R`)](01a_scopus_API_fed.R)  
      > [(`01b_scopus_API_uni.R`)](01b_scopus_API_uni.R)   
    
    - Organize the `csv` file of USGS publications downloaded from the USGS Publications Warehouse
    
      > [(`02_process_usgs.R`)](02_process_usgs.R)  
    
    - Combine the `.csv` files returned by Scopus for the affiliation IDs searched each year into a single `.csv` (one each for authors, affiliations, and papers):
      
      > [(`03_csv_binder_within_year.R`)](03_csv_binder_within_year.R)  
      
    - Bind the `.csv` files for each year into a single `.csv` with all years together (one each for authors, affiliations, and papers):
      
      > [(`04_csv_binder_across_years.R`)](04_csv_binder_across_years.R)  
  
    - correct / make changes to author, affiliation, and publication records  (uses several functions in the `code`,`code\figs`, `code\figs_uni` folders):
      
      > [(`05a_clean_fed.R`)](05a_clean_fed.R)  
      > [(`05b_clean_uni.R`)](05b_clean_uni.R)   
    
    - Generate and save data summaries, make figures (uses several functions in the `code`,`code\figs`, `code\figs_uni` folders):  
    
      > [(`06a_make_figs_fed.R`)](06a_make_figs_fed.R)
      > [(`06b_make_figs_fed.R`)](06b_make_figs_uni.R)
      
    - Bootstrap analysis
    
      > [(`code/bootstrap_npubs.R`)](code/bootstrap_npubs.R)  
      > [(`code/bootstrap_npubs_uni.R`)](code/bootstrap_npubs_uni.R)  
    
3. Rmd files used to prepare the manuscript:   
    - [(`docs/`)](docs/) folder  
    
4. [**A log of updates and corrections**](NEWS.md).

## Workflow

_Coming shortly - I am in the process of streamlining the workflow to allow working from a single MakeFile._

 

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

