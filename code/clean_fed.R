clean_fed <- function(date) {
  
  # into --------------------------------------------------------------------
  # 
  # This will take the super-file of all years together, 
  # add the usgs articles, remove duplicates, and check / edit federal 
  # affiliations
  
  
  library(janitor)
  library(tidyverse)
  library(progress)
  library(fs)
  library(data.table)
  
  # this is to clean fed files
  
  cat<-"fed"
  
  # create folders for output -----------------------------------------------
  
  
  # setting up the main directory
  main_dir1 <- "./docs"
  
  # setting up the sub directory
  sub_dir1 <- "summary_info"
  
  # check if sub directory exists 
  if (file.exists(sub_dir1)){
    
    # specifying the working directory
    setwd(file.path(main_dir1, sub_dir1))
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(main_dir1, sub_dir1))
    
  }
  
  
  
  
  # setting up the main directory
  main_dir2 <- paste(main_dir1,"/",sub_dir1,sep="")
  
  # setting up the sub directory
  sub_dir2 <- paste(cat,date,sep="_")
  
  # check if sub directory exists 
  if (file.exists(sub_dir2)){
    
    # specifying the working directory
    setwd(file.path(main_dir2, sub_dir2))
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(main_dir2, sub_dir2))
    
  }
  
  save_dir<-paste(main_dir2,sub_dir2,sep="/")
  
  # cat<-"fed"
  # date<-"20250901"
  # scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-09-01.csv")
  
  # cat<-"fed"
  # date<-"20251010"
  
  
  
  if (date=="20250901"){
    
    scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-09-01.csv") 
      
      }
  
  if (date=="20251010"){
    scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") 
  }
  
  
  if (date=="20251210"){
    scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") 
  }
  
  if (date=="20260101"){
    scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") 
  }
  
  
  scopus_ids_searched<-scopus_ids_searched %>% mutate(city=NA)
  # 
  # scopus_ids_searched<-read_csv("./data_clean/api_fed_affils_searched_2025-11-04.csv") %>% 
  # mutate(city=NA)
  
  message("STEP 1/7: loading the files of papers, affiliations, and authors...")
  
  # IDENTIFY FEDERAL AFFILIATIONS -------------------------------------------
  
  
  affils_df<-read_csv(paste("./data_raw/fed_",date,"/","all_affils_df_fed.csv",sep=""))
  
  message("STEP 2/7: tagging federal affiliations and authors...this might take a while...")
  
  source("./code/ID_fed_affiliations.R")
  affils_df<-ID_fed_affiliations(affils_df,scopus_ids_searched)
  
  
  
  authors_df<-read_csv(paste("./data_raw/fed_",date,"/","all_authors_df_fed.csv",sep=""))
  
  papers_df<-read_csv(paste("./data_raw/fed_",date,"/","all_papers_df_fed.csv",sep=""))
  
  # ADD FEDERAL AFFILIATIONS TO AUTHORS_DF ----------------------------------
  
  
  
  source("./code/ID_fed_authors.R")
  authors_df<-ID_fed_authors(authors_df,affils_df)
  
  # edits ---------------------------------------
  
  message("STEP 3/7: standardizing author names and addresses...")
  
  # remove useless columns
  
  authors_df<-authors_df %>% 
    select(-`afid.@_fa`
           # ,-authorID
    )
  
  papers_df<-
    papers_df %>% 
    filter(PY>2018) %>% 
    # filter(!is.na(DI)) %>% 
    filter(!is.na(DT)) %>% 
    tibble()
  
  
  
  # duplicates --------------------------------------------------------------
  
  message("STEP 4/7: checking for duplicates...")
  
  papers_df<-papers_df %>% 
    distinct(SO,PY,DI,TI,.keep_all = TRUE) %>% 
    mutate(PM=
             case_when(
               # those without a PM early online. one option is to randomly allocate:
               # is.na(PM) ~ sample(c(1:12), 1, replace = TRUE),
               # the other option is to put all in 12
               is.na(PM) ~ 12,
               .default = as.numeric(PM)
             )
    ) %>% 
    mutate(PT=if_else(PT=="j","journal",PT))
  
  
  papers_df<-papers_df %>% distinct(DI,TI,SO,EI,.keep_all=TRUE)
  papers_df<-papers_df %>% distinct(TI,SO,PY,PD,.keep_all=TRUE)
  papers_df<-papers_df %>% distinct(SO,PY,TI,EP,.keep_all=TRUE)
  # 
  # dup_titles<-papers_df %>%
  #   group_by(SO,PY,TI) %>% 
  #   tally() %>% 
  #   filter(n>1) %>% 
  #   arrange(desc(n))
  # 
  # 
  # unique_refid<-papers_df %>% 
  #   tibble() %>% 
  #   select(refID)
  
  # filter the authors and affils to the deduplicated refiD
  
  authors_df<-authors_df %>% 
    filter(refID %in% papers_df$refID)
  
  affils_df<-affils_df %>% 
    filter(refID %in% papers_df$refID)
  
  # use ellefsen k.j. to check
  
  
  
  affil_vector<-authors_df %>% select(affil_id) %>% distinct()
  
  affil_vector<-affils_df %>% 
    filter(affil_id%in%affil_vector$affil_id) %>% 
    filter(federal==TRUE) %>% 
    select(affil_id,agency,agency_primary, federal) %>% 
    distinct()
  
  
  affil_vector$affil_id<-as.character(affil_vector$affil_id)
  
  authors_df <-authors_df  %>% 
    left_join(affil_vector,by="affil_id") %>% 
    
    mutate(federal.y=if_else(federal.x==federal.y,NA,federal.y)) %>% 
    mutate(agency.y=if_else(agency.x==agency.y,NA,agency.y)) %>% 
    mutate(agency_primary.y=if_else(agency_primary.x==agency.y,NA,agency_primary.y)) %>% 
    rename(federal=federal.x,
           agency=agency.x,
           agency_primary=agency_primary.x) %>% 
    select(-federal.y,
           -agency.y,
           -agency_primary.y) %>% 
    relocate(c(federal, agency,agency_primary),.before=1)  
  
  authors_df<-authors_df %>% distinct()
  
  # use ellefsen kj to check
  
  # use ellefsen k.j. to check
  
  
  # USGS articles -----------------------------------------------------------
  
  
  message("STEP 5/7: verify USGS articles included...")
  
  source("./code/add_missing_usgs.R")
  updated_dfs<-add_missing_usgs(papers_df,authors_df)
  
  papers_df<-updated_dfs$papers
  authors_df<-updated_dfs$authors
  authors_df<-authors_df %>% 
    select(
      -PY,
      -country,
      -state,
      -city,
      -authorID)
  #  fix usgs affils
  
  source("./code/fix_usgs_affils.R")
  updated_authors<-fix_usgs_affils(authors_df,papers_df)
  authors_df<-updated_authors
  
  
  
  # There are still some that are showing up as FEDERAL  in some articles but 
  # not others, eg because USGS Coop Units are coded as university and not usgs by 
  # scopus. This takes those that have records coded as both fed and non-fed and
  # changes them all to fed.
  
  # tandf<-authors_df %>% select(SID,federal) %>% distinct() %>% group_by(SID) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)
  
  FED<-authors_df %>% 
    filter(federal==TRUE) %>% 
    select(SID,federal,agency,agency_primary) %>% 
    distinct()
  
  NONFED<-authors_df %>% 
    filter(federal!=TRUE|is.na(federal)) %>% 
    select(SID,federal,agency,agency_primary) %>% 
    distinct()
  
  need_to_fix<-inner_join(NONFED,FED,by="SID") %>% 
    distinct(SID,.keep_all = TRUE) %>% 
    filter(!is.na(agency_primary.y)) %>% 
    select(SID,federal=federal.y,
           agency=agency.y,
           agency_primary=agency_primary.y)
  
  authors_df<-authors_df %>% 
    left_join(need_to_fix,by="SID") %>% 
    relocate(federal.y,.after=federal.x) %>% 
    relocate(agency.y,.after=agency.x) %>% 
    relocate(agency_primary.y,.after=agency_primary.x) %>% 
    mutate(federal.x=coalesce(federal.y,federal.x)) %>% 
    mutate(agency.x=coalesce(agency.x,agency.y)) %>% 
    mutate(agency_primary.x=coalesce(agency_primary.x,agency_primary.y)) %>% 
    select(-agency_primary.y,
           -agency.y,
           -federal.y) %>% 
    rename(agency_primary=agency_primary.x,
           agency=agency.x,
           federal=federal.x) %>% 
    mutate(federal=if_else(is.na(federal),FALSE,federal))
  
  
  
  # make sure articles have at least 1 fed author ---------------------------
  
  message("STEP 6/7: making sure articles have at least 1 author from a federal agency (this takes a while)...")
  
  # moved this to make figs to allow greater control
  # chose publication types or titles to remove -----------------------------
  
  # unique(papers_df$DT)
  # ARTICLE TYPES - KEEP
  # "book chapter"
  # "article"
  # "review"
  # "note"
  # "data paper"
  # ARTICLE TYPES - REMOVE
  # "letter" # excluded below
  # "editorial" # excluded below" 
  
  # papers_df %>% filter(DT=="editorial") %>% select(TI)
  # authors_df %>% filter(is.na(agency)) %>% group_by(federal) %>% tally()
  # 
  # papers_cat<-c("article",
  #               "book chapter",
  #               "data paper",
  #               "note",
  #               "review")
  # 
  # papers_title<-c("editorial comment",
  #                 "editorial commentary",
  #                 "a quick glance at selected topics in this issue",
  #                 "reply by authors",
  #                 "conclusion",
  #                 "preface",
  #                 "a word from olaw and usda",
  #                 "introduction",
  #                 "editorial comment",
  #                 "editorial commentary",
  #                 "a quick glance at selected topics in this issue",
  #                 "conclusion",
  #                 "reply by authors",
  #                 "preface",
  #                 "a word from olaw and usda",
  #                 "author reply",
  #                 "a word from olaw",
  #                 "comment",
  #                 "commentary",
  #                 "conclusions",
  #                 "a word from usda and olaw",
  #                 "overview",
  #                 "introduction")
  # 
  # papers_df <- papers_df %>% 
  #   filter(DT%in%papers_cat) %>% 
  #   filter(TI%in%papers_title)
  # 
  # authors_df<-authors_df %>% 
  #   filter(refID%in%papers_df$refID)
  # 
  # 
  # rm(papers_cat,papers_title)
  
  # this calculates how many papers have authors from each institution
  papers_by_agency<-authors_df %>% 
    select(refID,agency_primary,author_order) %>% 
    group_by(refID) %>% 
    count(agency_primary) %>% 
    pivot_wider(names_from = agency_primary, values_from = n) %>% 
    replace(is.na(.), 0)
  
  
  
  # But wait! SCOPUS returned articles that included focal agency 
  # as any affiliation (2nd, 3rd, etc). for instance for refID = 60000650_2021-13,
  # the primary affiliation of all authors is 'Brigham and Women's Hospital' but 
  # harvard medical school or harvard college are secondary affiliations.
  # Need to 2x these to make sure should keep (i.e.) do we treat BW, Mass General
  # etc as harvard or not. 
  # Decision: NO primary affil only.
  
  
  NA_only_pubs<-papers_by_agency %>% 
    ungroup() %>% 
    rowwise() %>%
    mutate(flag = `NA` > 0 && all(c_across(-c(refID, `NA`)) == 0)) %>%
    ungroup() %>% 
    filter(flag==TRUE)
  
  
  if(nrow(NA_only_pubs)>0){
    
    # Filter out the ones where fed isn't primary affil
    authors_df<-authors_df %>% filter(!refID%in%NA_only_pubs$refID) 
    
    papers_df<-papers_df %>% filter(!refID%in%NA_only_pubs$refID) 
    # 
    # papers_by_agency<-authors_df %>% 
    #   select(refID,agency_primary,author_order) %>% 
    #   group_by(refID) %>% 
    #   count(agency_primary) %>% 
    #   pivot_wider(names_from = agency_primary, values_from = n) %>% 
    #   replace(is.na(.), 0)
    
    
  }
  
  
  
  # can 2x they were removed here: 
  # papers_by_agency2<-authors_df %>% 
  #   select(refID,agency_primary,author_order) %>% 
  #   group_by(refID) %>% 
  #   count(agency_primary) %>% 
  #   pivot_wider(names_from = agency_primary, values_from = n) %>% 
  #   replace(is.na(.), 0)
  
  # NA_only_pubs2<-papers_by_agency2 %>%
  #   ungroup() %>%
  #   rowwise() %>%
  #   mutate(flag = `NA` > 0 && all(c_across(-c(refID, `NA`)) == 0)) %>%
  #   ungroup() %>%
  #   filter(flag==TRUE)
  
  single_agency_counter <- authors_df %>%
    count(refID, agency_primary) %>%
    pivot_wider(names_from = agency_primary, values_from = n, values_fill = 0) %>%
    mutate(sum = rowSums(select(., where(is.integer)))) %>%
    relocate(sum, .after = refID)
  
  # can now count how many papers by agency (note - no fractional authorship)
  single_agency_publications<-single_agency_counter %>%
    mutate(agency=case_when(
      sum==hhs~"hhs",
      sum==dod~"dod",
      sum==commerce~"commerce",
      sum==epa~"epa",
      sum==other~"other",
      sum==doe~"doe",
      sum==labor~"labor",
      sum==nsf~"nsf",
      sum==va~"va",
      sum==usda~"usda",
      sum==interior~"interior",
      sum==smithsonian~"smithsonian",
      sum==nasa~"nasa",
      sum==education~"education",
      sum==doj~"doj",
      sum==state~"state",
      sum==dhs~"dhs",
      sum==dot~"dot",
      # sum==`federal reserve system`~"federal reserve system",
      sum==treasury~"treasury",
      sum==hud~"hud",
      .default = NA))%>%
    relocate(agency,.after=1) %>%
    filter(!is.na(agency)) %>%
    select(refID,agency)
  # 
  
  counts<-single_agency_publications %>% 
    group_by(agency) %>% 
    tally() %>% 
    arrange(desc(n))
  
  # write_csv(counts,"./docs/summary_info/total_papers_by_agency.csv")
  write_csv(counts,paste(save_dir,"/","total_papers_by_agency.csv",sep=""))
  
  
  
  # SAVE CLEAN FILES --------------------------------------------------------
  
  message("STEP 7/7: saving cleaned-up files...")
  
  
  write_rds(papers_df,paste("./data_clean/papers_df_clean","_",cat,"_",date,".rds",sep=""))
  
  write_rds(authors_df,paste("./data_clean/authors_df_clean","_",cat,"_",date,".rds",sep=""))
  
  write_rds(affils_df,paste("./data_clean/affils_df_clean","_",cat,"_",date,".rds",sep=""))
  
}