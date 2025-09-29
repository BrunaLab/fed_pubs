merge_with_usgs <- function(authors_df) {
  
  # THIS IS TO CONFIRM THAT USGS AUTHORS ARE CORRECTLY IDENTIFIED AS SUCH
  # (SCOPUS DOESNT CODE ALL OF THEM AS USGS EG COOP UNITS)
  
  usgs_authors<-read_rds("./data_intermediate/usgs_authors_clean.rds") %>% 
    rename(
      agency_primary=agency,
      agency_short=agency_2,
      affiliation=agency_3,
      ) %>% 
    select(PY,
    agency_primary,
    agency_short,
    affiliation,
    surname,
    given_name,
    first_middle_initials,
    -AF,
    federal,
    author_order,
    affil_id,
    entry_no,
    AU, 
    authorID) %>% 
    mutate(author_order=as.integer(author_order)) %>% 
    mutate(first_middle_initials=str_remove(first_middle_initials,".NA.")) %>% 
    rename(AF=AU) %>% 
    mutate(AF=str_replace(AF,"[.]","")) %>% 
    mutate(AF=str_replace(AF,"[,]",""))
  
  names(usgs_authors)
  
  usgs_authors$federal<-as.logical(usgs_authors$federal)
  
  usgs_authors<-usgs_authors %>% 
    distinct() %>% 
    select(author_order,AF,surname,given_name,affil_id,affiliation,agency_primary,federal)
  
  usgs_authors$AF
  
  authors_df<-authors_df %>% 
    mutate(AF=str_replace_all(AF,"[.]",""))
  foo<-authors_df %>% filter(AF%in%usgs_authors$AF) %>% 
    select(author_order,SID,AF,surname,given_name,agency,agency_primary,federal) %>% 
    distinct()
  
  foo<-left_join(usgs_authors,foo,by=c("AF","author_order","surname","given_name"))  
  foo<-foo %>% filter(federal.x==TRUE)
  foo<-foo %>% filter(is.na(federal.y))
  foo<-foo %>% 
    rename(correct_agency_primary=agency_primary.x,
           correct_federal=federal.x,
           correct_agency=affiliation)
  
  foo<-foo %>% 
    group_by(AF,SID) %>% 
    distinct(author_order,AF,SID,.keep_all = TRUE)
    
  authors_df2<-authors_df %>% left_join(foo,by=c("AF","SID"))
  
  
  foo<-foo %>% filter(federal.x==TRUE)
  
  foo<-foo %>% filter(is.na(federal.y))
  
  foo<-foo %>% 
    select(
      -agency,
    -agency_primary.y,
    -federal.y,
    -affil_id.y,
    -entry_no.x,
    -`afid.@_fa`,
    # -AF.x
    ) %>% 
    rename(agency_primary=agency_primary.x,
           # AF=AF.y,
           entry_no=entry_no.y)
  
  names(foo)
  
  
  agency_primary,
  agency_short,
  affiliation,
  surname,
  given_name.x,
  first_middle_initials,
  federal.x,
  author_order,
  affil_id.x,
  AF,
  authorID,
  refID,
  author_url,
  SID,
  given_name.y,
  OI,
  entry_no,
  source_file,
  source_file_combined)
  
  foo3<-anti_join(foo,foo2)
  # usgs_papers<-read_rds("./data_intermediate/usgs_papers_clean.rds")
  
  # get a df of papers already in papers_df from usgs
  # get the federal usgs authors of those papers (doi, author order) and 
  # change the affiliation in papers_df
  
  
  
  
  # See if any of the usgs pubs are aleready in the file and check author status 
  
  # by DOI
  
  # usgs<-read_csv("./data_raw/scopus_api/fed_files/usgs_with_doi.csv") 
  usgs_with_DI<-usgs_papers %>% filter(!is.na(DI))
  usgs_already_in_papers_df<-papers_df_trim %>% 
    filter(DI%in%usgs_with_DI$DI) %>% 
    select(refID,DI)
  
  # that means these usgs papers arent in df and need to be added
  usgs_NOT_in_papers_df<-anti_join(usgs_papers,usgs_already_in_papers_df,by="DI") %>% 
    mutate(source="usgs_database")
  
  # and these are the authors of those papers
  usgs_authors_for_papers_NOT<- usgs_authors %>% 
    filter(usgs_refID%in%usgs_NOT_in_papers_df$usgs_refID)
  
  names(usgs_NOT_in_papers_df)
  names(papers_df)
  
  
  
  usgs_NOT_in_papers_df<-
    usgs_NOT_in_papers_df %>% select(
  refID=usgs_refID,
  source_file=source,
  DI,
  scopus_article_id=usgs_refID,
  AF,
  ,BP,EP,IS,PT,PY,SN,EI,SO,TI,VL) %>% 
    mutate(entry_no=row_number())
  
  
  
  
  usgs_authors_for_papers_NOT
  
  
  
  usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
    mutate(AF=str_replace_all(AF,"[.]","")) %>% 
    separate(given_name,c("first","middle"),extra = "merge") 
  
  usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
    mutate(first_middle_initials=str_replace_all(first_middle_initials,".NA.","")) %>% 
    mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]","")) 
  
  usgs_authors_for_papers_NOT<-
  usgs_authors_for_papers_NOT %>% 
  select(
    refID=usgs_refID,
    source_file=source,
    SID=usgs_refID,
    PY,
    agency_primary=agency,
    agency_short=agency_2,
    affiliation=agency_3,
    country,
    state,
    city,
    surname,
    given_name=first,
    first_middle_initials,
    AF,
    federal,
    author_order,
    affil_id,
    entry_no,
    authorID,
    federal,
    ) %>% 
    mutate(entry_no=as.character(entry_no)) %>% 
    mutate(author_order=as.integer(author_order)) %>% 
    mutate(SID=as.character(SID)) %>% 
    mutate(federal=as.logical(federal))
  
  
  usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
    mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>%
    mutate(federal=as.logical(federal))
  
  authors_df$SID<-as.character(authors_df$SID)
  
  # add these: usgs_NOT_in_papers_df
  # to these: papers_df_trim
  
  names(usgs_NOT_in_papers_df)
  names(papers_df_trim)
  
  usgs_NOT_in_papers_df$scopus_article_id<-as.integer(usgs_NOT_in_papers_df$scopus_article_id)
  usgs_NOT_in_papers_df$BP<-as.double(usgs_NOT_in_papers_df$BP)
  usgs_NOT_in_papers_df$EP<-as.double(usgs_NOT_in_papers_df$EP)
  usgs_NOT_in_papers_df$PY<-as.integer(usgs_NOT_in_papers_df$PY)
  papers_df_trim<-bind_rows(papers_df_trim,usgs_NOT_in_papers_df)
  
  
  
  
  # add these: usgs_NOT_in_papers_df
  # to these: papers_df_trim
  
    names(usgs_authors_for_papers_NOT)
  names(authors_df_trim)
  usgs_authors_for_papers_NOT$SID<-as.integer(usgs_authors_for_papers_NOT$SID)
  authors_df_trim<-bind_rows(authors_df_trim,usgs_authors_for_papers_NOT)
  
  
  
  
  # TO CORRECT *ALL* THE USGS AUTHORS in authors_DF, not just the ones that were in the added papers
  
    
  
  usgs_authors<-read_rds("./data_intermediate/usgs_authors_clean.rds")
  usgs_authors<-usgs_authors %>% 
    filter(federal==TRUE)
  
  usgs_papers_key<-usgs_papers %>% 
    select(usgs_refID,DI) 
  
  papers_df_to_fix_au<-usgs_papers %>% 
    filter(usgs_refID%in%usgs_authors$usgs_refID) %>% 
    select(DI,TI,PY)
  
  papers_df<-papers_df_trim
  papers_df_to_fix_au$PY<-as.integer(papers_df_to_fix_au$PY)
  papers_df_cut<-inner_join(papers_df,papers_df_to_fix_au, by=c("DI","TI","PY")) %>% 
    select(refID)
  
  authors_df_fix<-authors_df %>% filter(refID%in%papers_df_cut$refID)
  authors_df_fix<-authors_df_fix %>% 
    mutate(AU=AF) %>% 
    mutate(AU=str_replace_all(AU,"[.]","")) %>% 
    mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]",""))
  usgs_authors$author_order<-as.integer(usgs_authors$author_order)
  usgs_authors$federal<-as.logical(usgs_authors$federal)
  usgs_authors<-usgs_authors %>% 
    rename(
    agency_primary=agency,
    agency_short=agency_2,
    affiliation=agency_3,
  )  %>% 
    mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]","")) %>%
    mutate(AU=str_replace_all(AU,"[,]",""))
  
  
  # foo<-full_join(authors_df_fix,usgs_authors,by=c("AU","surname","first_middle_initials"))
  foo<-full_join(authors_df_fix,usgs_authors,by=c("AU"))
  foo<-foo %>% 
    relocate(federal.y,.before=1) %>% 
    relocate(federal.x,.before=1) %>% 
    arrange(federal.x,federal.y) %>% 
    filter(is.na(federal.x)) %>% 
    distinct() %>% 
    select(SID,refID,agency,agency_primary)
  
  %>% 
    
    relocate(surname.x,.after=2) %>% 
    relocate(surname.y,.after=2)
  
  
  authors_
    group_by(DI) %>% 
    tally() %>% 
    filter(n>1) %>% 
    arrange(desc(n))
    
    
    distinct(DI)
  
  
  
  
  
  
  
  
  ######### Split country and reorganize to merge with author_df 
  # 
  # usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
  #   mutate(country=if_else(is.na(country),"usa",country))
  # 
  # max_cols <- usgs_authors_for_papers_NOT %>%
  #   pull(country) %>%
  #   str_split(",") %>%
  #   map_int(length) %>%
  #   max()
  # 
  # # Step 2: Create column names dynamically
  # col_names <- paste0("country", 1:max_cols)
  # 
  # # Step 3: Use separate with extra = "merge" and fill = "right"
  # df_sep <- usgs_authors_for_papers_NOT %>% 
  #   select(refID,country) %>% 
  #   separate(country, into = col_names, sep = ",", fill = "right", extra = "merge")
  # 
  # df_sep_1country<-df_sep %>% 
  #   group_by(refID) %>% 
  #   slice_head(n=1) %>% 
  #   filter(is.na(country2))
  # 
  # foo<-usgs_authors_for_papers_NOT %>% 
  #   mutate(country2 = strsplit(country, ",")) %>%
  #   unnest(refID) %>%
  #   group_by(refID) %>%
  #   mutate(row = row_number()) %>%
  #   spread(row, country)
  # 
  # 
  #   separate(country,)
  #   pivot_longer(cols=country,
  #                names_to="author_country",
  #                values_to="country") %>% 
  #   relocate(author_country,.before="surname")
  # 
  # 
  usgs_for_affils<-usgs_authors_for_papers_NOT %>%
    select(
    refID,
    affil_id,
    affiliation,
    city,
    # country,
    entry_no,
    source_file,
    agency_primary,
    federal
    ) %>% 
    mutate(agency="interior")
  
  names(affils_df)
  
  
  authors_df<-bind_rows(authors_df,usgs_authors_for_papers_NOT)
  
  papers_df <-papers_df %>% mutate(scopus_article_id=as.character(scopus_article_id))
  usgs_NOT_in_papers_df <-usgs_NOT_in_papers_df %>% mutate(BP=as.numeric(BP))
  usgs_NOT_in_papers_df <-usgs_NOT_in_papers_df %>% mutate(EP=as.numeric(EP))
  usgs_NOT_in_papers_df <-usgs_NOT_in_papers_df %>% mutate(PY=as.integer(PY))
  
  papers_df<-papers_df %>% 
    mutate(source="scopus")
  papers_df<-bind_rows(papers_df,usgs_NOT_in_papers_df)
  
  return(list(papers=papers_df,authors=authors_df))
  
  # 
  # 
  # names(authors_df)
  # # usgs_authors_for_papers_NOT<-
  # #   usgs_authors_for_papers_NOT %>% left_join(entry_info)
  # # 
  # 
  # 
  # 
  # 
  # names(usgs_authors_for_papers_NOT)
  # 
  # usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
  #   select(surname,AF, given_name, email,agency_short=agency_2,agency_primary=agency) %>% 
  #   # select(surname,AF, given_name, federal,email) %>% 
  #   # arrange(desc(federal)) %>% 
  #   separate(given_name,c("first","middle"),extra = "merge") 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # authors_df_usgs_check<-authors_df %>%
  #   filter(refID%in%usgs_already_in_papers_df$refID) %>% 
  #   select(AF,surname,given_name,SID,affil_id) 
  # # %>% 
  #   # select(AF,surname,given_name,federal,SID,affil_id) %>% 
  #   # mutate(federal=as.character(federal)) 
  # 
  # affils_for_usgs_check<-affils_df %>% filter(affil_id%in%authors_df_usgs_check$affil_id) %>% 
  #   # select(affil_id,agency,federal) %>% 
  #   select(affil_id) %>% 
  #   # mutate(federal=as.character(federal)) %>% 
  #   distinct()
  # 
  # affils_for_usgs_check$affil_id<-as.character(affils_for_usgs_check$affil_id)
  # authors_df_usgs_check$affil_id<-as.character(authors_df_usgs_check$affil_id)
  # authors_df_usgs_check<-left_join(authors_df_usgs_check,affils_for_usgs_check)
  # 
  # library(stringr)
  # 
  # 
  # 
  # authors_df_usgs_check<-authors_df_usgs_check %>% 
  #   mutate(AF=str_replace_all(AF,"[.]","")) %>% 
  #   separate(given_name,c("first","middle"),extra = "merge")
  # 
  # 
  # usgs_authors_clean<-usgs_authors %>% 
  #   select(surname,AF, given_name, email,agency_short=agency_2,agency_primary=agency) %>% 
  #   # select(surname,AF, given_name, federal,email) %>% 
  #   # arrange(desc(federal)) %>% 
  #   separate(given_name,c("first","middle"),extra = "merge") 
  # 
  # fed_check_usgs<-full_join(authors_df_usgs_check, usgs_authors_clean,by=c("surname","first")) 
  # 
  # 
  # fed_check_usgs_match<-fed_check_usgs %>% 
  #   # filter((federal.x=="TRUE" & federal.y=="TRUE")|(federal.x=="FALSE" & federal.y=="FALSE")) %>% 
  #   distinct() %>%
  #   select(-AF.y,
  #          -middle.x) %>% 
  #   rename(AF=AF.x,
  #          middle=middle.y) %>% 
  #   distinct()
  # 
  # # fed_check_usgs_remain<-anti_join(fed_check_usgs,fed_check_usgs_match) %>% distinct(SID,federal.x,federal.y,.keep_all=TRUE)
  # fed_check_usgs_remain<-anti_join(fed_check_usgs,fed_check_usgs_match,by="SID") %>% distinct(SID,federal.x,federal.y,.keep_all=TRUE)
  # fed_check_usgs_remain<-fed_check_usgs_remain %>% 
  #   mutate(federal.x=if_else(federal.y=="TRUE","TRUE",federal.x)) %>% 
  #   arrange(desc(federal.y),federal.x) %>% 
  #   select(SID,federal.x) %>% 
  #   filter(federal.x==TRUE)
  # 
  # 
  # authors_df<-authors_df %>%
  #   left_join(fed_check_usgs_remain,by="SID") %>% 
  #   mutate(federal=if_else(federal.x=="TRUE",TRUE,federal)) %>% 
  #   select(-federal.x)
  # 
  # 
  # authors_df %>% distinct(surname,given_name,.keep_all=TRUE) %>% group_by(surname,given_name) %>% tally(n_distinct(federal))
  # 
  # 
  # 
  # 
  # # <-read_csv("./data_raw/scopus_api/fed_files/usgs_no_doi.csv")
  # usgs_papers_no_doi<-usgs_papers %>% filter(is.na(DI))
  # match_title<-papers_df %>% filter(TI%in%usgs_papers_no_doi$TI) 
  # 
  # usgs_already_in_papers_df<-papers_df %>% filter(DI%in%usgs_with_DI$DI) %>% select(refID,DI)
  # 
  # # usgs_pubs<-read_rds("./data_intermediate/usgs_papers_clean.rds")
  # # 
  # 
  # usgs_pubs_to_add1<-anti_join(usgs_papers,usgs_already_in_papers_df,by="DI")
  # usgs_pubs_to_add2<-semi_join(usgs_papers,match_title,by="TI") 
  # 
  # usgs_papers_NOT_in_papers_df<-bind_rows(usgs_pubs_to_add1,usgs_pubs_to_add2) %>% 
  #   select(-c(Country,
  #             agency_3,
  #             State,
  #             City,
  #             URL,
  #             agency,
  #             agency_2)) 
  # 
  # # 
  # # 
  # # usgs_authors<-read_rds("./data_intermediate/usgs_authors_clean.rds") 
  # # 
  # 
  # add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%usgs_papers_NOT_in_papers_df$usgs_refID) %>% 
  #   mutate(from="usgs") %>% 
  #   rename(refID=usgs_refID) %>% 
  #   select(-c(agency_2,agency_3,country,city,from)) %>% 
  #   mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
  #   mutate(federal=as.logical(federal))
  # # ,
  # #          PY=as.numeric(PY)) 
  # # 
  # # add to authors_df and papers_df -----------------------------------------
  # authors_df<-authors_df %>% mutate(federal=as.logical(federal))
  # add_to_authors_df$author_order<-as.integer(add_to_authors_df$author_order)
  # authors_df$author_order<-as.integer(authors_df$author_order)
  # 
  # authors_df$authorID<-as.character(authors_df$authorID)
  # add_to_authors_df$authorID<-as.character(add_to_authors_df$authorID)
  # add_to_authors_df$PY<-as.numeric(add_to_authors_df$PY)
  # add_to_authors_df$entry_no<-as.integer(add_to_authors_df$entry_no)
  # 
  # # add_to_authors_df: are any of these federal in the main df?
  # 
  # 
  # 
  # 
  # add_to_authors_df_trim<-  add_to_authors_df %>% 
  #   select(AF,surname,given_name,federal,affil_id) %>% 
  #   mutate(federal=as.character(federal)) %>% 
  #   distinct() %>% 
  #   mutate(affil_id=as.integer(affil_id))
  # 
  # 
  # 
  # 
  # authors_df_fed_check<-authors_df %>% 
  #   select(AF,surname,given_name,federal,SID,affil_id) %>% 
  #   mutate(federal=as.character(federal)) %>% 
  #   distinct() 
  # 
  # # add_to_authors_df<-left_join(add_to_authors_df_trim,authors_df_fed_check,by="affil_id")
  # 
  # names<-names(authors_df)
  # names2<-names(add_to_authors_df)
  # names<-intersect(names,names2)
  # 
  # 
  # add_to_authors_df<-add_to_authors_df %>% mutate(affil_id=as.numeric(affil_id)) %>% select(all_of(names)) 
  # 
  # add_to_papers_df<-usgs_papers_NOT_in_papers_df %>% rename(refID=usgs_refID)
  # 
  # add_to_papers_df$PY<-as.numeric(add_to_papers_df$PY)
  # 
  # papers_df$BP<-as.character(papers_df$BP)
  # papers_df$EP<-as.character(papers_df$EP)
  # 
  # papers_df<-bind_rows(papers_df,add_to_papers_df)
  # 
  # return(list(papers=papers_df,authors=authors_df))
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # # 
  # # 
  # # 
  # # 
  # # 
  # # 
  # # usgs_DI<-usgs_pubs_to_add %>% 
  # #   select(DI,usgs_refID) %>% 
  # #   drop_na() 
  # # 
  # # papers_DI<-papers_df %>% 
  # #   select(DI,refID) %>% 
  # #   drop_na()
  # # 
  # # 
  # # usgs_papers_in_papers_df<-semi_join(papers_DI,usgs_DI,by="DI") %>% 
  # #   select(DI)
  # # 
  # # 
  # # 
  # # for_usgs_authors1<-semi_join(usgs,usgs_papers_in_papers_df) %>% select(usgs_refID,DI,PY)
  # # for_usgs_authors2<-semi_join(usgs_authors,for_usgs_authors1,by="usgs_refID")
  # # 
  # # for_usgs_authors2<-left_join(for_usgs_authors2,for_usgs_authors1) %>% 
  # #   filter(federal==TRUE) %>% 
  # #   select(DI,AF,affil_id,author_order,PY,federal) %>% 
  # #   distinct() %>% 
  # #   mutate(PY=as.integer(PY))
  # # 
  # # refID_to_correct_authors<-semi_join(papers_df,for_usgs_authors2,by="DI") %>% 
  # #   select(refID,DI,PY)
  # # 
  # # for_usgs_authors2<-left_join(for_usgs_authors2,refID_to_correct_authors)
  # # 
  # # # %>% 
  # # #   mutate(PY=as.numeric(PY))
  # # 
  # # 
  # # for_usgs_authors2$author_order<-as.numeric(for_usgs_authors2$author_order)
  # # 
  # # for_usgs_authors2$PY<-as.numeric(for_usgs_authors2$PY)
  # # 
  # # 
  # # authors_df<-left_join(authors_df,for_usgs_authors2,by=c("refID","author_order")) %>% 
  # #   mutate(federal.x=as.character(federal.x)) %>% 
  # #   mutate(federal.x=
  # #            case_when(
  # #              federal.y == "TRUE" ~ "TRUE",
  # #              .default = as.character(federal.x)
  # #            )
  # #   ) %>% 
  # #   mutate(affil_id.x=
  # #            case_when(
  # #              affil_id.y == "60011347" ~ "60011347",
  # #              .default = as.character(affil_id.x)
  # #            )
  # #   ) %>% 
  # #   select(-affil_id.y,
  # #          -federal.y) %>% 
  # #   rename(affil_id=affil_id.x,
  # #          federal=federal.x) %>% 
  # #   mutate(AF.y=if_else(AF.y==AF.x,NA,AF.y)) %>% 
  # #   select(-AF.y) %>% 
  # #   rename(AF=AF.x)
  # # 
  # # 
  # # 
  # # # now the ones that aren't in papers_df
  # # 
  # # 
  # # usgs_papers_NOT_in_papers_df<-anti_join(usgs_DI,papers_DI,by="DI") %>% 
  # #   select(DI)
  # # 
  # # 
  # # 
  # # 
  # # add_to_papers_df<-semi_join(usgs,usgs_papers_NOT_in_papers_df) %>% 
  # #   select(-c(Country,
  # #             agency_3,
  # #             State,
  # #             City,
  # #             URL,
  # #             agency,
  # #             agency_2)) %>% 
  # #   mutate(BP=as.numeric(BP),
  # #          EP=as.numeric(EP))
  # # # ,
  # # # PY=as.numeric(PY))
  # # 
  # # add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%add_to_papers_df$usgs_refID) %>% 
  # #   mutate(from="usgs") %>% 
  # #   rename(refID=usgs_refID) %>% 
  # #   select(-c(agency_2,agency_3,country,city,from)) %>% 
  # #   mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
  # #   mutate(federal=as.logical(federal))
  # # # ,
  # # #          PY=as.numeric(PY)) 
  # # # 
  # # # add to authors_df and papers_df -----------------------------------------
  # # authors_df<-authors_df %>% mutate(federal=as.logical(federal))
  # # add_to_authors_df$author_order<-as.integer(add_to_authors_df$author_order)
  # # authors_df$author_order<-as.integer(authors_df$author_order)
  # # 
  # # authors_df$authorID<-as.character(authors_df$authorID)
  # # add_to_authors_df$authorID<-as.character(add_to_authors_df$authorID)
  # # 
  # # add_to_authors_df$PY<-as.numeric(add_to_authors_df$PY)
  # # 
  # # add_to_authors_df$entry_no<-as.integer(add_to_authors_df$entry_no)
  # # 
  # # authors_df<-bind_rows(authors_df,add_to_authors_df)
  # # 
  # # add_to_papers_df<-add_to_papers_df %>% rename(refID=usgs_refID)
  # # 
  # # add_to_papers_df$PY<-as.numeric(add_to_papers_df$PY)
  # # papers_df<-bind_rows(papers_df,add_to_papers_df)
  # # 
  # # return(list(papers=papers_df,authors=authors_df))
  # # 
  # # 
  # 
}
