fix_usgs_affils <- function(authors_to_fix,papers_to_fix) {
  
  
# TO CORRECT *ALL* THE USGS AUTHORS in authors_to_fix, not just the ones that were in the added papers
  # 
  # authors_to_fix<-authors_df_trim
  # 
  # papers_to_fix<-papers_df_trim
  
usgs_papers<-read_rds("./data_clean/usgs_papers_clean.rds")

usgs_authors<-read_rds("./data_clean/usgs_authors_clean.rds")
usgs_authors<-usgs_authors %>% 
  filter(federal==TRUE)

usgs_papers_key<-usgs_papers %>% 
  select(usgs_refID,DI) 

papers_to_fix_to_fix_au<-usgs_papers %>% 
  filter(usgs_refID%in%usgs_authors$usgs_refID) %>% 
  select(DI,TI,PY)

papers_to_fix<-papers_to_fix
papers_to_fix_to_fix_au$PY<-as.integer(papers_to_fix_to_fix_au$PY)
papers_to_fix_cut<-inner_join(papers_to_fix,papers_to_fix_to_fix_au, by=c("DI","TI","PY")) %>% 
  select(refID)

authors_fixed<-authors_to_fix %>% filter(refID%in%papers_to_fix_cut$refID)

authors_fixed<-authors_fixed %>% 
  mutate(AU=AF) %>% 
  mutate(AU=str_replace_all(AU,"[.]","")) %>% 
  mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]","")) %>% 
  mutate(first=str_sub(first_middle_initials, start = 1, end=1)) %>% 
  filter(is.na(federal))



usgs_authors$author_order<-as.integer(usgs_authors$author_order)
usgs_authors$federal<-as.logical(usgs_authors$federal)
usgs_authors<-usgs_authors %>% 
  rename(
    agency_primary=agency,
    agency_short=agency_2,
    affiliation=agency_3,
  )  %>% 
  mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]","")) %>%
  mutate(AU=str_replace_all(AU,"[,]","")) %>% 
  mutate(AF=str_replace_all(AF,"[.]","")) %>% 
  mutate(first=str_sub(first_middle_initials, start = 1, end=1))

# DO IN ORDER FROM GREATEST RIGOR DOWN
# foo<-full_join(authors_fixed,usgs_authors,by=c("AU","surname","first_middle_initials"))

# update_df<-left_join(authors_fixed,usgs_authors,by=c("surname","first"))


update_df<-full_join(authors_fixed,usgs_authors,by=c("AU"))

update_df<-update_df %>% 
  relocate(federal.y,.before=1) %>% 
  relocate(federal.x,.before=1) %>% 
  arrange(federal.x,federal.y) %>% 
  select(-federal.x,
         -agency,
         -agency_primary.x) %>% 
  # filter(is.na(federal.x)) %>% 
  distinct() %>% 
  filter(federal.y==TRUE) %>% 
  select(federal.y,AF.x,SID,refID,agency_short,agency_primary.y) %>% 
  distinct() %>% 
  mutate(AF=AF.x) %>% 
  select(-AF.x) %>% 
  rename(agency_primary=agency_primary.y,
         federal=federal.y)  

# names(update_df)
authors_to_fix<-authors_to_fix %>% 
  mutate(AF=str_replace_all(AF,"[.]",""))  
  
# authors_to_fix<-left_join(authors_to_fix,update_df,by=c("SID","refID","AF")) 
authors_to_fix<-left_join(authors_to_fix,update_df,by=c("SID","refID")) 
authors_to_fix<-authors_to_fix %>% 
  mutate(name_check=(AF.x==AF.y))

# names(authors_to_fix)

authors_to_fix<-authors_to_fix%>% 
  mutate(federal.x=if_else(name_check==TRUE & (is.na(federal.x) & !is.na(federal.y)),federal.y,federal.x)) %>% 
  mutate(agency=if_else(name_check==TRUE &(is.na(agency) & !is.na(agency_short)),agency_short,agency)) %>% 
  mutate(agency_primary.x=if_else(name_check==TRUE & (is.na(agency_primary.x) & !is.na(agency_primary.y)),agency_primary.y,agency_primary.x)) %>%
  select(-federal.y,
  -agency_short,
  -agency_primary.y,
  -AF.y) %>% 
rename(federal=federal.x,
agency_primary=agency_primary.x,
AF=AF.x) 





# step 2 ------------------------------------------------------------------




authors_fixed<-authors_to_fix %>% filter(refID%in%papers_to_fix_cut$refID)

authors_fixed<-authors_fixed %>% 
  mutate(AU=AF) %>% 
  mutate(AU=str_replace_all(AU,"[.]","")) %>% 
  mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]","")) %>% 
  mutate(first=str_sub(first_middle_initials, start = 1, end=1)) %>% 
  filter(is.na(federal))
update_df<-left_join(authors_fixed,usgs_authors,by=c("surname","first"))

update_df<-update_df %>% 
  relocate(federal.y,.before=1) %>% 
  relocate(federal.x,.before=1) %>% 
  arrange(federal.x,federal.y) %>% 
  select(-federal.x,
         -agency,
         -agency_primary.x) %>% 
  # filter(is.na(federal.x)) %>% 
  distinct() %>% 
  filter(federal.y==TRUE) %>% 
  select(federal.y,AF.x,SID,refID,agency_short,agency_primary.y) %>% 
  distinct() %>% 
  mutate(AF=AF.x) %>% 
  select(-AF.x) %>% 
  rename(agency_primary=agency_primary.y,
         federal=federal.y)  

names(update_df)
authors_to_fix<-authors_to_fix %>% 
  mutate(AF=str_replace_all(AF,"[.]",""))  

# authors_to_fix<-left_join(authors_to_fix,update_df,by=c("SID","refID","AF")) 
authors_to_fix<-left_join(authors_to_fix,update_df,by=c("SID","refID")) 
authors_to_fix<-authors_to_fix %>% 
  mutate(name_check=(AF.x==AF.y))

# names(authors_to_fix)

authors_to_fix<-authors_to_fix%>% 
  mutate(federal.x=if_else(name_check==TRUE & (is.na(federal.x) & !is.na(federal.y)),federal.y,federal.x)) %>% 
  mutate(agency=if_else(name_check==TRUE & (is.na(agency) & !is.na(agency_short)),agency_short,agency)) %>% 
  mutate(agency_primary.x=if_else((is.na(agency_primary.x) & !is.na(agency_primary.y)),agency_primary.y,agency_primary.x)) %>% 
  select(-federal.y,
         -agency_short,
         -agency_primary.y,
         -AF.y) %>% 
  rename(federal=federal.x,
         agency_primary=agency_primary.x,
         AF=AF.x) %>% 
  select(-name_check)




# now by refID and name ---------------------------------------------------


# authors_to_fix_final_check<-authors_to_fix %>% 
#   filter(federal==TRUE & is.na(agency))
# 
# usgs_authors_final_check<-usgs_authors %>% select(authorID,
#                                                   refID=usgs_refID,
#                                                   agency_primary,
#                                                   agency_short,
#                                                   federal,
#                                                   author_order)
# 
# # names(authors_to_fix_final_check)
# authors_to_fix_final_check<-authors_to_fix_final_check %>% 
#   left_join(usgs_authors_final_check,by=c("authorID","author_order"))%>% 
#   distinct() %>% 
#   select(AF,
#          refID=refID.x,
#          authorID,
#          federal=federal.x,
#          agency=agency_short,
#          agency_primary=agency_primary.y,
#          author_order) %>% 
#   distinct()
# 
# authors_to_fix<-authors_to_fix %>% 
#   left_join(authors_to_fix_final_check,
#             by=c("refID","authorID","author_order")) %>% 
#   mutate(name_check=(AF.x==AF.y)) %>% 
# mutate(federal.x=if_else(name_check==TRUE & (is.na(federal.x) & !is.na(federal.y)),federal.y,federal.x)) %>% 
#   mutate(agency.x=if_else(name_check==TRUE & (is.na(agency.x) & !is.na(agency.y)),agency.y,agency.x)) %>% 
#   mutate(agency_primary.x=if_else((is.na(agency_primary.x) & !is.na(agency_primary.y)),agency_primary.y,agency_primary.x)) %>%
#   select(-federal.y,
#          -agency.y,
#          -agency_primary.y,
#          -AF.y) %>% 
#   rename(federal=federal.x,
#          agency_primary=agency_primary.x,
#          agency=agency.x,
#          AF=AF.x) %>% 
#   select(-name_check)
# 
# 
# 
# 
# authors_fixed<-authors_to_fix %>% filter(refID%in%papers_to_fix_cut$refID)
# 
# authors_fixed<-authors_fixed %>% 
#   mutate(AU=AF) %>% 
#   mutate(AU=str_replace_all(AU,"[.]","")) %>% 
#   mutate(first_middle_initials=str_replace_all(first_middle_initials,"[.]","")) %>% 
#   mutate(first=str_sub(first_middle_initials, start = 1, end=1)) %>% 
#   filter(is.na(agency)& !is.na(federal)) # for some reasons some are true but no agency
# 
# # authors_fixed$AU
# # usgs_authors$AU
# update_df<-left_join(authors_fixed,usgs_authors,by=c("AU"))
# 
# update_df<-update_df %>% 
#   relocate(federal.y,.before=1) %>% 
#   relocate(federal.x,.before=1) %>% 
#   arrange(federal.x,federal.y) %>% 
#   select(-federal.x,
#          -agency,
#          -agency_primary.x) %>% 
#   # filter(is.na(federal.x)) %>% 
#   distinct() %>% 
#   filter(federal.y==TRUE) %>% 
#   select(federal.y,AF.x,SID,refID,agency_short,agency_primary.y) %>% 
#   distinct() %>% 
#   mutate(AF=AF.x) %>% 
#   select(-AF.x) %>% 
#   rename(agency_primary=agency_primary.y,
#          federal=federal.y)  
# 
# names(update_df)
# authors_to_fix<-authors_to_fix %>% 
#   mutate(AF=str_replace_all(AF,"[.]",""))  
# 
# # authors_to_fix<-left_join(authors_to_fix,update_df,by=c("SID","refID","AF")) 
# authors_to_fix<-left_join(authors_to_fix,update_df,by=c("AF","refID")) 
# authors_to_fix<-authors_to_fix %>% 
#   mutate(name_check=(AF.x==AF.y))
# 
# names(authors_to_fix)
# 
# authors_to_fix<-authors_to_fix%>% 
#   mutate(federal.x=if_else(name_check==TRUE & (is.na(federal.x) & !is.na(federal.y)),federal.y,federal.x)) %>% 
#   mutate(agency=if_else(name_check==TRUE & (is.na(agency) & !is.na(agency_short)),agency_short,agency)) %>% 
#   mutate(agency_primary.x=if_else((is.na(agency_primary.x) & !is.na(agency_primary.y)),agency_primary.y,agency_primary.x)) %>% 
#   mutate(agency=if_else(name_check==TRUE & (is.na(agency) & !is.na(agency_short)),agency_short,agency)) %>% 
#   select(-federal.y,
#          -agency_short,
#          -agency_primary.y,
#          -AF.y) %>% 
#   rename(federal=federal.x,
#          agency_primary=agency_primary.x,
#          AF=AF.x) %>% 
#   select(-name_check)






# names_orig_authors_to_fix<-as.data.frame(names_orig_authors_to_fix)
# names_authors_to_fix<-as.data.frame(names(authors_to_fix))
# names_orig_authors_to_fix==names_authors_to_fix


authors_to_fix<-authors_to_fix %>% 
  mutate(agency=if_else(agency=="usgs","interior",agency))
# 
# 
# fednofed_chk<-authors_to_fix %>% 
#   filter(is.na(agency)) %>% 
#   group_by(surname,first) %>% 
#   # filter(surname=="powell") %>% 
#   
#   tally()
# 
# 
# fednofed_chk2<-authors_to_fix %>% 
#   filter(!is.na(agency)) %>% 
#   group_by(surname,first) %>% 
#   # filter(surname=="powell") %>% 
#   
#   tally() %>% 
#   rename(n2=n)
# 
# left_join(fednofed_chk,fednofed_chk2)
# 
# %>% 
# replace_na(list("federal"=0,"not_fed"=0)) %>% 
# filter(federal>0) %>% 
# filter(not_fed>0)
# summarize()  






return(authors_to_fix)
}
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
# 
# 
# 
# 
# 
# 
# # -------------------------------------------------------------------------
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
# 
# 
# 
# 
# 
# 
# foo %>% group_by(surname,first) %>% 
#   tally(n_distinct(AF.x)) %>% 
#   arrange(desc(n))
# 
# 
# %>% 
#   
#   
#   %>% 
#   
#   relocate(surname.x,.after=2) %>% 
#   relocate(surname.y,.after=2)
# 
# 
# authors_
# group_by(DI) %>% 
#   tally() %>% 
#   filter(n>1) %>% 
#   arrange(desc(n))
# 
# 
# distinct(DI)
# 
# 
# 
# 
# 
# 
# 
# 
# ######### Split country and reorganize to merge with author_df 
# # 
# # usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
# #   mutate(country=if_else(is.na(country),"usa",country))
# # 
# # max_cols <- usgs_authors_for_papers_NOT %>%
# #   pull(country) %>%
# #   str_split(",") %>%
# #   map_int(length) %>%
# #   max()
# # 
# # # Step 2: Create column names dynamically
# # col_names <- paste0("country", 1:max_cols)
# # 
# # # Step 3: Use separate with extra = "merge" and fill = "right"
# # df_sep <- usgs_authors_for_papers_NOT %>% 
# #   select(refID,country) %>% 
# #   separate(country, into = col_names, sep = ",", fill = "right", extra = "merge")
# # 
# # df_sep_1country<-df_sep %>% 
# #   group_by(refID) %>% 
# #   slice_head(n=1) %>% 
# #   filter(is.na(country2))
# # 
# # foo<-usgs_authors_for_papers_NOT %>% 
# #   mutate(country2 = strsplit(country, ",")) %>%
# #   unnest(refID) %>%
# #   group_by(refID) %>%
# #   mutate(row = row_number()) %>%
# #   spread(row, country)
# # 
# # 
# #   separate(country,)
# #   pivot_longer(cols=country,
# #                names_to="author_country",
# #                values_to="country") %>% 
# #   relocate(author_country,.before="surname")
# # 
# # 
# usgs_for_affils<-usgs_authors_for_papers_NOT %>%
#   select(
#     refID,
#     affil_id,
#     affiliation,
#     city,
#     # country,
#     entry_no,
#     source_file,
#     agency_primary,
#     federal
#   ) %>% 
#   mutate(agency="interior")
# 
# names(affils_df)
# 
# 
# authors_to_fix<-bind_rows(authors_to_fix,usgs_authors_for_papers_NOT)
# 
# papers_to_fix <-papers_to_fix %>% mutate(scopus_article_id=as.character(scopus_article_id))
# usgs_NOT_in_papers_to_fix <-usgs_NOT_in_papers_to_fix %>% mutate(BP=as.numeric(BP))
# usgs_NOT_in_papers_to_fix <-usgs_NOT_in_papers_to_fix %>% mutate(EP=as.numeric(EP))
# usgs_NOT_in_papers_to_fix <-usgs_NOT_in_papers_to_fix %>% mutate(PY=as.integer(PY))
# 
# papers_to_fix<-papers_to_fix %>% 
#   mutate(source="scopus")
# papers_to_fix<-bind_rows(papers_to_fix,usgs_NOT_in_papers_to_fix)
# 
# return(list(papers=papers_to_fix,authors=authors_to_fix))
# 
# # 
# # 
# # names(authors_df)
# # # usgs_authors_for_papers_NOT<-
# # #   usgs_authors_for_papers_NOT %>% left_join(entry_info)
# # # 
# # 
# # 
# # 
# # 
# # names(usgs_authors_for_papers_NOT)
# # 
# # usgs_authors_for_papers_NOT<-usgs_authors_for_papers_NOT %>% 
# #   select(surname,AF, given_name, email,agency_short=agency_2,agency_primary=agency) %>% 
# #   # select(surname,AF, given_name, federal,email) %>% 
# #   # arrange(desc(federal)) %>% 
# #   separate(given_name,c("first","middle"),extra = "merge") 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # authors_df_usgs_check<-authors_df %>%
# #   filter(refID%in%usgs_already_in_papers_to_fix$refID) %>% 
# #   select(AF,surname,given_name,SID,affil_id) 
# # # %>% 
# #   # select(AF,surname,given_name,federal,SID,affil_id) %>% 
# #   # mutate(federal=as.character(federal)) 
# # 
# # affils_for_usgs_check<-affils_df %>% filter(affil_id%in%authors_df_usgs_check$affil_id) %>% 
# #   # select(affil_id,agency,federal) %>% 
# #   select(affil_id) %>% 
# #   # mutate(federal=as.character(federal)) %>% 
# #   distinct()
# # 
# # affils_for_usgs_check$affil_id<-as.character(affils_for_usgs_check$affil_id)
# # authors_df_usgs_check$affil_id<-as.character(authors_df_usgs_check$affil_id)
# # authors_df_usgs_check<-left_join(authors_df_usgs_check,affils_for_usgs_check)
# # 
# # library(stringr)
# # 
# # 
# # 
# # authors_df_usgs_check<-authors_df_usgs_check %>% 
# #   mutate(AF=str_replace_all(AF,"[.]","")) %>% 
# #   separate(given_name,c("first","middle"),extra = "merge")
# # 
# # 
# # usgs_authors_clean<-usgs_authors %>% 
# #   select(surname,AF, given_name, email,agency_short=agency_2,agency_primary=agency) %>% 
# #   # select(surname,AF, given_name, federal,email) %>% 
# #   # arrange(desc(federal)) %>% 
# #   separate(given_name,c("first","middle"),extra = "merge") 
# # 
# # fed_check_usgs<-full_join(authors_df_usgs_check, usgs_authors_clean,by=c("surname","first")) 
# # 
# # 
# # fed_check_usgs_match<-fed_check_usgs %>% 
# #   # filter((federal.x=="TRUE" & federal.y=="TRUE")|(federal.x=="FALSE" & federal.y=="FALSE")) %>% 
# #   distinct() %>%
# #   select(-AF.y,
# #          -middle.x) %>% 
# #   rename(AF=AF.x,
# #          middle=middle.y) %>% 
# #   distinct()
# # 
# # # fed_check_usgs_remain<-anti_join(fed_check_usgs,fed_check_usgs_match) %>% distinct(SID,federal.x,federal.y,.keep_all=TRUE)
# # fed_check_usgs_remain<-anti_join(fed_check_usgs,fed_check_usgs_match,by="SID") %>% distinct(SID,federal.x,federal.y,.keep_all=TRUE)
# # fed_check_usgs_remain<-fed_check_usgs_remain %>% 
# #   mutate(federal.x=if_else(federal.y=="TRUE","TRUE",federal.x)) %>% 
# #   arrange(desc(federal.y),federal.x) %>% 
# #   select(SID,federal.x) %>% 
# #   filter(federal.x==TRUE)
# # 
# # 
# # authors_df<-authors_df %>%
# #   left_join(fed_check_usgs_remain,by="SID") %>% 
# #   mutate(federal=if_else(federal.x=="TRUE",TRUE,federal)) %>% 
# #   select(-federal.x)
# # 
# # 
# # authors_df %>% distinct(surname,given_name,.keep_all=TRUE) %>% group_by(surname,given_name) %>% tally(n_distinct(federal))
# # 
# # 
# # 
# # 
# # # <-read_csv("./data_raw/scopus_api/fed_files/usgs_no_doi.csv")
# # usgs_papers_no_doi<-usgs_papers %>% filter(is.na(DI))
# # match_title<-papers_to_fix %>% filter(TI%in%usgs_papers_no_doi$TI) 
# # 
# # usgs_already_in_papers_to_fix<-papers_to_fix %>% filter(DI%in%usgs_with_DI$DI) %>% select(refID,DI)
# # 
# # # usgs_pubs<-read_rds("./data_intermediate/usgs_papers_clean.rds")
# # # 
# # 
# # usgs_pubs_to_add1<-anti_join(usgs_papers,usgs_already_in_papers_to_fix,by="DI")
# # usgs_pubs_to_add2<-semi_join(usgs_papers,match_title,by="TI") 
# # 
# # usgs_papers_NOT_in_papers_to_fix<-bind_rows(usgs_pubs_to_add1,usgs_pubs_to_add2) %>% 
# #   select(-c(Country,
# #             agency_3,
# #             State,
# #             City,
# #             URL,
# #             agency,
# #             agency_2)) 
# # 
# # # 
# # # 
# # # usgs_authors<-read_rds("./data_intermediate/usgs_authors_clean.rds") 
# # # 
# # 
# # add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%usgs_papers_NOT_in_papers_to_fix$usgs_refID) %>% 
# #   mutate(from="usgs") %>% 
# #   rename(refID=usgs_refID) %>% 
# #   select(-c(agency_2,agency_3,country,city,from)) %>% 
# #   mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
# #   mutate(federal=as.logical(federal))
# # # ,
# # #          PY=as.numeric(PY)) 
# # # 
# # # add to authors_df and papers_to_fix -----------------------------------------
# # authors_df<-authors_df %>% mutate(federal=as.logical(federal))
# # add_to_authors_df$author_order<-as.integer(add_to_authors_df$author_order)
# # authors_df$author_order<-as.integer(authors_df$author_order)
# # 
# # authors_df$authorID<-as.character(authors_df$authorID)
# # add_to_authors_df$authorID<-as.character(add_to_authors_df$authorID)
# # add_to_authors_df$PY<-as.numeric(add_to_authors_df$PY)
# # add_to_authors_df$entry_no<-as.integer(add_to_authors_df$entry_no)
# # 
# # # add_to_authors_df: are any of these federal in the main df?
# # 
# # 
# # 
# # 
# # add_to_authors_df_trim<-  add_to_authors_df %>% 
# #   select(AF,surname,given_name,federal,affil_id) %>% 
# #   mutate(federal=as.character(federal)) %>% 
# #   distinct() %>% 
# #   mutate(affil_id=as.integer(affil_id))
# # 
# # 
# # 
# # 
# # authors_df_fed_check<-authors_df %>% 
# #   select(AF,surname,given_name,federal,SID,affil_id) %>% 
# #   mutate(federal=as.character(federal)) %>% 
# #   distinct() 
# # 
# # # add_to_authors_df<-left_join(add_to_authors_df_trim,authors_df_fed_check,by="affil_id")
# # 
# # names<-names(authors_df)
# # names2<-names(add_to_authors_df)
# # names<-intersect(names,names2)
# # 
# # 
# # add_to_authors_df<-add_to_authors_df %>% mutate(affil_id=as.numeric(affil_id)) %>% select(all_of(names)) 
# # 
# # add_to_papers_to_fix<-usgs_papers_NOT_in_papers_to_fix %>% rename(refID=usgs_refID)
# # 
# # add_to_papers_to_fix$PY<-as.numeric(add_to_papers_to_fix$PY)
# # 
# # papers_to_fix$BP<-as.character(papers_to_fix$BP)
# # papers_to_fix$EP<-as.character(papers_to_fix$EP)
# # 
# # papers_to_fix<-bind_rows(papers_to_fix,add_to_papers_to_fix)
# # 
# # return(list(papers=papers_to_fix,authors=authors_df))
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # usgs_DI<-usgs_pubs_to_add %>% 
# # #   select(DI,usgs_refID) %>% 
# # #   drop_na() 
# # # 
# # # papers_DI<-papers_to_fix %>% 
# # #   select(DI,refID) %>% 
# # #   drop_na()
# # # 
# # # 
# # # usgs_papers_in_papers_to_fix<-semi_join(papers_DI,usgs_DI,by="DI") %>% 
# # #   select(DI)
# # # 
# # # 
# # # 
# # # for_usgs_authors1<-semi_join(usgs,usgs_papers_in_papers_to_fix) %>% select(usgs_refID,DI,PY)
# # # for_usgs_authors2<-semi_join(usgs_authors,for_usgs_authors1,by="usgs_refID")
# # # 
# # # for_usgs_authors2<-left_join(for_usgs_authors2,for_usgs_authors1) %>% 
# # #   filter(federal==TRUE) %>% 
# # #   select(DI,AF,affil_id,author_order,PY,federal) %>% 
# # #   distinct() %>% 
# # #   mutate(PY=as.integer(PY))
# # # 
# # # refID_to_correct_authors<-semi_join(papers_to_fix,for_usgs_authors2,by="DI") %>% 
# # #   select(refID,DI,PY)
# # # 
# # # for_usgs_authors2<-left_join(for_usgs_authors2,refID_to_correct_authors)
# # # 
# # # # %>% 
# # # #   mutate(PY=as.numeric(PY))
# # # 
# # # 
# # # for_usgs_authors2$author_order<-as.numeric(for_usgs_authors2$author_order)
# # # 
# # # for_usgs_authors2$PY<-as.numeric(for_usgs_authors2$PY)
# # # 
# # # 
# # # authors_df<-left_join(authors_df,for_usgs_authors2,by=c("refID","author_order")) %>% 
# # #   mutate(federal.x=as.character(federal.x)) %>% 
# # #   mutate(federal.x=
# # #            case_when(
# # #              federal.y == "TRUE" ~ "TRUE",
# # #              .default = as.character(federal.x)
# # #            )
# # #   ) %>% 
# # #   mutate(affil_id.x=
# # #            case_when(
# # #              affil_id.y == "60011347" ~ "60011347",
# # #              .default = as.character(affil_id.x)
# # #            )
# # #   ) %>% 
# # #   select(-affil_id.y,
# # #          -federal.y) %>% 
# # #   rename(affil_id=affil_id.x,
# # #          federal=federal.x) %>% 
# # #   mutate(AF.y=if_else(AF.y==AF.x,NA,AF.y)) %>% 
# # #   select(-AF.y) %>% 
# # #   rename(AF=AF.x)
# # # 
# # # 
# # # 
# # # # now the ones that aren't in papers_to_fix
# # # 
# # # 
# # # usgs_papers_NOT_in_papers_to_fix<-anti_join(usgs_DI,papers_DI,by="DI") %>% 
# # #   select(DI)
# # # 
# # # 
# # # 
# # # 
# # # add_to_papers_to_fix<-semi_join(usgs,usgs_papers_NOT_in_papers_to_fix) %>% 
# # #   select(-c(Country,
# # #             agency_3,
# # #             State,
# # #             City,
# # #             URL,
# # #             agency,
# # #             agency_2)) %>% 
# # #   mutate(BP=as.numeric(BP),
# # #          EP=as.numeric(EP))
# # # # ,
# # # # PY=as.numeric(PY))
# # # 
# # # add_to_authors_df<-usgs_authors %>% filter(usgs_refID%in%add_to_papers_to_fix$usgs_refID) %>% 
# # #   mutate(from="usgs") %>% 
# # #   rename(refID=usgs_refID) %>% 
# # #   select(-c(agency_2,agency_3,country,city,from)) %>% 
# # #   mutate(affil_id=if_else(federal=="FALSE",NA,affil_id)) %>% 
# # #   mutate(federal=as.logical(federal))
# # # # ,
# # # #          PY=as.numeric(PY)) 
# # # # 
# # # # add to authors_df and papers_to_fix -----------------------------------------
# # # authors_df<-authors_df %>% mutate(federal=as.logical(federal))
# # # add_to_authors_df$author_order<-as.integer(add_to_authors_df$author_order)
# # # authors_df$author_order<-as.integer(authors_df$author_order)
# # # 
# # # authors_df$authorID<-as.character(authors_df$authorID)
# # # add_to_authors_df$authorID<-as.character(add_to_authors_df$authorID)
# # # 
# # # add_to_authors_df$PY<-as.numeric(add_to_authors_df$PY)
# # # 
# # # add_to_authors_df$entry_no<-as.integer(add_to_authors_df$entry_no)
# # # 
# # # authors_df<-bind_rows(authors_df,add_to_authors_df)
# # # 
# # # add_to_papers_to_fix<-add_to_papers_to_fix %>% rename(refID=usgs_refID)
# # # 
# # # add_to_papers_to_fix$PY<-as.numeric(add_to_papers_to_fix$PY)
# # # papers_to_fix<-bind_rows(papers_to_fix,add_to_papers_to_fix)
# # # 
# # # return(list(papers=papers_to_fix,authors=authors_df))
# # # 
# # # 
# 

