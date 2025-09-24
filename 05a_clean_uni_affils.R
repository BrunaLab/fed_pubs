# # into --------------------------------------------------------------------
# # 

# 
library(janitor)
library(tidyverse)
library(progress)
library(fs)
library(data.table)




# search for others from focal universities -------------------------------


# 
# 
# authors_df_complete <- setDT(read_rds("./data_clean/authors_df_uni_clean.rds")) %>%
#   mutate(uni=case_when(
#     is.na(uni) ~ "other",
#     .default = as.character(uni)
#   ))
# 
# more_uni_affils<-authors_df_complete %>%
#   filter(uni=="other") %>%
#   mutate(uni_check=uni) %>%
#   select(affil_id,affiliation,uni_check) %>%
#   distinct() %>%
#   mutate(uni_check=case_when(
#    str_detect(affiliation, "stanford") ~ "stanford",
#    str_detect(affiliation, "university of washington") ~ "washington",
#    # str_detect(affiliation, "minnesota") ~ "minn",
#    str_detect(affiliation, "university of florida") ~ "florida",
#    str_detect(affiliation, "harvard") ~ "harvard",
#    str_detect(affiliation, "university of pennsylvania") ~ "penn",
#    # str_detect(affiliation, "ohio") ~ "ohio_state",
#    str_detect(affiliation, "ucla") ~ "ucla",
#    str_detect(affiliation, "ucsd") ~ "ucsd",
#    str_detect(affiliation, "ucsf") ~ "ucsf",
#    str_detect(affiliation, "unc ") ~ "unc_ch",
#    str_detect(affiliation, "university of north carolina ") ~ "unc_ch",
# 
#    #
#    affil_id %in% c(
#    131963342,
#    132653442,
#    124914743,
#    124617761,
#    124032297) ~ "unc_ch",
# 
# 
#        affil_id %in% c(
#          131355735 ,
#          130351207 ,
#          113061768,
#          106792646) ~ "ucsd",
# 
#        affil_id %in% c(
#          101263798 ,
#          125938308 ,
#          106662827
#          ) ~ "ucsf",
# 
#        affil_id %in% c(
#          129662747,
#          101400071
# 
#        ) ~ "ucla",
# 
#    affil_id %in% c(
#    125946538,
#    127864429,
#    130095838,
#    131422883,
#    130730620,
#    108146869
#   ) ~ "ohio_state",
# 
# 
#        affil_id %in% c(
#          123974342,
#          125230920,
#          60009168,
#          125759259,
#          100831187,
#          126845023,
#          126240479,
#          128231243,
#          112852009,
#          119442123,
#          129018879,
#          100873105,
#          131500662,
#          114913407,
#          131526107,
#          124194303,
#          116083632,
#          112981335,
#          132781641,
#          124394781,
#          126357513,
#          132581151,
#          132580598,
#          115307337,
#          100681524,
#          117086987,
#          125122541,
#          106015426,
#          106619536,
#          120266513,
#          124450844,
#          130428418,
#          129485723,
#          129437094,
#          129110585,
#          119242229,
#          100386828,
#          124654537,
#          128997815,
#          123840127
#    ) ~ "minn",
# 
# 
# 
#    affil_id %in% c(
#      127591224,
#      112702028,
#      113039818,
#      123735230,
#      124527071,
#      116980359,
#      125632965,
#      115413707,
#      124037039,
#      128037203,
#      130000837,
#      106172312,
#      130217444,
#      130540841,
#      107453136,
#      131691667,
#      131458212,
#      130571409,
#      124509079,
#      131479354,
#      114876010,
#      113187823,
#      131329059,
#      127428940,
#      132475148,
#      125719197,
#      132717032,
#      106067728,
#      132717032,
#      108234125,
#      126641925,
#      129474808,
#      121836213,
#      123323512,
#      107149117,
#      114577272,
#      112696641,
#      106715564,
#      120203450,
#      126230053,
#      102057473
#    ) ~ "penn",
# 
#    TRUE ~ as.character(uni_check)
#    )
#   )
#
#
#
#
#
# more_uni_affils<-more_uni_affils %>%
#   mutate(uni_check=case_when(
# affil_id %in% c(
#   127182460,
#   124218990,
#   124438824,
#   126229901,
#   60019187,
#   122918964,
#   60122465,
#   60122464,
#   130797935,
#   115019433,
#   131200893,
#   100513236,
#   114100591,
#   123777168,
#   60028785,
#   123777239,
#   126553733,
#   132402479,
#   60018474,
#   60006951,
#   60093602,
#   60029194,
#   122737389,
# 60016655,
# 60029245,
# 125436009,
# 100450818,
# 114437724,
# 132494657,
# 100686306,
# 60001423,
# 60003835,
# 112933102,
#   60011132,
#   119957606
# ) ~ NA,
# 
# TRUE ~ as.character(uni_check)
#   )
#   )
#
#
# more_uni_affils<-more_uni_affils %>%
#   filter(uni_check!="other")

#
# write_csv(more_uni_affils,"./data_raw/affiliations_to_search/uni_affils/follow_up/uni_affils_follow_up.csv")

#
#
# dataDir <- "./data_raw/affiliations_to_search/uni_affils"
# 
# dataFls <- dir(dataDir, pattern = "csv$", full.names = TRUE)
# dataFls<-dataFls[ !dataFls == "./data_raw/affiliations_to_search/uni_affils/scopus_info_uni_affils.csv"]
# 
# # Read and tag each file
# dt_list <- lapply(dataFls, function(file) {
#   dt <- fread(file, fill = TRUE)
#   dt[, source_file := basename(file)]  # Add column with filename
#   return(dt)
# })
# 
# 
# # Combine all tagged data tables
# affils_df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
# affils_df<-affils_df %>%
#   pivot_longer(
#     cols = starts_with("V"),
#     names_to = "col",
#     values_to = "affil_id",
#     values_drop_na = TRUE) %>%
#   separate_wider_delim(source_file,delim = ".", names = c("uni", "csv")) %>%
#   distinct() %>%
#   select(-col,-csv)
# 
# affil_info<-read_csv("./data_raw/affiliations_to_search/uni_affils/scopus_info_uni_affils.csv")
# 
# affils_df<-left_join(affils_df,affil_info,by="affil_id") %>%
#   select(-document_count,-pub_count) %>%
#   filter(!is.na(affiliation)) %>%
#   distinct(affil_id,.keep_all = TRUE) %>%
#   mutate_all(tolower) %>%
#   mutate(affil_id=as.double(affil_id))
#
#
#
# affils_df_followup<- read_csv("./data_raw/affiliations_to_search/uni_affils/follow_up/uni_affils_follow_up.csv") %>%
#   rename(uni=uni_check)
#
# affils_df<-bind_rows(affils_df,affils_df_followup)

# write_csv(affils_df,"./data_raw/affiliations_to_search/uni_affils/follow_up/all_uni_affils_searched.csv")
  


# UNI
affils_df<-read_csv("./data_raw/affils/all_affils_df_uni.csv")
authors_df<-read_csv("./data_raw/authors/all_authors_df_uni.csv")
papers_df<-read_csv("./data_raw/papers/all_papers_df_uni.csv")
# 