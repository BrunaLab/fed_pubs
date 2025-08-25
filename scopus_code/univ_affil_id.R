

library(tidyverse)
library(rscopus)
# Define the folder path
folder_path <- "data_raw/uni_affils"

# List all files in the folder
file_list <- list.files(folder_path, full.names = TRUE)

# Read and process each file
uni_affils <- file_list %>%
  map_dfr(~ {
    read_delim(.x, delim = ",", col_names = FALSE) %>%
      pivot_longer(
        cols = starts_with("X"),
        names_to = "uni",
        values_to = "affil_id",
        values_drop_na = TRUE
      ) %>%
      select(-uni) %>%
      distinct() %>%
      mutate(file_name = basename(.x))
  })


search_term <- uni_affils$affil_id
search_term <- "109556305"
term <- seq_along(search_term)
all_affils<-data.frame()
for (h in term){
  
  query_string <-search_term[h]
  
  
  scopus_data_raw <- tryCatch({
    get_affiliation_info(
      affil_id = query_string,
      api_key = "e72ca4ba01261a08226d7ea9775ace33",
      verbose = TRUE
    )
  }, error = function(e) {
    message("Error retrieving data for affil_id: ", query_string)
    return(NULL)
  })
  
  
  # Skip if the result is NULL or empty
  if (is.null(scopus_data_raw)){
    next
  }else{
    all_affils <- bind_rows(all_affils, scopus_data_raw)
  }
}


scopus_info_uni_affils<-all_affils %>% 
  select("affil_id",
         affiliation="affil_name",
         pub_count="document-count",
         "city",
         "country",
         "document_count"=`document-count`) %>% 
  distinct() %>% 
  mutate(affil_id=as.numeric(affil_id))

uni_affils_clean<-left_join(uni_affils,scopus_info_uni_affils,by="affil_id") %>% 
  mutate(affiliation=case_when(
    affil_id==60121481~"Department of Philosophy", 
affil_id==60121482~"School of Arts and Humanities" , 
affil_id==60121483~"Department of History" , 
affil_id==60121485~"The Center for Hellenic Studies" , 
affil_id==60121487~"Institute of Arts and Humanities",  
affil_id==60121489~"Institute for Supply Chain Excellence and Innovation",  
affil_id==60121490~"Center for Social Innovation and Impact" , 
affil_id==60121491~"Atkinson Behavioral Research Laboratory", 
affil_id==60121513~"Policy Design and Evaluation Lab", 
affil_id==60121516~"Peter F. Cowhey Center on Global Transformation" , 
affil_id==60121525~"UCSD Department of Ethnic Studies" , 
affil_id==60121535~"Center for Global Mental Health", 
affil_id==60121544~"The Center for Research on Gender in Science Technology, Engineering, Mathematics, and Medicine", 
affil_id==60121553~"Advanced Cell Therapy Laboratory", 
affil_id==60121561~"Center for Network Medicine" , 
affil_id==60121591~"The HIV Institute", 
affil_id==60121640~"Center for NMR Spectroscopy & Imaging of Proteins" , 
affil_id==60121642~"School of Physical Sciences", 
affil_id==60121670~"Center for Extreme Events Research", 
affil_id==60121671~"Center for Machine-Intelligence Computing and Security", 
affil_id==60121673~"Center for Visual Computing", 
affil_id==60121687~"Division of Graduate Education and Postdoctoral Affairs", 
affil_id==60281120~"Power Management Integration Center", 
affil_id==60136676~"Technologies for Safe and Efficient Transportation", 
affil_id==60278651~"Hospital of the University of Pennsylvania - Cedar Avenue", 
affil_id==60156403~"Penn Research in Machine Learning", 
affil_id==60019829~"JDRF Center for Immunological Tolerance in Type 1 Diabetes", 
affil_id==60016830~"Harvard HMS and PHS Center for Physiological Genomics", 
affil_id==60138951~"Nationwide Center for Advanced Customer Insights",
.default = as.character(affiliation))
) %>% 
rename(uni=file_name)
# 
# 
# uni_affils_clean %>% group_by(uni,country) %>% tally() %>% arrange(desc(n))

write_csv(scopus_info_uni_affils,  "./data_raw/scopus_info_uni_affils.csv")

write_csv(uni_affils_clean,  "./data_clean/uni_affils_clean.csv")


