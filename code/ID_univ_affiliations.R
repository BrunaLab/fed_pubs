ID_univ_affiliations <- function(affils_df) {
uni_affils <- affils_df<-read_csv("./data_clean/api_uni_affils_searched_2025-09-01.csv") %>%
  mutate_all(tolower) %>%
  mutate(affil_id = as.numeric(affil_id)) %>% 
  mutate(country = case_when(
    country == "united states" ~ "usa",
    country == "virgin islands (u.s.)" ~ "us virgin islands",
    country == "virgin islands (british)" ~ "british virgin islands",
    country == "viet nam" ~ "vietnam",
    country == "united kingdom" ~ "uk",
    country == "cote d'ivoire" ~ "ivory coast",
    TRUE ~ country  # keep original value if no match
  ))

  # standardize COUNTRIES -----------------------------------------------
  # levels(as.factor(affils_df$country))

  
  affils_df <- affils_df %>%
    mutate(affil_id=as.numeric(affil_id)) %>% 
    mutate(country = case_when(
      country == "united states" ~ "usa",
      country == "virgin islands (u.s.)" ~ "us virgin islands",
      country == "virgin islands (british)" ~ "british virgin islands",
      country == "viet nam" ~ "vietnam",
      country == "united kingdom" ~ "uk",
      country == "cote d'ivoire" ~ "ivory coast",
      TRUE ~ country  # keep original value if no match
    ))
  
# 
  affils_df <- left_join(affils_df, uni_affils, by = c("affil_id", "affiliation")) %>%
    relocate(c(uni, affil_id), .after = entry_no) %>%
    select(-city.y, -country.y) %>%
    rename(city = city.x, country = country.x)

  return(affils_df)
}
