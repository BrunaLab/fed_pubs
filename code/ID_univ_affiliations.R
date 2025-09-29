ID_univ_affiliations <- function(affils_df) {
  uni_affils <- read_csv("./data_raw//affiliations_to_search/uni_affils/follow_up/all_uni_affils_searched.csv") %>%
    mutate_all(tolower) %>%
    mutate(affil_id = as.numeric(affil_id))

  levels(as.factor(affils_df$country))

  uni_affils$country[uni_affils$country == "united states"] <- "usa"
  uni_affils$country[uni_affils$country == "virgin islands (u.s.)"] <- "us virgin islands"
  uni_affils$country[uni_affils$country == "virgin islands (british)"] <- "british virgin islands"
  uni_affils$country[uni_affils$country == "viet nam"] <- "vietnam"
  uni_affils$country[uni_affils$country == "cote d'ivoire"] <- "ivory coast"

  # standardize COUNTRIES -----------------------------------------------
  levels(as.factor(affils_df$country))

  affils_df$country[affils_df$country == "united states"] <- "usa"
  affils_df$country[affils_df$country == "virgin islands (u.s.)"] <- "us virgin islands"
  affils_df$country[affils_df$country == "virgin islands (british)"] <- "british virgin islands"
  affils_df$country[affils_df$country == "viet nam"] <- "vietnam"
  affils_df$country[affils_df$country == "cote d'ivoire"] <- "ivory coast"

  affils_df <- left_join(affils_df, uni_affils, by = c("affil_id", "affiliation")) %>%
    relocate(c(uni, affil_id), .after = entry_no) %>%
    select(-city.y, -country.y) %>%
    rename(city = city.x, country = country.x)

  return(affils_df)
}
