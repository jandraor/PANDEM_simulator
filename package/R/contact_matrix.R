get_contact_matrix <- function(country_code, pop_a, pop_b, pop_c) {

  polymod       <- socialmixr::polymod
  country_names <- socialmixr::survey_countries(polymod)

  user_country <- countrycode::countrycode(country_code,
                                           origin = "eurostat",
                                           destination = "country.name")

  if(!user_country %in% country_names) {

    msg <- paste0("Country: '", country_code,"' not available")
    return(msg)
  }

  age_limits <- c(0, 25, 50)

  pop_df <- data.frame(lower.age.limit = age_limits,
                       population = c(pop_a, pop_b, pop_c))

  cm_object <- socialmixr::contact_matrix(socialmixr::polymod,
                                          age.limits = age_limits,
                                          countries  = user_country,
                                          survey.pop = pop_df,
                                          symmetric  = TRUE,
                                          quiet      = TRUE)

  contact_matrix <- t(cm_object$matrix)

  contact_list <- as.list(contact_matrix)

  names(contact_list) <- c("aa", "ba", "ca",
                           "ab", "bb", "cb",
                           "ac", "bc", "cc")

  contact_list
}
