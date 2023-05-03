get_contact_matrix <- function(country_code, pop_a, pop_b, pop_c, pop_d) {

  polymod       <- socialmixr::polymod
  country_names <- socialmixr::survey_countries(polymod)

  user_country <- countrycode::countrycode(country_code,
                                           origin = "eurostat",
                                           destination = "country.name")

  age_limits <- c(0, 15, 25, 65)

  pop_df <- data.frame(lower.age.limit = age_limits,
                       population = c(pop_a, pop_b, pop_c, pop_d))

  cm_object <- socialmixr::contact_matrix(socialmixr::polymod,
                                          age.limits = age_limits,
                                          countries  = user_country,
                                          survey.pop = pop_df,
                                          symmetric  = TRUE,
                                          quiet      = TRUE)

  contact_matrix <- t(cm_object$matrix)

  contact_list <- as.list(contact_matrix)

  names(contact_list) <- c("aa", "ba", "ca", "da",
                           "ab", "bb", "cb", "db",
                           "ac", "bc", "cc", "dc",
                           "ad", "bd", "cd", "dd")

  contact_list
}
