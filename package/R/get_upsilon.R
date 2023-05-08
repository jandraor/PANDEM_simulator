get_upsilon <- function(R0, country_code, pop_a, pop_b, pop_c, pop_d) {

  upsilon_vals <- seq(0, 1, 0.1)
  R0_vals      <-  purrr::map_dbl(seq(0, 1, 0.1), estimate_R0, country_code,
                                  pop_a, pop_b, pop_c, pop_d)

  df <- data.frame(upsilon = upsilon_vals, R0 = R0_vals)

  model <- lm(upsilon ~ R0, df)

  predict(model, newdata = data.frame(R0 = R0))
}
