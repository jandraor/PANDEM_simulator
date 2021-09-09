test_that("multiplication works", {

  cm <- "Netherlands"

  user_args <- list(S_A = 0.99, E_A = 0, I_A = 0.01, R_A = 0,
                    S_B = 0.99, E_B = 0, I_B = 0.01, R_B = 0,
                    S_C = 0.99, E_C = 0, I_C = 0.01, R_C = 0,
                    S_D = 0.99, E_D = 0, I_D = 0.01, R_D = 0,
                    sigma = 0.5, gamma = 0.5, cm = cm,
                    q = 0.1)

  whp        <- c("S", "E", "I", "R") # Within-host profile
  groups     <- c("A", "B", "C", "D")

  api_stocks <- paste(rep(whp, 4),  rep(groups, each = 4), sep = "_")

  age_limits <- c(0, 5, 15, 45)

  cm_object <- socialmixr::contact_matrix(socialmixr::polymod,
                                          age.limits = age_limits,
                                          countries  = cm,
                                          survey.pop = cm,
                                          symmetric  = TRUE,
                                          quiet      = TRUE)
  pop_size <- 1e4

  pop_df    <- cm_object$demography |>
    dplyr::rename(age_group  = age.group) |>
    dplyr::mutate(age_group  = format_age_group(as.character(age_group)),
                  group_name = groups,
                  population = discrete_separation(proportion, pop_size))

  actual_list   <- create_stock_list(user_args, api_stocks, whp, groups, pop_df)

  expected_list <- list(`S[A]` = 603,  `E[A]` = 0, `I[A]` = 6,  `R[A]` = 0,
                        `S[B]` = 1207, `E[B]` = 0, `I[B]` = 12, `R[B]` = 0,
                        `S[C]` = 4087, `E[C]` = 0, `I[C]` = 41, `R[C]` = 0,
                        `S[D]` = 4004, `E[D]` = 0, `I[D]` = 40, `R[D]` = 0)

  expect_equal(actual_list, expected_list)
})
