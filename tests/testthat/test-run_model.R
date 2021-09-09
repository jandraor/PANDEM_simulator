test_that("multiplication works", {
  fldr_path <- "./model_02/test_folder"
  model_id  <- "model_02"
  user_args <- list(S_A = 0.99, E_A = 0, I_A = 0.01, R_A = 0,
                    S_B = 0.99, E_B = 0, I_B = 0.01, R_B = 0,
                    S_C = 0.99, E_C = 0, I_C = 0.01, R_C = 0,
                    S_D = 0.99, E_D = 0, I_D = 0.01, R_D = 0,
                    sigma = 0.5, gamma = 0.5, cm = "Netherlands",
                    q = 0.1)

  par_list <- extract_pars(user_args, model_id)

  input_file <- file.path(fldr_path, "inputs.txt")

  write_input_file(input_file, par_list)

  input_df <- readr::read_tsv(input_file)

  expect_equal(colnames(input_df), names(par_list))
})
