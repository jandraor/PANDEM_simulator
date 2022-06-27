test_that("tidy_results() returns objects with the expected names", {

  sim_results <- readr::read_tsv("./model_03/output.txt",
                                 show_col_types = FALSE)
  model_id    <- "model_03"
  actual      <- tidy_results(sim_results, model_id)[[1]] |> names()
  expected    <- c("name", "category", "description", "array", "dimensions",
                   "sim_results")

  expect_equal(actual, expected)
})
