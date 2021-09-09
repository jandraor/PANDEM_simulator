test_that("create_sim_folder() creates a folder", {
  new_folder   <- create_sim_folder("model_01")
  verification    <- file.exists(file.path(new_folder, "model_01.stmx"))
  unlink(new_folder, recursive = TRUE)
  expect_equal(verification, TRUE)
})
