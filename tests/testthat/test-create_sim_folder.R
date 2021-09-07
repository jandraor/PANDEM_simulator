test_that("create_sim_folder() creates a folder", {
  relative_path   <- create_sim_folder("model_01")
  new_folder     <- file.path(here::here(), relative_path)
  verification    <- file.exists(file.path(new_folder, "model_01.stmx"))
  unlink(new_folder, recursive = TRUE)
  expect_equal(verification, TRUE)
})
