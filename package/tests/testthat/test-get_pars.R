test_that("get_pars() returns objects with the expected names", {

  actual   <- get_pars("model_03")[[1]] |> names()

  expected <- c("name", "category", "description", "array", "dimensions",
                "default_values")

  expect_equal(actual, expected)
})
