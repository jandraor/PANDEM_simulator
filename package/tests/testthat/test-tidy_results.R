test_that("tidy_results() returns objects with the expected names", {

  sim_results <- readr::read_tsv("./model_03/output.txt",
                                 show_col_types = FALSE)
  model_id    <- "model_03"
  actual      <- tidy_results(sim_results, model_id)[[1]] |> names()
  expected    <- c("name", "category", "description", "array", "dimensions",
                   "sim_results")

  expect_equal(actual, expected)
})

test_that("extract_indicator() returns the expected data frame", {

  sim_results <- data.frame(
    Days = 0,
    Hospital_Resources.ICU_Beds = 0,
    Hospital_Resources.occupied_ICU_beds = 0,
    Hospital.physical_ICU_beds_available = 0,
    Hospital.expected_ICU_beds_freed = 0,
    Hospital.physical_ICU_beds_needed = 0,
    Hospital.physical_ICU_beds_gap = 0,
    Hospital_Resources.staffed_equipped_ICU_beds_available = 0,
    Hospital_Resources.staffed_equipped_ICU_beds_needed = 0,
    Hospital_Resources.staffed_equipped_ICU_beds_gap = 0,
    Hospital.peak_demand_ICU_beds = 0)

  var_name <- "ICU_beds"

  actual   <- extract_indicator(sim_results, var_name, FALSE)

  expected <- data.frame(Days = 0, name = "ICU_Beds", value = 0)

  expect_equal(as.data.frame(actual), expected)
})

test_that("extract_indicator() returns the expected data frame for an array", {

  sim_results <- data.frame(0, 0, 0)

  colnames(sim_results) <- c("Days", "Hospital.ICU admissions[y]",
                             "Hospital.ICU admissions[o]")

  var_name <- "ICU admissions"

  actual   <- extract_indicator(sim_results, var_name, TRUE)

  expected <- data.frame(Days = c(0, 0),
                         name = c("ICU admissions[y]", "ICU admissions[o]"),
                         value = c(0, 0))

  expect_equal(as.data.frame(actual), expected)
})

test_that("extract_indicator() returns the expected data frame when the var name is also prefix", {

  sim_results <- data.frame(0, 0, 0, 0)

  colnames(sim_results) <- c("Days", "ctg.PPE", "ctg.PPE_needed", "ctg.PPE_gap")

  var_name <- "PPE"
  actual   <- extract_indicator(sim_results, var_name, FALSE)

  expected <- data.frame(Days = 0, name = "PPE", value = 0)

  expect_equal(as.data.frame(actual), expected)
})
