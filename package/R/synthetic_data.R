generate_synthetic_data <- function(filepath, country) {


  indicator_list <- list(
    list(col_pattern   = "Total confirmed cases",
         name          = "Cases",
         concentration = 5),
    list(col_pattern   = "Total Admissions",
         name          = "Hospitalisations",
         concentration = 15),
    list(col_pattern   = "total deaths",
         name          = "Deaths",
         concentration = 15))

  set.seed(123)

  output_df <- readr::read_tsv(filepath)

  purrr::map_df(indicator_list, \(indicator_obj) {

    col_pattern <- indicator_obj$col_pattern

    indicator_df <- dplyr::select(output_df, Days,
                              dplyr::contains(col_pattern))

    colnames(indicator_df) <- c("Time", paste(indicator_obj$name,
                                           LETTERS[1:3], sep = "_"))

    conc <- indicator_obj$concentration

    tidy_df <- indicator_df|> tidyr::pivot_longer(-Time) |>
      dplyr::group_by(name) |>
      dplyr::mutate(model_val = value - dplyr::lag(value, default = NA)) |>
      dplyr::filter(!is.na(model_val)) |>
      dplyr::ungroup() |>
      tidyr::separate(name, c("Var", "Age"),sep = "_") |>
      dplyr::select(-value) |>
      dplyr::group_by(Age) |>
      dplyr::mutate(synthetic_val = rnbinom(dplyr::n(),
                                                  mu = model_val,
                                                  size = conc)) |>
      dplyr::ungroup() |> dplyr::select(-Var) |>
      dplyr::mutate(indicator = indicator_obj$name,
             country   = country)
  }) ->  synthetic_data

  readr::write_csv(synthetic_data, paste0("../Synthetic_data/", country,".csv"))
}
