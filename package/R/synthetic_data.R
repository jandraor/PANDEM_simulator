generate_synthetic_data <- function(filepath) {

  set.seed(123)

  df                 <- readr::read_tsv(filepath)

  cases_df           <- dplyr::select(df, Days, dplyr::contains("Total cases"))

  colnames(cases_df) <- c("Time", paste0("Cases_", LETTERS[1:3]))


  tidy_cases <- cases_df |> tidyr::pivot_longer(-Time) |>
    dplyr::group_by(name) |>
    dplyr::mutate(Model_incidence = value - dplyr::lag(value, default = NA)) |>
    dplyr::filter(!is.na(Model_incidence)) |>
    dplyr::ungroup() |>
    tidyr::separate(name, c("Var", "Age"),sep = "_") |>
    dplyr::select(-value) |>
    dplyr::group_by(Age) |>
    dplyr::mutate(Synthetic_incidence = rnbinom(dplyr::n(),
                                                mu = Model_incidence,
                                                size = 10)) |>
    dplyr::ungroup() |> dplyr::select(-Var)


  synthetic_cases <- tidyr::pivot_wider(tidy_cases,
                                        values_from = c(Synthetic_incidence,
                                                        Model_incidence),
                                        names_from = Age)

  hosp_df <- dplyr::select(df, Days, dplyr::contains("Total Admissions"))

  colnames(hosp_df) <- c("Time", paste0("Hospitalisations_", LETTERS[1:3]))

  tidy_hosp <- hosp_df |> tidyr::pivot_longer(-Time) |>
    dplyr::group_by(name) |>
    dplyr::mutate(Model_hospitalisation = value - dplyr::lag(value, default = NA)) |>
    dplyr::filter(!is.na(Model_hospitalisation)) |>
    dplyr::ungroup() |>
    tidyr::separate(name, c("Var", "Age"),sep = "_") |>
    dplyr::select(-value) |>
    dplyr::group_by(Age) |>
    dplyr::mutate(Synthetic_hospitalisation = rnbinom(dplyr::n(),
                                                mu = Model_hospitalisation,
                                                size = Inf)) |>
    dplyr::ungroup() |> dplyr::select(-Var)

  synthetic_hospitalisation <- tidyr::pivot_wider(
    tidy_hosp,
    values_from = c(Synthetic_hospitalisation, Model_hospitalisation),
    names_from = Age)

  dplyr::bind_cols(synthetic_cases,
                   dplyr::select(synthetic_hospitalisation, - Time))


}
