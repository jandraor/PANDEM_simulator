tidy_results <- function(sim_results, model_id) {

  if(model_id != "model_03") {
    return(sim_results)
  }

  var_names <- colnames(sim_results)

  indicator_list <- get_output_indicators(model_id)

  lapply(indicator_list, function(indicator_obj) {

    var_name <- indicator_obj$name

    data_df <- dplyr::select(sim_results, Days, dplyr::contains(var_name)) |>
      tidyr::pivot_longer(-Days) |>
      dplyr::mutate(name = stringr::str_replace(name, "^.+\\.", ""))

    if(indicator_obj$array) {

      data_df <- data_df |>
        dplyr::mutate(array_info = stringr::str_extract(name, "\\[.+\\]"),
                      name       = stringr::str_replace_all(name, "\\[.+\\]", ""),
                      array_info = stringr::str_replace_all(array_info, "\\[|\\]", "")) |>
        tidyr::separate(array_info, names(indicator_obj$dimensions))

    }

    indicator_obj$sim_results <- data_df

    indicator_obj

  }) -> result_list

  result_list


}
