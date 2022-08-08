tidy_results <- function(sim_results, model_id) {

  if(!model_id %in% c("model_03", "model_05")) {
    return(sim_results)
  }

  colnames(sim_results) <- sanitise_name(colnames(sim_results))

  var_names <- colnames(sim_results)

  indicator_list <- get_output_indicators(model_id)

  lapply(indicator_list, function(indicator_obj) {

    var_name <- indicator_obj$name

    data_df <- extract_indicator(sim_results, var_name, indicator_obj$array)

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

extract_indicator <- function(sim_results, var_name, is_array) {

  if(!is_array) regex <- stringr::str_glue("^.+\\.{var_name}$")

  if(is_array) regex <- stringr::str_glue("^.+\\.{var_name}\\[.+\\]")

  dplyr::select(sim_results, Days, dplyr::matches(regex)) |>
    tidyr::pivot_longer(-Days) |>
    dplyr::mutate(name = stringr::str_replace(name, "^.+\\.", ""))

}
