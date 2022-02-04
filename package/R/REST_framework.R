#* @get /sim/<model_id>
#* @post /sim/<model_id>
#* @serializer unboxedJSON
simulator <- function(req, res) {

  user_args <- req$args
  model_id  <- user_args$model_id

  # available models
  avl_mdl <- stringr::str_glue("model_{c('01', '02')}")

  if(!model_id %in% avl_mdl) {

    error_msg  <- stringr::str_glue("Model '{model_id}' not found")
    res$status <- 400

    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status  = 400,
      message = error_msg
    ))

    return(res)
  }

  fldr_path <- create_sim_folder(model_id)
  sens      <- ifelse(is.null(user_args$sens), 0, user_args$sens)


  if(sens == 0) {

    par_list    <- extract_pars(user_args, model_id)
    sim_results <- run_model(model_id, par_list, fldr_path)
    unlink(fldr_path, recursive = TRUE)

    return(sim_results)
  }

  if(sens == 1) {

    expected_pars <- user_pars(model_id)
    user_inputs   <- user_args[expected_pars]
    sep_vals      <- lapply(user_inputs, \(vals) strsplit(vals, ",")[[1]])
    lengths       <- purrr::map_int(sep_vals, \(vals) length(vals))

    if(length(unique(lengths)) != 1) {
      error_msg  <- "Unequal number of values for each parameter"
      res$status <- 400

      res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
        status  = 400,
        message = error_msg
      ))

      return(res)
    }

    n_sims <- lengths[1]

    purrr::map_df(seq_len(n_sims), function(i) {

      iter_vals   <- purrr::map(sep_vals, i)
      par_list    <- extract_pars(iter_vals, model_id)
      par_list    <- sanitise_par_names(par_list)
      sim_results <- run_model(model_id, par_list, fldr_path) |>
        dplyr::mutate(iter = i)

    }) -> sim_results

    unlink(fldr_path, recursive = TRUE)
    return(sim_results)
  }
}
