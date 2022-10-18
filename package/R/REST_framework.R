#* @get /sim/<model_id>
#* @post /sim/<model_id>
#* @serializer unboxedJSON
simulator <- function(req, res) {

  user_args <- req$args
  model_id  <- user_args$model_id

  # available models
  avl_mdl <- stringr::str_glue("model_{c('01', '02', '03', '05')}")

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
    sim_results <- tidy_results(sim_results, model_id)
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

#* Download the Stella model file (.stmx), which is in XMILE (XML) format
#* @get /model/<model_id>
#* @serializer contentType list(type='xml')
# Author: Caroline Green, NUIG, 10th March 2022
# Purpose: Allow download of .stmx file, if found
# Assumption: Model file is contained within a subfolder of the same name, eg model_01/model_01.stmx
# Dependencies: The R packages plumber, readr and stringr are needed and are assumed to be loaded already
function(req, res) {

  user_args <- req$args
  model_id  <- user_args$model_id

  # Filename sent as parameter should not include file extension - search for a '.'
  if (!stringr::str_detect(model_id, "\\.")) {
    # Construct filename to include folder name and .stmx file extension
    filename <- paste0(model_id, "/", model_id, ".stmx")
    print(paste("Request to get model file", filename))
    if(file.exists(filename)){
      print(paste("File", filename, "found"))
      # Read whole file
      model_file_contents <- readr::read_file(filename)
      # Return the file as a downloadable attachment
      plumber::as_attachment(model_file_contents, filename)
    } else {
      # Return error (file not found)
      res$status = 404
      print(paste("File", filename, "not found"))
    }
  } else {
    # Return error (invalid filename)
    res$status = 404
    print("Filename entered contains a full stop. Enter file name without a file extension!")
  }
}

#* @get /parameters/<model_id>
#* @serializer unboxedJSON
function(req, res) {

  user_args <- req$args
  model_id  <- user_args$model_id

  # available models
  avl_mdl <- stringr::str_glue("model_{c('03', '05')}")

  if(!model_id %in% avl_mdl) {

    error_msg  <- stringr::str_glue("Model '{model_id}' not found")
    res$status <- 400

    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status  = 400,
      message = error_msg
    ))

    return(res)
  }

  get_pars(model_id)
}


#* @post /test_inputs/<model_id>
#* @serializer unboxedJSON
function(req, res) {

  user_args <- req$args
  model_id  <- user_args$model_id

  # available models
  avl_mdl <- stringr::str_glue("model_{c('05')}")

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

  par_list    <- extract_pars(user_args, model_id)
  sim_results <- run_model(model_id, par_list, fldr_path, "inputs")

  sim_results
}

#* @get /contacts
#* @serializer unboxedJSON
function(req, res) {

  user_args      <- req$args
  country_code   <- user_args$country_code
  pop_a          <- as.numeric(user_args$pop_a)
  pop_b          <- as.numeric(user_args$pop_b)
  pop_c          <- as.numeric(user_args$pop_c)
  contact_matrix <- get_contact_matrix(country_code, pop_a, pop_b, pop_c)

  if(class(contact_matrix) == "character") {

    error_msg  <- contact_matrix
    res$status <- 400

    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status  = 400,
      message = error_msg
    ))

    return(res)
  }

  contact_matrix
}

#* @get /R0
#* @serializer unboxedJSON
function(req, res) {

  user_args      <- req$args
  country_code   <- user_args$country_code
  prob_inf       <- as.numeric(user_args$prob_inf)
  pop_a          <- as.numeric(user_args$pop_a)
  pop_b          <- as.numeric(user_args$pop_b)
  pop_c          <- as.numeric(user_args$pop_c)

  R0_val <- estimate_R0(par_upsilon, country_code, pop_a, pop_b, pop_c)

  list(R0 = R0_val)
}

