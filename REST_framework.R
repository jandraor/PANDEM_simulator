#* @get /sim/<model_id>
function(req) {
  
  user_args <- req$args
  model_id  <- user_args$model_id
  
  # available models
  avl_mdl <- stringr::str_glue("model_{c('01', '02')}")
  
  if(!model_id %in% avl_mdl) {
    error_msg <- list(error = stringr::str_glue("Model '{model_id}' not found"))
    return(jsonlite::toJSON(error_msg, auto_unbox = TRUE))
  }
  
  fldr_path <- create_sim_folder(model_id)
  ci   <- ifelse(is.null(user_args$ci), 0, user_args$ci)
  sens <- ifelse(is.null(user_args$sens), 0, user_args$sens)


  if(sens == 0) {
    
    par_list    <- extract_pars(user_args, model_id)
    sim_results <- run_model(model_id, par_list, fldr_path) 
    
    if(ci == 0) {
      unlink(fldr_path, recursive = TRUE)
      return(jsonlite::toJSON(sim_results))
    }
    
    if(ci == 1) {
      quantiles_df <- create_ci_df(sim_results)
      return(jsonlite::toJSON(quantiles_df))
    }
  }
  
  if(sens == 1) {
    
    expected_pars <- user_pars(model_id)
    user_inputs   <- user_args[expected_pars]
    sep_vals      <- lapply(user_inputs, \(vals) strsplit(vals, ",")[[1]])
    lengths       <- purrr::map_int(sep_vals, \(vals) length(vals))        
    
    if(length(unique(lengths)) != 1) {
      error_msg <- list(error = "Unequal number of values for each parameter")
      return(jsonlite::toJSON(error_msg, auto_unbox = TRUE))
    }
    
    n_sims <- lengths[1]
    
    purrr::map_df(seq_len(n_sims), function(i) {
      
      iter_vals   <- purrr::map(sep_vals, i)
      par_list    <- extract_pars(iter_vals, model_id)
      par_list    <- sanitise_par_names(par_list)
      sim_results <- run_model(model_id, par_list, fldr_path) |> 
        dplyr::mutate(iter = i)
      
    }) -> sim_results
    
    if(ci == 0) {
      unlink(fldr_path, recursive = TRUE)
      return(jsonlite::toJSON(sim_results))
    }
    
    if(ci == 1) {
      
      purrr::map_df(1:n_sims, function(i) {
        
        iter_df <- dplyr::filter(sim_results, iter == i)
        create_ci_df(iter_df) |> dplyr::mutate(iter = i)
      }) -> scenarios_ci_df
      
      return(jsonlite::toJSON(scenarios_ci_df))
    }

  }
}

create_ci_df <- function(sim_df) {
  n_meas      <- 100 # number of measurements
  
  dC_vals <- sim_df$dC
  
  purrr::map_df(1:60, function(i) {
    
    measurements <- rpois(n_meas, dC_vals[[i]])
    quantiles    <- quantile(measurements, c(0.025, 0.25, 0.5, 0.75, 0.975))
    
    data.frame(time = i, 
               var  = "dC", 
               q2.5  = quantiles[[1]],
               q25   = quantiles[[2]],
               q50   = quantiles[[3]],
               q75   = quantiles[[4]],
               q97.5 = quantiles[[5]])
  })
}

create_sim_folder <- function(model_id) {
  
  folder_id       <- paste0(sample(c(0:9, LETTERS), 8, T), collapse = '')
  root_folder     <- stringr::str_glue("./{model_id}")
  new_folder_path <- file.path(root_folder, folder_id)
  dir.create(new_folder_path)
  
  stl_file        <- stringr::str_glue("{model_id}.stmx") # Stella file
  src_file        <- stringr::str_glue("./{model_id}/{model_id}.stmx")
  new_file_path   <- file.path(new_folder_path, stl_file)
  file.copy(src_file, new_file_path)
  new_folder_path
  
}

run_model <- function(model_id, par_list, fldr_path) {
  
  input_file <- file.path(fldr_path, "inputs.txt")
  if(file.exists(input_file)) file.remove(input_file)
  
  output_file <- file.path(fldr_path, "output.txt")
  if(file.exists(output_file)) file.remove(output_file)
  file.create(output_file)
  
  inputs     <- as.data.frame(par_list)
  readr::write_tsv(inputs, input_file)
  
  mdl_path <- file.path(fldr_path, paste0(model_id, ".stmx"))
  sys_cmd  <- paste("./stella_simulator", mdl_path)
  system(sys_cmd)
  
  readr::read_tsv(output_file)
}

extract_pars <- function(user_args, model_id) {
  
  if(model_id == "model_01") {
    
    expected_pars <- user_pars(model_id)
    par_list      <- user_args[expected_pars] |> sanitise_par_names()
  
  }
  
  if(model_id == "model_02") {
    
    pop_size   <- 1e4
    
    whp        <- c("S", "E", "I", "R") # Within-host profile
    groups     <- c("A", "B", "C", "D")
    api_stocks <- paste(rep(whp, 4),  rep(groups, each = 4), sep = "_")
    
    expected_pars <- c(api_stocks, "q", "gamma", "sigma", "cm")
    actual_pars   <- names(user_args)
    vald          <- expected_pars %in% actual_pars # Validation
    
    if(any(vald == FALSE)) {
      mis_par <- paste(expected_pars[!vald], collapse = ", ") # Missing params
      msg     <- stringr::str_glue("Parameters '{mis_par}' not found")
      stop(msg, call. = FALSE)
    }
      
    valid_cms <- c("Belgium", "Germany", "Finland", "United Kingdom",
                   "Italy", "Luxembourg", "Netherlands", "Poland")
    
    cm <- user_args$cm
    
    if(!cm %in% valid_cms) {
      msg <- stringr::str_glue("Contact matrix for '{cm}' not available")
      stop(msg, call. = FALSE)
    }
      
    age_limits <- c(0, 5, 15, 45)
    
    cm_object <- socialmixr::contact_matrix(socialmixr::polymod, 
                                            age.limits = age_limits,
                                            countries  = cm,
                                            survey.pop = cm,
                                            symmetric  = TRUE)
    
    pop_df    <- cm_object$demography |> 
      dplyr::rename(age_group  = age.group) |>
      dplyr::mutate(age_group  = format_age_group(as.character(age_group)),
                    group_name = groups,
                    population = discrete_separation(proportion, pop_size))
    
    w_matrix           <- cm_object$matrix
    colnames(w_matrix) <- groups
    rownames(w_matrix) <- groups
    
    contact_list        <- c(w_matrix) |> as.list()
    combs               <- paste0(rep(groups, each = 4), rep(groups, 4))
    names(contact_list) <- stringr::str_glue("w[{combs}]")

    stock_list <- user_args[api_stocks]
    
    stocks_df <- data.frame(id         = api_stocks, 
                            pct        = as.numeric(stock_list),
                            group_name = rep(groups, each = 4)) |> 
      dplyr::left_join(pop_df[ ,c("group_name", "population")]) |>
      dplyr::group_by(group_name) |>
      dplyr::mutate(pop_by_age = discrete_separation(pct, unique(population)))
    
    stl_stock_names   <- stringr::str_glue("{rep(whp, each = 4)}[{rep(groups, 4)}]")
    stock_list        <- as.list(stocks_df$pop_by_age)
    names(stock_list) <- as.character(stl_stock_names)
    
    par_list <- c(stock_list, contact_list)
    
    par_list$par_gamma <- user_args$gamma
    par_list$par_sigma <- user_args$sigma
    par_list$q         <- user_args$q
  }
  
  par_list
}

format_age_group <- function(ag_vector) {
  pattern <- stringr::regex("\\[(\\d+),(\\d+)\\)")
  
  new_ag_vector <- vector(length = length(ag_vector), mode = "character") 
  
  for(i in seq_along(ag_vector)) {
    current_ag <- ag_vector[[i]]
    
    if(stringr::str_detect(current_ag, pattern)) {
      output_sm   <- stringr::str_match(current_ag, pattern)
      lower_bound <- output_sm[[2]] |> stringr::str_pad(width = 2, pad = "0")
      upper_bound <- (as.numeric(output_sm[[3]]) - 1) |> 
        stringr::str_pad(width = 2, pad = "0")
      current_ag  <- paste(lower_bound, upper_bound, sep = "-")
    }
    
    new_ag_vector[[i]] <- current_ag
  }
  new_ag_vector
}

discrete_separation <- function(props, total) {
  vals2 <- round(total * props[-1], 0)
  vals1 <- total - sum(vals2)
  
  c(vals1, vals2)
}

user_pars <- function(model_id) {
  
  if(model_id == "model_01") {
    
    pars <- c("beta", "gamma", "sigma", "S", "E", "I", "R")
  }
  
  if(model_id == "model_02") {
    
    whp        <- c("S", "E", "I", "R") # Within-host profile
    groups     <- c("A", "B", "C", "D")
    api_stocks <- paste(rep(whp, 4),  rep(groups, each = 4), sep = "_")
    pars       <- c(api_stocks, "q", "gamma", "sigma", "cm")
  }
  
  pars
}  

sanitise_par_names <- function(par_list) {
  
  forb_names <- c("sigma", "gamma", "beta")
  
  old_names <- names(par_list)
  
  new_names <- ifelse(old_names %in% forb_names, paste0("par_", old_names), 
                      old_names)
  
  names(par_list) <- new_names
  
  par_list
}
