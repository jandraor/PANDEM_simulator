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
  
  par_list <- extract_pars(user_args, model_id)
  
  ci <- ifelse(is.null(user_args$ci), 0, user_args$ci)
  sens <- ifelse(is.null(user_args$sens), 0, user_args$sens)


  if(sens == 0) {
    
    sim_results <- run_model(par_list, fldr_path) 
    
    if(ci == 0) {
      return(jsonlite::toJSON(sim_results))
    }
    
    if(ci == 1) {
      quantiles_df <- create_ci_df(sim_results)
      return(jsonlite::toJSON(quantiles_df))
    }
  }
  
  if(sens == 1) {
    
    beta_vals  <- strsplit(beta, ",")[[1]]
    sigma_vals <- strsplit(sigma, ",")[[1]]
    gamma_vals <- strsplit(gamma, ",")[[1]]
    S_0_vals   <- strsplit(S0, ",")[[1]]
    E_0_vals   <- strsplit(E0, ",")[[1]]
    I_0_vals   <- strsplit(I0, ",")[[1]]
    R_0_vals   <- strsplit(R0, ",")[[1]]
    
    beta_length  <- length(beta_vals)
    sigma_length <- length(sigma_vals)
    gamma_length <- length(gamma_vals)
    S_length     <- length(S_0_vals)
    E_length     <- length(E_0_vals)
    I_length     <- length(I_0_vals)
    R_length     <- length(R_0_vals)
    
    lengths <- c(beta_length, sigma_length, gamma_length, S_length,
                 E_length, I_length, R_length)
    
    if(length(unique(lengths)) != 1) {
      error_msg <- list(error = "Unequal number of values for each parameter")
      return(jsonlite::toJSON(error_msg, auto_unbox = TRUE))
    }
    
    n_sims <- beta_length
    
    purrr::map_df(1:n_sims, function(i) {
      
      par_list <- list(par_beta  = beta_vals[[i]],
                       par_gamma = gamma_vals[[i]],
                       par_sigma = sigma_vals[[i]],
                       S         = S_0_vals[[i]],
                       E         = E_0_vals[[i]],
                       I         = I_0_vals[[i]],
                       R         = R_0_vals[[i]])
      
      sim_results <- run_model(par_list, fldr_path) |> dplyr::mutate(iter = i)
    }) -> sim_results
    
    if(ci == 0) {
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

run_model <- function(par_list, fldr_path) {
  
  input_file <- file.path(fldr_path, "inputs.txt")
  if(file.exists(input_file)) file.remove(input_file)
  
  output_file <- file.path(fldr_path, "output.txt")
  if(file.exists(output_file)) file.remove(output_file)
  file.create(output_file)
  
  inputs     <- as.data.frame(par_list)
  readr::write_tsv(inputs, input_file)
  
  mdl_path <- file.path(fldr_path, "model_01.stmx")
  sys_cmd  <- paste("./stella_simulator", mdl_path)
  system(sys_cmd)
  
  readr::read_tsv(output_file) |>
    dplyr::mutate(dC = C - dplyr::lag(C)) |>
    dplyr::filter(Day != 0)
}

extract_pars <- function(user_args, model_id) {
  
  if(model_id == "model_01") {
    
    par_list <- list(par_beta  = user_args$beta,
                     par_gamma = user_args$gamma,
                     par_sigma = user_args$sigma,
                     gamma     = user_args$gamma,
                     S         = user_args$S0,
                     E         = user_args$E0,
                     I         = user_args$I0,
                     R         = user_args$R0)
  }
  
  par_list
}

