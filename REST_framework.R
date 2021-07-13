#* @get /sim
function() {
  system("./stella_simulator SIR_Test.stmx")
  sim_results <- readr::read_tsv("./output.txt")
  jsonlite::toJSON(sim_results)
}

#* @param beta Effective contact rate
#* @param sigma rate of onset of infectiousness
#* @param gamma Recovery rate
#* @param S0 Initial susceptible
#* @param E0 Initial exposed
#* @param I0 Initial infectious
#* @param R0 Initial recovered
#* @get /model_01
function(beta, sigma, gamma, S0, E0, I0, R0, sens = 0, ci = 0) {
  
  run_model <- function(beta, sigma, gamma, S0, E0, I0, R0) {
    
    input_file <- "./model_01/inputs.txt"
    
    if(file.exists(input_file)) file.remove(input_file)
    
    beta_val  <- beta
    sigma_val <- sigma
    gamma_val <- gamma
    S_0_val   <- S0
    E_0_val   <- E0
    I_0_val   <- I0
    R_0_val   <- R0
    
    inputs <- data.frame(par_beta  = beta_val,
                         par_gamma = gamma_val,
                         par_sigma = sigma_val,
                         S         = S_0_val,
                         E         = E_0_val,
                         I         = I_0_val,
                         R         = R_0_val)
    
    readr::write_tsv(inputs, input_file)
    
    system("./stella_simulator ./model_01/model_01.stmx")
    
    readr::read_tsv("./model_01/output.txt") |>
      dplyr::mutate(dC = C - dplyr::lag(C)) |>
      dplyr::filter(Day != 0)
  }
  
  if(sens == 0) {
    
    sim_results <- run_model(beta, sigma, gamma, S0, E0, I0, R0) 
    
    if(ci == 0) {
      readr::write_csv(sim_results, "./test_single_run.csv")  
      return(jsonlite::toJSON(sim_results))
    }
    
    if(ci == 1) {
      quantiles_df <- create_ci_df(sim_results)
      readr::write_csv(quantiles_df, "./test_single_run_ci.csv")
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
    I_0_vals     <- length(I_0_vals)
    R_0_vals     <- length(R_0_vals)
    
    lengths <- c(beta_length, sigma_length, gamma_length, S_length,
                 E_length, I_0_vals, R_0_vals )
    
    if(length(unique(lengths)) != 1) {
      error_msg <- list(error = "Unequal number of values for each parameter")
      return(jsonlite::toJSON(error_msg, auto_unbox = TRUE))
    }
    
    n_sims <- beta_length
    
    purrr::map_df(1:n_sims, function(i) {
      sim_results <- run_model(beta_vals[[i]], sigma_vals[[i]], gamma_vals[[i]],
                               S_0_vals[[i]], E_0_vals[[i]], I_0_vals[[i]], 
                               R_0_vals[[i]]) |>
        dplyr::mutate(iter = i)
    }) -> sim_results
    
    if(ci == 0) {
      readr::write_csv(sim_results, "./test_multiple_run.csv")
      return(jsonlite::toJSON(sim_results))
    }
    
    if(ci == 1) {
      
      purrr::map_df(1:sims, function(i) {
        
        iter_df <- dplyr::filter(sim_results, iter == i)
        create_ci_df(iter_df) |> mutate(iter == i)
      }) -> scenarios_ci_df
      
      readr::write_csv(sim_results, "./test_multiple_run_ci.csv")
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

