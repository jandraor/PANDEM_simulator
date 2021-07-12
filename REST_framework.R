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
function(beta, sigma, gamma, S0, E0, I0, R0, sens = 0) {
  
  
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
    readr::read_tsv("./model_01/output.txt")
  }
  
  if(sens = 0) {
    sim_results <- run_model(beta, sigma, gamma, S0, E0, I0, R0)
    readr::write_csv(sim_results, "./test_single_run.csv")
    return(jsonlite::toJSON(sim_results))
  }
  
  if(sens == 1) {
    
    beta_vals  <- strsplit(beta, ",")[[1]]
    sigma_vals <- strsplit(sigma, ",")[[1]]
    gamma_vals <- strsplit(gamma, ",")[[1]]
    S_0_vals   <- strsplit(S0, ",")[[1]]
    E_0_vals   <- strsplit(E0, ",")[[1]]
    I_0_vals   <- strsplit(I0, ",")[[1]]
    R_0_vals   <- strsplit(R0, ",")[[1]]
    
    n_sims <- nrow(beta_vals)
    
    purrr::map_df(1:n_sims, function(i) {
      sim_results <- run_model(beta_vals[[i]], sigma[[i]], gamma[[i]], S0[[i]],
                               E0[[i]], I0[[i]], R0[[i]]) |>
        dplyr::mutate(iter = i)
    }) -> sim_results
    
    readr::write_csv(sim_results, "./test_multiple_run.csv")
    jsonlite::toJSON(sim_results)
  }
}

