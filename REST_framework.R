#* @get /sim
function() {
  system("./stella_simulator SIR_Test.stmx")
  sim_results <- readr::read_tsv("./output.txt")
  jsonlite::toJSON(sim_results)
}

#* @get /model_01
function() {
  library(readr)
  
  beta_val  <- 1
  gamma_val <- 0.5
  sigma_val <- 0.5
  S_0_val   <- 999
  E_0_val   <- 0
  I_0_val   <- 1
  R_0_val   <- 0
  
  inputs <- data.frame(par_beta  = beta_val,
                       par_gamma = gamma_val,
                       par_sigma = sigma_val,
                       S         = S_0_val,
                       E         = E_0_val,
                       I         = I_0_val,
                       R         = R_0_val)
  
  write_tsv(inputs, "./model_01/inputs.txt")
  
  system("./stella_simulator ./model_01/model_01.stmx")
  sim_results <- readr::read_tsv("./model_01/output.txt")
  jsonlite::toJSON(sim_results)
}

