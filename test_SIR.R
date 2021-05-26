#* @get /sim
function() {
  system("./stella_simulator SIR_Test.stmx")
  sim_results <- readr::read_tsv("./output.txt")
  jsonlite::toJSON(sim_results)
}
