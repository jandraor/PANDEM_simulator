write_input_file <- function(input_file, par_list) {

  inputs           <- as.data.frame(par_list)
  colnames(inputs) <- names(par_list)
  readr::write_tsv(inputs, input_file, escape = "none")

}

run_model <- function(model_id, par_list, fldr_path) {

  input_file <- file.path(fldr_path, "inputs.txt")
  if(file.exists(input_file)) file.remove(input_file)
  write_input_file(input_file, par_list)

  output_file <- file.path(fldr_path, "output.txt")
  if(file.exists(output_file)) file.remove(output_file)
  file.create(output_file)

  mdl_path <- file.path(fldr_path, paste0(model_id, ".stmx"))
  sys_cmd  <- paste("./stella_simulator", mdl_path)
  system(sys_cmd)

  readr::read_tsv(output_file)
}
