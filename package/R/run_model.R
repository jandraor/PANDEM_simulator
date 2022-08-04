write_input_file <- function(input_file, par_list) {

  inputs           <- as.data.frame(par_list)
  colnames(inputs) <- names(par_list)
  readr::write_tsv(inputs, input_file, escape = "none")

}

run_model <- function(model_id, par_list, fldr_path,
                      output_type = "indicators") {

  input_file <- file.path(fldr_path, "inputs.txt")
  if(file.exists(input_file)) file.remove(input_file)
  write_input_file(input_file, par_list)


  output_file1 <- file.path(fldr_path, "output.txt")
  create_output_file(output_file1)

  output_file2 <- file.path(fldr_path, "inputs_val.txt")
  create_output_file(output_file2)


  mdl_path <- file.path(fldr_path, paste0(model_id, ".stmx"))
  sys_cmd  <- paste("./stella_simulator", mdl_path)
  system(sys_cmd)

  if(output_type == "indicators") return(read_output_file(output_file1))


  if(output_type == "inputs") {

    inputs_df <- read_output_file(output_file2)
    dplyr::slice(inputs_df, 1)
  }
}

create_output_file <- function(output_file) {

  if(file.exists(output_file)) file.remove(output_file)

  file.create(output_file)
}

read_output_file <- function(output_file) {

  df        <- readr::read_tsv(output_file)

  col_names <- colnames(inputs_df) |>
    stringr::str_replace_all(" ", "_")

  colnames(df) <- col_names

  df
}
