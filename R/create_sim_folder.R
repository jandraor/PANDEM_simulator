create_sim_folder <- function(model_id) {

  folder_id       <- paste0(sample(c(0:9, LETTERS), 8, T), collapse = '')
  root_folder     <- file.path(here::here(), model_id)
  new_folder_path <- file.path(root_folder, folder_id)
  dir.create(new_folder_path, recursive = TRUE)

  stl_file        <- stringr::str_glue("{model_id}.stmx") # Stella file
  src_file        <- file.path(root_folder, stl_file)
  new_file_path   <- file.path(new_folder_path, stl_file)
  file.copy(src_file, new_file_path)
  new_folder_path
}
