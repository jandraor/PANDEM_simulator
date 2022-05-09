get_pars <- function(model_id) {

  file_name <- stringr::str_glue("./{model_id}/{model_id}.stmx")
  raw_xml   <- xml2::read_xml(file_name)
  dims_obj  <- create_dims_obj(raw_xml)

  mdl_xml   <- xml2::xml_find_all(raw_xml, ".//d1:model")
  mdl_names <- sapply(mdl_xml, \(elem_tag) xml2::xml_attr(elem_tag, "name"))

  input_idx     <- which(grepl("^Inputs_", mdl_names))
  input_modules <- mdl_xml[input_idx]

  par_list <- lapply(input_modules, create_par_obj, dims_obj)

  unlist(par_list, recursive = FALSE)
}

get_output_indicators <- function(model_id) {

  file_name <- stringr::str_glue("./{model_id}/{model_id}.stmx")
  raw_xml   <- xml2::read_xml(file_name)
  dims_obj  <- create_dims_obj(raw_xml)

  mdl_xml   <- xml2::xml_find_all(raw_xml, ".//d1:model")
  mdl_names <- sapply(mdl_xml, \(elem_tag) xml2::xml_attr(elem_tag, "name"))

  output_idx     <- which(grepl("PANDEM2_Outputs", mdl_names))
  output_module  <- mdl_xml[output_idx]

  create_par_obj(output_module, dims_obj)
}







