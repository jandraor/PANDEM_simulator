create_dims_obj <- function(raw_xml) {

  dim_xml       <- xml2::xml_find_first(raw_xml, ".//d1:dimensions")
  dim_elems_xml <- xml2::xml_find_all(dim_xml, ".//d1:dim")

  dim_names <- sapply(dim_elems_xml, function(dim_tag) {
    xml2::xml_attr(dim_tag, "name")
  })

  dims_list        <- lapply(dim_elems_xml, extract_dim_elems)
  names(dims_list) <- dim_names

  dims_list
}

extract_dim_elems <- function(dim_tag) {

  flag_size <- xml2::xml_has_attr(dim_tag, "size")

  if(flag_size) {
    dim_size <- xml2::xml_attr(dim_tag, "size")
    dim_size <- as.numeric(dim_size)
    return(1:dim_size)
  }

  elems_xml <- xml2::xml_find_all(dim_tag, ".//d1:elem")

  sapply(elems_xml, function(elem_tag) xml2::xml_attr(elem_tag, "name"))
}

create_par_obj <- function(module_xml, dims_obj) {

  vars_xml   <- xml2::xml_find_first(module_xml, ".//d1:variables")
  auxs_xml   <- xml2::xml_find_all(vars_xml, ".//d1:aux")

  lapply(auxs_xml, \(aux_obj) {

    aux_name <- xml2::xml_attr(aux_obj, "name")

    dims_aux <- xml2::xml_find_first(aux_obj, ".//d1:dimensions")

    is_arrayed <- ifelse(length(dims_aux) > 0, TRUE, FALSE)

    par_obj <- list(name  = aux_name,
                    array = is_arrayed)

    if(is_arrayed) {

      dim_tags  <- xml2::xml_find_all(dims_aux, ".//d1:dim")
      dim_names <- sapply( dim_tags, \(elem_tag) xml2::xml_attr(elem_tag, "name"))

      dims_list         <- lapply(dim_names, \(dim_name) dims_obj[[dim_name]])
      names(dims_list)  <- dim_names

      par_obj$dimensions <- dims_list
    }

    par_obj
  })
}
