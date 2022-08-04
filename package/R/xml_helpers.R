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

create_par_obj <- function(module_xml, dims_obj, type) {

  module_name <- xml2::xml_attr(module_xml, "name")
  category    <- module_name

  vars_xml   <- xml2::xml_find_first(module_xml, ".//d1:variables")
  auxs_xml   <- xml2::xml_find_all(vars_xml, ".//d1:aux")

  lapply(auxs_xml, format_par_obj, module_name, dims_obj, type)
}

format_par_obj <- function(aux_obj, category, dims_obj, type) {

  aux_name   <- xml2::xml_attr(aux_obj, "name")
  name       <- sanitise_name(aux_name)

  dims_aux   <- xml2::xml_find_first(aux_obj, ".//d1:dimensions")

  is_arrayed <- ifelse(length(dims_aux) > 0, TRUE, FALSE)

  desc_txt  <- xml2::xml_find_first(aux_obj, ".//d1:doc") |>
    xml2::xml_text()

  par_obj <- list(name         = name,
                  category     = category,
                  description  = desc_txt,
                  array        = is_arrayed)

  if(!is_arrayed & type == "input") {

    def_val <- xml2::xml_find_first(aux_obj, ".//d1:eqn") |>
      xml2::xml_text() |> as.numeric()

    dv_list         <- list(def_val)

    names(dv_list)  <- stringr::str_glue("{category}.{aux_name}") |>
      stringr::str_replace_all(" ", "_")

    par_obj$default_values <- dv_list
  }

  if(is_arrayed) {

    dim_tags  <- xml2::xml_find_all(dims_aux, ".//d1:dim")
    dim_names <- sapply( dim_tags, \(elem_tag) xml2::xml_attr(elem_tag, "name"))

    dims_list         <- lapply(dim_names, \(dim_name) dims_obj[[dim_name]])
    names(dims_list)   <- dim_names

    par_obj$dimensions <- dims_list

    if(type == "input") {

      cld_xml      <- xml2::xml_children(aux_obj)
      child_names  <- xml2::xml_name(cld_xml)

      if("eqn" %in% child_names) {

        par_val <- xml2::xml_find_first(aux_obj, ".//d1:eqn") |>
          xml2::xml_text() |> as.numeric()

        combs <- purrr::cross(dims_list)

        elems <- purrr::map_chr(combs, \(elem_obj) paste(elem_obj, collapse = ","))

        elems_list <- rep(par_val, length(elems)) |> as.list()
      }

      if(!("eqn" %in% child_names)) {

        elements_xml <- xml2::xml_find_all(aux_obj, ".//d1:element")

        elems <- xml2::xml_attr(elements_xml, "subscript") |>
          stringr::str_remove(" ")

        elems_list <- xml2::xml_find_all(elements_xml, ".//d1:eqn") |>
          xml2::xml_text() |> as.numeric() |> as.list()

      }

      labels            <- stringr::str_glue("{category}.{name}[{elems}]")
      names(elems_list) <- stringr::str_replace_all(labels, " ", "_")

      par_obj$default_values <- elems_list
    }
  }

  par_obj
}


sanitise_name <- function(name) {
  stringr::str_replace_all(name, " ", "_")
}
