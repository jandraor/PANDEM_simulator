extract_pars <- function(user_args, model_id) {

  if(model_id == "model_01") {

    expected_pars <- user_pars(model_id)
    par_list      <- user_args[expected_pars] |> sanitise_par_names()

  }

  if(model_id == "model_02") {

    pop_size   <- 1e4

    whp        <- c("S", "E", "I", "R") # Within-host profile
    groups     <- c("A", "B", "C", "D")
    api_stocks <- paste(rep(whp, 4),  rep(groups, each = 4), sep = "_")

    expected_pars <- c(api_stocks, "q", "gamma", "sigma", "cm")
    actual_pars   <- names(user_args)
    vald          <- expected_pars %in% actual_pars # Validation

    if(any(vald == FALSE)) {
      mis_par <- paste(expected_pars[!vald], collapse = ", ") # Missing params
      msg     <- stringr::str_glue("Parameters '{mis_par}' not found")
      stop(msg, call. = FALSE)
    }

    valid_cms <- c("Belgium", "Germany", "Finland", "United Kingdom",
                   "Italy", "Luxembourg", "Netherlands", "Poland")

    cm <- user_args$cm

    if(!cm %in% valid_cms) {
      msg <- stringr::str_glue("Contact matrix for '{cm}' not available")
      stop(msg, call. = FALSE)
    }

    age_limits <- c(0, 5, 15, 45)

    cm_object <- socialmixr::contact_matrix(socialmixr::polymod,
                                            age.limits = age_limits,
                                            countries  = cm,
                                            survey.pop = cm,
                                            symmetric  = TRUE,
                                            quiet      = TRUE)

    pop_df    <- cm_object$demography |>
      dplyr::rename(age_group  = age.group) |>
      dplyr::mutate(age_group  = format_age_group(as.character(age_group)),
                    group_name = groups,
                    population = discrete_separation(proportion, pop_size))

    w_matrix           <- t(cm_object$matrix)
    colnames(w_matrix) <- groups
    rownames(w_matrix) <- groups

    contact_list        <- c(w_matrix) |> as.list()
    combs               <- paste0(rep(groups, each = 4), rep(groups, 4))
    names(contact_list) <- stringr::str_glue("w[{combs}]")

    stock_list <- create_stock_list(user_args, api_stocks, whp, groups, pop_df)

    par_list <- c(stock_list, contact_list)

    par_list$par_gamma <- user_args$gamma
    par_list$par_sigma <- user_args$sigma
    par_list$q         <- user_args$q
  }

  if(model_id == "model_03") par_list <- user_args

  par_list
}

create_stock_list <- function(user_args, api_stocks, whp, groups, pop_df) {

  pct_list <- user_args[api_stocks]

  whp_n    <- length(whp)

  stocks_df <- data.frame(id         = api_stocks,
                          pct        = as.numeric(pct_list),
                          group_name = rep(groups, each = whp_n)) |>
    dplyr::left_join(pop_df[ ,c("group_name", "population")],
                     by = "group_name") |>
    dplyr::group_by(group_name) |>
    dplyr::mutate(pop_by_age = discrete_separation(pct, unique(population)))

  stl_stock_names   <- stocks_df |>
    dplyr::mutate(id = stringr::str_replace(id, "_", "\\["),
                  id = paste0(id, "]"))
  stock_list        <- as.list(stocks_df$pop_by_age)
  names(stock_list) <- stl_stock_names$id

  stock_list
}

format_age_group <- function(ag_vector) {
  pattern <- stringr::regex("\\[(\\d+),(\\d+)\\)")

  new_ag_vector <- vector(length = length(ag_vector), mode = "character")

  for(i in seq_along(ag_vector)) {
    current_ag <- ag_vector[[i]]

    if(stringr::str_detect(current_ag, pattern)) {
      output_sm   <- stringr::str_match(current_ag, pattern)
      lower_bound <- output_sm[[2]] |> stringr::str_pad(width = 2, pad = "0")
      upper_bound <- (as.numeric(output_sm[[3]]) - 1) |>
        stringr::str_pad(width = 2, pad = "0")
      current_ag  <- paste(lower_bound, upper_bound, sep = "-")
    }

    new_ag_vector[[i]] <- current_ag
  }
  new_ag_vector
}

discrete_separation <- function(props, total) {
  vals2 <- round(total * props[-1], 0)
  vals1 <- total - sum(vals2)

  c(vals1, vals2)
}
