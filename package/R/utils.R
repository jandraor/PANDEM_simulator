user_pars <- function(model_id) {

  if(model_id == "model_01") {

    pars <- c("beta", "gamma", "sigma", "S", "E", "I", "R")
  }

  if(model_id == "model_02") {

    whp        <- c("S", "E", "I", "R") # Within-host profile
    groups     <- c("A", "B", "C", "D")
    api_stocks <- paste(rep(whp, 4),  rep(groups, each = 4), sep = "_")
    pars       <- c(api_stocks, "q", "gamma", "sigma", "cm")
  }

  pars
}

sanitise_par_names <- function(par_list) {

  forb_names <- c("sigma", "gamma", "beta")

  old_names <- names(par_list)

  new_names <- ifelse(old_names %in% forb_names, paste0("par_", old_names),
                      old_names)

  names(par_list) <- new_names

  par_list
}
