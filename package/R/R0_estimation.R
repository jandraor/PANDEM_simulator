# par_upsilon Probability of infection
estimate_R0 <- function(par_upsilon, country_code, pop_a, pop_b, pop_c, pop_d) {

  contacts       <- get_contact_matrix(country_code, pop_a, pop_b, pop_c, pop_d)
  contact_matrix <- matrix(as.numeric(contacts), nrow = 4)
  beta_matrix    <- contact_matrix  * par_upsilon

  par_eta        <- 0.5
  par_sigma      <- 0.5
  par_gamma      <- 0.5
  par_omega      <- 0.84
  par_nu         <- 1 # Rate at which pre-clinical individuals develop symptoms.
  par_xi         <- 1/3

  n_col    <- 16
  F_matrix <- matrix(rep(0, n_col ** 2), ncol = n_col)
  V_matrix <- matrix(rep(0, n_col ** 2), ncol = n_col)

  for(i in 1:4) {

    # New infections from pre-clinical/clinical infectious individuals
    F_matrix[4 * (i - 1) + 1, c(2, 3)]   <- beta_matrix[i, 1]
    F_matrix[4 * (i - 1) + 1, c(6, 7)]   <- beta_matrix[i, 2]
    F_matrix[4 * (i - 1) + 1, c(10, 11)] <- beta_matrix[i, 3]
    F_matrix[4 * (i - 1) + 1, c(14, 15)] <- beta_matrix[i, 4]

    # New infections caused by asymptomatic individuals
    F_matrix[4 * (i - 1) + 1, c(4, 8, 12, 16)] <- par_eta * beta_matrix[i, 1:4]

    V_matrix[4 * (i - 1) + 1, 4 * (i - 1) + 1]   <- par_sigma
    V_matrix[4 * (i - 1) + 2, 4 * (i - 1) + 1]   <- - par_omega * par_sigma
    V_matrix[4 * (i - 1) + 2, 4 * (i - 1) + 2]   <- par_nu
    V_matrix[4 * (i - 1) + 3, 4 * (i - 1) + 2]   <- - par_nu
    V_matrix[4 * (i - 1) + 3, 4 * (i - 1) + 3]   <- par_gamma
    V_matrix[4 * i          , 4 * (i - 1) + 1]   <- (-1 + par_omega) * par_sigma
    V_matrix[4 * i, 4 * i]                       <- par_xi
  }

  V_inverse <- solve(V_matrix)

  NGM <- F_matrix %*% V_inverse

  eigensystem <- eigen(NGM)
  eigenvalues <- eigensystem$values

  spectral_radius <- max(eigenvalues)

  spectral_radius
}
