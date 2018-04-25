flu_priors <- function (params) {
  beta_prior_params <- c(0, 10)
  gamma_prior_params <- c(1.8, 0.5)
  beta_prior <- dunif(params[["beta"]], min=0, max=10, log=TRUE)
  gamma_prior <- dlnorm(1/params[["gamma"]], meanlog=1.8, sdlog=0.5, log=TRUE)
  return (beta_prior+gamma_prior)
}