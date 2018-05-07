flu_priors <- function (params) {
  beta_prior <- dunif(params[["beta"]], min=0, max=10, log=TRUE)
  gamma_prior <- dlnorm(1/params[["gamma"]], meanlog=1.8, sdlog=0.5, log=TRUE)
  # Complete the following line
  return ()
}