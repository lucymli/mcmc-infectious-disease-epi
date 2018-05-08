###########################################################################
## TASK: Complete the flu_priors() function to calculate the prior probability
## of parameters beta and gamma.
## Your answer be typed within the parentheses in return ()
###########################################################################
flu_priors <- function (params) {
  beta_prior <- dunif(params[["beta"]], min=0, max=10, log=TRUE)
  gamma_prior <- dlnorm(1/params[["gamma"]], meanlog=1.8, sdlog=0.5, log=TRUE)
  # COMPLETE THE LINE BELOW
  return (beta_prior+gamma_prior)
}