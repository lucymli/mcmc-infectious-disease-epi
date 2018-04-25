MHmcmc <- function (data, initial_states, likelihood, priors, mcmc_options, parameters=NULL) {
  # Initialize parameters
  init_params <- parameters
  # Calculate likelihood and prior of initial parameters
  init_llik <- likelihood(init_params, data, initial_states)
  init_prior <- priors(init_params)
  # Initialize log
  max_log_iter <- floor(mcmc_options$niter/mcmc_options$log_every)
  parameter_log <- data.frame(iter=seq(1, by=mcmc_options$log_every, length.out=max_log_iter),
                              posterior=NA, prior=NA, likelihood=NA, acceptance=1,
                              t(replicate(max_log_iter, init_params)))
  parameter_log[1, names(init_params)] <- init_params
  parameter_log[1, c("posterior", "prior", "likelihood")] <- c(init_llik+init_prior, init_prior, init_llik)
  total_accepted <- 0
  for (i in 2:mcmc_options$niter) {
    # Propose new parameters
    new_params <- c(beta=rnorm(1, init_params[["beta"]], 1e-4),
                    gamma=1/rnorm(1, 1/init_params[["gamma"]], 0.1))
    new_llik <- likelihood(new_params, data, initial_states)
    new_prior <- priors(new_params)
    # Accept/reject
    ratio <- new_llik + new_prior - (init_llik + init_prior)
    accepted <- log(runif(1)) < ratio
    if (accepted) {
      init_llik <- new_llik
      init_prior <- new_prior
      init_params <- new_params
      total_accepted <- total_accepted + 1
    }
    # Log sampled parameter values
    if (i%%mcmc_options$log_every==0) {
      log_i <- i/mcmc_options$log_every
      parameter_log[log_i, c("posterior", "prior", "likelihood")] <- 
        c(init_llik + init_prior, init_prior, init_llik)
      parameter_log[log_i, "acceptance"] <- total_accepted/i
      parameter_log[log_i, names(init_params)] <- init_params
      if (mcmc_options$verbose) {
        cat (paste0("Iteration ", i, " of ", mcmc_options$niter, ") ", 
                    scales::percent(total_accepted/i), " acceptance\n"))
      }
    }
  }
  return (parameter_log)
}
