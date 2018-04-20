library(outbreaks)
library(deSolve)
library(ggplot2)

# Load data ---------------------------------------------------------------

data("influenza_england_1978_school", package="outbreaks")
data("ebola_kikwit_1995", package="outbreaks")


# MCMC function -----------------------------------------------------------

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


# SIR model ---------------------------------------------------------------

SIR <- function (t, x, params) {
  with (as.list(c(params, x)), {
    N <- S + I + R
    dS <- -beta*S*I
    dI <- +beta*S*I - gamma*I
    dR <- gamma*I
    der <- list(c(dS, dI, dR))
  })
}

# flu data analysis -------------------------------------------------------

flu_parameters <- c(beta=0.5/760, gamma=1/7)
flu_data <- influenza_england_1978_school$in_bed
flu_initial_states <- c(S=763-3, I=3, R=0)
flu_simulate <- function (params, initial_states) {
  data.frame(ode(initial_states, 1:length(flu_data), SIR, params))
}
flu_likelihood <- function (params, data, initial_states) {
  times <- 1:length(data)
  expected_I <- ode(initial_states, times, SIR, params)
  log_likelihood <- sum(dpois(data, expected_I[, 3], log=TRUE))
  return (log_likelihood)
}
flu_priors <- function (params) {
  beta_prior_params <- c(0, 10)
  gamma_prior_params <- c(1.8, 0.5)
  beta_prior <- dunif(params[["beta"]], min=0, max=10, log=TRUE)
  gamma_prior <- dlnorm(1/params[["gamma"]], meanlog=1.8, sdlog=0.5, log=TRUE)
  return (beta_prior+gamma_prior)
}
flu_mcmc_options <- list(niter=10000, log_every=1, verbose=TRUE)
flu_mcmc_output <- MHmcmc(flu_data, flu_initial_states, flu_likelihood, flu_priors, flu_mcmc_options, parameters=flu_parameters)

save.image("../02WorkingExample/initial.RData")

write.table(flu_mcmc_output, "flu_mcmc_output.txt")

# flu analysis plots ------------------------------------------------------

flu_inferred_final <- flu_simulate(unlist(flu_mcmc_output[nrow(flu_mcmc_output), c("beta", "gamma")]), 
                                   initial_states=flu_initial_states)

flu_trace_plot <- 
  ggplot(flu_mcmc_output[-1:-1000, ], aes(x=iter)) + theme_classic () +
  geom_line(aes(y=beta/gamma*flu_initial_states[["S"]])) +
  xlab("Iteration") +
  ylab(bquote(R[0]))
flu_trace_plot
