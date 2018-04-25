library(deSolve)

SIR <- function (t, x, params) {
  with (as.list(c(params, x)), {
    N <- S + I + R
    dS <- -beta*S*I
    dI <- +beta*S*I - gamma*I
    dR <- gamma*I
    der <- list(c(dS, dI, dR))
  })
}

flu_likelihood <- function (params, data, initial_states) {
  times <- 1:length(data) # Time vector
  # Numeric solution to the deterministic SIR model
  expected_I <- ode(initial_states, times, SIR, params)[, 3]
  # Calculate the probability of observing data (number in bed) according to a 
  # Poisson distribution
  log_likelihood <- sum(dpois(data, expected_I, log=TRUE))
  return (log_likelihood)
}