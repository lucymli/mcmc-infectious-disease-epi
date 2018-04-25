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
  times <- 1:length(data)
  expected_I <- ode(initial_states, times, SIR, params)[, 3]
  # Write your code here
  
  return (log_likelihood)
}