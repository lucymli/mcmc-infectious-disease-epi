###########################################################################
## TASK: Complete the flu_likelihood() function to calculate the probability
## of observing the data D(t) given the expected prevalence I(t) for the
## current parameter values. Assume that D(t) ~ Poisson (I(t)).
## Your answer should look something like sum(dpois(, , log=TRUE))
###########################################################################

library(deSolve)

# Likelihood function -----------------------------------------------------

flu_likelihood <- function (params, data, initial_states) {
  # 1. params: a vector with the values of beta and gamma
  # 2. data: a vector with the number of students in bed on each day from 
  #          Jan 22 to Feb 4, inclusive. D(t)
  # 3. initial_states: a vector containing the initial number of Susceptible, 
  #                    Infected, and Recovered individuals.
  times <- 1:length(data)
  # ode() numerically solves the SIR model differential equations to produce the
  # expected number of infected individuals I(t) ('expected_I') for a set of 
  # parameters and initial conditions.
  expected_I <- ode(initial_states, times, SIR, params)[, 3]
  # COMPLETE LIKELIHOOD CALCULATION BELOW
  log_likelihood <- 
  return (log_likelihood)
}