library(deSolve)
library(ggplot2)

# Load data ---------------------------------------------------------------

influenza_england_1978_school <- 
  structure(list(date = structure(c(2943, 2944, 2945, 2946, 2947, 2948, 2949, 2950, 2951, 2952, 2953, 2954, 2955, 2956), class = "Date"), 
                 in_bed = c(3L, 8L, 26L, 76L, 225L, 298L, 258L, 233L, 189L, 128L, 68L, 29L, 14L, 4L),
                 convalescent = c(0L, 0L, 0L, 0L, 9L, 17L, 105L, 162L, 176L, 166L, 150L, 85L, 47L, 20L)), 
            .Names = c("date", "in_bed", "convalescent"), row.names = c(NA, -14L), class = "data.frame")
source("scripts/MHmcmc.R")
source("scripts/likelihood-correct.R")
source("scripts/prior.R")

# SIR model ---------------------------------------------------------------

SIR <- function (t, x, params) {
  # Used with the ode() function to simulate epidemics according to the
  # deterministic SIR model
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


flu_mcmc_options <- list(niter=10000, log_every=1, verbose=TRUE)

set.seed(8373729)
flu_mcmc_output <- MHmcmc(flu_data, flu_initial_states, flu_likelihood, flu_priors, flu_mcmc_options, parameters=flu_parameters)

flu_parameters2 <- flu_parameters * 0.5
flu_mcmc_output2 <- MHmcmc(flu_data, flu_initial_states, flu_likelihood, flu_priors, flu_mcmc_options, parameters=flu_parameters)

save.image("initial.RData")

write.table(flu_mcmc_output, "flu_mcmc_output.txt")

# flu analysis plots ------------------------------------------------------

flu_trace_plot <- 
  ggplot(flu_mcmc_output[-1:-1000, ], aes(x=iter)) + theme_classic () +
  geom_line(aes(y=beta/gamma*flu_initial_states[["S"]])) +
  xlab("Iteration") +
  ylab(bquote(R[0]))

flu_posterior_plot <- 
  ggplot(flu_mcmc_output[-1:-1000, ], aes(x=beta/gamma*flu_initial_states[1])) + theme_classic() +
  geom_histogram() +
  xlab(bquote(R[0])) +
  ylab("Density")

flu_posterior_trajectory <- 
  do.call(rbind, lapply(round(seq(1001, nrow(flu_mcmc_output), length.out=100)), function (i) {
  data.frame(Date=influenza_england_1978_school$date,
             Infected=flu_simulate(unlist(flu_mcmc_output[i, c("beta", "gamma")]), flu_initial_states)[, 3],
             rep=i)
}))

flu_posterior_trajectory_plot <- 
  ggplot(flu_posterior_trajectory, aes(x=Date, y=Infected)) + 
  theme_classic() +
  stat_summary(geom="pointrange", fun.y=mean, fun.ymin=min, fun.ymax=max, size=0.5) +
  geom_point(data=influenza_england_1978_school, aes(x=date, y=in_bed), colour="violetred1") +
  xlab("Date") + ylab("Number in bed each day") +
  scale_x_date(date_breaks="2 days", date_labels="%b %d")

save(flu_trace_plot, flu_posterior_plot, flu_posterior_trajectory_plot, file="flu_plots.RData")
