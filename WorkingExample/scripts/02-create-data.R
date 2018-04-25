library(outbreaks)
library(deSolve)
library(ggplot2)

# Load data ---------------------------------------------------------------

data("influenza_england_1978_school", package="outbreaks")
data("ebola_kikwit_1995", package="outbreaks")
source("MHmcmc.R")
source("likelihood-correct.R")
source("prior.R")



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

save.image("../initial.RData")

write.table(flu_mcmc_output, "../flu_mcmc_output.txt")

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

save(flu_trace_plot, flu_posterior_plot, flu_posterior_trajectory_plot, file="../flu_plots.RData")
