# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.

open_mcmc_file <- function () {
  file.edit("02WorkingExample/scripts/script-MHmcmc.R")
}

data("influenza_england_1978_school", package="outbreaks")
flu_parameters <- c(beta=0.5/760, gamma=1/7)
flu_data <- influenza_england_1978_school$in_bed
flu_initial_states <- c(S=763-3, I=3, R=0)

