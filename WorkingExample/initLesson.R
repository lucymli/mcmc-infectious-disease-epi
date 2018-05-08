# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.

.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

base_path <- file.path(.get_course_path(), "mcmc-infectious-disease-epi", "WorkingExample")

open_mcmc_file <- function () {
  file.edit(paste0(base_path, "/scripts/MHmcmc.R"))
}



base_env <- new.env()

load(paste0(base_path, "/initial.RData"), base_env)
load(paste0(base_path, "/flu_plots.RData"), base_env)

SIR <- base_env$SIR

MHmcmc <- base_env$MHmcmc
flu_data <- base_env$flu_data
initial_states <- base_env$flu_initial_states
likelihood_function <- base_env$flu_likelihood
prior_function <- base_env$flu_priors
mcmc_options <- base_env$flu_mcmc_options
initial_parameters <- base_env$flu_parameters
mcmc_output <- base_env$flu_mcmc_output

plot_data <- function () {
  P <- ggplot(base_env$influenza_england_1978_school) + theme_classic() +
  geom_point(aes(x=date, y=in_bed)) +
  xlab("Date") +
  ylab("In bed (prevalence)") +
  scale_x_date(date_labels="%b %d", date_breaks="2 days")
  print(P)
}
