# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.

.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

open_mcmc_file <- function () {
  file.edit("scripts/MHmcmc.R")
}

base_path <- file.path(.get_course_path(), "mcmc-infectious-disease-epi", "WorkingExample")

base_env <- new.env()

load(paste0(base_path, "/initial.RData"), base_env)
load(paste0(base_path, "/flu_plots.RData"), base_env)