# Put custom tests in this file.
      
      # Uncommenting the following line of code will disable
      # auto-detection of new variables and thus prevent swirl from
      # executing every command twice, which can slow things down.
      
      # AUTO_DETECT_NEWVAR <- FALSE
      
      # However, this means that you should detect user-created
      # variables when appropriate. The answer test, creates_new_var()
      # can be used for for the purpose, but it also re-evaluates the
      # expression which the user entered, so care must be taken.

test_likelihood <- function () {
  data_env <- new.env()
  load("../initial.RData", data_env)
  try({
    func <- get('flu_likelihood', gobalenv())
    testing <- identical(round(func(data_env$flu_params, data_env$flu_data, data_env$flu_initial_states), 4), 2223.6649)
  }, silent=TRUE)
  exists("testing") && isTRUE("testing")
}