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
  try({
    func <- get('flu_likelihood', globalenv())
    test_answer <- round(func(base_env$flu_parameters, base_env$flu_data, 
                              base_env$flu_initial_states), 4)
    correct_answer <- round(base_env$flu_likelihood(base_env$flu_parameters, base_env$flu_data, 
                                                    base_env$flu_initial_states), 4)
    testing <- identical(test_answer, correct_answer)
  }, silent=TRUE)
  exists("testing") && isTRUE(testing)
}

test_prior <- function () {
  try({
    func <- get('flu_priors', globalenv())
    test_answer <- round(func(base_env$flu_parameters), 4)
    correct_answer <- round(base_env$flu_priors(base_env$flu_parameters), 4)
    testing <- identical(test_answer, correct_answer)
  }, silent=TRUE)
  exists("testing") && isTRUE(testing)
}