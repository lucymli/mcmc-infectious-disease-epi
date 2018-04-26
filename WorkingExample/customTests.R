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
  data_env <- base_env
  try({
    func <- get('flu_likelihood', globalenv())
    test_answer <- round(func(data_env$flu_parameters, data_env$flu_data, 
                              data_env$flu_initial_states), 4)
    correct_answer <- round(data_env$flu_likelihood(data_env$flu_parameters, data_env$flu_data, 
                                                    data_env$flu_initial_states), 4)
    testing <- identical(test_answer, correct_answer)
  }, silent=TRUE)
  exists("testing") && isTRUE(testing)
}