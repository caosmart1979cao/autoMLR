#' Creates an mlr3 learner
#'
#' Dynamically creates an mlr3 learner object based on a model configuration.
#' The predict type is automatically set to "prob" to enable AUC calculation.
#'
#' @param config A list containing model configuration from \code{get_model_config()}.
#' @return An mlr3 \code{Learner} object.
#' @export
create_learner <- function(config) {
  mlr3::lrn(config$LearnerID, predict_type = "prob")
}

#' Defines a hyperparameter search space
#'
#' Returns a pre-defined hyperparameter search space for a given model configuration.
#'
#' @param config A list containing model configuration.
#' @return A \code{paradox::ParamSet} object representing the search space.
#' @export
define_search_space <- function(config) {
  # First, check if the model is marked as tunable in the registry.
  if (!is.null(config$Tunable) && config$Tunable == FALSE) {
    stop(paste0("Model '", config$Code, "' (", config$FullName, ") is not configured for tuning in this package version."))
  }
  
  switch(config$Code,
         "DT" = paradox::ps(
           cp = paradox::p_dbl(lower = 0.001, upper = 0.1),
           maxdepth = paradox::p_int(lower = 2, upper = 10),
           minbucket = paradox::p_int(lower = 2, upper = 15)
         ),
         "RF" = paradox::ps(
           mtry.ratio = paradox::p_dbl(lower = 0.1, upper = 0.9),
           num.trees = paradox::p_int(lower = 50, upper = 500)
         ),
         "XGB" = paradox::ps(
           eta = paradox::p_dbl(lower = 0.01, upper = 0.3),
           nrounds = paradox::p_int(lower = 50, upper = 250),
           max_depth = paradox::p_int(lower = 2, upper = 8)
         ),
         "SVM" = paradox::ps(
           cost = paradox::p_dbl(lower = 0.01, upper = 10),
           gamma = paradox::p_dbl(lower = 0.001, upper = 1),
           kernel = paradox::p_fct(c("radial", "polynomial", "sigmoid"))
         ),
         "GLMNET" = paradox::ps(
           alpha = paradox::p_dbl(lower = 0, upper = 1), # 0=Ridge, 1=Lasso
           s = paradox::p_dbl(lower = 0.001, upper = 0.1)  # Lambda
         ),
         "LGBM" = paradox::ps(
           learning_rate = paradox::p_dbl(lower = 0.01, upper = 0.3),
           num_iterations = paradox::p_int(lower = 50, upper = 250),
           max_depth = paradox::p_int(lower = 2, upper = 8),
           num_leaves = paradox::p_int(lower = 10, upper = 40)
         ),
         "LASSO" = paradox::ps(
           alpha = paradox::p_fct(c(1)), # Fixed to 1 for Lasso
           s = paradox::p_dbl(lower = 0.001, upper = 0.1)
         ),
         "RIDGE" = paradox::ps(
           alpha = paradox::p_fct(c(0)), # Fixed to 0 for Ridge
           s = paradox::p_dbl(lower = 0.001, upper = 0.1)
         ),
         "GAMBOOST" = paradox::ps(
           mstop = paradox::p_int(lower = 50, upper = 500),
           nu = paradox::p_dbl(lower = 0.01, upper = 0.3) # Learning rate
         ),
         # Default case for models that are Tunable but not yet implemented
         stop(paste0("Search space for model '", config$Code, "' is defined as Tunable but a space has not been implemented yet."))
  )
}