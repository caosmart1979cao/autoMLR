#' Creates an mlr3 learner
#'
#' Dynamically creates an mlr3 learner object based on a model configuration.
#' The predict type is automatically set to "prob" for classification learners
#' to enable AUC calculation with the default measure.
#'
#' @param config A list containing model configuration from \code{get_model_config()}.
#' @return An mlr3 \code{Learner} object.
#' @export
create_learner <- function(config) {
  learner <- mlr3::lrn(config$LearnerID)

  # Set predict_type to "prob" ONLY for classification learners,
  # as this is required for the default AUC measure. Other learner types
  # (regr, surv) retain their defaults which are suitable for their measures (e.g., "response", "crank").
  if (startsWith(learner$id, "classif.")) {
    learner$predict_type <- "prob"
  }

  return(learner)
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
         "CPH" = paradox::ps(
           # Example for a survival model, can be expanded
           # Note: CoxPH in 'survival' package doesn't have many hyperparameters to tune via mlr3
           # This is a placeholder for potentially more complex survival learners
         ),
         "SURV_RF" = paradox::ps(
           mtry.ratio = paradox::p_dbl(lower = 0.1, upper = 0.9),
           num.trees = paradox::p_int(lower = 50, upper = 500),
           min.node.size = paradox::p_int(lower = 1, upper = 10)
         ),
         "GBM" = paradox::ps(
           n.trees = paradox::p_int(lower = 50, upper = 500),
           interaction.depth = paradox::p_int(lower = 1, upper = 5),
           shrinkage = paradox::p_dbl(lower = 0.001, upper = 0.1)
         ),
         "NNET" = paradox::ps(
           size = paradox::p_int(lower = 1, upper = 10),
           decay = paradox::p_dbl(lower = 1e-4, upper = 0.1)
         ),
         # Default case for models that are Tunable but not yet implemented
         stop(paste0("Search space for model '", config$Code, "' is defined as Tunable but a space has not been implemented yet."))
  )
}

#' Creates a list of mlr3 learners
#'
#' Takes a list of model configurations and returns a list of corresponding
#' mlr3 learner objects.
#'
#' @param configs A list of configuration lists from \code{get_model_config()}.
#' @return A list of mlr3 \code{Learner} objects.
#' @export
create_multiple_learners <- function(configs) {
  # Use lapply to apply the create_learner function to each config in the list
  learners <- lapply(configs, create_learner)

  # Set the ID for each learner to be its short code for easy identification
  learner_codes <- sapply(configs, function(c) c$Code)
  names(learners) <- learner_codes

  return(learners)
}
