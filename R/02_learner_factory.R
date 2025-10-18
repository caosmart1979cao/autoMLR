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

#' Defines a hyperparameter search space with pipeline-aware naming
#'
#' Returns a pre-defined hyperparameter search space for a given model configuration.
#' It automatically prefixes parameter names with the learner's ID to make them
#' compatible with GraphLearners (pipelines). If no search space is defined for a
#' model, it returns an empty ParamSet and issues a warning.
#'
#' @param config A list containing model configuration.
#' @return A \code{paradox::ParamSet} object representing the search space.
#' @export
define_search_space <- function(config) {
  # First, check if the model is marked as tunable in the registry.
  if (!is.null(config$Tunable) && config$Tunable == FALSE) {
    # Return an empty search space for non-tunable models.
    return(paradox::ps())
  }

  learner_id <- config$LearnerID

  # Use a list to construct parameters dynamically
  params <- switch(config$Code,
                   "DT" = list(
                     paradox::p_dbl(lower = 0.001, upper = 0.1),
                     paradox::p_int(lower = 2, upper = 10),
                     paradox::p_int(lower = 2, upper = 15)
                   ),
                   "RF" = list(
                     paradox::p_dbl(lower = 0.1, upper = 0.9),
                     paradox::p_int(lower = 50, upper = 500)
                   ),
                   "XGB" = list(
                     paradox::p_dbl(lower = 0.01, upper = 0.3),
                     paradox::p_int(lower = 50, upper = 250),
                     paradox::p_int(lower = 2, upper = 8)
                   ),
                   "SVM" = list(
                     paradox::p_dbl(lower = 0.01, upper = 10),
                     paradox::p_dbl(lower = 0.001, upper = 1),
                     paradox::p_fct(c("radial", "polynomial", "sigmoid"))
                   ),
                   # Add other tunable models here...

                   # DEFAULT CASE: If a model is marked as Tunable but no space is defined,
                   # return an empty set and warn the user.
                   {
                     warning(paste0("Search space for model '", config$Code, "' is not implemented. It will not be tuned."))
                     list() # Return an empty list
                   }
  )

  # If the params list is empty, return an empty ParamSet
  if (length(params) == 0) {
    return(paradox::ps())
  }

  # Dynamically assign names based on the learner ID
  param_names <- switch(config$Code,
                        "DT" = c("cp", "maxdepth", "minbucket"),
                        "RF" = c("mtry.ratio", "num.trees"),
                        "XGB" = c("eta", "nrounds", "max_depth"),
                        "SVM" = c("cost", "gamma", "kernel"),
                        # Add other tunable models here...
                        NULL
  )

  if (!is.null(param_names)) {
    names(params) <- paste0(learner_id, ".", param_names)
  }

  # Construct the final ParamSet
  return(do.call(paradox::ps, params))
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
  learners <- lapply(configs, create_learner)
  learner_codes <- sapply(configs, function(c) c$Code)
  names(learners) <- learner_codes
  return(learners)
}

