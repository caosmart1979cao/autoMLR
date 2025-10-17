#' Run automated hyperparameter tuning
#'
#' The core tuning engine. It bundles a task, learner, and search space to find
#' the optimal hyperparameters using random search and cross-validation.
#'
#' @param task The \code{mlr3} classification task.
#' @param learner The \code{mlr3} learner object.
#' @param search_space The \code{paradox::ParamSet} object.
#' @param n_evals The number of iterations for random search.
#' @param cv_folds The number of folds for cross-validation.
#' @return A finished \code{mlr3tuning::TuningInstanceBatchSingleCrit} object containing all results.
#' @export
run_hyperparameter_tuning <- function(task, learner, search_space, n_evals = 20, cv_folds = 5) {
  # ... (The rest of the function code remains the same as before)
  resampling <- mlr3::rsmp("cv", folds = cv_folds)
  measure <- mlr3::msr("classif.auc")
  terminator <- mlr3tuning::trm("evals", n_evals = n_evals)
  tuner <- mlr3tuning::tnr("random_search")
  instance <- mlr3tuning::TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = terminator
  )
  tuner$optimize(instance)
  return(instance)
}