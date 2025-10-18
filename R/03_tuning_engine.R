#' Run automated hyperparameter tuning
#'
#' The core tuning engine. It bundles a task, learner, and search space to find
#' the optimal hyperparameters using random search and cross-validation.
#' The evaluation measure is dynamically selected based on the task type,
#' unless specified otherwise.
#'
#' @param task The \code{mlr3} task (e.g., TaskClassif, TaskRegr, TaskSurv).
#' @param learner The \code{mlr3} learner object.
#' @param search_space The \code{paradox::ParamSet} object.
#' @param n_evals The number of iterations for random search.
#' @param cv_folds The number of folds for cross-validation.
#' @param measure An optional \code{mlr3::Measure} object to use for optimization.
#'   If NULL, a default is chosen based on the task type (AUC for classif, RMSE for regr, C-index for surv).
#' @return A finished \code{mlr3tuning::TuningInstanceBatchSingleCrit} object containing all results.
#' @export
run_hyperparameter_tuning <- function(task, learner, search_space, n_evals = 20, cv_folds = 5, measure = NULL) {

  # Dynamically select the optimization measure if not provided
  if (is.null(measure)) {
    measure <- switch(task$task_type,
                      "classif" = mlr3::msr("classif.auc"),
                      "regr"    = mlr3::msr("regr.rmse"),
                      "surv"    = mlr3::msr("surv.cindex"),
                      stop("Unsupported task type for default measure selection: ", task$task_type)
    )
    message("No measure provided. Using default for '", task$task_type, "' task: '", measure$id, "'")
  }

  resampling <- mlr3::rsmp("cv", folds = cv_folds)
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
