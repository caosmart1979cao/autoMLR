#' Run a benchmark comparison of multiple models, with automated tuning.
#'
#' This function orchestrates a complete benchmark workflow. For each specified model,
#' it automatically wraps it in an AutoTuner if it's marked as tunable, then
#' runs a benchmark comparison across all models on a given resampling strategy.
#'
#' @param task The \code{mlr3} task object (e.g., TaskClassif, TaskRegr, TaskSurv).
#' @param model_codes A character vector of model codes from the registry to compare.
#' @param registry The model registry \code{data.frame}.
#' @param n_evals The number of tuning evaluations for each tunable model.
#' @param inner_cv_folds The number of CV folds for the inner hyperparameter tuning loop.
#' @param outer_resampling The \code{mlr3} resampling strategy for the final outer
#'   benchmark comparison (e.g., \code{rsmp("cv", folds = 3)}).
#' @return An \code{mlr3::BenchmarkResult} object containing the performance of all models.
#' @export
run_model_benchmark <- function(task, model_codes, registry, n_evals = 20, inner_cv_folds = 5, outer_resampling) {

  learners_to_benchmark <- list()

  for (code in model_codes) {
    config <- get_model_config(registry, code)
    base_learner <- create_learner(config)

    # Check if the model is tunable according to the registry
    if (!is.null(config$Tunable) && config$Tunable == TRUE) {
      message(paste0("Configuring AutoTuner for '", code, "'..."))

      # This is a tunable model, wrap it in an AutoTuner for nested resampling
      search_space <- define_search_space(config)
      terminator <- mlr3tuning::trm("evals", n_evals = n_evals)
      tuner <- mlr3tuning::tnr("random_search")
      inner_resampling <- mlr3::rsmp("cv", folds = inner_cv_folds)

      # Dynamically select the measure based on task type
      measure <- switch(task$task_type,
                        "classif" = mlr3::msr("classif.auc"),
                        "regr"    = mlr3::msr("regr.rmse"),
                        "surv"    = mlr3::msr("surv.cindex"),
                        stop("Unsupported task type for benchmark measure selection.")
      )

      # Create the AutoTuner
      auto_tuner <- mlr3tuning::AutoTuner$new(
        learner = base_learner,
        resampling = inner_resampling,
        measure = measure,
        search_space = search_space,
        terminator = terminator,
        tuner = tuner
      )
      auto_tuner$id <- code # Set a clean ID for the benchmark results
      learners_to_benchmark[[code]] <- auto_tuner

    } else {
      # This is a non-tunable model, use it directly
      message(paste0("Adding non-tunable model '", code, "' to benchmark."))
      base_learner$id <- code
      learners_to_benchmark[[code]] <- base_learner
    }
  }

  # Ensure there are learners to benchmark
  if (length(learners_to_benchmark) == 0) {
    stop("No valid learners were configured for the benchmark.")
  }

  # Create the benchmark design
  design <- mlr3::benchmark_grid(
    tasks = task,
    learners = learners_to_benchmark,
    resamplings = outer_resampling
  )

  message(paste0("\n--- Starting benchmark of ", length(learners_to_benchmark), " model(s) ---"))
  # Set store_models = TRUE to inspect models later if needed
  bmr <- mlr3::benchmark(design, store_models = TRUE)

  message("✔ Benchmark finished.")
  return(bmr)
}

#' Visualize benchmark results
#'
#' Creates a boxplot comparing the performance of different learners from a
#' benchmark result.
#'
#' @param bmr The \code{BenchmarkResult} object from \code{run_model_benchmark}.
#' @param measure An optional \code{mlr3::Measure} to plot. If NULL, plots the primary
#'   measure from the benchmark result.
#' @return A \code{ggplot2} object.
#' @export
plot_benchmark_summary <- function(bmr, measure = NULL) {
  if (!requireNamespace("mlr3viz", quietly = TRUE)) {
    stop("Package 'mlr3viz' is required for plotting. Please install it.")
  }

  mlr3viz::autoplot(bmr, measure = measure) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "Model Benchmark Comparison",
      subtitle = "Performance across resampling folds"
    ) +
    # Improve axis labels for better readability
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
}
