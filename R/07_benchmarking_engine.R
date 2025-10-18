#' Run a multi-model benchmark with nested resampling
#'
#' This function orchestrates a full benchmark comparison of multiple learners.
#' For each learner, it automatically wraps it in an AutoTuner to perform
#' hyperparameter tuning using an "inner" cross-validation loop. Then, it
#' evaluates the performance of these self-tuning learners on an "outer"
#' cross-validation loop to get an unbiased performance estimate.
#'
#' @param task The \code{mlr3} task.
#' @param learners A **named** list of \code{mlr3} learners to benchmark, where names are model codes (e.g., 'DT').
#' @param n_evals The number of tuning evaluations for the inner loop (per learner).
#' @param outer_cv_folds The number of folds for the outer resampling loop.
#' @return A \code{mlr3::BenchmarkResult} object.
#' @export
run_model_benchmark <- function(task, learners, n_evals = 15, outer_cv_folds = 3) {

  if (!requireNamespace("mlr3tuning", quietly = TRUE)) {
    stop("Package 'mlr3tuning' is required for benchmarking with nested resampling.")
  }

  # A critical check to ensure the learners list is correctly named.
  if (is.null(names(learners)) || any(names(learners) == "")) {
    stop("'learners' must be a named list, where names are the model codes (e.g., 'DT', 'RF').")
  }

  # Ensure the registry is loaded to get configs
  registry <- load_model_registry()

  # 1. Create a list of AutoTuners, one for each learner, iterating over the NAMES.
  auto_tuners <- lapply(names(learners), function(model_code) {

    learner <- learners[[model_code]]
    config <- get_model_config(registry, model_code)

    # Do not try to tune models marked as not tunable
    if (is.null(config$Tunable) || config$Tunable == FALSE) {
      return(learner) # Return the base learner directly
    }

    search_space <- define_search_space(config)

    # If the search space is empty, don't wrap in an AutoTuner
    if (search_space$length == 0) {
      warning(paste("Search space for", model_code, "is empty. Skipping tuning."))
      return(learner)
    }

    inner_resampling <- mlr3::rsmp("cv", folds = 3)
    terminator <- mlr3tuning::trm("evals", n_evals = n_evals)
    tuner <- mlr3tuning::tnr("random_search")

    # Create the AutoTuner for this learner
    at <- mlr3tuning::AutoTuner$new(
      learner = learner,
      resampling = inner_resampling,
      measure = mlr3::msr("classif.auc"), # This could be made dynamic in a future version
      search_space = search_space,
      terminator = terminator,
      tuner = tuner,
      # Store the inner tuning instance for later retrieval in reports
      store_tuning_instance = TRUE
    )

    # The AutoTuner's ID is what will appear in the benchmark results
    at$id <- paste0(model_code, "_tuned")
    return(at)
  })

  # 2. Define the outer resampling strategy
  outer_resampling <- mlr3::rsmp("cv", folds = outer_cv_folds)

  # 3. Create the benchmark design
  design <- mlr3::benchmark_grid(
    task = task,
    learner = auto_tuners,
    resampling = outer_resampling
  )

  # 4. Run the benchmark
  message("Starting benchmark with nested resampling. This may take a while...")
  # CRITICAL FIX: store_backends = TRUE is essential for caching in vignettes
  benchmark_result <- mlr3::benchmark(design, store_models = TRUE, store_backends = TRUE)

  return(benchmark_result)
}

#' Plot benchmark summary
#'
#' Generates a bar plot comparing the performance of learners from a benchmark result.
#'
#' @param benchmark_result The \code{mlr3::BenchmarkResult} object.
#' @return A ggplot object.
#' @export
plot_benchmark_summary <- function(benchmark_result) {
  if (!requireNamespace("mlr3viz", quietly = TRUE)) {
    stop("Package 'mlr3viz' is required for this visualization.")
  }

  mlr3viz::autoplot(benchmark_result) +
    ggplot2::ggtitle("Model Benchmark Comparison") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

