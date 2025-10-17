#' Summarize tuning results
#'
#' Extracts key results (best parameters, best performance, and full archive)
#' from a finished tuning instance.
#'
#' @param tuning_instance The finished tuning instance from \code{run_hyperparameter_tuning()}.
#' @return A list containing \code{best_params}, \code{best_performance}, and \code{full_archive}.
#' @export
summarize_tuning_results <- function(tuning_instance) {
  # ... (The rest of the function code remains the same as before)
  results_archive <- data.table::as.data.table(tuning_instance$archive)
  return(
    list(
      best_params = tuning_instance$result_learner_param_vals,
      best_performance = tuning_instance$result_y,
      full_archive = results_archive
    )
  )
}


#' Plot tuning summary
#'
#' Generates one of several standard diagnostic plots for a tuning instance.
#'
#' @param tuning_instance The finished tuning instance.
#' @param type The type of plot. Can be "parameter", "performance", or "history".
#' @return A ggplot object.
#' @export
plot_tuning_summary <- function(tuning_instance, type = "parameter") {
  # ... (The rest of the function code remains the same as before)
  if (!type %in% c("parameter", "performance", "history")) {
    stop("Plot type must be one of 'parameter', 'performance', or 'history'.")
  }
  mlr3viz::autoplot(tuning_instance, type = type)
}

#' Save tuning results
#'
#' Saves the full tuning archive and a summary of the best results to CSV files.
#'
#' @param summary_list A list from \code{summarize_tuning_results()}.
#' @param output_dir The directory to save the files in.
#' @param model_code The short code of the model (e.g., "DT").
#' @return Invisibly returns NULL. Called for its side effect of writing files.
#' @export
save_tuning_results <- function(summary_list, output_dir, model_code) {
  # ... (The rest of the function code remains the same as before, including the fix)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  archive_to_save <- data.table::copy(summary_list$full_archive)
  list_cols <- names(which(sapply(archive_to_save, is.list)))
  if (length(list_cols) > 0) {
    archive_to_save[, (list_cols) := NULL]
  }
  file_path_full <- file.path(output_dir, paste0(model_code, "_tune_results.csv"))
  data.table::fwrite(archive_to_save, file = file_path_full)
  best_params_df <- as.data.frame(summary_list$best_params)
  best_params_df$auc <- summary_list$best_performance[[1]]
  file_path_summary <- file.path(output_dir, paste0(model_code, "_tune_summary.csv"))
  write.csv(best_params_df, file = file_path_summary, row.names = FALSE)
  message("Tuning results saved to: ", normalizePath(output_dir))
}