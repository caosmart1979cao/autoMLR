#' Train the final model
#'
#' Applies the best hyperparameters to a learner and trains it on the full dataset.
#'
#' @param learner The \code{mlr3} learner object.
#' @param task The \code{mlr3} task.
#' @param best_params A list of the best hyperparameters from \code{summarize_tuning_results()}.
#' @return The trained final learner object.
#' @export
train_final_model <- function(learner, task, best_params) {
  # ... (The rest of the function code remains the same as before)
  learner$param_set$values <- best_params
  learner$train(task)
  return(learner)
}

#' Generate a training report
#'
#' Creates a text file summarizing the entire training and tuning process.
#'
#' @param config The model configuration list.
#' @param tuning_summary The summary list from \code{summarize_tuning_results()}.
#' @param output_dir The directory to save the report in.
#' @return Invisibly returns NULL. Called for its side effect of writing a file.
#' @export
generate_training_report <- function(config, tuning_summary, output_dir) {
  # ... (The rest of the function code remains the same as before)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  report_path <- file.path(output_dir, paste0(config$Code, "_training_report.txt"))
  sink(report_path)
  cat("=========================================\n")
  cat("      Model Training Summary Report      \n")
  cat("=========================================\n\n")
  cat("Model Name:", config$FullName, "(", config$Code, ")\n")
  cat("Learner ID:", config$LearnerID, "\n")
  cat("Description:", config$Description, "\n\n")
  cat("--- Hyperparameter Tuning ---\n")
  cat("Metric:", "AUC", "\n")
  cat("Search Method:", "Random Search", "\n\n")
  cat("--- Best Performance ---\n")
  cat("AUC:", round(tuning_summary$best_performance[[1]], 4), "\n\n")
  cat("--- Best Hyperparameters ---\n")
  print(tuning_summary$best_params)
  sink()
  message("Training report saved to: ", normalizePath(report_path))
}


#' Save final artifacts
#'
#' Saves the final trained model object and the full tuning instance to .rds files for later use.
#'
#' @param final_learner The trained final learner object.
#' @param tuning_instance The original tuning instance.
#' @param output_dir The directory to save the files in.
#' @param model_code The short code of the model.
#' @return Invisibly returns NULL. Called for its side effect of writing files.
#' @export
save_final_artifacts <- function(final_learner, tuning_instance, output_dir, model_code) {
  # ... (The rest of the function code remains the same as before)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  model_file <- file.path(output_dir, paste0(model_code, "_final_model.rds"))
  saveRDS(final_learner, file = model_file)
  instance_file <- file.path(output_dir, paste0(model_code, "_tuning_instance.rds"))
  saveRDS(tuning_instance, file = instance_file)
  message("Final model and tuning instance saved to: ", normalizePath(output_dir))
}