#' Generate SHAP (SHapley Additive exPlanations) plots for a trained model.
#'
#' This function takes a trained learner and test data to compute and visualize
#' SHAP values, providing insights into model predictions. It requires the 'shapviz'
#' and 'kernelshap' packages.
#'
#' @param trained_learner A trained mlr3 \code{Learner} object.
#' @param task The \code{mlr3} task used for training.
#' @param test_data A \code{data.frame} of the test set (features only).
#' @return A named list of ggplot objects:
#'   \itemize{
#'     \item \code{importance}: SHAP Feature Importance (Bar Plot).
#'     \item \code{beeswarm}: SHAP Bee Swarm Plot.
#'     \item \code{dependence}: SHAP Dependence Plots for the top 4 features.
#'   }
#' @export
generate_shap_plots <- function(trained_learner, task, test_data) {
  if (!requireNamespace("shapviz", quietly = TRUE) || !requireNamespace("kernelshap", quietly = TRUE)) {
    stop("Packages 'shapviz' and 'kernelshap' are required. Please install them.")
  }

  # Use a smaller background dataset for faster computation, as recommended
  background_data <- task$data(rows = sample(task$row_ids, min(50, task$nrow)))
  feature_data <- background_data[, task$feature_names, with = FALSE]

  # Calculate SHAP values using the model-agnostic kernelSHAP method
  shap_values <- kernelshap::kernelshap(
    object = trained_learner,
    X = test_data,
    bg_X = feature_data
  )

  # Create a shapviz object for plotting
  shap_viz_obj <- shapviz::shapviz(shap_values)

  # 1. Feature Importance Plot
  plot_importance <- shapviz::sv_importance(shap_viz_obj, kind = "bar", show_numbers = TRUE) +
    ggplot2::ggtitle("SHAP Feature Importance") +
    ggplot2::labs(
      subtitle = "Mean absolute SHAP value across all samples",
      x = "Mean |SHAP value| (impact on model output magnitude)"
    )

  # 2. Bee Swarm Plot
  plot_beeswarm <- shapviz::sv_importance(shap_viz_obj, kind = "beeswarm") +
    ggplot2::ggtitle("SHAP Value Distribution (Bee Swarm)") +
    ggplot2::labs(
      subtitle = "Each point is a sample, color indicates feature value",
      x = "SHAP value (impact on model output)"
    )

  # 3. Dependence Plots for top 4 features
  top_features <- names(shapviz::sv_importance(shap_viz_obj, return_data = TRUE)$features[1:4])
  plot_dependence <- shapviz::sv_dependence(shap_viz_obj, v = top_features) +
    ggplot2::ggtitle("SHAP Feature Dependence") +
    ggplot2::labs(subtitle = "Relationship between feature value and its SHAP value")

  # Return all plots in a named list
  return(
    list(
      importance = plot_importance,
      beeswarm = plot_beeswarm,
      dependence = plot_dependence
    )
  )
}
