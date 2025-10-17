#' Load the model registry
#'
#' Reads the model registry from the CSV file located in the package's external data directory.
#'
#' @return A data.frame containing the model registry.
#' @export
load_model_registry <- function() {
  registry_path <- system.file("extdata", "model_registry.csv", package = "autoMLR")
  if (registry_path == "") {
    registry_path <- "inst/extdata/model_registry.csv"
  }
  if (!file.exists(registry_path)) {
    stop("Model registry file not found.")
  }
  read.csv(registry_path, stringsAsFactors = FALSE)
}

#' List available models
#'
#' Prints a formatted table of available models from the registry, with an option to filter by category.
#'
#' @param registry A data.frame from \code{load_model_registry()}.
#' @param category An optional character string to filter models by category (e.g., "Boosting").
#' @return Invisibly returns NULL. This function is called for its side effect of printing to the console.
#' @export
list_available_models <- function(registry, category = NULL) {
  # ... (The rest of the function code remains the same as before)
  if (!is.null(category)) {
    registry <- registry[registry$Category == category, ]
    if(nrow(registry) == 0) {
      message("No models found in category: ", category)
      return(invisible())
    }
  }
  cat("Available Models:\n")
  print(registry[, c("Code", "FullName", "Category")])
}


#' Get a model's configuration
#'
#' Retrieves all configuration details for a specific model from the registry.
#'
#' @param registry A data.frame from \code{load_model_registry()}.
#' @param model_code The short code of the model (e.g., "DT").
#' @return A list containing the configuration for the specified model.
#' @export
get_model_config <- function(registry, model_code) {
  # ... (The rest of the function code remains the same as before)
  if (!model_code %in% registry$Code) {
    stop("Model code '", model_code, "' not found in the registry.")
  }
  as.list(registry[registry$Code == model_code, ])
}