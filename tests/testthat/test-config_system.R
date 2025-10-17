# This command makes all functions from our package available for testing
devtools::load_all()

test_that("Model registry loads correctly", {
  registry <- load_model_registry()
  # Test 1: It should be a data frame
  expect_s3_class(registry, "data.frame")
  # Test 2: It should not be empty
  expect_gt(nrow(registry), 0)
})

test_that("Model config retrieval works and throws errors correctly", {
  registry <- load_model_registry()
  
  # Test 1: Correctly retrieves a known model
  dt_config <- get_model_config(registry, "DT")
  expect_type(dt_config, "list")
  expect_equal(dt_config$LearnerID, "classif.rpart")
  
  # Test 2: Throws an error for a model that does not exist
  expect_error(
    get_model_config(registry, "FAKE_MODEL"),
    "Model code 'FAKE_MODEL' not found in the registry."
  )
})