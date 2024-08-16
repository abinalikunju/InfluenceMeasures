library(testthat)
library(InfluenceMeasures)

# Sample data and model
set.seed(123)
test_data <- data.frame(x = 1:50, y = rnorm(50))
test_model <- lm(y ~ x, data = test_data)

test_that("influence_analysis function works correctly", {
  result <- influence_analysis(test_data, test_model)

  expect_type(result, "list")
  expect_named(result, c("cooks_d", "dffits", "hadi", "plot"))
  expect_length(result$cooks_d, 50)
  expect_s3_class(result$plot, "ggplot")

  # Test error handling
  expect_error(influence_analysis(test_data, "not a model"), "Model must be a linear model object")
  expect_error(influence_analysis(test_data[1:25,], test_model), "Data and model dimensions do not match")

  test_data_na <- test_data
  test_data_na$y[1] <- NA
  expect_error(influence_analysis(test_data_na, test_model), "Data contains NA values")
})

test_that("Individual measures are calculated correctly", {
  measures <- c(cooks_distance(test_model), dffits_measure(test_model), hadi_measure(test_model, test_data))
  expect_true(all(is.finite(measures)))
})

test_that("Plotting function works", {
  plot <- create_plot(cooks_distance(test_model), dffits_measure(test_model), hadi_measure(test_model, test_data), "all")
  expect_s3_class(plot, "ggplot")
})
