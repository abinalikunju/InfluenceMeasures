influence_analysis <- function(data, model, measure = "all") {
  # Input validation
  if (!inherits(model, "lm")) stop("Model must be a linear model object")
  if (!is.data.frame(data)) stop("Data must be a data frame")
  if (nrow(data) != length(model$fitted.values)) stop("Data and model dimensions do not match")
  if (any(is.na(data))) stop("Data contains NA values")
  if (any(is.infinite(as.matrix(data)))) stop("Data contains infinite values")

  # Calculate influence measures
  cooks_d <- cooks.distance(model)
  dffits <- dffits(model)
  hadi <- hadi_measure(model, data)

  # Create plot based on selected measure
  plot <- create_plot(cooks_d, dffits, hadi, measure)

  # Return results
  list(cooks_d = cooks_d, dffits = dffits, hadi = hadi, plot = plot)
}
