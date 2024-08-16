create_base_plot <- function(influence_values, measure) {
  plot(influence_values, type = "h", main = paste("Influence Measure:", measure),
       xlab = "Observation", ylab = "Influence")
  abline(h = 4/length(influence_values), col = "red", lty = 2)
  invisible(recordPlot())
}


create_ggplot <- function(influence_values, measure) {
  library(ggplot2)
  df <- data.frame(observation = seq_along(influence_values), influence = influence_values)
  ggplot(df, aes(x = observation, y = influence)) +
    geom_segment(aes(xend = observation, yend = 0), color = "blue") +
    geom_hline(yintercept = 4/length(influence_values), color = "red", linetype = "dashed") +
    labs(title = paste("Influence Measure:", measure),
         x = "Observation", y = "Influence") +
    theme_minimal()
}
