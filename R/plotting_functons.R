#' plot_series
#'
#' @param price_series The price series to be visualised.
#'
#' @return
#' @export
plot_price_series <- function(price_series, series_name) {
  library("ggplot2")
  price_series %>%
    zoo::fortify.zoo() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = Index, y = {{ series_name }}) +
    ggplot2::geom_line() +
    ggplot2::xlab("timestamp") +
    ggplot2::ggtitle("Traded Price Series") +
    ggplot2::theme_light()
}

#' plot_survival_curves
#'
#' @param results_matrix
#'
#' @return A plot of survival curves
#' @export
plot_survival_curves <- function(results_matrix) {
  plot(
    results_matrix[, 3],
    col = "green",
    type = "l",
    lwd = 2,
    lty = 4,
    xlab = "Time [minutes]",
    ylab = "Execution Probability",
    main = "Comparison of Probability Approaches",
    ylim = c(0, 1)
  )

  lines(results_matrix[, 4],
    lwd = 2,
    lty = 1,
    col = "black"
  )

  lines(results_matrix[, 2],
    lwd = 2,
    lty = 2,
    col = "blue"
  )

  legend(
    "bottomright",
    legend = c(expression(P[EPU](t)), expression(P[TTF](t)), expression(P[EPL](t))),
    col = c("green", "black", "blue"),
    lty = c(4, 1, 2),
    lwd = c(2, 2, 2)
  )
}
