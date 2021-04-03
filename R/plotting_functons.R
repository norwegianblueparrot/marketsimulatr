#' plot_series
#'
#' @param price_series The price series to be visualised.
#'
#' @return
#' @export
plot_price_series <- function(price_series, series_name) {
  library(ggplot2)
  price_series %>%
    zoo::fortify.zoo() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = Index, y = {{ series_name }}) +
    ggplot2::geom_line() +
    ggplot2::xlab("timestamp") +
    ggplot2::ggtitle("Traded Price Series") +
    ggplot2::theme_light()
}
