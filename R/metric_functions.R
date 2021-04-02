#' calculate_measures
#'
#' @param order_book
#' @param fill_times
#' @param num_orders
#'
#' @return
#' @export
#'
#' @examples
calculate_measures <- function(order_book, fill_times, num_orders) {
  # fill optional arguments if not present
  if (missing(num_orders)) {
    num_orders <- nrow(order_book)
  }

  # extract the unique time to fill values
  if (missing(fill_times)) {
    fill_times <- sort(unique(order_book$time_to_fill), na.last = TRUE)
  }

  # calculate number of unique fill times
  num_times <- length(fill_times)

  # preallocate matrix to hold results
  results_matrix <- matrix(ncol = 4, nrow = num_times)
  colnames(results_matrix) <- c("time_to_fill", "p_epl", "p_epu", "km")

  # for each fill time, calculate the three error metrics described in the report.
  for (i in 1:num_times)
  {
    results_matrix[i, 1] <- fill_times[i]
    sum_ttf <- sum(order_book$time_to_fill <= fill_times[i])
    results_matrix[i, 2] <- sum_ttf / num_orders
    sum_ttc <- sum(order_book$time_to_cancel > fill_times[i])
    if (sum_ttc != 0) {
      results_matrix[i, 3] <- sum_ttf / sum_ttc
    }

    active_orders <- num_orders - (
      sum(order_book$time_to_fill < fill_times[i]) + sum(order_book$time_to_cancel < fill_times[i])
    )
    results_matrix[i, 4] <- (active_orders - sum(order_book$time_to_fill == fill_times[i])) /
      active_orders
    if (i == num_times) {
      results_matrix[i, 3] <- 1
      results_matrix[i, 4] <- 0
    }
  }

  # add an extra row for t = 0 (all orders survive/unfilled)
  results_matrix <- rbind(c(0, 0, 0, 1), results_matrix)
  results_matrix[, 4] <- 1 - cumprod(results_matrix[, 4])

  # plot the probability curves
  # TODO replace with ggplot2
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
  return(results_matrix)
}
