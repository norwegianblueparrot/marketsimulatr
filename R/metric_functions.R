#' calculate_metrics
#'
#' @param order_book The order book to analyse.
#' @param fill_times The fill time of the orders (optional).
#' @param num_orders The number of orders to analyse (optional).
#'
#' @return
#' @export
calculate_metrics <- function(order_book, fill_times, num_orders) {
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
  colnames(results_matrix) <-
    c("time_to_fill", "p_epl", "p_epu", "km")

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
    results_matrix[i, 4] <-
      (active_orders - sum(order_book$time_to_fill == fill_times[i])) /
        active_orders
    if (i == num_times) {
      results_matrix[i, 3] <- 1
      results_matrix[i, 4] <- 0
    }
  }

  # add an extra row for t = 0 (all orders survive/unfilled)
  results_matrix <- rbind(c(0, 0, 0, 1), results_matrix)
  results_matrix[, 4] <- 1 - cumprod(results_matrix[, 4])

  return(results_matrix)
}

#' analyse_limit_orders
#'
#' @param order_book A populated order book to analyse
#'
#' @return
#' @export
#'
analyse_limit_orders <- function(order_book) {
  order_record <- order_book
  data.table::setkey(order_record, order_id, order_type)

  # extract limit order book information
  limit_orders <- order_record[data.table::CJ(unique(order_id), c("limit_bid", "limit_ask"),
    sorted =
      FALSE
  ), nomatch = 0]

  # calculate fill times - rounded to nearest minute (ceiling)
  limit_orders[, c("time_to_first", "time_to_fill", "time_to_cancel") :=
    list(
      ceiling(as.numeric(first_fill_time - submitted_time) /
        60),
      ceiling(as.numeric(fill_time - submitted_time) / 60),
      ceiling(as.numeric(cancel_time - submitted_time) / 60)
    )]

  # replace NA times with infinities
  for (j in seq_len(ncol(limit_orders))) {
    data.table::set(limit_orders, which(is.na(limit_orders[[j]])), j, Inf)
  }

  # extract limit bids and asks
  limit_bid <- limit_orders[data.table::CJ(unique(order_id), c("limit_bid"),
    sorted =
      FALSE
  ), nomatch = 0]
  limit_ask <- limit_orders[data.table::CJ(unique(order_id), c("limit_ask"),
    sorted =
      FALSE
  ), nomatch = 0]


  # order execution - all orders
  fill_times <- sort(unique(limit_orders$time_to_fill), na.last = TRUE)
  order_dist <- sort(unique(limit_bid[, order_distance]))
  all_prob_metrics <- marketsimulatr::calculate_metrics(
    order_book = limit_orders,
    fill_times = fill_times
  )
}
