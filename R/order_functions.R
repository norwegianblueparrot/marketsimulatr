#' create_order_book
#'
#' Create an order book based on a continuous-time, stochastic, double auction.
#'
#' Given a starting price, orders (limit and market) are generated at random times
#' using a Poisson distribution. Prices are generated within an interval around the
#' current trading price.
#'
#' Market orders are filled immediately. If insufficient market depth is available,
#' a market maker will intervene. The market maker charges 5% above/below the current
#' price if it has to intervene.
#'
#' Limit orders are submitted, and some are cancelled randomly. The ratio of limit
#' orders to market orders in controlled by `bid_order_ratio` and `ask_order_ratio`.
#'
#' A minimum tick size can be specified. Order sizes are randomly generated - the sizes
#' are multiples of a number specified in the config file.
#'
#' The models assumes that the best bid is lower than the best ask: p_b < p_a.
#' If no orders are present in the book, the next bid, b, will be uniformly chosen in
#' (p - price_band*tick_size) <= b <= p,
#' and the next ask will be placed in
#' p <= a <= (p + price_band*tick_size),
# 'where p is the price of the last trade.
#
# The trade price is calculated as a weighted average of the limit order prices used to fill it.
#'
#' @param start_price The starting price of the simulation.
#' @param price_limit The maximum value the simulated price can attain.
#' @param price_band The size of the interval around the current price within which new orders/prices will be generated.
#' @param tick_size The minimum allowable price change.
#' @param market_sell The arrival rate of market sell orders.
#' @param market_buy The arrival rate of market buy orders.
#' @param bid_order_ratio The ratio of limit bid orders to market bid orders.
#' @param ask_order_ratio The ratio of limit ask orders to market ask orders.
#' @param cancel_bid The cancel rate of limit bid orders.
#' @param cancel_ask The cancel rate of limit ask orders.
#' @param n_events The total number of trades to simulate.
#'
#' @return A list containing the full order book, the remaining bid and ask books and the generated price series.
#' @export
create_order_book <- function(start_price,
                              price_limit,
                              price_band,
                              tick_size,
                              market_sell,
                              market_buy,
                              bid_order_ratio,
                              ask_order_ratio,
                              cancel_bid,
                              cancel_ask,
                              n_events) {
  # This function creates a simple limit order book with n_events orders and a starting price of start_price.
  # Price changes happen uniformly in the interval price_band around the current_price.
  # The ratio of limit order and market order arrival rates is defined by order.ratio
  set.seed(4321)

  # limit orders and cancellations arrive at rates proportional to the market order arrival rates
  limit_ask <- market_buy * ask_order_ratio
  limit_bid <- market_sell * bid_order_ratio

  # set cancellation rates
  cancel_ask <- limit_ask * cancel_ask
  cancel_bid <- limit_bid * cancel_bid

  # assume first order is submitted now, and market operates 24/7
  current_time <- Sys.time()

  # initialise bid/ask limit order books, and overall limit order_record.
  # will initially n_events rows (preallocating for speed)
  # unused rows will be trimmed before results are returned
  order_record <- data.table::data.table(
    submitted_time = .POSIXct(rep(NA_real_, length = n_events)),
    order_id = seq.int(from = 1, to = n_events, by = 1),
    order_type = vector(mode = "character", length = n_events),
    order_size = rep(NA_integer_, length = n_events),
    order_price = rep(NA_real_, length = n_events),
    order_distance = rep(NA_real_, length = n_events),
    first_fill_time = .POSIXct(rep(NA_real_, length = n_events)),
    fill_time = .POSIXct(rep(NA_real_, length = n_events)),
    cancel_time = .POSIXct(rep(NA_real_, length = n_events)),
    fill_price = vector(mode = "list", length = n_events),
    fill_id = vector(mode = "list", length = n_events),
    partial_fill_id = vector(mode = "list", length = n_events)
  )
  data.table::setkey(order_record, order_id)

  bid_order_book <- data.table::data.table(
    submitted_time = .POSIXct(rep(NA_real_, length = n_events)),
    order_id = rep(NA_integer_, length = n_events),
    order_size = rep(NA_integer_, length = n_events),
    order_price = rep(NA_real_, length = n_events)
  )

  ask_order_book <- data.table::data.table(
    submitted_time = .POSIXct(rep(NA_real_, length = n_events)),
    order_id = rep(NA_integer_, length = n_events),
    order_size = rep(NA_integer_, length = n_events),
    order_price = rep(NA_real_, length = n_events)
  )

  # initial best bids and best asks - set to zero
  best_bid <- 0
  best_ask <- 0
  next_bid_slot <- 1
  next_ask_slot <- 1

  current_price <- start_price

  # simulation runs for n_events iterations
  event_count <- 0
  while (event_count < n_events) {
    event_count <- event_count + 1

    message(glue::glue("Event: {event_count}"))
    message(glue::glue("Current price is: {current_price}"))

    # call function to create an order for the current iteration - can be one of six order_types, each with its own probability
    # sent current_time as time_stamp
    # loop iteration is used as an order ID
    current_order <- marketsimulatr::create_orders(
      time_stamp = current_time,
      order_number = event_count,
      market_sell = market_sell,
      market_buy = market_buy,
      limit_ask = limit_ask,
      limit_bid = limit_bid,
      cancel_ask = cancel_ask,
      cancel_bid = cancel_bid
    )

    # update current_time to reflect order time (order time follows Poisson process)
    current_time <- zoo::index(current_order)
    order_size <- as.integer(current_order$order_size)

    # Market maker intervenes0 @ 5% higher/lower than market price
    mm_buy <-
      plyr::round_any(current_price * 0.995, tick_size, f = floor)
    mm_sell <-
      plyr::round_any(current_price * 1.005, tick_size, f = ceiling)

    # deal with orders
    # we always assume that the best bid is lower than the best ask: p_b < p_a.
    # If no orders are present in the book, the next bid, b, will be uniformly chosen in
    # (p - price_band*tick_size) <= b <= p,
    # and the next ask will be placed in
    # p <= a <= (p + price_band*tick_size),
    # where p is the price of the last trade.

    # TODO this could probably be done with a switch/case_when, with a function per order-type
    if (current_order$order_type == 1) {
      order_type <- "limit_bid"
      # limit buy orders are uniformly placed in the price classes
      # from (p_a - price_band*tick_size) to (p_a - 1*tick_size) where p_a is the current best ask.
      # when p_a is between 1*tick_size and price_band*tick_size the admissible bid price interval is restricted.
      if (best_ask == 0) {
        if (current_price <= price_band * tick_size) {
          p <- sample(as.integer(current_price / tick_size), 1) * tick_size
        } else {
          p <- current_price - (sample((price_band + 1), 1) - 1) * tick_size
        }
      } else {
        if (best_ask == tick_size) {
          next # no order possible at this price (best bid and best ask have to separated by at least one tick & both positive)
        } else if (best_ask <= price_band * tick_size) {
          p <-
            best_ask - sample((as.integer(best_ask / tick_size) - 1), 1) * tick_size
        } else {
          p <- best_ask - sample(price_band, 1) * tick_size
        }
      }

      # record the order information in the main order_record
      order_record[event_count, ":="(
        submitted_time = current_time,
        order_id = as.integer(event_count),
        order_type = ..order_type,
        order_size = as.integer(..order_size),
        order_price = p,
        order_distance = ceiling((abs(best_ask - p) / tick_size))
      )]

      # update the appropriate limit order book
      # find first empty row in bid order book, then fill with current bid info. then sort
      bid_order_book[next_bid_slot, ":="(
        submitted_time = current_time,
        order_id = as.integer(event_count),
        order_size = ..order_size,
        order_price = p
      )]

      # sort bid order book so the highest bid is at the top of the pile.
      # quotes are ordered by price, then time (earliest first)
      bid_order_book[1:next_bid_slot, names(bid_order_book) := data.table::setorderv(bid_order_book[1:next_bid_slot],
        c("order_price", "submitted_time"),
        c(-1, 1),
        na.last = TRUE
      )]
      next_bid_slot <- next_bid_slot + 1
    } else if (current_order$order_type == 2) {
      order_type <- "limit_ask"
      # Limit ask orders are informally placed in the price classes from (p_b + 1*tick_size) to(p_b + price_band*tick_size).
      # when p_b is between (price_limit - price_band*tick_size + tick_size) and price_limit, the admissible ask price interval is restricted.
      if (best_bid == 0) {
        if (current_price > price_limit - (price_band * tick_size)) {
          p <-
            current_price + (sample((as.integer((price_limit - current_price) / tick_size) + 1), 1) - 1) * tick_size
        } else {
          p <- current_price + (sample((price_band + 1), 1) - 1) * tick_size
        }
      } else {
        if (best_bid == price_limit) {
          next # no order possible at this price (best bid and best ask have to separated by at least one tick & both positive)
        } else if (best_bid > price_limit - (price_band * tick_size)) {
          p <-
            best_bid + sample(as.integer((price_limit - best_bid) / tick_size), 1) *
              tick_size
        } else {
          p <- best_bid + sample(price_band, 1) * tick_size
        }
      }

      # update order_record
      # record the order information in the overall order_record
      order_record[event_count, ":="(
        submitted_time = current_time,
        order_id = as.integer(event_count),
        order_type = ..order_type,
        order_size = as.integer(..order_size),
        order_price = p,
        order_distance = ceiling((abs(best_bid - p) / tick_size))
      )]

      # update the appropriate limit order book
      # find first empty row in bid order book, then fill with current bid info. then sort
      ask_order_book[next_ask_slot, ":="(
        submitted_time = current_time,
        order_id = as.integer(event_count),
        order_size = ..order_size,
        order_price = p
      )]

      ask_order_book[1:next_ask_slot, names(ask_order_book) := data.table::setorderv(ask_order_book[1:next_ask_slot],
        c("order_price", "submitted_time"),
        c(1, 1),
        na.last = TRUE
      )]

      next_ask_slot <- next_ask_slot + 1
    } else if (current_order$order_type == 3) {
      order_type <- "market_buy"
      # record the order information in the main record
      # initially price is set to NA as the price will be determined when the order is filled
      order_record[event_count, ":="(
        submitted_time = current_time,
        order_id = as.integer(event_count),
        order_type = ..order_type,
        order_size = as.integer(..order_size),
        order_price = NA_integer_
      )]
      if (best_ask != 0) {
        # fill order
        updated_info <- marketsimulatr::fill_order(
          current_order,
          ask_order_book,
          order_record,
          current_price,
          tick_size,
          next_ask_slot,
          mm_sell
        )
        # update current_price and order book position
        current_price <- updated_info$updated_price
        next_ask_slot <- updated_info$next_order_position
        if (!exists("price_series")) {
          price_series <- xts::xts(current_price, order.by = current_time)
          colnames(price_series) <- "traded_price"
        } else {
          price_series <- rbind(
            price_series,
            xts::xts(current_price, order.by = current_time)
          )
        }
      } else {
        # order cannot be filled from order book - Market maker intervenes
        message("Market maker match market buy.")
        order_record[event_count, ":="(
          fill_price = list(mm_sell),
          order_price = mm_sell,
          fill_id = list(0)
        )]
        # update current_price
        current_price <- mm_sell
      }
    } else if (current_order$order_type == 4) {
      order_type <- "market_sell"
      # update order_record
      order_record[event_count, ":="(
        submitted_time = current_time,
        order_id = as.integer(event_count),
        order_type = ..order_type,
        order_size = as.integer(..order_size),
        order_price = NA_integer_
      )]

      if (best_bid != 0) {
        # fill order
        updated_info <- marketsimulatr::fill_order(
          current_order,
          bid_order_book,
          order_record,
          current_price,
          tick_size,
          next_bid_slot,
          mm_buy
        )
        # update current_price and next available limit order book space
        current_price <- updated_info$updated_price
        next_bid_slot <- updated_info$next_order_position

        # record price_series
        if (!exists("price_series")) {
          price_series <- xts::xts(current_price, order.by = current_time)
          colnames(price_series) <- "traded_price"
        } else {
          price_series <- rbind(
            price_series,
            xts::xts(current_price, order.by = current_time)
          )
        }
      } else {
        message("Market maker match market sell.")
        # update order_record
        order_record[event_count, ":="(
          fill_price = list(mm_buy),
          order_price = mm_buy,
          fill_id = list(0)
        )]
        # update current_price
        current_price <- mm_buy
      }
    } else if (current_order$order_type == 5) {
      order_type <- "cancel_bid"
      # if orders exist in the book, select one at random and cancel.
      if (next_bid_slot > 1) {
        number_orders <- next_bid_slot - 1
        cancel_number <- sample(number_orders, 1)
        cancelled_order <- bid_order_book[cancel_number]
        cancelled_id <- cancelled_order$order_id
        # update order_record
        order_record[event_count, ":="(
          submitted_time = current_time,
          order_id = as.integer(event_count),
          order_type = ..order_type,
          order_size = NA_integer_,
          order_price = NA_real_,
          order_distance = NA_integer_
        )]
        # add cancellation time and update next available order book position
        order_record[order_id == cancelled_id, ":="(cancel_time = current_time)]
        next_bid_slot <-
          marketsimulatr::remove_and_update(bid_order_book, cancel_number, next_bid_slot)
      } else {
        event_count <- event_count - 1
        # next
      }
    } else if (current_order$order_type == 6) {
      order_type <- "cancel_ask"
      # if orders exist in the book, select one at random and cancel.
      if (next_ask_slot > 1) {
        number_orders <- next_ask_slot - 1
        cancel_number <- sample(number_orders, 1)
        cancelled_order <- ask_order_book[cancel_number]
        cancelled_id <- cancelled_order$order_id

        # update order_record
        order_record[event_count, ":="(
          submitted_time = current_time,
          order_id = as.integer(event_count),
          order_type = ..order_type,
          order_size = NA_integer_,
          order_price = NA_real_,
          order_distance = NA_integer_
        )]
        # add cancellation time and update next available order book position
        order_record[order_id == cancelled_id, ":="(cancel_time = current_time)]
        next_ask_slot <-
          marketsimulatr::remove_and_update(ask_order_book, cancel_number, next_ask_slot)
      } else {
        event_count <- event_count - 1
        # next
      }
    } else {
      break
    }

    # update best bid and best ask prices
    if (next_bid_slot == 1) {
      best_bid <- 0
    }
    else {
      best_bid <- bid_order_book$order_price[1]
    }

    if (next_ask_slot == 1) {
      best_ask <- 0
    }
    else {
      best_ask <- ask_order_book$order_price[1]
    }
  }

  # remove NA rows from orderbooks, plot price_series, and bundle everything together
  ask_order_book <- ask_order_book[!is.na(submitted_time)]
  bid_order_book <- bid_order_book[!is.na(submitted_time)]
  quantmod::chartSeries(price_series, main = "Simulated Exchange Rate")
  return.list <- list(
    "price_series" = price_series,
    "order_record" = order_record,
    "bid_order_book" = bid_order_book,
    "ask_order_book" = ask_order_book
  )
  return(return.list)
}

#' create_orders
#'
#' Generate orders based on roulette wheel selection.
#'
#' @param time_stamp The time of the order
#' @param order_number The order number (i.e. the event number in the simulation loop)
#' @param market_sell The arrival intensity of market sell orders.
#' @param market_buy The arrival intensity of market buy orders.
#' @param limit_ask The arrival intensity of limit ask orders.
#' @param limit_bid The arrival intensity of limit bid orders.
#' @param cancel_bid The cancellation intensity of limit bid orders.
#' @param cancel_ask The cancellation intensity of limit ask orders.
#'
#' This function creates orders. The order type is based on roulette wheel selection,
#' where the "sizes" of the roulette wheel segments are the intensity rates of the the
#' order types.
#'
#' These intensity rate is fed into a Poission distribution to create the timestamp of the order.
#'
#' The order size is created as a random multiple of some number (e.g. all order sizes are multiples of 50)
#'
#' @return
#' @export
#'
create_orders <- function(time_stamp,
                          order_number,
                          market_sell,
                          market_buy,
                          limit_ask,
                          limit_bid,
                          cancel_bid,
                          cancel_ask) {
  # This function creates orders randomly.
  # Each type of order is generated using a Poissonian arrival time with a rate specific to the order type

  # Implementation based on:
  # Ergodic Transition in a Simple Model of the Continuous Double Auction - Radivojevic et  al., PLoS, 2014
  # Statistical Theory of the Continuous Double Auction - Smith et al., Quantitative Finance, 2003
  # Quantitative Model of Price Diffusion and Market Friction Based on Trading as a Mechanistic Random Process - Daniels et al., Physical Review Letter   # , 2003
  # A Stochastic Model for Order Book Dynamics - Cont et al., Operations Research, 2010

  # these lines and the if-else block perform roulette wheel selection for order types
  c <-
    market_sell + market_buy + limit_ask + limit_bid + cancel_bid + cancel_ask
  x <- runif(1)

  order_type <- numeric(length = 1)
  order_number <- order_number
  order_size <- numeric(length = 1)

  # TODO change the numeric order_type to something a bit more sensible.
  if (x < limit_bid / c) {
    order_type <- 1
    t <- -log(runif(1)) / c
  } else if (x < (limit_bid + limit_ask) / c) {
    order_type <- 2
    t <- -log(runif(1)) / c
  } else if (x < (limit_bid + limit_ask + market_buy) / c) {
    order_type <- 3
    t <- -log(runif(1)) / c
  } else if (x < (limit_bid + limit_ask + market_buy + market_sell) / c) {
    order_type <- 4
    t <- -log(runif(1)) / c
  } else if (x < (limit_bid + limit_ask + market_buy + market_sell + cancel_bid) / c) {
    order_type <- 5
    t <- -log(runif(1)) / c
  } else {
    order_type <- 6
    t <- -log(runif(1)) / c
  }
  t <- time_stamp + t * 60

  # randomly create order size in 50,100,...,1000 chunks.
  # If cancellation order, set amount to Infinity (we will randomly cancel an order, regardless of size)
  min_order <- config::get("min_order_size")
  max_order <- config::get("max_order_size")
  order_step <- config::get("order_step_size")

  if (order_type <= 4) {
    order_size <- seq(min_order, max_order, order_step)
    rand_order <- sample(length(order_size), 1)
    order_size <- order_size[rand_order]
  } else {
    order_size <- Inf # if a cancel order, set order size to Inf
  }
  order_row <-
    xts::xts(cbind(order_number, order_type, order_size), order.by = t)
  colnames(order_row) <- c("order_id", "order_type", "order_size")
  return(order_row)
}

#' remove_and_update
#'
#' Remove orders from the limit books and update the available market depth.
#'
#' @param order_book The book to be updated
#' @param remove_rows The orders to be removed (because order is canceller or order has been used to fill an order)
#' @param next_slot The next orders in the book to be shifted upwards once the required orders are removed.
#'
#' @return
#' @export
remove_and_update <- function(order_book, remove_rows, next_slot) {
  # number of orders to remove
  num_orders <- length(remove_rows)
  # removing single or multiple orders

  if (num_orders == 1) {
    # is there only one order in the book anyway?
    if (remove_rows == 1 & next_slot == 2) {
      # if only one order in book, just set first row to NA
      order_book[1, ":="(
        submitted_time = NA_real_,
        order_id = NA_integer_,
        order_size = NA_integer_,
        order_price = NA_real_
      )]
      next_slot <- 1
      return(next_slot)
    } else if (remove_rows == (next_slot - 1)) {
      # if removing last order in book, just convert this row to NA (no need to move any other rows)
      order_book[remove_rows, ":="(
        submitted_time = NA_real_,
        order_id = NA_integer_,
        order_size = NA_integer_,
        order_price = NA_real_
      )]
      # there is one less non-NA row, so update next_slot value
      next_slot <- next_slot - 1
      return(next_slot)
    } else {
      num_to_move <- (next_slot - 1) - (remove_rows)
      order_book[
        remove_rows:(remove_rows + (num_to_move - 1)),
        names(order_book) := order_book[(remove_rows + 1):(next_slot - 1)]
      ]
      order_book[(next_slot - 1), ":="(
        submitted_time = NA_real_,
        order_id = NA_integer_,
        order_size = NA_integer_,
        order_price = NA_real_
      )]
      next_slot <- next_slot - 1
      return(next_slot)
    }
  } else {
    # removing multiple rows here
    if (num_orders == (next_slot - 1)) {
      order_book[remove_rows, names(order_book) := data.table::data.table(
        submitted_time = .POSIXct(rep(NA_real_, length = num_orders)),
        order_id = rep(NA_integer_, length = num_orders),
        order_size = rep(NA_integer_, length = num_orders),
        order_price = rep(NA_real_, length = num_orders)
      )]
      next_slot <- next_slot - num_orders
      return(next_slot)
    } else {
      remaining.orders <- (remove_rows[num_orders] + 1):(next_slot - 1)
      num_to_move <- length(remaining.orders)

      # shift values around
      order_book[
        remove_rows[1]:(remove_rows[1] + (num_to_move - 1)),
        names(order_book) := order_book[remaining.orders]
      ]

      # replace moved rows with NAs
      na.rows <- (next_slot - num_orders):((next_slot) - 1)
      order_book[na.rows, names(order_book) := data.table::data.table(
        submitted_time = .POSIXct(rep(NA_real_, length = num_orders)),
        order_id = rep(NA_integer_, length = num_orders),
        order_size = rep(NA_integer_, length = num_orders),
        order_price = rep(NA_real_, length = num_orders)
      )]
      next_slot <- next_slot - num_orders
      return(next_slot)
    }
  }
}

#' fill_order
#'
#' Fill orders once available market depth is available.
#'
#' @param market_order The order to be filled.
#' @param order_book The order book
#' @param record The trading record (which is tracking the traded prices)
#' @param current_price The current trading price
#' @param tick_size The minimum tick size allowed
#' @param next_order_position The position of the next order in the book
#' @param mm_price THe market makers price
#'
#' @return
#' @export
fill_order <- function(market_order,
                       order_book,
                       record,
                       current_price,
                       tick_size,
                       next_order_position,
                       mm_price) {
  current_id <- as.integer(market_order$order_id)
  current_size <- as.integer(market_order$order_size)

  # if the current order can be filled with the available book depth
  if (current_size <= sum(order_book$order_size, na.rm = TRUE)) {
    # find how many of the current limit orders are needed to fill the current order
    num_orders <- 1
    while (sum(order_book$order_size[1:num_orders]) < current_size) {
      num_orders <- num_orders + 1
    }
    # extract the orders we need to fill the current order
    filled_orders <- order_book[1:num_orders, ]

    filled_order_id <- list(filled_orders$order_id)
    partial_filled_id <- NA_integer_

    # if the current order size matches the current book depth exactly, the transaction price
    # is simply the weighted average of the available prices and limit order amounts
    if (sum(filled_orders$order_size) == current_size) {
      # the transaction price will be the weighted average of the limit orders used to fill the market order (rounded to nearest tick)
      market_price <- plyr::round_any(
        weighted.mean(filled_orders$order_price, filled_orders$order_size),
        tick_size
      )

      # remove orders from book as they have now been filled
      next_order_position <-
        marketsimulatr::remove_and_update(order_book, 1:num_orders, next_order_position)
    } else {
      # need to find out how many shares from "worst" limit order are used in order fill/left over after fill.
      surplus <- sum(filled_orders$order_size) - current_size
      # calculate the number of shares to be used in order
      # filled_orders[num_orders]$order_size = filled_orders[num_orders]$order_size - surplus
      filled_orders[num_orders, ":="(order_size = filled_orders[num_orders, order_size] - surplus)]

      # calculate weighted average as current price
      market_price <- plyr::round_any(
        weighted.mean(filled_orders$order_price, filled_orders$order_size),
        tick_size
      )

      # remove the filled orders from the book, apart from the last order (still has some remaining volume). It should now
      # be at the top of the book, but with a new quantity (the surplus)
      # if only one order was needed to fill the order (and in this case there will be a surplus left over)
      if (num_orders == 1) {
        # limit order has not been fully filled
        filled_order_id <- NULL
        # limit order has been partially filled
        partial_filled_id <- as.integer(filled_orders$order_id)
        # update the remaining volume of the limit order
        order_book[1, ":="(order_size = surplus)]
      } else {
        # if any orders were filled add their IDs to a list
        filled_order_id <-
          list(filled_orders[1:(num_orders - 1)]$order_id)
        # get the ID of the partially filled order
        partial_filled_id <-
          as.integer(filled_orders[num_orders]$order_id)
        next_order_position <-
          marketsimulatr::remove_and_update(order_book, 1:(num_orders -
            1), next_order_position)
        # update the available volume of the remaining best order
        order_book[1, ":="(order_size = surplus)]
      }
    }

    # update the trading record to reflect order fill times/ partial fill times and IDs
    # update the market order that was just filled
    # fields that need updated
    record[order_id == current_id, ":="(
      order_price = market_price,
      fill_price = list(filled_orders$order_price),
      fill_id = list(filled_orders$order_id)
    )]

    # update the limit orders that filled the market order
    if (!is.null(filled_order_id)) {
      fill_time <- zoo::index(market_order)
      for (i in unlist(filled_order_id))
      {
        record[order_id == as.integer(i), ":="(fill_time = fill_time)]
      }
    }

    if (!is.null(partial_filled_id)) {
      fill_time <- zoo::index(market_order)
      record[order_id == as.integer(partial_filled_id), ":="(first_fill_time = fill_time)]
    }

    # return the relevant values to update main simulation loop
    return_list <- list(
      updated_price = market_price,
      next_order_position = next_order_position
    )
    return(return_list)
  } else {
    message(
      "Not enough volume. Market maker intervenes to fill order.",
      "\n"
    )
    available_orders <- order_book[1:(next_order_position - 1)]
    available_volume <- sum(available_orders$order_size)
    required_volume <-
      as.integer(market_order$order_size) - available_volume
    # "add" a new limit order at Market maker's rate to fill the market order
    available_orders <-
      rbind(
        available_orders,
        list(
          zoo::index(market_order),
          0,
          as.integer(required_volume),
          mm_price
        )
      )
    market_price <- plyr::round_any(
      weighted.mean(
        available_orders$order_price,
        available_orders$order_size
      ),
      tick_size
    )

    # update the market order in the overall record
    record[order_id == current_id, ":="(
      order_price = market_price,
      fill_price = list(available_orders$order_price),
      fill_id = list(available_orders$order_id)
    )]

    # update the limit orders in the overall record (fill times)
    fill_time <- zoo::index(market_order)
    for (i in available_orders$order_id[1:(next_order_position - 1)])
    {
      record[order_id == as.integer(i), ":="(fill_time = fill_time)]
    }

    # clear the order book
    next_order_position <-
      marketsimulatr::remove_and_update(
        order_book,
        1:(next_order_position - 1),
        next_order_position
      )

    return_list <- list(
      updated_price = market_price,
      next_order_position = next_order_position
    )
    return(return_list)
  }
}
