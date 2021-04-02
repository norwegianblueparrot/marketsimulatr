# This code is based on some of the work presented in:
# Algorithmic Trading: Model of Execution Probability and Order Placement Strategy - Yingsaeree, Ph.D. Thesis, 2012
cat("\f")
devtools::load_all()

# select config
Sys.setenv("R_CONFIG_ACTIVE" = "test")

# current price
start_price <- config::get("start_price")
# upper limit on price
price_limit <- config::get("price_limit")
# number of ticks from price within which limit orders are placed
price_band <- config::get("price_band")
# minimum price increment
tick_size <- config::get("tick_size")
# buy orders per minute
market_buy <- config::get("market_buy")
# sell orders per minute
market_sell <- config::get("market_sell")
# ratio of bids to market sells
bid_order_ratio <- config::get("bid_order_ratio")
# ratio of asks to market buys
ask_order_ratio <- config::get("ask_order_ratio")
# proportion on limit bids cancelled
cancel_bid <- config::get("cancel_bid")
# proportion of limit asks cancelled
cancel_ask <- config::get("cancel_ask")
# days to simulate
days <- config::get("days")

# this gives us a rough figure for orders per minute (of all types)
total_orders <- ceiling(
  market_buy +
    market_sell +
    (market_buy * ask_order_ratio) +
    (market_sell * bid_order_ratio) +
    (market_buy * ask_order_ratio * cancel_ask) +
    (market_sell * bid_order_ratio * cancel_bid)
)

# number of minutes to simulate
n_minutes <- days * 1440 # days * number of minutes in one day

# number of orders to generate (number of minutes * number of orders per minute)
n_events <- n_minutes * total_orders

start_time <- Sys.time()

simulation.results <- marketsimulatr::create_order_book(
  start_price,
  price_limit,
  price_band,
  tick_size,
  market_sell,
  market_buy,
  bid_order_ratio,
  ask_order_ratio,
  cancel_bid,
  cancel_ask,
  n_events
)
end_time = Sys.time()
print(difftime(end_time, start_time))
