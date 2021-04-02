#
# # load simulation results
# load("CFDataset28Days1B1S0.2LB0.2LA0.8CB0.8CA120960Trades.Rdata")
#
# # store data
# order.record <- simulation.results$OrderRecord
# price.series <- simulation.results$PriceSeries
# setkey(order.record, OrderID, OrderType)
#
# # extract limit order book information
# limit.orders <- order.record[CJ(unique(OrderID), c("LimitBid", "LimitAsk"),
#   sorted =
#     FALSE
# ), nomatch = 0]
#
# # calculate fill times - rounded to nearest minute (ceiling)
# limit.orders[, c("TimeToFirst", "TimeToFill", "TimeToCancel") :=
#   list(
#     ceiling(as.numeric(FirstFillTime - SubmittedTime) /
#       60),
#     ceiling(as.numeric(FillTime - SubmittedTime) / 60),
#     ceiling(as.numeric(CancelTime - SubmittedTime) / 60)
#   )]
#
# # replace NA times with infinities
# for (j in seq_len(ncol(limit.orders))) {
#   set(limit.orders, which(is.na(limit.orders[[j]])), j, Inf)
# }
#
# # extract limit bids and asks
# limit.bid <- limit.orders[CJ(unique(OrderID), c("LimitBid"),
#   sorted =
#     FALSE
# ), nomatch = 0]
# limit.ask <- limit.orders[CJ(unique(OrderID), c("LimitAsk"),
#   sorted =
#     FALSE
# ), nomatch = 0]
#
#
# # order execution - all orders
# fill.times <- sort(unique(limit.orders$TimeToFill), na.last = TRUE)
# order.dist <- sort(unique(limit.bid[, OrderDistance]))
# prob.measures.all <- CalculateMeasures(
#   order.book = limit.orders,
#   fill.times = fill.times
# )
