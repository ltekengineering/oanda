% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oanda.r
\name{oanda.orders.createStopLossOrder}
\alias{oanda.orders.createStopLossOrder}
\title{Create a stop loss order}
\usage{
oanda.orders.createStopLossOrder(
  account,
  tradeID,
  price,
  distance,
  clientExtensions
)
}
\arguments{
\item{account}{The string representation of an Account Identifier.}

\item{tradeID}{The ID of the Trade to close when the price threshold is breached.}

\item{price}{The price threshold specified for the Limit Order. The Limit Order will only be filled by a market price that is equal to or better than this price.}

\item{distance}{Specifies the distance (in price units) from the Trade’s open price to use as the Stop Loss Order price. Only one of the distance and price fields may be specified.}

\item{clientExtensions}{The Client Extensions to add to the Take Profit Order when created. This is optional.}
}
\value{
R list object detailing the response from the server.
}
\description{
A StopLossOrderRequest specifies the parameters that may be set when creating a Stop Loss Order. Only one of the price and distance fields may be specified.
}
\examples{
oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", price = 1.32821)
info <- oanda.orders.createMarketIfTouchedOrder("A comment","strategy_9","my_order_10000")
oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", price = 1.32821, clientExtensions = info)
oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", distance = 0.02821)
oanda.orders.createStopLossOrder('101-001-3704066-001', "6368", distance = 0.02821, clientExtensions = info)
}
