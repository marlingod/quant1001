rm(list=ls())
library(blotter)
library(quantmod)
library(TTR)
library(quantstrat)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(lattice)
library(pander)
library(data.table)

Sys.setenv(TZ = "UTC")
currency('USD')
init_date <- "2007-12-31"
start_date <- "2008-01-01"
end_date <- "2009-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE

library(parallel)

if( Sys.info()['sysname'] == "Windows") {
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

checkBlotterUpdate <- function(port.st = portfolio.st, 
                               account.st = account.st, 
                               verbose = TRUE) {
  
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(
    sapply(
      syms, 
      FUN = function(x) eval(
        parse(
          text = paste("sum(p$symbols", 
                       x, 
                       "posPL.USD$Net.Trading.PL)", 
                       sep = "$")))))
  
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  
  if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
  }
  
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  
  if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match account P&L")
  }
  
  if(sum(duplicated(index(p$summary)))) {
    ok <- FALSE
    if(verbose)print("duplicate timestamps in portfolio summary")
    
  }
  
  if(sum(duplicated(index(a$summary)))) {
    ok <- FALSE
    if(verbose) print("duplicate timestamps in account summary")
  }
  return(ok)
}

symbols <-c("IWM")
getSymbols(Symbols = symbols, 
           src = "google", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)
stock(symbols, currency = "USD", multipler=1)

.fast <- 10
.slow <- 30
.threshold <- 0.0005
.orderqty <- 100
.txnfees <- -10
#.stoploss <- 3e-3 # 0.003 or 0.3%
.StopLoss = seq(0.05, 0.6, length.out = 48)/100

portfolio.st <- "Quantstrat"
account.st <- "Strategies"
strategy.st <- "MACD.TS"

rm.strat(portfolio.st)
rm.strat(account.st)

trailingStopPercent <- 0.07
trade_size <- init_equity/length(symbols)

rm.strat(portfolio.st)
rm.strat(account.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
## [1] "Quantstrat"
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
## [1] "Strategies"
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE)

osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...) {
  if(!exists("trade_size")) stop("You must set trade_size")
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(trade_size/ClosePrice,-2)
  return(orderqty)
}

add.indicator(strategy = strategy.st,
             name = "MACD",
             arguments = list(x = quote(Cl(mktdata))),
             label = "osc")

add.signal(strategy = strategy.st,
           name="sigThreshold",
           arguments = list(column ="signal.osc", 
                            relationshipo = "gt", 
                            threshold = 0, 
                            cross = TRUE), 
           label = "signal.gt.zero")
## [1] "MACD.TS"
add.signal(strategy = strategy.st,
           name="sigThreshold",
           arguments = list(column = "signal.osc", 
                            relationship = "lt", 
                            threshold = 0, 
                            cross = TRUE), 
           label = "signal.lt.zero")

add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "signal.gt.zero",
                          sigval = TRUE,
                          orderqty = 100,
                          orderside = "long",
                          ordertype = "market",
                          osFUN = "osFixedDollar", 
                          orderset = "ocolong"),
         type = "enter",
         label = "LE")
## [1] "MACD.TS"
add.rule(strategy = strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "signal.lt.zero", 
                          sigval = TRUE, 
                          replace = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          orderset = "ocolong"), 
         type = "exit", 
         label = "LX")
## [1] "MACD.TS"
add.rule(strategy = strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "signal.gt.zero", 
                          sigval = TRUE, 
                          replace = FALSE, 
                          orderside = "long", 
                          ordertype = "stoptrailing", 
                          tmult = TRUE, 
                          threshold = quote(trailingStopPercent), 
                          orderqty = "all", 
                          orderset = "ocolong"), 
         type = "chain", 
         parent = "LE", 
         label = "StopTrailingLong", 
         enabled = FALSE)

enable.rule(strategy.st, type = "chain", label = "StopTrailingLong")