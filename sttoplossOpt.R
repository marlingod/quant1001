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

portfolio.st <- "Port.Luxor.Stop.Loss.Opt"
account.st <- "Acct.Luxor.Stop.Loss.Opt"
strategy.st <- "Luxor.Stop.Loss.Opt"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
## [1] "Port.Luxor.Stop.Loss"
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
## [1] "Acct.Luxor.Stop.Loss"
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .fast),
              label = "nFast")
## [1] "Strat.Luxor.Stop.Loss"
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .slow),
              label = "nSlow")

add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long"
)
## [1] "Strat.Luxor.Stop.Loss"
add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long" ,
                          ordertype = "stoplimit",
                          prefer = "High",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = +.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocolong"),
         type = "enter",
         label = "EnterLONG")
## [1] "Strat.Luxor.Stop.Loss"
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          prefer = "Low",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = -.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocoshort"),
         type = "enter",
         label = "EnterSHORT")
## [1] "Strat.Luxor.Stop.Loss"
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "long" ,
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "exit",
         label = "Exit2SHORT")
## [1] "Strat.Luxor.Stop.Loss"
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "exit",
         label = "Exit2LONG")
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain", 
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = FALSE)
## [1] "Strat.Luxor.Stop.Loss"
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain", 
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled = FALSE)

for(symbol in symbols){
  addPosLimit(portfolio = portfolio.st,
              symbol = symbol,
              timestamp = init_date,
              maxpos = .orderqty)
}

add.distribution(strategy.st,
                 paramset.label = "StopLoss",
                 component.type = "chain",
                 component.label = "StopLossLONG",
                 variable = list(threshold = .StopLoss),
                 label = "StopLossLONG")
## [1] "Luxor.Stop.Loss.Opt"
add.distribution(strategy.st,
                 paramset.label = "StopLoss",
                 component.type = "chain",
                 component.label = "StopLossSHORT",
                 variable = list(threshold = .StopLoss),
                 label = "StopLossSHORT")

add.distribution.constraint(strategy.st,
                            paramset.label = "StopLoss",
                            distribution.label.1 = "StopLossLONG",
                            distribution.label.2 = "StopLossSHORT",
                            operator = "==",
                            label = "StopLoss")

enable.rule(strategy.st, 'chain', 'StopLoss')

cwd <- getwd()
setwd("C:/Users/Geo/Documents/malin/stock/quant0527/data")
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- apply.paramset(strategy.st, 
                            paramset.label = "StopLoss", 
                            portfolio.st = portfolio.st, 
                            account.st = account.st, 
                            nsamples = .nsamples, 
                            verbose = TRUE)
  
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}