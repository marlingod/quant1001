#Hello World
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
sessionInfo()

## set the time and the currency
Sys.setenv(TZ = "UTC")
currency('USD')
init_date <- "2007-12-31"
start_date <- "2008-01-01"
end_date <- "2009-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE

##### get the symbols#####
symbols <-c("IWM")
getSymbols(Symbols = symbols, 
           src = "google", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)
stock(symbols, currency = "USD", multipler=1)
##Create 30 mn \

IWMN <- to.minutes30(IWM)
IWMN <- align.time(IWMN, 1800)
head(IWMN)
length(IWM)
##creatre
portfolio.st <- "Port.Luxor.MA.Opt"
account.st <- "Acct.Luxor.MA.Opt"
strategy.st <- "Strat.Luxor.MA.Opt"

.fastSMA <- (1:30)
.slowSMA <- (20:80)
.nsamples <- 5

### Check blotter function from GUY
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
##

#remove presvious run (from quantstrat package)
rm.strat(portfolio.st)
rm.strat(account.st)

#initialize porfolio ( from blotter package)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

#initialize account ( from blotter package)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)

# Initializa orders (from quantstrat package)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

strategy(strategy.st, store = TRUE) ## quantstrat

### Add  Indicators ( All form quantstrat)
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 10),
              label = "nFast")

add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 30), 
              label = "nSlow")

##Add Signal
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")


### Add rules
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long", 
                          threshold = 0.0005,
                          prefer = "High", 
                          TxnFees = -10, 
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
#rule 2
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005, 
                          orderside = "short", 
                          replace = FALSE, 
                          TxnFees = -10, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")

#exits rules
add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "short", 
                          sigval = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2SHORT")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "long", 
                          sigval = TRUE, 
                          orderside = "short", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2LONG")



add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list(n = .fastSMA),
                 label = "nFAST")
## [1] "Strat.Luxor.MA.Opt"
add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list(n = .slowSMA),
                 label = "nSLOW")
add.distribution.constraint(strategy.st,
                            paramset.label = "SMA",
                            distribution.label.1 = "nFAST",
                            distribution.label.2 = "nSLOW",
                            operator = "<",
                            label = "SMA.Constraint")
library(parallel)

if( Sys.info()['sysname'] == "Windows") {
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}
#addPosLimit(portfolio.st, 'IWM', timestamp=init_date, maxpos=500, minpos=0)

setwd("C:\Users\Geo\Documents\malin\stock\data")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- apply.paramset(strategy.st,
                            paramset.label = "SMA",
                            portfolio.st = portfolio.st,
                            account.st = account.st, 
                            nsamples = .nsamples)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}