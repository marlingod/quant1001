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
Sys.setenv(TZ = "UTC")
currency('USD')
init_date <- "2007-12-31"
start_date <- "2008-01-01"
end_date <- "2009-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE

.fast = 10
.slow = 30
##### get the symbols#####
symbols <-c("IWM")
getSymbols(Symbols = symbols, 
           src = "google", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)
stock(symbols, currency = "USD", multipler=1)

## porfolio
rm.strat(portfolio.st)
rm.strat(account.st)
portfolio.st <- "Luxor_port1"
account.st <- "Luxor_Acct1"
strategy.st <- "Luxor_strat1"

initPortf(portfolio.st, symbols=symbols, currency='USD')
initAcct(account.st, portfolios=portfolio.st, currency='USD')

### quantstrat

initOrders(portfolio.st)

### define strategy

strategy(strategy.st, store=TRUE)

### indicators

add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

### signals

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

### rules

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        #TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        #TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', prefer='High', #threshold=.threshold,
                        orderqty='all',
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low', #threshold=-.threshold,
                        orderqty='all',
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

###############################################################################

applyStrategy(strategy.st, portfolio.st)

print(getOrderBook(portfolio.st)[[portfolio.st]]$IWM)
View(mktdata)


updatePortf(portfolio.st, Symbols=symbols)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
chart.Posn(portfolio.st)
