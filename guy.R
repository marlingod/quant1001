library(knitr)
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

library(PerformanceAnalytics)
library(quantmod)
library(lattice)

startDate <- '2010-01-01'  # start of data
endDate <-  '2015-05-01'   # end of data
Sys.setenv(TZ="UTC")       # set time zone
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")


getSymbols('IWM', src="google")


if(file.exists("XLX.RData"))
{
  load("XLX.RData")
} else {
  getSymbols(symbols, from=startDate, to=endDate, index.class="POSIXct", src="google")
  for(symbol in symbols) {
    x<-get(symbol)
    x<-adjustOHLC(x,symbol.name=symbol)
    x<-to.weekly(x,indexAt='lastof',drop.time=TRUE)
    indexFormat(x)<-'%Y-%m-%d'
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
  }
  save(list=symbols,file="XLX.RData")
}

prices <- NULL
for(i in 1:length(symbols))
  prices <- cbind(prices,Cl(get(symbols[i])))
colnames(prices) <- symbols
returns <- diff(log(prices))[-1, ]
num.ass <- ncol(returns)

xyplot(prices, xlab = "", layout = c(3, 3),type=c("l","g"))

stacked.df <- stack(as.data.frame(returns))
colnames(stacked.df) <- c("returns", "symbol")

densityplot(~returns | symbol, stacked.df, cex = 0.25, xlab="",type=c("l","g"))


#####################################
args(BBands)
b <- BBands(HLC=HLC(XLF["2013"]), n=20, sd=2)
tail(b)
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart_Series(XLF,TA='add_BBands(lwd=2)',theme=myTheme,name="XLF")

#######################################
initDate <- '2009-12-31'
initEq <- 1e6
currency("USD")
stock(symbols, currency="USD", multiplier=1)

## ----results='hide'------------------------------------------------------
rm.strat("multiAsset.bb1") # remove portfolio, account, orderbook if re-run
initPortf(name="multiAsset.bb1", symbols, initDate=initDate)
initAcct(name="multiAsset.bb1", portfolios="multiAsset.bb1",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb1", initDate=initDate)
strategy("bbands", store=TRUE)


#############################
args(BBands)

## ----results='hide'------------------------------------------------------
add.indicator("bbands", name = "BBands",
              arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='bbInd')

## ------------------------------------------------------------------------
args(add.signal)

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
           arguments=list(columns=c("High","up"),relationship="gt"),
           label="H.gt.UpperBand")

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
           arguments=list(columns=c("Low","dn"),relationship="lt"),
           label="L.lt.LowerBand")

## ------------------------------------------------------------------------
args(add.rule)

## ------------------------------------------------------------------------
args(ruleSignal)

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
         arguments=list(sigcol="H.gt.UpperBand",sigval=TRUE,
                        orderqty=+100, ordertype='market', orderside='long'),
         type='enter',
         label='LongEntry')

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
         arguments=list(sigcol="L.lt.LowerBand",sigval=TRUE,
                        orderqty= 'all', ordertype='market', orderside='long'),
         type='exit',
         label='LongExit')

## ------------------------------------------------------------------------
args(applyStrategy)
nSD = 2
nMA = 20

## ----results='hide'------------------------------------------------------
out <- applyStrategy("bbands",
                     portfolios="multiAsset.bb1",parameters=list(sd=nSD,n=nMA))