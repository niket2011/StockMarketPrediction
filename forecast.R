setwd( "C:/Users/parek/Downloads")
mydata<-read.csv("Gold Futures Historical Data.csv")
mydata
library(MASS)
library(tseries)
library(forecast)
col1 = 1;
col2 = 2;
data = mydata[c(col1, col2)];
data
data$Price <- as.numeric(gsub(",", "", data$Price))
data$Price
stock=log(data$Price[132:33])
stock
plot(data)
pricearima=ts(stock,start=c(2008,12),frequency = 12)
fitstock=auto.arima(pricearima)
fitstock
auto.arima(pricearima,ic='aic',trace=TRUE)
plot(pricearima,type='l')
title='Gold Price'
exp(stock)
myforecast=forecast(fitstock,h=33)
plot(myforecast)
myforecast
forecastedvalues=as.numeric(myforecast$mean)
finalforecastedvalues=exp(forecastedvalues)
finalforecastedvalues
dataerror=data.frame(data$Price[1:33],finalforecastedvalues)
col_headings=c("Actual Price","Forecasted Price")
names(dataerror)=col_headings
attach(dataerror)
(dataerror)
percentage_error=((dataerror$`Actual Price`-dataerror$`Forecasted Price`)/(dataerror$`Actual Price`))
percentage_error
mean(percentage_error)
plot(dataerror$`Actual Price`, type="l")
lines(dataerror$`Forecasted Price`, col="red")
