# ARIMA Models  ###########
install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(readxl)
# Converting data into time series object
# Loading  Data

plastic<-read.csv("E://Assignment//forecasting//plastic.csv")
View(plastic)
pla<-ts(plastic$Sales,frequency = 12,start = c(49))
View(pla)
plot(pla)
# dividing entire data into training and testing data 
train=pla[1:48]
test=pla[49:60]

# converting time series object
trainn<-ts(train,frequency = 12)
testt<-ts(test,frequency = 12)
plot(train)
acf(train)
pacf(train)
# Auto.Arima model on the price agg data 
library(forecast)
model_AA <- auto.arima(trainn) 
m<-arima(train,order = c(0,1,0))
model_AA
plot(m)
m

acf(model_AA$residuals)
windows()
plot(forecast(model_AA,h=12),xaxt="n") #xaxt- x axis text 

# Forecasted values for the next 12 quarters
forecast_new <- data.frame(forecast(model_AA,h=12))
forecast_new
