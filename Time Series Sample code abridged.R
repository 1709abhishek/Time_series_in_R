data("AirPassengers")

cycle(AirPassengers)

plot(aggregate(AirPassengers,FUN=mean))

boxplot(AirPassengers~cycle(AirPassengers))



#If the data must be collected over time, such as stock prices over a ten-year
#period, the independence condition may be a big problem because the data
#from the previous time period may be related to the data from the next time
#period. This kind of data requires time series analysis

#data is stationary: 
#1. mean is constant
#2. variance is constant

#dickey Fuller Test
#Types of time series
# general trend
# seasonal
# Irregular fluctuation




#what is time series data. 

install.packages('tseries')
install.packages('forecast')

library(tseries)
library(forecast)

setwd("C:/Users/Chakraborty Sandeep/Desktop/My Douments/coaching/R/Time Series")
calls <- read.csv("data_for_forecasting.csv")

View(calls)

View(sort(calls$callsoffered))
#step1 plot series
plot(calls$callsoffered, type="l")


#build baseline
forecast1 <- meanf(calls$callsoffered,3)
forecast2 <- naive(calls$callsoffered,3)
plot(forecast1)
plot(forecast2)


summary(forecast1)
summary(forecast2)

#mean error (ME), 
#root mean squared error (RMSE), 
#mean absolute error (MAE), 
#mean percentage error (MPE), 
#mean absolute percentage error (MAPE), 
#mean absolute scaled error (MASE)


accuracy(forecast1)
accuracy(forecast2)

#define interval of forecast 
#For 80%- predvalue +- (1.28*stddev of residuals)
#For 95%- predvalue +- (1.96*stddev of residuals)


#use decomposition to studying time series data, and 
#exploring the historical changes over time
#STL means Seasonal Trend Decomposition using Loess,The 
#two main parameters to be chosen when using STL are the 
#trend window (t.window) and seasonal window (s.window). 
#These control how rapidly the trend and seasonal components 
#can change. Small values allow more rapid change.
calls_ts <- ts(calls[,2],start=c(2014,1),freq=7)
plot.ts(calls_ts)
decom <- stl(calls_ts, s.window="periodic")
plot(decom)
head(calls_ts,100)



#HOLT WINTERS TECHNIQUE
hws<-HoltWinters(calls_ts)

sales.pred <- predict(hws,n.ahead=12,prediction.interval=T)

hws

#Predicted values
sales.pred

#Plot the base level graph, giving limits to x
#par(mfrow = c(1,1)) 
plot.ts(calls_ts)

#Historical Fitted values, now third column is trend. Xhat is prediction
hws$fitted

#Fit the historical fitted values
lines(hws$fitted[,1],col="green")

#Fit the future predicted values
lines(sales.pred[,1],col="blue")

#Fit the upper interval predicted values
lines(sales.pred[,2],col="red")

#Fit the upper interval predicted values
lines(sales.pred[,3],col="red")

#MAPE calculation

act <- calls_ts[-c(1:12)]
prd <- hws$fitted[,1]
mean(abs((act-prd)/act))

#univariate seasonal forecasting using arima 
fit <- auto.arima(calls$callsoffered,seasonal=TRUE)
summary(fit)

accuracy(forecast(fit))
plot(forecast(fit,h=10),include=80)
#do not get bothered by the 'unable to fit' message as it 
#just means that the algorithm based on 
#Hyndman and Khandakar algorithm tires several 
#combination of orders and one of them might not 
#be converging to a solution drift is similar to 
#having a constant in the equation. 
#Here it signifies that after the diffrencing 
#the mean of series is not 0 so drift adjusts for it.

#run dynamic regression models
#build a few effects


#Need to go through the requirement over here???

weekdays(c(1,2,3,4,5))
a <-  calls$Date
class(a)
b <- as.Date(calls$Date,format="%d-%b-%y")
class(b)

as.Date(calls$Date,format="%d-%b-%y")

calls[,"dow"] <-  weekdays(as.Date(calls$Date,format="%d-%b-%y"))
calls[,"mon_fl"] <- ifelse(calls[,"dow"] == "Monday" , 1,0)

View(calls)


#use them in arima
fit <- Arima(calls$callsoffered, xreg=calls$mon_fl,order=c(4,1,5))
summary(fit)



