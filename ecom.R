library(tseries)
library(ggplot2)
library(forecast)
library(ggplot2)
library(lmtest)
library(fpp2)
library(xts)
library(zoo)
#creating a time series object in r.
ecom<-read.csv.zoo("eComm_US.csv")
ecom_ts<-ts(ecom, start=c(1999, 4), frequency=4)
is.ts(ecom_ts)
str(ecom_ts)
class(ecom_ts)
plot(ecom_ts , main ='Sales of the ecomm company'
     , ylab='Sales', xlab = 'Years')

autoplot(ecom_ts)

ggtsdisplay(ecom_ts)

#Naive model for forecasting 

ecom_ts_naive <- snaive(ecom_ts, h=3)
plot(ecom_ts_naive)
autoplot(ecom_ts_naive
         , ylab='Sales', xlab = 'Years')
summary(ecom_ts_naive)
checkresiduals(ecom_ts_naive)


accuracy(forecst)
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  ecom_holt_tune<- hw(ecom_ts, alpha = alpha[i], h = 3)
  RMSE[i] <- accuracy(ecom_holt_tune)[2]
}
print(RMSE)
plot(alpha, RMSE)


ecom_holt <- hw(ecom_ts,alpha= 0.4,h=3)
autoplot(ecom_holt)
accuracy(ecom_holt) 
summary(ecom_holt)
predict(ecom_holt)
checkresiduals(ecom_holt)

# Sarima Model 

ecom_log = log(ecom_ts)

BoxCox.lambda(ecom_log)

adf.test(ecom_ts, alternative="stationary")

ecom_ts_diff = diff(ecom_log,lag =4,differences = 1)
adf.test(ecom_log, alternative="stationary")

y = diff(diff(ecom_log,lag =4,differences = 1))

acf(y)    #q=0

#Autocorrealtion 
pacf(y)  
oats_def_mod <- (auto.arima(ecom_log, seasonal = TRUE))
oats_def_mod

oats_mod <- arima(ecom_log, order = c(0,2,0), seasonal = list(order = c(1,2,0), period = 4))
oats_mod
forecast(oats_def_mod, h=3)
accuracy(oats_def_mod)
autoplot(forecast(oats_def_mod, h=3))

MAPE =10^0.2685803
MAPE
checkresiduals(oats_def_mod)

forecast(oats_mod, h=3)
accuracy(oats_mod)
autoplot(forecast(oats_mod, h=3))

MAPE =10^0.2685803
MAPE
checkresiduals(oats_mod)


