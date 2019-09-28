library(forecast)
library(xts)
library(readxl)

# Importing Data
excel_sheets("Demand Data.xlsx")
data = read_excel('Demand Data.xlsx')
data = ts(data[,2],start = c(2003,1),frequency = 12)

# We can see upward trend and seasonality in the demand
plot(data, xlab = 'Years', ylab = 'Demand')

# Removing Upward trend 
plot(diff(data), ylab = 'Differenced Demand')
# Removing variance
plot(log10(data), ylab = 'Log (Demand)')

par(mfrow = c(1,2))
plot(data, xlab = 'Years', ylab = 'Demand')
plot(diff(log10(data)), ylab = 'Differenced Log (Demand)')

# Calculating Autocorrelation factor and Partial autocorrelation factor 
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main = 'ACF Demand')
pacf(ts(diff(log10(data))),main = 'PACF Demand')

# ARIMA model using auto.arima
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation = FALSE, trace = FALSE)
summary(ARIMAfit)

# Predicting for next 3 years
par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)
pred
plot(data, type = 'l', xlim = c(2004,2018), ylim = c(1,1600), xlab = 'Year',
     ylab = 'Demand')
lines(10^(pred$pred), col = 'blue') # predicted value
lines(10^(pred$pred + 2*pred$se), col = 'red') # 95% CI
lines(10^(pred$pred - 2*pred$se), col = 'red') # 95% CI

# Checking ACF and PACF for ARIMA model to ensure no more information is available
par(mfrow = c(1,2))
acf(ts(ARIMAfit$residuals), main = 'ACF Residual')
pacf(ts(ARIMAfit$residuals), main = 'PACF Residual')
