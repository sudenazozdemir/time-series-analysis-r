# TIME SERIES ANALYSIS OF USD/TRY EXCHANGE RATE
# Author: Sude Naz Özdemir

rm(list = ls())
cat("\014")
graphics.off()

library(tseries)
library(quantmod)
library(ggplot2)
library(strucchange)
library(urca)
# Importing USD/TRY exchange rate data

getSymbols("USDTRY=X", src = "yahoo")

usd_close <- Cl(`USDTRY=X`)
usd_close <- na.omit(usd_close)
# Plotting original series

plot(usd_close,
     main = "USD/TRY Exchange Rate",
     ylab = "Exchange Rate",
     xlab = "Time",
     col = "black")
# Augmented Dickey-Fuller Test (Level Series)

adf_level <- adf.test(usd_close)

adf_level
# First Difference Transformation

usd_diff <- diff(usd_close)

usd_diff <- na.omit(usd_diff)

# Plotting differenced series

plot(usd_diff,
     main = "Differenced USD/TRY Series",
     ylab = "Differenced Value",
     xlab = "Time",
     col = "blue")
# ADF Test for Differenced Series

adf_diff <- adf.test(usd_diff)

adf_diff
# ACF Plot

acf(usd_diff,
    main = "ACF of Differenced USD/TRY Series")
# PACF Plot

pacf(usd_diff,
     main = "PACF of Differenced USD/TRY Series")
# Structural Break Visualization

plot(usd_close,
     main = "USD/TRY Exchange Rate with Structural Break",
     ylab = "Exchange Rate",
     xlab = "Time",
     col = "black")

abline(v = as.Date("2021-12-01"),
       col = "red",
       lwd = 2,
       lty = 2)
# Structural Break Interpretation Note

break_date <- "2021-12"

cat("Potential structural break detected around:", break_date)
library(forecast)
# ARIMA Model Selection

arima_model <- auto.arima(usd_close)

summary(arima_model)
# Forecasting

forecast_result <- forecast(arima_model, h = 30)

plot(forecast_result,
     main = "USD/TRY Exchange Rate Forecast",
     ylab = "Exchange Rate",
     xlab = "Time")