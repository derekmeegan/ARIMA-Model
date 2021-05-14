library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(forecast)

arima <- function(symbol, start, end, forecastLength) {
  getSymbols(symbol, src = "yahoo", from = start, to = end, auto.assign = FALSE) %>% 
    Cl() %>% 
    as.ts() %>% 
    auto.arima()  %>% 
    forecast(h=forecastLength) %>% 
    autoplot(include=150)+
    theme_bw()
}

symbol <- "spy"
start <- "2020-05-14"
end <- Sys.Date() + 1
forecastLength <- 50

arima(symbol, start, end, forecastLength)
