if(!require(forecast)) install.packages("forecast"); library(forecast) 

sarima <- read_csv2("sarima_r.csv")

ts_dengue <- ts(data = sarima$casos, start = c(2012), frequency = 12)

ts.plot(ts_dengue)

auto.arima(ts_dengue)

prev <- Arima(ts_dengue, order = c(0,0,2), seasonal = c(0,0,1))

previsao <- forecast(prev, h=12)

plot(previsao)

previsao

summary(prev)
