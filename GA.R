install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

mydata <- read.csv(file.choose())


train <- mydata[1:72,]
class(train)

test <- mydata[73:84,]
class(test)


datatrain <- ts(train, start = c(1987, 1), frequency = 12)
class(datatrain)
datatrain

kpss.test(sin(datatrain))
adf.test(diff(log(datatrain)))
plot(datatrain)

kpss.test(tan(datatrain))


acf(tan(datatrain))
pacf(tan(datatrain))

model <- arima(log(datatrain), c(0,1,0), seasonal = list(order=c(0,1,0), period=12))
pred <- forecast(model, 12)
pred$mean
predf <- 2.718^(pred$mean)
predf


datatest <- ts(test, start = c(1992, 1), frequency = 12)
datatest
datatrain

sum(predf)-sum(datatest)

MAPE <- mean((abs(sum(predf)-sum(datatest)))/sum(datatest))
MAPE

MAPE1 <- mean(abs(datatest-predf$mean)/datatest)


Box.test(model$residuals, type = "Ljung-Box")

plot(decompose(datatrain, type = "additive"))


modelhws <- HoltWinters(datatrain)
pred <- forecast(modelhws, n.ahead = 1*12)
