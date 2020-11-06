#read the data
train <- read.csv("/Users/wyf/Desktop/train.csv", header = TRUE)
head(train)
#read the data into time series
hpi <- ts(train[,'hpi'], frequency = 4, start = c(2004,3))
ach <- ts(train[,'hpi_annual_change'], frequency = 4, start = c(2004,3))

plot.ts(hpi)
plot.ts(ach)

d_hpi <- ts(train[,"hpi_annual_change"], frequency = 4, start = c(2004,3))
plot.ts(d_hpi)

#component analysis or Spectral analysis
hpi_components = decompose(hpi)
plot(hpi_components)

#ARIMA 
#Differencing
diffhpi1 <- diff(hpi, differences = 1)
diffhpi2 <- diff(hpi, differences = 2)
diffhpi4 <- diff(hpi, differences = 4) #seasonal differencing

#parameter estimation
library(forecast)
forecast::ggtsdisplay(diffhpi2)
#fit the model
fit1 <- arima(hpi, order=c(1,1,0),seasonal = c(2,0,0))
forecast::ggtsdisplay(residuals(fit1))

#check the AIC value
fit1

#check the parameter with auto arima function
forecast::auto.arima(hpi, trace = TRUE)

#auto.arima provides the model to be ARIMA(1,1,0)(2,0,0)[4]
pred1 <- forecast::forecast(fit1, h=18)

#plot the forecast
plot(pred1, ylim = c(1100,3100))

#accuracy 
test <- read.csv("/Users/wyf/Desktop/test.csv", header = TRUE)
actual <- ts(test[,'Hpi'], frequency = 4, start = c(2015,3))
forecast::accuracy(pred1,actual) # arima

#comparison
pred2 <- forecast::holt(hpi,h=18)
forecast::accuracy(pred2,actual) # exp
plot(pred2)

#forecast future value
full <- read.csv("/Users/wyf/Desktop/houseprice.csv", header = TRUE)
head(full)
hpif <- ts(full[,'hpi'], frequency = 4, start = c(2004,3))
plot(hpif, ylim = c(1100,3100))
fit_full2 <- arima(hpif, order=c(1,1,0),seasonal = c(2,0,0))
pred_full2 <- forecast::forecast(fit_full2, h=12)
plot(pred_full2)

#train the full dataset, auto.arima gives another best model
#compare the results
forecast::auto.arima(hpif, trace = TRUE)
fit_full1 <- arima(hpif, order=c(1,1,0))
pred_full1 <- forecast::forecast(fit_full1, h=12)
plot(pred_full1)

#arimax - 6 variables
variables <- c('Interest_rate', 'CPI', 'GDP_annual_change', 
               'current_account_balance','Employment','Unemployment_rate')
regressor <- ts(train[, variables], frequency = 4, start = c(2004,3))
regressor_test <- ts(test[, variables], frequency = 4, start = c(2015,3))

forecast::auto.arima(hpi, trace = TRUE, xreg = regressor)
fit3 <- arima(hpi, order=c(2,0,0), xreg = regressor)
pred3 <- predict(fit3, newxreg= regressor_test)
forecast::accuracy(pred3$pred, actual)
plot(pred3$pred)

#arimax- 2 variables
variable2 <- c('CPI','Employment')
regressor2 <- ts(train[, variable2], frequency = 4, start = c(2004,3))
regressor_test2 <- ts(test[, variable2], frequency = 4, start = c(2015,3))

forecast::auto.arima(hpi, trace = TRUE, xreg = regressor2)
fit4 <- arima(hpi, order=c(1,0,0), seasonal = c(0,0,1), xreg = regressor2)
pred4 <- predict(fit4, newxreg= regressor_test2)
forecast::accuracy(pred4$pred, actual)
plot(pred4$pred)

#combine the pred3 and the train set to show a similar plot
pred3_plus <- ts(c(hpi,pred3$pred), frequency = 4, start = c(2004,3))
plot(pred3_plus, ylim = c(1100,3100))
abline(v=2015.5)

pred4_plus <- ts(c(hpi,pred4$pred), frequency = 4, start = c(2004,3))
plot(pred4_plus, ylim = c(1100,3100))
abline(v=2015.5)

#pca
regressor_full <- ts(full[, variables], frequency = 4, start = c(2004,3))
pca <- prcomp(regressor_full,center = TRUE, scale. = TRUE)
summary(pca)

fit_full2

#linear regression
fit_lr <- lm(hpi~regressor)
pred5 <- predict(fit_lr, newdata = regressor_test)
forecast::accuracy(pred5, actual)

#correlation test
testv <- c('hpi','Interest_rate', 'CPI', 'GDP_annual_change', 
               'current_account_balance','Employment','Unemployment_rate')
testvv <- ts(full[, testv], frequency = 4, start = c(2004,3))
library(corrplot)
corrplot(cor(testvv), method = 'number', type = 'upper')

#regional analysis
region_data <- read.csv("/Users/wyf/Desktop/region.csv", header = TRUE)
head(region_data)
vs <- c('price','gdp','unemployment','employment')
df <- region_data[,vs]
corrplot(cor(df), method = 'number', type = 'upper')

fit5 <- lm(price~employment, data = region_data)
summary(fit5)
