install.packages("tseries")
install.packages("forecast")
install.packages("ggplot2")
library("forecast")
library("ggplot2")
library("tseries")
data <- read.csv(file.choose(), header =T)
data
X1= data$Date
X2= data$Price
X3= data$Open
X4= data$High
X5= data$Low
X6= data$Volume
X7= data$Chg.
X2 <- ts(as.numeric(X2), start= c(2010,1), frequency = 12)
str(X2)
head(X2)
reg_price=lm(X2 ~ time(X2))
plot(X2, main= "Monthly Stock prices of Tata motors")
abline(reg_price, col= "green")
monthplot(X2, main="Monthly plot of stock prices")
#seasonplot is a function in forecast package
seasonplot(X2, xlab=" ", col= c("red","blue","green","black","yellow","violet","darkorange","darkolivegreen4"), year.labels= T, labelgap= 0.45, cex= 0.7, main="Seasonal Plot of Stock prices ")
decomp = stl(X2,s.window=12)
plot(decomp)
deseasoned_price= seasadj(decomp)
ts.plot(deseasoned_price, X2, col=c("red", "blue"), main = "comparision of monthly price and deseasonalied price")
abline(lm(deseasoned_price ~ time(deseasoned_price)), col ="black")
plot(ma(X2, 3, centre= TRUE), col= c("green"))
plot(ma(X2,12, centre= TRUE))
ts.plot(ma(X2, 3, centre= TRUE),ma(X2,12, centre= TRUE), X2,main = "comparision between moving average price curve at unit 3 and 12 and original price")
ses_price= ses(deseasoned_price, h=12)
autoplot(ses_price)
summary(ses_price)
holt_price= holt(deseasoned_price, h=12)
autoplot(holt_price)
summary(holt_price)
adf.test(deseasoned_price, alternative = "stationary")
detrended_price= diff(deseasoned_price, differences=2)
plot(detrended_price)
adf.test(detrended_price, alternative= "stationary")
acf(detrended_price, main= "ACF for differenced series")
pacf(detrended_price, main= "PACF for detrended series")
#(p,d,q): PACF -> p; ACF->q; diff->d
priceARIMA1= arima(detrended_price, order= c(1,2,1))
priceARIMA1
fcast_priceARIMA= forecast(priceARIMA1, h=8)
priceTest= c(452.10,420.70,420.80,484.95,526.30,595.55,644.30,607.30)
accuracy(fcast_priceARIMA, priceTest)

