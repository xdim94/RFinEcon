require (quantmod)
require(zoo)
library(openxlsx)
library(xts)
library(dplyr)
#Start Date of the Sample 
start.date <- as.Date(c("2010-01-01"))
# End Date of the Sample 
end.date <- as.Date(c("2021-12-31"))
europepricesmonthly<-as.xts(europepricesmonthly[,-1],order.by = europepricesmonthly$)
bricspricesmonthly<-as.xts(bricspricesdaily[,-1],order.by = bricspricesdaily$Date)
forexpricesmonthly<-as.xts(monthly_forex_prices[,-1],order.by = monthly_forex_prices$Date)
plot(europepricesmonthly$EUSTX50)
plot(europepricesmonthly$`CAC 40`)
plot(europepricesmonthly$`IBEX 35`)
plot(europepricesmonthly$DAX)
plot(europepricesmonthly$IT40)
plot(bricspricesmonthly$`SZSE GDP100`)
plot(bricspricesmonthly$NIFTY50)
plot(bricspricesmonthly$IBOVESPA)
plot(bricspricesmonthly$IMOEX)
plot(bricspricesmonthly$`JSE 25`)
plot(forexpricesmonthly$`EUR/CNY`)
plot(forexpricesmonthly$EURZAR)
plot(forexpricesmonthly$EURINR)
plot(forexpricesmonthly$EURRUB)
plot(forexpricesmonthly$EURBRL)
acf(europepricesmonthly$EUSTX50,lag=20)
acf(europepricesmonthly$`CAC 40`,lag=20)
acf(europepricesmonthly$`IBEX 35`,lag=20)
acf(europepricesmonthly$DAX,lag=20)
acf(europepricesmonthly$IT40,lag=20)
log_returns_europe=diff(log(europepricesmonthly))
plot(log_returns_europe$EUSTX50)
plot(log_returns_europe$`CAC 40`)
plot(log_returns_europe$`IBEX 35`)
plot(log_returns_europe$DAX)
plot(log_returns_europe$IT40)
c
plot(log_returns_brics$`SZSE GDP100`)
plot(log_returns_brics$NIFTY50)
plot(log_returns_brics$IBOVESPA)
plot(log_returns_brics$IMOEX)
plot(log_returns_brics$`JSE 25`)
acf(na.omit(log_returns_europe$EUSTX50),lag=20)
acf(na.omit(log_returns_europe$`CAC 40`),lag=20)
acf(na.omit(log_returns_europe$`IBEX 35`),lag=20)
acf(na.omit(log_returns_europe$DAX),lag=20)
acf(na.omit(log_returns_europe$IT40),lag=20)
coeffEUSTX <- acf(na.omit(log_returns_europe$EUSTX50),lag=20)
coeffEUSTX
coeffCAC <- acf(na.omit(log_returns_europe$`CAC 40`),lag=20)
coeffCAC
coeffIBEX <- acf(na.omit(log_returns_europe$`IBEX 35`),lag=20)
coeffIBEX
coeffDAX <- acf(na.omit(log_returns_europe$DAX),lag=20)
coeffDAX
coeffIT40 <- acf(na.omit(log_returns_europe$IT40),lag=20)
coeffIT40
coeffSZSE <- acf(na.omit(log_returns_brics$`SZSE GDP100`),lag=20)
coeffSZSE
coeffNIFTY <- acf(na.omit(log_returns_brics$NIFTY50),lag=20)
coeffNIFTY
coeffIBO <- acf(na.omit(log_returns_brics$IBOVESPA),lag=20)
coeffIBO
coeffMOEX <- acf(na.omit(log_returns_brics$IMOEX),lag=20)
coeffMOEX
coeffJSE <- acf(na.omit(log_returns_brics$`JSE 25`),lag=20)
coeffJSE
require(quantmod);require(PerformanceAnalytics); require(rcompanion)
require(forecast)
table.Stats(log_returns_europe$EUSTX50)
table.Stats(log_returns_europe$`CAC 40`)
table.Stats(log_returns_europe$`IBEX 35`)
table.Stats(log_returns_europe$DAX)
table.Stats(log_returns_europe$IT40)
hist(log_returns_europe$EUSTX50)
hist(log_returns_europe$`CAC 40`)
hist(log_returns_europe$`IBEX 35`)
hist(log_returns_europe$DAX)
hist(log_returns_europe$IT40)
plotNormalDensity(log_returns_europe$EUSTX50,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe$`CAC 40`,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe$`IBEX 35`,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe$DAX,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe$IT40,col2 = "blue",col3 = "red")
Box.test(log_returns_europe$EUSTX50,lag=5,type="Box-Pierce")
Box.test(log_returns_europe$`CAC 40`,lag=5,type="Box-Pierce")
Box.test(log_returns_europe$`IBEX 35`,lag=5,type="Box-Pierce")
Box.test(log_returns_europe$DAX,lag=5,type="Box-Pierce")
Box.test(log_returns_europe$IT40,lag=5,type="Box-Pierce")
Box.test(log_returns_europe$EUSTX50,lag=5,type="Ljung-Box")
Box.test(log_returns_europe$`CAC 40`,lag=5,type="Ljung-Box")
Box.test(log_returns_europe$`IBEX 35`,lag=5,type="Ljung-Box")
Box.test(log_returns_europe$DAX,lag=5,type="Ljung-Box")
Box.test(log_returns_europe$IT40,lag=5,type="Ljung-Box")
table.Stats(log_returns_brics$`SZSE GDP100`)
table.Stats(log_returns_brics$`JSE 25`)
table.Stats(log_returns_brics$NIFTY50)
table.Stats(log_returns_brics$IMOEX)
table.Stats(log_returns_brics$IBOVESPA)
hist(log_returns_brics$`SZSE GDP100`)
hist(log_returns_brics$`JSE 25`)
hist(log_returns_brics$NIFTY50)
hist(log_returns_brics$IMOEX)
hist(log_returns_brics$IBOVESPA)
plotNormalDensity(returns_areurope$EUSTX50,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$`CAC 40`,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$`IBEX 35`,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$DAX,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$IT40,col2 = "blue",col3 = "red")
# Load the required library
# Load the required libraries
library(cowplot)

# Assuming you already have the functions plotNormalDensity() for each index
# Also, assuming the individual plots are correctly created in the code below

# Define individual plots
plot_eustx50 <- plotNormalDensity(returns_areurope$EUSTX50, col2 = "blue", col3 = "red")
plot_cac40 <- plotNormalDensity(returns_areurope$`CAC 40`, col2 = "blue", col3 = "red")
plot_ibex35 <- plotNormalDensity(returns_areurope$`IBEX 35`, col2 = "blue", col3 = "red")
plot_dax <- plotNormalDensity(returns_areurope$DAX, col2 = "blue", col3 = "red")
plot_it40 <- plotNormalDensity(returns_areurope$IT40, col2 = "blue", col3 = "red")

# Combine the plots using cowplot
combined_plot <- plot_grid(plot_eustx50, plot_cac40, plot_ibex35, plot_dax, plot_it40, ncol = 1)

# Print the combined plot
print(combined_plot)

print(combined_plot)




plotNormalDensity(log_returns_brics$`SZSE GDP100`,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics$`JSE 25`,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics$NIFTY50,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics$IMOEX,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics$IBOVESPA,col2 = "blue",col3 = "red")
Box.test(log_returns_brics$`SZSE GDP100`,lag=5,type="Box-Pierce")
Box.test(log_returns_brics$`JSE 25`,lag=5,type="Box-Pierce")
Box.test(log_returns_brics$NIFTY50,lag=5,type="Box-Pierce")
Box.test(log_returns_brics$IMOEX,lag=5,type="Box-Pierce")
Box.test(log_returns_brics$IBOVESPA,lag=5,type="Box-Pierce")
Box.test(log_returns_brics$`SZSE GDP100`,lag=5,type="Ljung-Box")
Box.test(log_returns_brics$`JSE 25`,lag=5,type="Ljung-Box")
Box.test(log_returns_brics$NIFTY50,lag=5,type="Ljung-Box")
Box.test(log_returns_brics$IMOEX,lag=5,type="Ljung-Box")
Box.test(log_returns_brics$IBOVESPA,lag=5,type="Ljung-Box")
require(tseries)
jarque.bera.test(na.omit(bricspricesmonthly$`SZSE GDP100`))
jarque.bera.test(na.omit(bricspricesmonthly$`JSE 25`))
jarque.bera.test(na.omit(bricspricesmonthly$NIFTY50))
jarque.bera.test(na.omit(bricspricesmonthly$IMOEX))
jarque.bera.test(na.omit(bricspricesmonthly$IBOVESPA))
jarque.bera.test(na.omit(europepricesmonthly$EUSTX50))
jarque.bera.test(na.omit(europepricesmonthly$`CAC 40`))
jarque.bera.test(na.omit(europepricesmonthly$`IBEX 35`))
jarque.bera.test(na.omit(europepricesmonthly$DAX))
jarque.bera.test(na.omit(europepricesmonthly$IT40))
jarque.bera.test(na.omit(log_returns_europe$EUSTX50))
jarque.bera.test(na.omit(log_returns_europe$`CAC 40`))
jarque.bera.test(na.omit(log_returns_europe$`IBEX 35`))
jarque.bera.test(na.omit(log_returns_europe$DAX))
jarque.bera.test(na.omit(log_returns_europe$IT40))
jarque.bera.test(na.omit(log_returns_brics$`SZSE GDP100`))
jarque.bera.test(na.omit(log_returns_brics$`JSE 25`))
jarque.bera.test(na.omit(log_returns_brics$NIFTY50))
jarque.bera.test(na.omit(log_returns_brics$IMOEX))
jarque.bera.test(na.omit(log_returns_brics$IBOVESPA))
qqnorm(log_returns_europe$EUSTX50,col="blue")
qqline(log_returns_europe$EUSTX50,col="red")
qqnorm(log_returns_europe$`CAC 40`,col="blue")
qqline(log_returns_europe$`CAC 40`,col="red")
qqnorm(log_returns_europe$`IBEX 35`,col="blue")
qqline(log_returns_europe$`IBEX 35`,col="red")
qqnorm(log_returns_europe$DAX,col="blue")
qqline(log_returns_europe$DAX,col="red")
qqnorm(log_returns_europe$IT40,col="blue")
qqline(log_returns_europe$IT40,col="red")
library(urca)
adfeustx=ur.df(na.omit(log_returns_europe$EUSTX50),type = "none",selectlags = "AIC")
summary(adfeustx)
adftestcac=ur.df(na.omit(log_returns_europe$`CAC 40`),type = "none",selectlags = "AIC")
summary(adftestcac)
adftestibex=ur.df(na.omit(log_returns_europe$`IBEX 35`),type = "none",selectlags = "AIC")
summary(adftestibex)
adftestdax=ur.df(na.omit(log_returns_europe$DAX),type = "none",selectlags = "AIC")
summary(adftestdax)
adftestit40=ur.df(na.omit(log_returns_europe$IT40),type = "none",selectlags = "AIC")
summary(adftestit40)
pacf(na.omit(log_returns_europe$EUSTX50,lag=20))
pacf(na.omit(log_returns_europe$`CAC 40`,lag=20))
pacf(na.omit(log_returns_europe$`IBEX 35`,lag=20))
pacf(na.omit(log_returns_europe$DAX,lag=20))
pacf(na.omit(log_returns_europe$IT40,lag=20))
logsqeustx=log_returns_europe$EUSTX50^2
logsqcac=log_returns_europe$`CAC 40`^2
logsqibex=log_returns_europe$`IBEX 35`^2
logsqdax=log_returns_europe$DAX^2
logsqit40=log_returns_europe$IT40^2
acf(na.omit(logsqeustx,lag.max = 20))
acf(na.omit(logsqcac,lag.max = 20))
acf(na.omit(logsqibex,lag.max = 20))
acf(na.omit(logsqdax,lag.max = 20))
acf(na.omit(logsqit40,lag.max = 20))
abslogreteustx=abs(log_returns_europe$EUSTX50)
abslogretcac=abs(log_returns_europe$`CAC 40`)
abslogretibex=abs(log_returns_europe$`IBEX 35`)
abslogretdax=abs(log_returns_europe$DAX)
abslogretit40=abs(log_returns_europe$IT40)
acf(na.omit(abslogreteustx,lag.max=20))
acf(na.omit(abslogretcac,lag.max=20))
acf(na.omit(abslogretibex,lag.max=20))
acf(na.omit(abslogretdax,lag.max=20))
acf(na.omit(abslogretit40,lag.max=20))
testset_eustx=europepricesmonthly$EUSTX50[133:144]
testset_cac=europepricesmonthly$`CAC 40`[133:144]
testset_ibex=europepricesmonthly$`IBEX 35`[133:144]
testset_dax=europepricesmonthly$DAX[133:144]
testset_it40=europepricesmonthly$IT40[133:144]
trainset_eustx=europepricesmonthly$EUSTX50[1:132]
trainset_cac=europepricesmonthly$`CAC 40`[1:132]
trainset_ibex=europepricesmonthly$`IBEX 35`[1:132]
trainset_dax=europepricesmonthly$DAX[1:132]
trainset_it40=europepricesmonthly$IT40[1:132]
plot(trainset_eustx,main="Prices for EUSTX50",ylab="Price",col="blue")
plot(trainset_cac,main="Prices for CAC40",ylab="Price",col="blue")
plot(trainset_ibex,main="Prices for IBEX35",ylab="Price",col="blue")
plot(trainset_dax,main="Prices for DAX",ylab="Price",col="blue")
plot(trainset_it40,main="Prices for IT40",ylab="Price",col="blue")
S0eustx=as.numeric(europepricesmonthly$EUSTX50[1])
S0cac=as.numeric(europepricesmonthly$`CAC 40`[1])
S0ibex=as.numeric(europepricesmonthly$`IBEX 35`[1])
S0dax=as.numeric(europepricesmonthly$DAX[1])
S0it40=as.numeric(europepricesmonthly$IT40[1])
qqnorm(log_returns_brics$`SZSE GDP100`,col="blue")
qqline(log_returns_brics$`SZSE GDP100`,col="red")
qqnorm(log_returns_brics$`JSE 25`,col="blue")
qqline(log_returns_brics$`JSE 25`,col="red")
qqnorm(log_returns_brics$NIFTY50,col="blue")
qqline(log_returns_brics$NIFTY50,col="red")
qqnorm(log_returns_brics$IMOEX,col="blue")
qqline(log_returns_brics$IMOEX,col="red")
qqnorm(log_returns_brics$IBOVESPA,col="blue")
qqline(log_returns_brics$IBOVESPA,col="red")
adfszse=ur.df(na.omit(log_returns_brics$`SZSE GDP100`),type = "none",selectlags = "AIC")
summary(adfszse)
adfjse=ur.df(na.omit(log_returns_brics$`JSE 25`),type = "none",selectlags = "AIC")
summary(adfjse)
adfnifty=ur.df(na.omit(log_returns_brics$NIFTY50),type = "none",selectlags = "AIC")
summary(adfnifty)
adfimoex=ur.df(na.omit(log_returns_brics$IMOEX),type = "none",selectlags = "AIC")
summary(adfimoex)
adfibovespa=ur.df(na.omit(log_returns_brics$IBOVESPA),type = "none",selectlags = "AIC")
summary(adfibovespa)
pacf(na.omit(log_returns_brics$`SZSE GDP100`,lag=20))
pacf(na.omit(log_returns_brics$`JSE 25`,lag=20))
pacf(na.omit(log_returns_brics$NIFTY50,lag=20))
pacf(na.omit(log_returns_brics$IMOEX,lag=20))
pacf(na.omit(log_returns_brics$IBOVESPA,lag=20))
logsqszse=log_returns_brics$`SZSE GDP100`^2
logsqjse=log_returns_brics$`JSE 25`^2
logsqnifty=log_returns_brics$NIFTY50^2
logsqimoex=log_returns_brics$IMOEX^2
logsqibovespa=log_returns_brics$IBOVESPA^2
acf(na.omit(logsqszse,lag.max = 20))
acf(na.omit(logsqjse,lag.max = 20))
acf(na.omit(logsqnifty,lag.max = 20))
acf(na.omit(logsqimoex,lag.max = 20))
acf(na.omit(logsqibovespa,lag.max = 20))
abslogretszse=abs(log_returns_brics$`SZSE GDP100`)
abslogretjse=abs(log_returns_brics$`JSE 25`)
abslogretnifty=abs(log_returns_brics$NIFTY50)
abslogretimoex=abs(log_returns_brics$IMOEX)
abslogretibovespa=abs(log_returns_brics$IBOVESPA)
acf(na.omit(abslogretszse,lag.max=20))
acf(na.omit(abslogretjse,lag.max=20))
acf(na.omit(abslogretnifty,lag.max=20))
acf(na.omit(abslogretimoex,lag.max=20))
acf(na.omit(abslogretibovespa,lag.max=20))
testset_szse=bricspricesmonthly$`SZSE GDP100`[133:144]
testset_jse=bricspricesmonthly$`JSE 25`[133:144]
testset_nifty=bricspricesmonthly$NIFTY50[133:144]
testset_imoex=bricspricesmonthly$IMOEX[133:144]
testset_ibovespa=bricspricesmonthly$IBOVESPA[133:144]
trainset_szse=bricspricesmonthly$`SZSE GDP100`[1:132]
trainset_jse=bricspricesmonthly$`JSE 25`[1:132]
trainset_nifty=bricspricesmonthly$NIFTY50[1:132]
trainset_imoex=bricspricesmonthly$IMOEX[1:132]
trainset_ibovespa=bricspricesmonthly$IBOVESPA[1:132]
plot(trainset_szse,main="Prices for SZSE",ylab="Price",col="blue")
plot(trainset_jse,main="Prices for JSE",ylab="Price",col="blue")
plot(trainset_nifty,main="Prices for NIFTY",ylab="Price",col="blue")
plot(trainset_imoex,main="Prices for IMOEX",ylab="Price",col="blue")
plot(trainset_ibovespa,main="Prices for IBOVESPA",ylab="Price",col="blue")
S0szse=as.numeric(bricspricesmonthly$`SZSE GDP100`[1])
S0jse=as.numeric(bricspricesmonthly$`JSE 25`[1])
S0nifty=as.numeric(bricspricesmonthly$NIFTY50[1])
S0imoex=as.numeric(bricspricesmonthly$IMOEX[1])
S0ibovespa=as.numeric(bricspricesmonthly$IBOVESPA[1])
table.Stats(europepricesmonthly$EUSTX50)
table.Stats(europepricesmonthly$`CAC 40`)
table.Stats(europepricesmonthly$DAX)
table.Stats(europepricesmonthly$`IBEX 35`)
table.Stats(europepricesmonthly$IT40)
qqnorm(europepricesmonthly$EUSTX50,col="blue")
qqline(europepricesmonthly$EUSTX50,col="red")
qqnorm(europepricesmonthly$`CAC 40`,col="blue")
qqline(europepricesmonthly$`CAC 40`,col="red")
qqnorm(europepricesmonthly$`IBEX 35`,col="blue")
qqline(europepricesmonthly$`IBEX 35`,col="red")
qqnorm(europepricesmonthly$DAX,col="blue")
qqline(europepricesmonthly$DAX,col="red")
qqnorm(europepricesmonthly$IT40,col="blue")
qqline(europepricesmonthly$IT40,col="red")
table.Stats(log_returns_brics$`SZSE GDP100`)
table.Stats(log_returns_brics$NIFTY50)
table.Stats(log_returns_brics$IBOVESPA)
table.Stats(log_returns_brics$IMOEX)
table.Stats(log_returns_brics$`JSE 25`)
qqnorm(bricspricesmonthly$`SZSE GDP100`,col="blue")
qqline(bricspricesmonthly$`SZSE GDP100`,col="red")
qqnorm(bricspricesmonthly$`JSE 25`,col="blue")
qqline(bricspricesmonthly$`JSE 25`,col="red")
qqnorm(bricspricesmonthly$NIFTY50,col="blue")
qqline(bricspricesmonthly$NIFTY50,col="red")
qqnorm(bricspricesmonthly$IMOEX,col="blue")
qqline(bricspricesmonthly$IMOEX,col="red")
qqnorm(bricspricesmonthly$IBOVESPA,col="blue")
qqline(bricspricesmonthly$IBOVESPA,col="red")
table.Stats(bricspricesmonthly$`SZSE GDP100`)
table.Stats(bricspricesmonthly$`JSE 25`)
table.Stats(bricspricesmonthly$NIFTY50)
table.Stats(bricspricesmonthly$IMOEX)
table.Stats(bricspricesmonthly$IBOVESPA)
library(xts)
#GBM 
#EUSTX50
S0 <- as.numeric(europepricesmonthly$EUSTX50[nrow(europepricesmonthly)]) # initial stock price (set as the last price in the xts object)
mu_eustx <- mean(log_returns_europe$EUSTX50[!is.na(log_returns_europe$EUSTX50)])
# drift (annualized)
sigma_eustx <- sd(log_returns_europe$EUSTX50[!is.na(log_returns_europe$EUSTX50)])  # volatility (annualized)

# Define the time intervals
years <- 12
n <- 12 * years  # number of time intervals (assuming monthly intervals)
dt <- 1/12  # time interval (in years)

# Generate random numbers
set.seed(42)  # for reproducibility
epsilon <- rnorm(n)

# Calculate the monthly returns
monthly_returns_eustx <- exp((mu_eustx - 0.5 * sigma_eustx ^ 2) * dt + sigma_eustx * epsilon * sqrt(dt))

# Calculate the stock prices
stock_prices_eustx <- S0 * c(1, cumprod(monthly_returns_eustx))
stock_prices_eustx_xts <- xts(stock_prices_eustx, order.by = index(europepricesmonthly$EUSTX50)[nrow(europepricesmonthly$EUSTX50)] + seq(0:n) * 30.44 * 24 * 60 * 60)

# Visualize the results
plot(stock_prices_eustx_xts, type = "l", xlab = "Time", ylab = "Stock Price", main = "Geometric Brownian Motion simulation")

#CAC40
# Define the parameters
S0cac <- as.numeric(europepricesmonthly$`CAC 40`[nrow(europepricesmonthly)]) # initial stock price (set as the last price in the xts object)
mu_cac <- mean(log_returns_europe$`CAC 40`[!is.na(log_returns_europe$`CAC 40`)])
# drift (annualized)
sigma_cac <- sd(log_returns_europe$`CAC 40`[!is.na(log_returns_europe$`CAC 40`)])  # volatility (annualized)

# Define the time intervals
years <- 12
n <- 12 * years  # number of time intervals (assuming monthly intervals)
dt <- 1/12  # time interval (in years)

# Generate random numbers
set.seed(42)  # for reproducibility
epsilon <- rnorm(n)

# Calculate the monthly returns
monthly_returns_cac <- exp((mu_cac - 0.5 * sigma_cac ^ 2) * dt + sigma_cac * epsilon * sqrt(dt))

# Calculate the stock prices
stock_prices_cac <- S0cac * c(1, cumprod(monthly_returns_eustx))
stock_prices_cac_xts <- xts(stock_prices_cac, order.by = index(europepricesmonthly$`CAC 40`)[nrow(europepricesmonthly$`CAC 40`)] + seq(0:n) * 30.44 * 24 * 60 * 60)

# Visualize the results
plot(stock_prices_cac_xts, type = "l", xlab = "Time", ylab = "Stock Price", main = "Geometric Brownian Motion simulation")

#DAX
# Define the parameters
S0dax <- as.numeric(europepricesmonthly$DAX[nrow(europepricesmonthly)]) # initial stock price (set as the last price in the xts object)
mu_dax <- mean(log_returns_europe$DAX[!is.na(log_returns_europe$DAX)])
# drift (annualized)
sigma_dax <- sd(log_returns_europe$DAX[!is.na(log_returns_europe$DAX)])  # volatility (annualized)

# Define the time intervals
years <- 12
n <- 12 * years  # number of time intervals (assuming monthly intervals)
dt <- 1/12  # time interval (in years)

# Generate random numbers
set.seed(42)  # for reproducibility
epsilon <- rnorm(n)

# Calculate the monthly returns
monthly_returns_dax <- exp((mu_dax - 0.5 * sigma_dax ^ 2) * dt + sigma_dax * epsilon * sqrt(dt))

# Calculate the stock prices
stock_prices_dax <- S0dax * c(1, cumprod(monthly_returns_eustx))
stock_prices_dax_xts <- xts(stock_prices_dax, order.by = index(europepricesmonthly$DAX)[nrow(europepricesmonthly$DAX)] + seq(0:n) * 30.44 * 24 * 60 * 60)

# Visualize the results
plot(stock_prices_dax_xts, type = "l", xlab = "Time", ylab = "Stock Price", main = "Geometric Brownian Motion simulation")

#IBEX35
S0ibex <- as.numeric(europepricesmonthly$`IBEX 35`[nrow(europepricesmonthly)]) # initial stock price (set as the last price in the xts object)
mu_ibex <- mean(log_returns_europe$`IBEX 35`[!is.na(log_returns_europe$`IBEX 35`)])
# drift (annualized)
sigma_ibex <- sd(log_returns_europe$`IBEX 35`[!is.na(log_returns_europe$`IBEX 35`)])  # volatility (annualized)

# Define the time intervals
years <- 12
n <- 12 * years  # number of time intervals (assuming monthly intervals)
dt <- 1/12  # time interval (in years)

# Generate random numbers
set.seed(42)  # for reproducibility
epsilon <- rnorm(n)

# Calculate the monthly returns
monthly_returns_ibex <- exp((mu_ibex - 0.5 * sigma_ibex ^ 2) * dt + sigma_ibex * epsilon * sqrt(dt))

# Calculate the stock prices
stock_prices_ibex <- S0ibex * c(1, cumprod(monthly_returns_ibex))
stock_prices_ibex_xts <- xts(stock_prices_ibex, order.by = index(europepricesmonthly$`IBEX 35`)[nrow(europepricesmonthly$`IBEX 35`)] + seq(0:n) * 30.44 * 24 * 60 * 60)

# Visualize the results
plot(stock_prices_ibex_xts, type = "l", xlab = "Time", ylab = "Stock Price", main = "Geometric Brownian Motion simulation")

#IT40
S0it <- as.numeric(europepricesmonthly$IT40[nrow(europepricesmonthly)]) # initial stock price (set as the last price in the xts object)
mu_it <- mean(log_returns_europe$IT40[!is.na(log_returns_europe$IT40)])
# drift (annualized)
sigma_it <- sd(log_returns_europe$IT40[!is.na(log_returns_europe$IT40)])  # volatility (annualized)

# Define the time intervals
years <- 12
n <- 12 * years  # number of time intervals (assuming monthly intervals)
dt <- 1/12  # time interval (in years)

# Generate random numbers
set.seed(42)  # for reproducibility
epsilon <- rnorm(n)

# Calculate the monthly returns
monthly_returns_it <- exp((mu_it - 0.5 * sigma_it ^ 2) * dt + sigma_it * epsilon * sqrt(dt))

# Calculate the stock prices
stock_prices_it <- S0it * c(1, cumprod(monthly_returns_it))
stock_prices_it_xts <- xts(stock_prices_it, order.by = index(europepricesmonthly$IT40)[nrow(europepricesmonthly$IT40)] + seq(0:n) * 30.44 * 24 * 60 * 60)

# Visualize the results
plot(stock_prices_it_xts, type = "l", xlab = "Time", ylab = "Stock Price", main = "Geometric Brownian Motion simulation")

#GBM in one plot


# Define GBM parameters

library(xts)

# Define function to simulate GBM
simulate_gbmEUR <- function(mu, sigma, Sa, dt, n_months) {
  # Generate random normal errors
  epsilon <- rnorm(n_months, 0, 1)
  # Calculate monthly returns and stock prices
  monthly_returnsEUR <- exp((mu - 0.5 * sigma^2) * dt + sigma * epsilon * sqrt(dt))
  stock_pricesEUR <- Sa * c(1, cumprod(monthly_returnsEUR))
  # Return as xts object
  return(xts(stock_pricesEUR[-length(stock_pricesEUR)], order.by = seq(as.Date("2022-01-01"), by = "month", length.out = n_months)))
}

# Define parameters for each index
params <- list(
  EUSTX50 = list(mu = mu_eustx, sigma = sigma_eustx, Sa = 4298.91),
  CAC40 = list(mu = mu_cac, sigma = sigma_cac, Sa = 7153.03),
  IBEX35 = list(mu = mu_ibex, sigma = sigma_ibex, Sa = 8713.18),
  DAX = list(mu = mu_dax, sigma = sigma_dax, Sa = 15884.86),
  IT40 = list(mu = mu_it, sigma = sigma_it, Sa = 27346.83)
)

# Set simulation parameters
n_months <- 144
dt <- 1/12

# Simulate GBM for each index
simulated_pricesEUR <- lapply(params, function(p) {
  simulate_gbmEUR(p$mu, p$sigma, p$Sa, dt, n_months)
})

# Combine simulated prices into one xts object
all_pricesEUR <- do.call(cbind, simulated_pricesEUR)

# Merge with real prices xts object
GBMEUR <- merge(all_pricesEUR, europepricesmonthly)

# Plot all prices on one plot
plot(GBMEUR)







##
sigma_szse <- sd(log_returns_brics$`SZSE GDP100`[!is.na(log_returns_brics$`SZSE GDP100`)])
sigma_nifty <- sd(log_returns_brics$NIFTY50[!is.na(log_returns_brics$`NIFTY50`)])
sigma_jse <- sd(log_returns_brics$`JSE 25`[!is.na(log_returns_brics$`JSE 25`)])
sigma_imoex <- sd(log_returns_brics$IMOEX[!is.na(log_returns_brics$IMOEX)])
sigma_ibovespa <- sd(log_returns_brics$IBOVESPA[!is.na(log_returns_brics$IBOVESPA)])
mu_szse <- mean(log_returns_brics$`SZSE GDP100`[!is.na(log_returns_brics$`SZSE GDP100`)])
mu_nifty <- mean(log_returns_brics$NIFTY50[!is.na(log_returns_brics$NIFTY50)])
mu_jse <- mean(log_returns_brics$`JSE 25`[!is.na(log_returns_brics$`JSE 25`)])
mu_imoex <- mean(log_returns_brics$IMOEX[!is.na(log_returns_brics$IMOEX)])
mu_ibovespa <- mean(log_returns_brics$IBOVESPA[!is.na(log_returns_brics$IBOVESPA)])


# Define GBM parameters

library(xts)

# Define function to simulate GBM
simulate_gbmBRICS <- function(mu, sigma, Sb, dt, n_months) {
  # Generate random normal errors
  epsilon <- rnorm(n_months, 0, 1)
  # Calculate monthly returns and stock prices
  monthly_returnsBRICS <- exp((mu - 0.5 * sigma^2) * dt + sigma * epsilon * sqrt(dt))
  stock_pricesBRICS <- Sb * c(1, cumprod(monthly_returnsBRICS))
  # Return as xts object
  return(xts(stock_pricesBRICS[-length(stock_pricesBRICS)], order.by = seq(as.Date("2022-01-01"), by = "month", length.out = n_months)))
}

# Define parameters for each index
params <- list(
  SZSE = list(mu = mu_szse, sigma = sigma_szse, Sb = 10564.29),
  NIFTY = list(mu = mu_nifty, sigma = sigma_nifty, Sb = 17354.05),
  IMOEX = list(mu = mu_imoex, sigma = sigma_imoex, Sb = 3787.26),
  IBOVESPA = list(mu = mu_ibovespa, sigma = sigma_ibovespa, Sb = 104822),
  JSE = list(mu = mu_jse, sigma = sigma_jse, Sb = 95457.11)
)

# Set simulation parameters
n_months <- 144
dt <- 1/12

# Simulate GBM for each index
simulated_pricesBRICS <- lapply(params, function(u) {
  simulate_gbmBRICS(u$mu, u$sigma, u$Sb, dt, n_months)
})

# Combine simulated prices into one xts object
all_pricesBRICS <- do.call(cbind, simulated_pricesBRICS)

# Merge with real prices xts object
GBMBRICS <- merge(all_pricesBRICS,bricspricesmonthly)

# Plot all prices on one plot
plot(GBMBRICS)


#GARCH (1,1)
library(rugarch)
speceus = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "norm")

fiteus <- ugarchfit(spec = spec, data = na.omit(log_returns_europe$EUSTX50))
summary(fiteus)
plot(fiteus, which = 'all', main = "Estimated Volatility of GARCH(1,1) Model for EUSTX50")



speccac = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                     distribution.model = "norm")

fitcac <- ugarchfit(spec = spec, data = na.omit(log_returns_europe$`CAC 40`))
summary(fitcac)
plot(fitcac, which = 'all', main = "Estimated Volatility of GARCH(1,1) Model for CAC40")


specibex = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                     distribution.model = "norm")

fitibex <- ugarchfit(spec = spec, data = na.omit(log_returns_europe$`IBEX 35`))
summary(fitibex)
plot(fitibex, which = 'all', main = "Estimated Volatility of GARCH(1,1) Model for IBEX35")


specdax = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                     distribution.model = "norm")

fitdax <- ugarchfit(spec = spec, data = na.omit(log_returns_europe$DAX))
summary(fitdax)
plot(fitdax, which = 'all', main = "Estimated Volatility of GARCH(1,1) Model for DAX")


specit = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                     distribution.model = "norm")

fitit <- ugarchfit(spec = spec, data = na.omit(log_returns_europe$IT40))
summary(fitit)
plot(fitit, which = 'all', main = "Estimated Volatility of GARCH(1,1) Model for IT40")

fiteus
fitcac
fitibex
fitdax
fitit
##############
testset_logeus=log_returns_europe$EUSTX50[133:144]
trainset_logeus=log_returns_europe$EUSTX50[2:132]
testset_logcac=log_returns_europe$`CAC 40`[133:144]
trainset_logcac=log_returns_europe$`CAC 40`[2:132]
testset_logibex=log_returns_europe$`IBEX 35`[133:144]
trainset_logibex=log_returns_europe$`IBEX 35`[2:132]
testset_logdax=log_returns_europe$DAX[133:144]
trainset_logdax=log_returns_europe$DAX[2:132]
testset_logit40=log_returns_europe$IT40[133:144]
trainset_logit40=log_returns_europe$IT40[2:132]



#garch_modeleus=ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)))
#garch_fiteus=ugarchfit(spec = garch_modeleus, data = trainset_logeus)
#garcheus_forecast=ugarchforecast(garch_fiteus, n.ahead = nrow(trainset_logeus),cond.dist="sstd")
#str(garcheus_forecast)


#varianceeus <- garcheus_forecast@forecast$variance
#garch_modeleus2=ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)))
#garch_fiteus2=ugarchfit(spec = garch_modeleus2, data = testset_logeus)
#garcheus_forecast2=ugarchforecast(garch_fiteus2, n.ahead = nrow(testset_logeus),cond.dist="sstd")
#View(garcheus2_forecast)

#############
#plot(varianceeus, col = "blue", main = "GARCH forecast of variance")
#lines(var(testset_logeus), col = "red")
#legend("topleft", c("Forecasted variance", "Actual variance"), col = c("blue", "red"), lty = 1)
library(forecast)
###########DAILY PRICES 
europepricesdaily<-as.xts(europedailyprices[,-1],order.by = europedailyprices$Date)
bricspricesdaily<-as.xts(bricsdailyprices[,-1],order.by = bricsdailyprices$Date)
plot(europepricesdaily$EUSTX50)
plot(europepricesdaily$`CAC40`)
plot(europepricesdaily$`IBEX35`)
plot(europepricesdaily$DAX)
plot(europepricesdaily$IT40)
plot(bricspricesdaily$`SZSE GDP100`)
plot(bricspricesdaily$NIFTY50)
plot(bricspricesdaily$BOVESPA)
plot(bricspricesdaily$MOEX)
plot(bricspricesdaily$`FTSE JSE`)
acf(europepricesdaily$EUSTX50,lag=1167)
acf(europepricesdaily$`CAC40`,lag=1167)
acf(europepricesdaily$`IBEX35`,lag=1167)
acf(europepricesdaily$DAX,lag=1167)
acf(europepricesdaily$IT40,lag=1167)
log_returns_europe_daily=diff(log(europepricesdaily))
# Assuming log_returns_europe_daily is a data frame or matrix of log returns
require(tseries)
arithmetic_returnseudaily <- exp(log_returns_europe_daily) - 1
arithmetic_returnsbricsdaily <- exp(log_returns_brics_daily) - 1

table.Stats(arithmetic_returnsbricsdaily$BOVESPA)
table.Stats(arithmetic_returnsbricsdaily$MOEX)
table.Stats(arithmetic_returnsbricsdaily$NIFTY50)
table.Stats(arithmetic_returnsbricsdaily$`SZSE GDP100`)
table.Stats(arithmetic_returnsbricsdaily$`FTSE JSE`)
#table.Stats(arithmetic_returnseudaily$`IBEX35`)
require(tseries)
jarque.bera.test(na.omit(arithmetic_returnsbricsdaily$BOVESPA))
                jarque.bera.test(na.omit(arithmetic_returnsbricsdaily$MOEX))
                 
jarque.bera.test(na.omit(arithmetic_returnsbricsdaily$NIFTY50))
jarque.bera.test(na.omit(arithmetic_returnsbricsdaily$`SZSE GDP100`))
jarque.bera.test(na.omit(arithmetic_returnsbricsdaily$`FTSE JSE`))
jarque.bera.test(na.omit(arithmetic_returnsbricsdaily$`BOVESPA`))
arithmetic_returnseudaily <- exp(log_returns_europe_daily) - 1

table.Stats(arithmetic_returnseudaily$EUSTX50)
table.Stats(arithmetic_returnseudaily$`CAC40`)
table.Stats(arithmetic_returnseudaily$`IBEX35`)
table.Stats(arithmetic_returnseudaily$DAX)
table.Stats(arithmetic_returnseudaily$IT40)
jarque.bera.test(na.omit(arithmetic_returnseudaily$EUSTX50))
jarque.bera.test(na.omit(arithmetic_returnseudaily$`CAC40`))
jarque.bera.test(na.omit(arithmetic_returnseudaily$`IBEX35`))
jarque.bera.test(na.omit(arithmetic_returnseudaily$DAX))
jarque.bera.test(na.omit(arithmetic_returnseudaily$IT40))
arithmetic_returnseudaily <- exp(log_returns_europe_daily) - 1
table.Stats(arithmetic_returnseudaily$EUSTX50)
table.Stats(arithmetic_returnseudaily$`CAC40`)
table.Stats(arithmetic_returnseudaily$`IBEX35`)
table.Stats(arithmetic_returnseudaily$DAX)
table.Stats(arithmetic_returnseudaily$IT40)
jarque.bera.test(na.omit(arithmetic_returnseudaily$EUSTX50))
jarque.bera.test(na.omit(arithmetic_returnseudaily$`CAC40`))
jarque.bera.test(na.omit(arithmetic_returnseudaily$`IBEX35`))
jarque.bera.test(na.omit(arithmetic_returnseudaily$DAX))
jarque.bera.test(na.omit(arithmetic_returnseudaily$IT40))
plot(log_returns_europe_daily$EUSTX50)
plot(log_returns_europe_daily$`CAC40`)
plot(log_returns_europe_daily$`IBEX35`)
plot(log_returns_europe_daily$DAX)
plot(log_returns_europe_daily$IT40)
log_returns_brics_daily=diff(log(bricspricesdaily))
plot(log_returns_brics_daily$`SZSE GDP100`)
plot(log_returns_brics_daily$NIFTY50)
plot(log_returns_brics_daily$BOVESPA)
plot(log_returns_brics_daily$MOEX)
plot(log_returns_brics_daily$`FTSE JSE`)
acf(na.omit(log_returns_europe_daily$EUSTX50),lag=600)
acf(na.omit(log_returns_europe_daily$`CAC40`),lag=600)
acf(na.omit(log_returns_europe_daily$`IBEX35`),lag=600)
acf(na.omit(log_returns_europe_daily$DAX),lag=600)
acf(na.omit(log_returns_europe_daily$IT40),lag=600)
coeffEUSTXdaily <- acf(na.omit(log_returns_europe_daily$EUSTX50),lag=600)
coeffEUSTXdaily
coeffCACdaily <- acf(na.omit(log_returns_europe_daily$`CAC40`),lag=600)
coeffCACdaily
coeffIBEXdaily <- acf(na.omit(log_returns_europe_daily$`IBEX35`),lag=600)
coeffIBEXdaily
coeffDAXdaily <- acf(na.omit(log_returns_europe_daily$DAX),lag=600)
coeffDAXdaily
coeffIT40daily <- acf(na.omit(log_returns_europe_daily$IT40),lag=600)
coeffIT40daily
coeffSZSEdaily <- acf(na.omit(log_returns_brics_daily$`SZSE GDP100`),lag=600)
coeffSZSEdaily
coeffNIFTYdaily <- acf(na.omit(log_returns_brics_daily$NIFTY50),lag=600)
coeffNIFTYdaily
coeffIBOdaily <- acf(na.omit(log_returns_brics_daily$BOVESPA),lag=600)
coeffIBOdaily
coeffMOEXdaily <- acf(na.omit(log_returns_brics_daily$MOEX),lag=600)
coeffMOEXdaily
coeffJSEdaily <- acf(na.omit(log_returns_brics_daily$`FTSE JSE`),lag=600)
coeffJSEdaily
require(quantmod);require(PerformanceAnalytics); require(rcompanion)
require(forecast)
table.Stats(log_returns_europe_daily$EUSTX50)
table.Stats(log_returns_europe_daily$`CAC40`)
table.Stats(log_returns_europe_daily$`IBEX35`)
table.Stats(log_returns_europe_daily$DAX)
table.Stats(log_returns_europe_daily$IT40)
hist(log_returns_europe_daily$EUSTX50)
hist(log_returns_europe_daily$`CAC40`)
hist(log_returns_europe_daily$`IBEX35`)
hist(log_returns_europe_daily$DAX)
hist(log_returns_europe_daily$IT40)
plotNormalDensity(log_returns_europe_daily$EUSTX50,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe_daily$`CAC40`,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe_daily$`IBEX35`,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe_daily$DAX,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_europe_daily$IT40,col2 = "blue",col3 = "red")
Box.test(log_returns_europe_daily$EUSTX50,lag=5,type="Box-Pierce")
Box.test(log_returns_europe_daily$`CAC40`,lag=5,type="Box-Pierce")
Box.test(log_returns_europe_daily$`IBEX35`,lag=5,type="Box-Pierce")
Box.test(log_returns_europe_daily$DAX,lag=5,type="Box-Pierce")
Box.test(log_returns_europe_daily$IT40,lag=5,type="Box-Pierce")
Box.test(log_returns_europe_daily$EUSTX50,lag=5,type="Ljung-Box")
Box.test(log_returns_europe_daily$`CAC40`,lag=5,type="Ljung-Box")
Box.test(log_returns_europe_daily$`IBEX35`,lag=5,type="Ljung-Box")
Box.test(log_returns_europe_daily$DAX,lag=5,type="Ljung-Box")
Box.test(log_returns_europe_daily$IT40,lag=5,type="Ljung-Box")
table.Stats(log_returns_brics_daily$BOVESPA)
table.Stats(log_returns_brics_daily$MOEX)
table.Stats(log_returns_brics_daily$NIFTY50)
table.Stats(log_returns_brics_daily$`SZSE GDP100`)
table.Stats(log_returns_brics_daily$`FTSE JSE`)
hist(log_returns_brics_daily$BOVESPA)
hist(log_returns_brics_daily$MOEX)
hist(log_returns_brics_daily$NIFTY50)
hist(log_returns_brics_daily$`SZSE GDP100`)
hist(log_returns_brics_daily$`FTSE JSE`)
plotNormalDensity(log_returns_brics_daily$BOVESPA,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics_daily$MOEX,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics_daily$NIFTY50,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics_daily$`SZSE GDP100`,col2 = "blue",col3 = "red")
plotNormalDensity(log_returns_brics_daily$`FTSE JSE`,col2 = "blue",col3 = "red")
Box.test(log_returns_brics_daily$BOVESPA,lag=5,type="Box-Pierce")
Box.test(log_returns_brics_daily$MOEX,lag=5,type="Box-Pierce")
Box.test(log_returns_brics_daily$NIFTY50,lag=5,type="Box-Pierce")
Box.test(log_returns_brics_daily$`SZSE GDP100`,lag=5,type="Box-Pierce")
Box.test(log_returns_brics_daily$`FTSE JSE`,lag=5,type="Box-Pierce")
Box.test(log_returns_brics_daily$BOVESPA,lag=5,type="Ljung-Box")
Box.test(log_returns_brics_daily$MOEX,lag=5,type="Ljung-Box")
Box.test(log_returns_brics_daily$NIFTY50,lag=5,type="Ljung-Box")
Box.test(log_returns_brics_daily$`SZSE GDP100`,lag=5,type="Ljung-Box")
Box.test(log_returns_brics_daily$`FTSE JSE`,lag=5,type="Ljung-Box")
require(tseries)
jarque.bera.test(na.omit(bricspricesdaily$BOVESPA))
jarque.bera.test(na.omit(bricspricesdaily$MOEX))
jarque.bera.test(na.omit(bricspricesdaily$NIFTY50))
jarque.bera.test(na.omit(bricspricesdaily$`SZSE GDP100`))
jarque.bera.test(na.omit(bricspricesdaily$`FTSE JSE`))
jarque.bera.test(na.omit(europepricesdaily$EUSTX50))
jarque.bera.test(na.omit(europepricesdaily$IT40))
jarque.bera.test(na.omit(europepricesdaily$CAC40))
jarque.bera.test(na.omit(europepricesdaily$DAX))
jarque.bera.test(na.omit(europepricesdaily$IBEX35))
jarque.bera.test(na.omit(log_returns_europe_daily$EUSTX50))
jarque.bera.test(na.omit(log_returns_europe_daily$IT40))
jarque.bera.test(na.omit(log_returns_europe_daily$CAC40))
jarque.bera.test(na.omit(log_returns_europe_daily$DAX))
jarque.bera.test(na.omit(log_returns_europe_daily$IBEX35))
jarque.bera.test(na.omit(log_returns_brics_daily$BOVESPA))
jarque.bera.test(na.omit(log_returns_brics_daily$MOEX))
jarque.bera.test(na.omit(log_returns_brics_daily$NIFTY50))
jarque.bera.test(na.omit(log_returns_brics_daily$`SZSE GDP100`))
jarque.bera.test(na.omit(log_returns_brics_daily$`FTSE JSE`))
qqnorm(log_returns_europe_daily$EUSTX50,col="blue")
qqline(log_returns_europe_daily$EUSTX50,col="red")
qqnorm(log_returns_europe_daily$CAC40,col="blue")
qqline(log_returns_europe_daily$CAC40,col="red")
qqnorm(log_returns_europe_daily$IT40,col="blue")
qqline(log_returns_europe_daily$IT40,col="red")
qqnorm(log_returns_europe_daily$DAX,col="blue")
qqline(log_returns_europe_daily$DAX,col="red")
qqnorm(log_returns_europe_daily$IBEX35,col="blue")
qqline(log_returns_europe_daily$IBEX35,col="red")
qqnorm(log_returns_brics_daily$BOVESPA,col="blue")
qqline(log_returns_brics_daily$BOVESPA,col="red")
qqnorm(log_returns_brics_daily$MOEX,col="blue")
qqline(log_returns_brics_daily$MOEX,col="red")
qqnorm(log_returns_brics_daily$NIFTY50,col="blue")
qqline(log_returns_brics_daily$NIFTY50,col="red")
qqnorm(log_returns_brics_daily$`SZSE GDP100`,col="blue")
qqline(log_returns_brics_daily$`SZSE GDP100`,col="red")
qqnorm(log_returns_brics_daily$`FTSE JSE`,col="blue")
qqline(log_returns_brics_daily$`FTSE JSE`,col="red")
adfszsedaily=ur.df(na.omit(log_returns_brics_daily$`SZSE GDP100`),type = "none",selectlags = "AIC")
summary(adfszsedaily)
adfjsedaily=ur.df(na.omit(log_returns_brics_daily$`FTSE JSE`),type = "none",selectlags = "AIC")
summary(adfjsedaily)
adfniftydaily=ur.df(na.omit(log_returns_brics_daily$NIFTY50),type = "none",selectlags = "AIC")
summary(adfniftydaily)
adfimoexdaily=ur.df(na.omit(log_returns_brics_daily$MOEX),type = "none",selectlags = "AIC")
summary(adfimoexdaily)
adfibovespadaily=ur.df(na.omit(log_returns_brics_daily$BOVESPA),type = "none",selectlags = "AIC")
summary(adfibovespadaily)

#GBM in one plot


# Define GBM parameters

library(xts)
sigmad_eustx <- sd(log_returns_europe_daily$EUSTX50[!is.na(log_returns_europe_daily$EUSTX50)])
sigmad_cac <- sd(log_returns_europe_daily$CAC40[!is.na(log_returns_europe_daily$CAC40)])
sigmad_ibex <- sd(log_returns_europe_daily$IBEX35[!is.na(log_returns_europe_daily$IBEX35)])
sigmad_dax <- sd(log_returns_europe_daily$DAX[!is.na(log_returns_europe_daily$DAX)])
sigmad_it <- sd(log_returns_europe_daily$IT40[!is.na(log_returns_europe_daily$IT40)])
mud_eustx <- mean(log_returns_europe_daily$EUSTX50[!is.na(log_returns_europe_daily$EUSTX50)])
mud_cac <- mean(log_returns_europe_daily$CAC40[!is.na(log_returns_europe_daily$CAC40)])
mud_ibex <- mean(log_returns_europe_daily$IBEX35[!is.na(log_returns_europe_daily$IBEX35)])
mud_dax <- mean(log_returns_europe_daily$DAX[!is.na(log_returns_europe_daily$DAX)])
mud_it <- mean(log_returns_europe_daily$IT40[!is.na(log_returns_europe_daily$IT40)])
# Define function to simulate GBM
simulate_gbmEURdaily <- function(mud, sigmad, Sdd, dt_d, n_days) {
  # Generate random normal errors
  epsilond <- rnorm(n_days, 0, 1)
  # Calculate monthly returns and stock prices
  daily_returnsEUR <- exp((mud - 0.5 * sigmad^2) * dt_d + sigmad * epsilond * sqrt(dt_d))
  daily_pricesEUR <- Sdd * c(1, cumprod(daily_returnsEUR))
  # Return as xts object
  return(xts(daily_pricesEUR[-length(daily_pricesEUR)], order.by = seq(as.Date("2022-01-01"), by = "day", length.out = n_days)))
}

# Define parameters for each index
params <- list(
  EUSTX50 = list(mud = mud_eustx, sigmad = sigmad_eustx, Sdd = 4284.83),
  CAC40 = list(mud = mud_cac, sigmad = sigmad_cac, Sdd = 7153.03),
  IBEX35 = list(mud = mud_ibex, sigmad = sigmad_ibex, Sdd = 8713.8),
  DAX = list(mud = mud_dax, sigmad = sigmad_dax, Sdd = 15884.86),
  IT40 = list(mud = mud_it, sigmad = sigmad_it, Sdd = 27346.83)
)

# Set simulation parameters
nd_days <- 5000
dt_d <- 1/365

# Simulate GBM for each index
simulated_pricesEURdaily <- lapply(params, function(z) {
  simulate_gbmEURdaily(z$mud, z$sigmad, z$Sdd, dt_d, nd_days)
})

# Combine simulated prices into one xts object
all_pricesdailyEUR <- do.call(cbind, simulated_pricesEURdaily)

# Merge with real prices xts object
GBMEURdaily <- merge(all_pricesdailyEUR, europepricesdaily)

# Plot all prices on one plot
plot(GBMEURdaily)

###GBM BRICS
sigmad_szse <- sd(log_returns_brics_daily$`SZSE GDP100`[!is.na(log_returns_brics_daily$`SZSE GDP100`)])
sigmad_iboveps <- sd(log_returns_brics_daily$BOVESPA[!is.na(log_returns_brics_daily$BOVESPA)])##################not correct name of ibovespa
sigmad_imoex <- sd(log_returns_brics_daily$MOEX[!is.na(log_returns_brics_daily$MOEX)])
sigmad_nifty <- sd(log_returns_brics_daily$NIFTY50[!is.na(log_returns_brics_daily$NIFTY50)])
sigmad_jse <- sd(log_returns_brics_daily$`FTSE JSE`[!is.na(log_returns_brics_daily$`FTSE JSE`)])
mud_szse <- mean(log_returns_brics_daily$`SZSE GDP100`[!is.na(log_returns_brics_daily$`SZSE GDP100`)])
mud_nifty <- mean(log_returns_brics_daily$NIFTY50[!is.na(log_returns_brics_daily$`SZSE GDP100`)])
mud_jse <- mean(log_returns_brics_daily$`FTSE JSE`[!is.na(log_returns_brics_daily$`FTSE JSE`)])
mud_imoex <- mean(log_returns_brics_daily[!is.na(log_returns_brics_daily$MOEX)])
mud_ibovespa <- mean(log_returns_brics_daily$BOVESPA[!is.na(log_returns_brics_daily$BOVESPA)])

# Define function to simulate GBM
simulate_gbmBRICSdaily <- function(mud, sigmad, SddB, dt_d, n_days) {
  # Generate random normal errors
  epsilond <- rnorm(n_days, 0, 1)
  # Calculate monthly returns and stock prices
  daily_returnsBRICS <- exp((mud - 0.5 * sigmad^2) * dt_d + sigmad * epsilond * sqrt(dt_d))
  daily_pricesBRICS <- SddB * c(1, cumprod(daily_returnsBRICS))
  # Return as xts object
  return(xts(daily_pricesBRICS[-length(daily_pricesBRICS)], order.by = seq(as.Date("2022-01-01"), by = "day", length.out = n_days)))
}

# Define parameters for each index
params <- list(
  SZSEGDP100 = list(mud = mud_szse, sigmad = sigmad_szse, SddB = 10564.29),
  NIFTY50 = list(mud = mud_nifty, sigmad = sigmad_nifty, SddB = 17354.05),
  FTSEJSE = list(mud = mud_jse, sigmad = sigmad_jse, SddB = 95457.11),
  MOEX = list(mud = mud_imoex, sigmad = sigmad_imoex, SddB = 3787.26),
  BOVESPA = list(mud = mud_ibovespa, sigmad = sigmad_iboveps, SddB = 104822) ###################not correct name of ibovespa
)

# Set simulation parameters
nd_days <- 5000
dt_d <- 1/365

# Simulate GBM for each index
simulated_pricesBRICSdaily <- lapply(params, function(w) {
  simulate_gbmBRICSdaily(w$mud, w$sigmad, w$SddB, dt_d, nd_days)
})

# Combine simulated prices into one xts object
all_pricesdailyBRICS <- do.call(cbind, simulated_pricesBRICSdaily)

# Merge with real prices xts object
GBMBRICSdaily <- merge(all_pricesdailyBRICS, bricspricesdaily)

# Plot all prices on one plot
plot(GBMBRICSdaily)


#TEST-TRAINSET
testset_logeusdaily=log_returns_europe_daily$EUSTX50[2453:3065]
trainset_logeusdaily=log_returns_europe_daily$EUSTX50[2:2452]
testset_logcacdaily=log_returns_europe_daily$`CAC40`[2453:3065]
trainset_logcacdaily=log_returns_europe_daily$`CAC40`[2:2452]
testset_logibexdaily=log_returns_europe_daily$`IBEX35`[2453:3065]
trainset_logibexdaily=log_returns_europe_daily$`IBEX35`[2:2452]
testset_logdaxdaily=log_returns_europe_daily$DAX[2453:3065]
trainset_logdaxdaily=log_returns_europe_daily$DAX[2:2452]
testset_logit40daily=log_returns_europe_daily$IT40[2453:3065]
trainset_logit40daily=log_returns_europe_daily$IT40[2:2452]


modeldailyeus=auto.arima(trainset_logeusdaily)
forecastdailyeus=forecast(modeldailyeus,h=nrow(testset_logeusdaily))
accuracy(forecastdailyeus,testset_logeusdaily)
plot(forecastdailyeus)

#estimate of arima
model1eusdaily=Arima(na.omit(log_returns_europe_daily$EUSTX50),order = c(2,1,0))
model2eusdaily=Arima(na.omit(log_returns_europe_daily$EUSTX50),order = c(1,1,1))
model3eusdaily=Arima(na.omit(log_returns_europe_daily$EUSTX50),order = c(0,1,2))
checkresiduals(model1eusdaily)
checkresiduals(model2eusdaily)
checkresiduals(model3eusdaily)
autoplot(model1eusdaily)
# models normality of residuals?
# qq plot
qqnorm(model1eusdaily$residuals,col="blue")
qqline(model1eusdaily$residuals,col="red")
# JB test
jarque.bera.test(model1eusdaily$residuals)
qqnorm(model2eusdaily$residuals,col="blue")
qqline(model2eusdaily$residuals,col="red")
# JB test
jarque.bera.test(model2eusdaily$residuals)
qqnorm(model3eusdaily$residuals,col="blue")
qqline(model3eusdaily$residuals,col="red")
jarque.bera.test(model3eusdaily$residuals)
esmodeleusdaily=ets(europepricesdaily$EUSTX50)
n=500
es_forecastesudaily=forecast(esmodeleusdaily,h=500)
es_forecastesudaily
plot(es_forecastesudaily)

#GARCH(1,1) for daily
speceustxdaily = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                     distribution.model = "norm")

fiteustxdaily <- ugarchfit(spec = spec, data = na.omit(log_returns_europe_daily$EUSTX50))
summary(fiteustxdaily)
plot(fiteustxdaily, which = 'all', main = "Estimated Volatility of GARCH(1,1) Model for EUSTX50")


# calculate arithmetic returns using diff
returns_areurope <- xts(diff(europepricesmonthly, lag = 1) / europepricesmonthly[-length(europepricesmonthly)])
returns_abrics <- xts(diff(bricspricesmonthly, lag = 1) / europepricesmonthly[-length(europepricesmonthly)])
returns_abrics
# view the returns
returns_areurope

table.Stats(returns_areurope$`EUSTX50`)
table.Stats(returns_areurope$`CAC 40`)
table.Stats(returns_areurope$`IBEX 35`)
table.Stats(returns_areurope$DAX)
table.Stats(returns_areurope$IT40)
require(tseries)
# load the tseries package (if not already loaded)
library(tseries)

# perform Jarque-Bera test on returns
test_result1 <- jarque.bera.test(na.omit(returns_areurope$`IT40`))
test_result2 <- jarque.bera.test(na.omit(returns_areurope$`DAX`))
test_result3 <- jarque.bera.test(na.omit(returns_areurope$EUSTX50))
test_result4 <- jarque.bera.test(na.omit(returns_areurope$`IBEX 35`))
test_result5 <- jarque.bera.test(na.omit(returns_areurope$`CAC 40`))


# view the test result
test_result1
test_result2
test_result3
test_result4
test_result5



table.Stats(returns_abrics$`SZSE GDP100`)
table.Stats(returns_abrics$`JSE 25`)
table.Stats(returns_abrics$NIFTY50)
table.Stats(returns_abrics$IMOEX)
table.Stats(returns_abrics$IBOVESPA)
require(tseries)
# load the tseries package (if not already loaded)
library(tseries)

# perform Jarque-Bera test on returns
test_result111 <- jarque.bera.test(na.omit(returns_abrics$`SZSE GDP100`))
test_result222 <- jarque.bera.test(na.omit(returns_abrics$`JSE 25`))
test_result333 <- jarque.bera.test(na.omit(returns_abrics$NIFTY50))
test_result444 <- jarque.bera.test(na.omit(returns_abrics$IMOEX))
test_result555 <- jarque.bera.test(na.omit(returns_abrics$IBOVESPA))


# view the test result
test_result111
test_result222
test_result333
test_result444
test_result555


install.packages("ggplot2")
library(ggplot2)

# Assuming you have already calculated 'returns_areurope'
# Convert 'returns_areurope' to a data.frame
returns_areurope_df <- data.frame(date = index(returns_areurope), returns = coredata(returns_areurope))

# Create the normality plot for Europe returns
ggplot(returns_areurope_df, aes(sample = returns_areurope_df)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normality Plot for Europe Returns")

plotNormalDensity(returns_areurope$EUSTX50,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$`CAC 40`,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$`IBEX 35`,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$DAX,col2 = "blue",col3 = "red")
plotNormalDensity(returns_areurope$IT40,col2 = "blue",col3 = "red")

library(vars)
# Calculate arithmetic returns
#europepricesdaily<-as.xts(europe_indices_monthly[,-1],order.by = europe_indices_monthly$Date)
ayyeurreturns <- diff(log(europepricesdaily), lag = 1)
# Assuming you have already installed the "urca" package
library(urca)
# Remove rows with missing values
ayyeurreturns <- na.omit(ayyeurreturns)

# Perform ADF test on each time series to check for stationarity
adf_test_results1111 <- lapply(ayyeurreturns, ur.df, type = "trend")

# Print the ADF test results for each series
print(adf_test_results)

# Select lag order based on AIC and BIC
lag_orders <- VARselect(ayyeurreturns, type = "both")





# Obtain the selected lag order based on AIC and BIC
selected_order_aic <- lag_orders$selection[which.min(lag_orders$AIC)]
selected_order_bic <- lag_orders$selection[which.min(lag_orders$BIC)]

# Use the selected lag order for the VAR model
var_model_aic <- VAR(ayyeurreturns, p = selected_order_aic)
var_model_bic <- VAR(ayyeurreturns, p = selected_order_bic)

# Perform the Granger causality test for each combination of indices
granger_test_result_aic <- causality(var_model_aic, cause = "EUSTX50", effect = c("CAC40", "IBEX35", "DAX", "IT40"))
granger_test_result_bic <- causality(var_model_bic, cause = "EUSTX50", effect = c("CAC40", "IBEX35", "DAX", "IT40"))
# Convert xts object to matrix




ayyeurreturns_matrix <- as.matrix(ayyeurreturns)

# Fit VAR model using the selected lag order based on AIC
var_model_aic <- VAR(ayyeurreturns_matrix, p = selected_order_aic)

# Fit VAR model using the selected lag order based on BIC
var_model_bic <- VAR(ayyeurreturns_matrix, p = selected_order_bic)

# Perform the Granger causality test for each combination of indices using the AIC-based lag order
granger_test_result_aic <- causality(var_model_aic, cause = "europe1", effect = c("europe2", "europe3", "europe4", "europe5"))

# Perform the Granger causality test for each combination of indices using the BIC-based lag order
granger_test_result_bic <- causality(var_model_bic, cause = "europe1", effect = c("europe2", "europe3", "europe4", "europe5"))



returns_areurope=na.omit(returns_areurope)
adf_test_results1111 <- lapply(returns_areurope, ur.df, type = "trend")
print(adf_test_results1111)
