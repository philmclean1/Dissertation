#New GARCH for company that has not been shorted
library(tidyverse)
library(plyr)
library(dplyr)

price_data <- read.csv("Price data/final_prices.csv")

#Use A.G. Barr instead
BAG <- filter(price_data, code == "BAG")
BAG <- subset(BAG, select = c("price", "date"))
BAG$date <- as.Date(BAG$date, format = "%d/%m/%Y")
BAG <- arrange(BAG, date)
BAG <- na.omit(BAG)
BAG <- BAG[c("date", "price")]

#Plot to make sure drops occur
plot(BAG$date, BAG$price, type = "l") #Looks fine
ggplot(BAG, aes(date, price)) + geom_line() + labs(title = "A. G. Barr PLC: Daily Share Price",
                                                   x = "Date", y = "Share Price")

BAG_prior <- filter(BAG, date <= "2017-06-19")

ggplot(BAG_prior, aes(date, price)) + geom_line() + 
  labs(title = "A. G. Barr PLC: Daily Share Price", x = "Date", y = "Share Price")


#It reached a peak of 653 on 2017-06-19 then dropped to 580.5 on 2017-07-17
BAG2 <- filter(BAG, date <= "2017-06-19")

#Obtain the returns
library(quantmod)
BAG2$returns <- Delt(BAG2$price)
BAG2$price <- NULL
BAG2 <- na.omit(BAG2)

#Use zoo package
library(zoo)
BAG_zoo <- read.zoo(BAG2)


#Check if ARCH effect (conditional heteroskedasticity) is present in the data
library(ggplot2)
ggplot(BAG2, aes(date, returns)) + geom_line() +
  labs(title = "Daily Returns: A.G. Barr", x = "Date", y = "Returns")

#Plot acf and pacf
acf(BAG2$returns^2, main = "Autocorrelation Plot for Sq Returns", sub = "A.G. Barr")
pacf(BAG2$returns^2, main = "Partial Autocorrelation Plot for Sq Returns", sub = "A.G. Barr")


#Ljung Box test - H0 is NO autocorrelation
#Looking for autocorrelation in the squared data
#This is taken as the proxy for volatility
Box.test(coredata(BAG_zoo^2), type = "Ljung-Box", lag = 10)
#Extremely significant p-value so reject null hypothesis


#Lagrange Multiplier (LM)
#Also tests for volatility
#Null hypothesis: no ARCH effects
#Use FinTS package
library(FinTS)

ArchTest(coredata(BAG_zoo), lags = 10)
#Extremely significant p-value so reject null hypothesis

#Fit GARCH model
library(rugarch)
BAG_garch_11_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                                mean.model = list(armaOrder = c(0,0)))

BAG_garch_11_fit <- ugarchfit(spec = BAG_garch_11_spec, data = BAG_zoo)
BAG_garch_11_fit #report this?

BAG_garch_11_roll <- ugarchroll(BAG_garch_11_spec, data = BAG_zoo, n.start = 730,
                                refit.every = 1, refit.window = "moving", solver = "hybrid",
                                calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)

report(BAG_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(BAG_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

#GJR
BAG_gjr_garch_11_spec <-  ugarchspec(variance.model = list(model = "gjrGARCH", 
                                  garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))

BAG_gjr_garch_11_fit <- ugarchfit(spec = BAG_gjr_garch_11_spec, data = BAG_zoo)
BAG_gjr_garch_11_fit

BAG_gjr_garch_11_roll <- ugarchroll(BAG_gjr_garch_11_spec, data = BAG_zoo, n.start = 730,
                                    refit.every = 1, refit.window = "moving", solver = "hybrid",
                                    calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)


report(BAG_gjr_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(BAG_gjr_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

#GJR with GED
BAG_gjr_garch_11_spec_ged <-  ugarchspec(variance.model = list(model = "gjrGARCH", 
                                garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "ged")

BAG_gjr_garch_11_fit_ged <- ugarchfit(spec = BAG_gjr_garch_11_spec_ged, data = BAG_zoo)
BAG_gjr_garch_11_fit_ged

BAG_gjr_garch_11_roll_ged <-ugarchroll(BAG_gjr_garch_11_spec_ged, data = BAG_zoo, n.start = 730,
                                  refit.every = 1, refit.window = "moving", solver = "hybrid",
                                     calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)


report(BAG_gjr_garch_11_roll_ged, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(BAG_gjr_garch_11_roll_ged, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

#Only models which are significant at 95% confidence level are garch(1,1) and gjrnorm
infocriteria(BAG_garch_11_fit) #AIC = -5.733762, use this one
infocriteria(BAG_gjr_garch_11_fit) #AIC = -5.711297

#Make new data frame until close of position
BAG3 <- filter(BAG, date <= "2017-07-17") #Keep until close of position (trough)

BAG3$returns <- Delt(BAG3$price)
BAG3$price <- NULL
BAG3 <- na.omit(BAG3)

BAG3$forecast <- BAG3$returns

#Obtain forecast values
BAG_garch_11_forecast <- ugarchforecast(BAG_garch_11_fit, n.ahead = 21)

fitted(BAG_garch_11_forecast)
sigma(BAG_garch_11_forecast)


BAG_CB <- as.data.frame(fitted(BAG_garch_11_forecast), ncol = 1)
colnames(BAG_CB) <- c("forecast")
BAG_CB$sigma <- sigma(BAG_garch_11_forecast)
BAG_CB$lowCB <- BAG_CB$forecast - (BAG_CB$sigma * qnorm(0.975))
BAG_CB$highCB <- BAG_CB$forecast + (BAG_CB$sigma * qnorm(0.975))
BAG_CB$actuals <- BAG3$returns[1208:1228]
BAG_CB$prob <- pnorm(BAG_CB$actuals, BAG_CB$forecast, BAG_CB$sigma)

#Replace forecast values in the data set
BAG3$forecast[1209:1228] <- 0.0006047133

BAG3$sigma <- 0

test <- as.matrix(sigma(BAG_garch_11_forecast))

BAG3$sigma[1209:1228] <- test
rm(test)

#Plot
ggplot(BAG3[1179:1228,], aes(date, returns)) + geom_line(stat = 'identity', size = 1) +
  geom_line(data=BAG3[1179:1228,], aes(date, forecast), size = 1.25, linetype = "dashed") +
  geom_ribbon(data=BAG3[1199:1228,],
              aes(ymin=forecast-(sigma*qnorm(0.975)),ymax=forecast + (sigma*qnorm(0.975))),
                                    alpha=0.3) +
  labs(title = "A.G. Barr Returns Forecast", subtitle = "Using GARCH(1,1)",
       x = "Date", y = "Returns")

############## DTY ################
DTY <- filter(price_data, code == "DTY")
DTY <- subset(DTY, select = c("price", "date"))
DTY$date <- as.Date(DTY$date, format = "%d/%m/%Y")
DTY <- arrange(DTY, date)
DTY <- na.omit(DTY)
DTY <- DTY[c("date", "price")]

#Plot
ggplot(DTY, aes(date, price)) + geom_line() + 
  labs(title = "Dignity PLC: Daily Share Price", x = "Date", y = "Share Price")


#Only use prices up to and including the position opening date
DTY <- filter(DTY, date <= "2018-01-16")

#Plot share prices
library(ggplot2)
ggplot(DTY, aes(date, price)) + geom_line() + labs(title = "Dignity PLC: Daily Share Price",
                                                   x = "Date", y = "Share Price")

#Need to obtain the returns
library(quantmod)
DTY$returns <- Delt(DTY$price)
DTY$price <- NULL
DTY <- na.omit(DTY)

#Use zoo package
library(zoo)

DTY_zoo <- read.zoo(DTY)

head(DTY_zoo)

#Check if ARCH effect (conditional heteroskedasticity) is present in the data
plot(DTY_zoo, main = "Company A Daily Returns", xlab = "Date", ylab = "Returns")

ggplot(DTY, aes(date, returns)) + geom_line() +
  labs(title = "Daily Returns: Dignity PLC", x = "Date", y = "Returns")

#Ljung Box test - H0 is NO autocorrelation
#Looking for autocorrelation in the squared data
#This is taken as the proxy for volatility
Box.test(coredata(DTY_zoo^2), type = "Ljung-Box", lag = 12)
#Extremely significant p-value so reject null hypothesis


#Lagrange Multiplier (LM)
#Also tests for volatility
#Null hypothesis: no ARCH effects
#Use FinTS package
library(FinTS)

ArchTest(coredata(DTY_zoo), lags = 12)
#Extremely significant p-value so reject null hypothesis


#Evidence of ARCH effects present in the data
library(rugarch)

DTY_garch_11_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                                mean.model = list(armaOrder = c(0,0)))

DTY_garch_11_fit <- ugarchfit(spec = DTY_garch_11_spec, data = DTY_zoo)
DTY_garch_11_fit

DTY_garch_11_roll <- ugarchroll(DTY_garch_11_spec, data = DTY_zoo, n.start = 1095,
                                refit.every = 1, refit.window = "moving", solver = "hybrid",
                                calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)

report(DTY_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(DTY_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)
#Rejected with 0.99 conf but accepted with 0.95 conf

#Plot using zoo object of forecasted VaR
DTY_VaR <- zoo(DTY_garch_11_roll@forecast$VaR[,1])
DTY_actual <- zoo(DTY_garch_11_roll@forecast$VaR[,2])
index(DTY_VaR) <- as.Date(rownames(DTY_garch_11_roll@forecast$VaR))
index(DTY_actual) <- as.Date(rownames(DTY_garch_11_roll@forecast$VaR))

plot(DTY_actual, type = "b", main = "DTY VaR Backtesting at 99% Conf Level",
     xlab = "Date", ylab = "VaR %")
lines(DTY_VaR, col = "green")

#Forecast
DTY_garch_11_forecast <- ugarchforecast(DTY_garch_11_fit, n.ahead = 3)
DTY_garch_11_forecast

qnorm(0.99)

0.02067*qnorm(0.99)


### What about GJR-GARCH? ###
DTY_gjr_garch_11_spec <-  ugarchspec(variance.model = list(model = "gjrGARCH", 
                                                           garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))

DTY_gjr_garch_11_fit <- ugarchfit(spec = DTY_gjr_garch_11_spec, data = DTY_zoo)
DTY_gjr_garch_11_fit

DTY_gjr_garch_11_roll <- ugarchroll(DTY_gjr_garch_11_spec, data = DTY_zoo, n.start = 1095,
                                    refit.every = 1, refit.window = "moving", solver = "hybrid",
                                    calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)


report(DTY_gjr_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(DTY_gjr_garch_11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)
#Significant at 99% confidence level for unconditional but not conditional
#Significant at 95% confidence level for both

#Plot using zoo object of forecasted VaR
DTY_VaR_gjr <- zoo(DTY_gjr_garch_11_roll@forecast$VaR[,1])
DTY_actual_gjr <- zoo(DTY_gjr_garch_11_roll@forecast$VaR[,2])
index(DTY_VaR_gjr) <- as.Date(rownames(DTY_gjr_garch_11_roll@forecast$VaR))
index(DTY_actual_gjr) <- as.Date(rownames(DTY_gjr_garch_11_roll@forecast$VaR))

plot(DTY_actual_gjr, type = "b", main = "Company A VaR Backtesting",
     xlab = "Date", ylab = "VaR %")
lines(DTY_VaR_gjr, col = "green")

#Forecast
DTY_gjr_garch_11_forecast <- ugarchforecast(DTY_gjr_garch_11_fit, n.ahead = 3)
DTY_gjr_garch_11_forecast

#Plot the forecast
plot(DTY_gjr_garch_11_forecast)
1

fitted(DTY_gjr_garch_11_forecast)
sigma(DTY_gjr_garch_11_forecast)

#create data frame
#DTY_forecast <- as.data.frame(c("2018-01-17", "2018-01-18", "2018-01-19"))
#colnames(DTY_forecast) <- c("date")
#DTY_forecast$date <- as.Date(DTY_forecast$date)#, format = "%d/%m/%Y")
#DTY_forecast$returns <- c(rep(0.0006554602, 3))
#DTY_forecast$sigma <- c(0.02409525, 0.02320916, 0.02240267)

#Create dataset which includes the dates until the close of short
DTY2 <- filter(price_data, code == "DTY")
DTY2 <- subset(DTY2, select = c("price", "date"))
DTY2$date <- as.Date(DTY2$date, format = "%d/%m/%Y")
DTY2 <- arrange(DTY2, date)
DTY2 <- na.omit(DTY2)
DTY2 <- DTY2[c("date", "price")]
DTY2 <- filter(DTY2, date <= "2018-01-19") #Keep until close of position

DTY2$returns <- Delt(DTY2$price)
DTY2$price <- NULL
DTY2 <- na.omit(DTY2)

DTY2$forecast <- DTY2$returns
DTY2$forecast[1360:1362] <- 0.0006554602

DTY2$sigma <- 0
DTY2$sigma[1360] <- 0.02409525 * qnorm(0.975)
DTY2$sigma[1361] <- 0.02320916 * qnorm(0.975)
DTY2$sigma[1362] <- 0.02240267 * qnorm(0.975)


DTY2 <- DTY2[1333:1362,]
library(ggplot2)
ggplot(DTY2, aes(date, returns)) + geom_line(stat = 'identity', size = 1.3) +
  geom_line(data=DTY2, aes(date, forecast), size = 1.25, linetype = "dashed") +
  geom_ribbon(data=DTY2[28:30,],aes(ymin=forecast-sigma,ymax=forecast + sigma),alpha=0.3) +
  labs(title = "Dignity PLC: Returns Forecast", subtitle = "Using GJR-GARCH(1,1)",
       x = "Date", y = "Returns")

#Plot the confidence interval
DTY_lower_bound <- 0.0006554602 - (0.02240267 * qnorm(0.975)) #-0.04325297
DTY_upper_bound <- 0.0006554602 + (0.02240267 * qnorm(0.975)) #0.04456389
DTY_x <- seq(-4, 4, length = 1000) * 0.02240267 + 0.0006554602
DTY_y <- dnorm(DTY_x, 0.0006554602, 0.02240267)

plot(DTY_x, DTY_y, type="n", xlab = "Return", ylab = "", 
     main = "95% CI of Return when Position was Closed", axes = FALSE, sub = "Dignity PLC",
     frame.plot = TRUE, xlim = c(-0.5, max(DTY_x)))
lines(DTY_x, DTY_y, lwd = 2)
Axis(side=1)
abline(v = DTY2$returns[30], col = "red", lwd = 2)
legend("top", lwd = 3, col = "red", legend = "Actual return", angle = 90)


#Probability of obtaining actual values
pnorm(0.0005173306, 0.0006554602, 0.02409525) #0.497713
pnorm(-0.0093071355, 0.0006554602, 0.02320916) #0.3338697
pnorm(-0.4979123173, 0.0006554602, 0.02240267) #5.063005e-110


### Testing different GARCH models ###

#GJR with GED errors
DTY_gjr_garch_11_spec_ged <-  ugarchspec(variance.model = list(model = "gjrGARCH", 
                                                               garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)),
                                         distribution.model = "ged")

DTY_gjr_garch_11_fit_ged <- ugarchfit(spec = DTY_gjr_garch_11_spec_ged, data = DTY_zoo)
DTY_gjr_garch_11_fit_ged

DTY_gjr_garch_11_roll_ged <-ugarchroll(DTY_gjr_garch_11_spec_ged, data = DTY_zoo, n.start = 1095,
                                       refit.every = 1, refit.window = "moving", solver = "hybrid",
                                       calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)


report(DTY_gjr_garch_11_roll_ged, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(DTY_gjr_garch_11_roll_ged, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

#The best model based on AIC is gjr with ged but that is not significant
infocriteria(DTY_garch_11_fit)
infocriteria(DTY_gjr_garch_11_fit)
infocriteria(DTY_gjr_garch_11_fit_ged)

#Fit all models with only two years initialisation period
garch_1_1_roll <-ugarchroll(DTY_garch_11_spec, data = DTY_zoo, n.start = 730,
                            refit.every = 1, refit.window = "moving", solver = "hybrid",
                            calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)

gjr_garch_roll <-ugarchroll(DTY_gjr_garch_11_spec, data = DTY_zoo, n.start = 730,
                            refit.every = 1, refit.window = "moving", solver = "hybrid",
                            calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)

ged_gjr_garch_roll <-ugarchroll(DTY_gjr_garch_11_spec_ged, data = DTY_zoo, n.start = 730,
                                refit.every = 1, refit.window = "moving", solver = "hybrid",
                                calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)

report(garch_1_1_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(garch_1_1_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

report(gjr_garch_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(gjr_garch_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

report(ged_gjr_garch_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(ged_gjr_garch_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)


### ACF ###
acf(DTY$returns^2, main = "Autocorrelation Plot for Sq Returns", sub = "Dignity PLC")
pacf(DTY$returns^2, main = "Partial Autocorrelation Plot for Sq Returns", sub = "Dignity PLC")

#Try the model with ARMA(1,3)???
test_spec <-  ugarchspec(variance.model = list(model = "gjrGARCH", 
                                               garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,3)))

test_fit <- ugarchfit(spec = test_spec, data = DTY_zoo)
test_fit

test_roll <- ugarchroll(test_spec, data = DTY_zoo, n.start = 1095,
                        refit.every = 1, refit.window = "moving", solver = "hybrid",
                        calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)


report(test_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(test_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

infocriteria(test_fit)
infocriteria(DTY_gjr_garch_11_fit) #Original model was better

