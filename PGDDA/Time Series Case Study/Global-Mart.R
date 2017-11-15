############### Retail-Giant Sales Forecasting Case Study #####
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Global Mart" is an online store super giant having worldwide operations.
# It takes orders and delivers across the globe and deals with all the major
# product categories - consumer, corporate & home office.

## AIM: The aim is to forecast the sales and the demand for the next 6 months,
# that would help to manage the revenue and inventory accordingly.This is to be
# done for the top 2 most profitable and consistently profitable segments.

################################################################

# Data Understanding
#------------------Data Understanding and Preparation ----------------------------
#Read raw data

# The following packages are loaded assuming they are already installed in the 
# R environment. If not, they can be installed using the command 
# "install.packages("package-name", dependencies = TRUE)"

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(graphics)
library(forecast)
library(tseries)

global <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE)

str(global)

summary(global)

#Since this is a transaction level data, the row id should be unique and
# no NAs should be present
length(unique(global$Row.ID)) == nrow(global)
sum(is.na(global$Row.ID))

#Identify columns having NA values
colSums(is.na(global))
table(is.na(global))

narows <- which(rowSums(is.na(global)) > 0)
length(narows)

#Postal.Code is the only column having NA values. However, it won't affect the
# analysis as we are looking at only time series data.

#Check for number of markets
unique(global$Market)
length(unique(global$Market))
unique(global$Segment)
length(unique(global$Segment))
#Number of markets and segments are same as provided in the Problem statement


#Converting Date columns to Date format
global$Order.Date <- dmy(global$Order.Date)
global$Ship.Date <- dmy(global$Ship.Date)
str(global)

#Converting strings to factors for categorical variables
categorical <- c("Ship.Mode", "Segment", "Market", "Region", "Category", "Sub.Category",
                 "Order.Priority")

global[,categorical] <- lapply(global[,categorical], as.factor)
str(global)
summary(global)

#creating segments by columns, Market, Segment, year and month
segmented <- global %>% mutate(month = as.numeric(format(Order.Date, "%m")), year = as.numeric(format(Order.Date, "%Y"))) %>%
                        group_by(Market, Segment, year, month) %>%
                        summarise(monthly_sales = sum(Sales),
                                  monthly_qty = sum(Quantity), 
                                  monthly_profit = sum(Profit))
View(segmented)

str(segmented)

#Calculating coefficient of variation for each segment
#Also calcualting total profit for each segment
segment_cov <- segmented %>% group_by(Market, Segment) %>%
               mutate(Coeff.Var = sd(monthly_profit)/mean(monthly_profit)*100) %>%
               mutate(segment_profit = sum(monthly_profit))
               
segment_cov <- segment_cov %>%  select(Market, Segment, segment_profit, Coeff.Var)

segment_cov <- segment_cov[!duplicated(segment_cov),] %>%
               arrange(desc(segment_profit), Coeff.Var)

View(segment_cov)

#Plot comparing the Coefficient of variation of Various markets and Segments
ggplot(segment_cov, aes(x = Market, y = Coeff.Var, fill = Segment)) +
       geom_bar(stat = "identity", color = "black", position = "dodge") +
       geom_text(stat = "identity", aes( label = round(Coeff.Var)), position = position_dodge(0.9),vjust = 0) +
       labs(y = "Coefficient of Variation", x = "Market") 

#Plot comparing the Total Profits of Various markets and Segments
ggplot(segment_cov, aes(x = Market, y = segment_profit, fill = Segment)) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  geom_text(stat = "identity", aes( label = round(segment_profit)), position = position_dodge(0.9),vjust = 0) +
  labs(y = "Total Profits", x = "Market") 

#Based on the segment_cov table, the 2 most profitable and consistently profitable
#segments are APAC-Consumer and EU-Consumer

#Function for assigning serial numbers to months
serialMonths <- function(x){ 
i= 1
for (i in 1:length(x)){
  x[i] = i
  i = i + 1
}
return(x)
}

apac_sales <- segmented %>% filter(Market == "APAC" & Segment == "Consumer") %>%
                            select(Market, Segment, year, month, monthly_sales)

apac_qty <- segmented %>% filter(Market == "APAC" & Segment == "Consumer") %>%
                            select(Market, Segment, year, month, monthly_qty)

eu_sales <- segmented %>% filter(Market == "EU" & Segment == "Consumer") %>%
                            select(Market, Segment, year, month, monthly_sales)

eu_qty <- segmented %>% filter(Market == "EU" & Segment == "Consumer") %>%
                            select(Market, Segment, year, month, monthly_qty)

apac_sales$month <- serialMonths(apac_sales$month)

View(apac_sales)

apac_qty$month <- serialMonths(apac_qty$month)

View(apac_qty)

eu_sales$month <- serialMonths(eu_sales$month)

View(eu_sales)

eu_qty$month <- serialMonths(eu_qty$month)

View(eu_qty)

#---------------Data Vizualization ------------------------------------------
#Creating and visualizing time series for the 4 data sets

apac_sales_ts <- ts(apac_sales$monthly_sales, start = c(2011,1), frequency = 12)

plot.ts(apac_sales_ts, main = "APAC, Consumer", xlab = "Year", ylab = "Sales")

#Identifying trend 
abline(reg = lm(apac_sales_ts ~ time(apac_sales_ts)))

#Year on year trend
plot(aggregate(apac_sales_ts, FUN = mean))

#Seasonal effect
boxplot(apac_sales_ts ~ cycle(apac_sales_ts), main = "APAC, Consumer",
        xlab = "Month", ylab = "Sales", col = "red")
#A seasonal effect can be seen as sales increase during months 5 and 6 and 
#also during months 10 and 11.

apac_qty_ts <- ts(apac_qty$monthly_qty, start = c(2011,1), frequency = 12)

plot.ts(apac_qty_ts, main = "APAC, Consumer", xlab = "Year", ylab = "Quantity")

#Identifying trend 
abline(reg = lm(apac_qty_ts ~ time(apac_qty_ts)))

#Year on year trend
plot(aggregate(apac_qty_ts, FUN = mean))

#Seasonal effect
boxplot(apac_qty_ts ~ cycle(apac_qty_ts), main = "APAC, Consumer",
        xlab = "Month", ylab = "Quantity", col = "yellow")
#A seasonal effect can be seen as sales increase during months 5 and 6 and 
#also during 11.

eu_sales_ts <- ts(eu_sales$monthly_sales, start = c(2011,1), frequency = 12)

plot.ts(eu_sales_ts, main = "EU, Consumer", xlab = "Year", ylab = "Sales")

#Identifying trend 
abline(reg = lm(eu_sales_ts ~ time(eu_sales_ts)))

#Year on year trend
plot(aggregate(eu_sales_ts, FUN = mean))

#Seasonal effect
boxplot(eu_sales_ts ~ cycle(eu_sales_ts),main = "EU, Consumer",
        xlab = "Month", ylab = "Sales", col = "blue")
#A seasonal effect can be seen as sales increase during months 8 and 9 and 
#also during months 11 and 12.

eu_qty_ts <- ts(eu_qty$monthly_qty, start = c(2011,1), frequency = 12)

plot.ts(eu_qty_ts, main = "EU, Consumer", xlab = "Year", ylab = "Quantity")

#Identifying trend 
abline(reg = lm(eu_qty_ts ~ time(eu_qty_ts)))

#Year on year trend
plot(aggregate(eu_qty_ts, FUN = mean))

#Seasonal effect
boxplot(eu_qty_ts ~ cycle(eu_qty_ts), main = "EU, Consumer",
        xlab = "Month", ylab = "Quantity", col = "green")
#A seasonal effect can be seen as sales increase during months 8 and 9 and 
#also during 11 and 12.

#################### Model Building and Evaluation ##########################
#------------------APAC,Consumer Category Sales Forecasting ---------------------
#------------------ Classical Decomposition Method ---------------------

#Creating the model using first 42 rows and the remaining 6 rows will be
#used for testing the model.

indata_apac_sales <- apac_sales[1:42,]

indata_apac_sales_ts <- ts(indata_apac_sales$monthly_sales, start = c(2011,1), frequency = 12)

plot.ts(indata_apac_sales_ts)

# function for smoothing time series
smoothing_ma <- function(timeser, Window){
w <- Window
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
 }

return(smoothedseries)
}
#Smoothing the series - Moving Average Smoothing

smoothed_apac_sales <- smoothing_ma(indata_apac_sales_ts, 1)

lines(smoothed_apac_sales, col="blue", lwd=2)

timevals_in <- indata_apac_sales$month

#Building a model on the smoothed time series using classical decomposition
#converting the smoothed time series to a dataframe.

smth_apac_sales_df <- as.data.frame(cbind(timevals_in, as.vector(smoothed_apac_sales)))
colnames(smth_apac_sales_df) <- c('Month', 'Sales')

#Original timeseries shows increasing amplitude of peaks and troughs along with
#increasing trend but it is not very clear that the amplitudes are really increasing
#or it is because of random spikes. So, we will try to fit both multiplicative model
# and additive models with trend and seasonality and then find which one fits
#best.
#Seasonality will be modeled using a sinusoidal function
#After several trials, the following combination gave the least MAPE

lmfit_apac_sales <- lm(Sales ~ sin(0.5*Month) + poly(Month,1),  data=smth_apac_sales_df)
global_pred_apac_sales <- predict(lmfit_apac_sales, Month=timevals_in)
summary(global_pred_apac_sales)

global_pred_apac_sales_ts <- ts(global_pred_apac_sales, start = c(2011,1), frequency = 12)

lines(global_pred_apac_sales_ts, col='red', lwd=3)

#Locally predictable part as ARMA series
local_pred_apac_sales_ts <- indata_apac_sales_ts - global_pred_apac_sales_ts
plot(local_pred_apac_sales_ts, col='red', type = "l")
acf(local_pred_apac_sales_ts, lag.max = 20)
acf(local_pred_apac_sales_ts, type="partial", lag.max = 20)
apac_sales_armafit <- auto.arima(local_pred_apac_sales_ts)

tsdiag(apac_sales_armafit)
apac_sales_armafit

#ARIMA(0,0,0)(1,1,0)[12] indicates there is a locally predictable part as
#seasonal ARIMA with AR at lag 1.

#We'll check if the residual series is white noise

apac_sales_resid <- local_pred_apac_sales_ts-fitted(apac_sales_armafit)

#Conducting stationarity tests for noise
adf.test(apac_sales_resid,alternative = "stationary")
#As adf test p-value is slightly aboce.05, we will run Ljung-Box test
#also for Stationarity
kpss.test(apac_sales_resid)
Box.test(apac_sales_resid, lag = 20, type = "Ljung-Box")

#Since Both KPSS and Ljung-Box test indicate stationarity, we can take the 
#residual series as noise

#Evaluating the model using MAPE
#Make a prediction for the last 6 months

outdata_apac_sales <- as.data.frame(apac_sales[43:48,])
timevals_out <- outdata_apac_sales$month

global_pred_outdata_apac_sales <- predict(lmfit_apac_sales,data.frame(Month =timevals_out))

#Since there is locally predictable series, making local predictions  
local_pred_outdata_apac_sales <- predict(apac_sales_armafit, n.ahead = 6)

#Combining local and global predictions
apac_sales_fcast <- global_pred_outdata_apac_sales + as.vector(local_pred_outdata_apac_sales$pred)

#Now, let's compare our prediction with the actual values, using MAPE

apac_sales_MAPE_classic <- accuracy(apac_sales_fcast, outdata_apac_sales[,5])[5]
apac_sales_MAPE_classic

#Hence MAPE classical = 13.60702

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

apac_sales_classic_pred <- ts(c(global_pred_apac_sales + apac_sales_armafit$fitted, 
                                apac_sales_fcast), start = c(2011,1), frequency = 12)
plot(apac_sales_ts, col = "black", main = "APAC, Consumer Sales", xlab = "Year", ylab = "Sales")
lines(apac_sales_classic_pred, col = "red")

#Forecast for next 6 months using classical decomposition model
apac_sales_forecast_classic <- forecast(apac_sales_classic_pred, h = 6)

apac_sales_forecast_classic

plot(apac_sales_forecast_classic, main = "APAC Consumer Sales Forecast (Classical Decompositon)",
     xlab = "Year", ylab = "Sales")

#--------------Auto Arima for APAC,Consumer Sales Forecasting --------------
# ARIMA fit

apac_sales_autoarima <- auto.arima(indata_apac_sales_ts, allowdrift = FALSE)
apac_sales_autoarima
tsdiag(apac_sales_autoarima)
plot(apac_sales_autoarima$x, col="black")
lines(fitted(apac_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise

apac_sales_resi_auto_arima <- apac_sales_ts - fitted(apac_sales_autoarima)

#Conducting stationarity tests for noise
adf.test(apac_sales_resi_auto_arima,alternative = "stationary")
kpss.test(apac_sales_resi_auto_arima)
Box.test(apac_sales_resi_auto_arima, lag = 20, type = "Ljung-Box")
#Since Both KPSS and Ljung-Box test indicate stationarity, we can take the 
#residual series as noise

#Also, let's evaluate the model using MAPE
apac_sales_fcast_auto_arima <- predict(apac_sales_autoarima, n.ahead = 6)

apac_sales_MAPE_auto_arima <- accuracy(apac_sales_fcast_auto_arima$pred,outdata_apac_sales[,5])[5]
apac_sales_MAPE_auto_arima

# MAPE for auto-arima = 24.30024

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

apac_sales_auto_arima_pred <- ts(c(fitted(apac_sales_autoarima),apac_sales_fcast_auto_arima$pred), start = c(2011,1),frequency = 12)
plot(apac_sales_ts, col = "black")
lines(apac_sales_auto_arima_pred, col = "red")

#Make forecats for the next 6 months using AutoArima

apac_sales_forecast_arima <- forecast(apac_sales_auto_arima_pred, h = 6)

apac_sales_forecast_arima

plot(apac_sales_forecast_arima, main = "APAC Sales Forecast (AUTO ARIMA Method)")

#Since classical decomposition has lower MAPE, hence classical decomposition
#model is better than the auto arima model

#------------------APAC,Consumer Quantity Forecasting ---------------------
#------------------ Classical Decomposition Method ---------------------

#Creating the model using first 42 rows and the remaining 6 rows will be
#used for testing the model.

indata_apac_qty <- apac_qty[1:42,]

indata_apac_qty_ts <- ts(indata_apac_qty$monthly_qty, start = c(2011,1), frequency = 12)

plot.ts(indata_apac_qty_ts)

#Smoothing the series - Moving Average Smoothing

smoothed_apac_qty <- smoothing_ma(indata_apac_qty_ts, 1)

lines(smoothed_apac_qty, col="blue", lwd=2)

timevals_in <- indata_apac_qty$month

#Building a model on the smoothed time series using classical decomposition
#converting the smoothed time series to a dataframe.

smth_apac_qty_df <- as.data.frame(cbind(timevals_in, as.vector(smoothed_apac_qty)))
colnames(smth_apac_qty_df) <- c('Month', 'Quantity')

#Original model shows constant amplitude of peaks and troughs along with
#increasing trend. So, we will try to fit an additive model with trend
#and seasonality.
#Fitting a multiplictive model with trend and seasonality to the data. Seasonality
#will be modeled using a sinusoidal function
#After several trials, the following combination gave the least MAPE

lmfit_apac_qty <- lm(Quantity ~ sin(0.5*Month) + poly(Month,3),  data=smth_apac_qty_df)
global_pred_apac_qty <- predict(lmfit_apac_qty, Month=timevals_in)
summary(global_pred_apac_qty)

global_pred_apac_qty_ts <- ts(global_pred_apac_qty, start = c(2011,1), frequency = 12)

lines(global_pred_apac_qty_ts, col='red', lwd=2)

#Locally predictable part as ARMA series
local_pred_apac_qty_ts <- indata_apac_qty_ts - global_pred_apac_qty_ts
plot(local_pred_apac_qty_ts, col='red', type = "l")
acf(local_pred_apac_qty_ts, lag.max = 20)
acf(local_pred_apac_qty_ts, type="partial", lag.max = 20)
apac_qty_armafit <- auto.arima(local_pred_apac_qty_ts)

tsdiag(apac_qty_armafit)
apac_qty_armafit

#ARIMA(0,0,0)(1,0,0)[12] indicates there is a locally predictable part as
#seasonal AR(1) model.

#We'll check if the residual series is white noise

apac_qty_resid <- local_pred_apac_qty_ts - fitted(apac_qty_armafit)

#Conducting stationarity tests for noise
adf.test(apac_qty_resid,alternative = "stationary")
kpss.test(apac_qty_resid)
Box.test(apac_qty_resid, lag = 20, type = "Ljung-Box")
#Since All 3 tests indicate stationarity, we can take the 
#residual series as noise


#Evaluating the model using MAPE
#Make a prediction for the last 6 months

outdata_apac_qty <- as.data.frame(apac_qty[43:48,])
timevals_out <- outdata_apac_qty$month

global_pred_outdata_apac_qty <- predict(lmfit_apac_qty,data.frame(Month =timevals_out))

#Since there is locally predictable series, making local predictions  
local_pred_outdata_apac_qty <- predict(apac_qty_armafit, n.ahead = 6)

#Combining local and global predictions
apac_qty_fcast <- global_pred_outdata_apac_qty + as.vector(local_pred_outdata_apac_qty$pred)

#Now, let's compare our prediction with the actual values, using MAPE

apac_qty_MAPE_classic <- accuracy(apac_qty_fcast, outdata_apac_qty[,5])[5]
apac_qty_MAPE_classic

#Hence MAPE classical = 19.68485

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

apac_qty_classic_pred <- ts(c(global_pred_apac_qty + apac_qty_armafit$fitted, apac_qty_fcast), start = c(2011,1), frequency = 12)
plot(apac_qty_ts, col = "black", main = "APAC Quantity",
                  xlab = "Year", ylab = "Quantity")
lines(apac_qty_classic_pred, col = "red")

#Forecast using Classical Decomposition model
apac_qty_forecast <- forecast(apac_qty_classic_pred, h = 6)

apac_qty_forecast

plot(apac_qty_forecast, main = "APAC Quantity Forecast (Classical Decomposition)",
     xlab = "Year", ylab = "Quantity")

#--------------Auto Arima for APAC,Consumer Quantity Forecasting --------------
# ARIMA fit

apac_qty_autoarima <- auto.arima(indata_apac_qty_ts, allowdrift = FALSE)
apac_qty_autoarima
tsdiag(apac_qty_autoarima)
plot(apac_qty_autoarima$x, col="black")
lines(fitted(apac_qty_autoarima), col="red")

#Again, let's check if the residual series is white noise

apac_qty_resi_auto_arima <- apac_qty_ts - fitted(apac_qty_autoarima)

#Conducting stationarity tests for noise
adf.test(apac_qty_resi_auto_arima,alternative = "stationary")
kpss.test(apac_qty_resi_auto_arima)
Box.test(apac_qty_resi_auto_arima, lag = 20, type = "Ljung-Box")
#Since All 3 tests indicate stationarity, we can take the 
#residual series as noise

#Also, let's evaluate the model using MAPE
apac_qty_fcast_auto_arima <- predict(apac_qty_autoarima, n.ahead = 6)

apac_qty_MAPE_auto_arima <- accuracy(apac_qty_fcast_auto_arima$pred,outdata_apac_qty[,5])[5]
apac_qty_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

apac_qty_auto_arima_pred <- ts(c(fitted(apac_qty_autoarima),apac_qty_fcast_auto_arima$pred), start = c(2011,1),frequency = 12)
plot(apac_qty_ts, col = "black")
lines(apac_qty_auto_arima_pred, col = "red")

# MAPE for auto-arima = 23.33336

#Make forecats for the next 6 months using AutoArima

apac_qty_forecast_arima <- forecast(apac_qty_auto_arima_pred, h = 6)

apac_qty_forecast_arima

plot(apac_qty_forecast_arima, main = "APAC Quantity Forecast (AUTO ARIMA Method)")

#Since classical decomposition has lower MAPE, hence classical decomposition
#model is better than Auto arima model

#------------------EU,Consumer Sales Forecasting ---------------------
#------------------ Classical Decomposition Method ---------------------

#Creating the model using first 42 rows and the remaining 6 rows will be
#used for testing the model.

indata_eu_sales <- eu_sales[1:42,]

indata_eu_sales_ts <- ts(indata_eu_sales$monthly_sales, start = c(2011,1), frequency = 12)

plot.ts(indata_eu_sales_ts)

#Smoothing the series - Moving Average Smoothing
smoothed_eu_sales <- smoothing_ma(indata_eu_sales_ts, 1)

lines(smoothed_eu_sales, col="blue", lwd=2)

timevals_in <- indata_eu_sales$month

#Building a model on the smoothed time series using classical decomposition
#converting the smoothed time series to a dataframe.

smth_eu_sales_df <- as.data.frame(cbind(timevals_in, as.vector(smoothed_eu_sales)))
colnames(smth_eu_sales_df) <- c('Month', 'Sales')

#Original timeseries shows increasing amplitude of peaks and troughs along with
#increasing trend but it is not very clear that the amplitudes are really increasing
#or it is because of random spikes. So, we will try to fit both multiplicative model
# and additive models with trend and seasonality and then find which one fits
#best.
#Seasonality will be modeled using a sinusoid function
#After several trials, the following combination gave the least MAPE

lmfit_eu_sales <- lm(Sales ~ sin(0.5*Month) + poly(Month,3),  data=smth_eu_sales_df)
global_pred_eu_sales <- predict(lmfit_eu_sales, Month=timevals_in)
summary(global_pred_eu_sales)

global_pred_eu_sales_ts <- ts(global_pred_eu_sales, start = c(2011,1), frequency = 12)

lines(global_pred_eu_sales_ts, col='red', lwd=2)

#Locally predictable part as ARMA series
local_pred_eu_sales_ts <- indata_eu_sales_ts - global_pred_eu_sales_ts
plot(local_pred_eu_sales_ts, col='red', type = "l")
acf(local_pred_eu_sales_ts, lag.max = 20)
acf(local_pred_eu_sales_ts, type="partial", lag.max = 20)
eu_sales_armafit <- auto.arima(local_pred_eu_sales_ts)

tsdiag(eu_sales_armafit)
eu_sales_armafit
#ARIMA(0,0,0)(1,1,0)[12] indicates there is a locally predictable part as
#seasonal ARIMA with AR at lag 1.

#We'll check if the residual series is white noise
eu_sales_resid <- local_pred_eu_sales_ts - fitted(eu_sales_armafit)

#Conducting stationarity tests for noise
adf.test(eu_sales_resid,alternative = "stationary")
kpss.test(eu_sales_resid)
Box.test(eu_sales_resid, lag = 20, type = "Ljung-Box")
#Since All 3 tests indicate stationarity, we can take the 
#residual series as noise

#Evaluating the model using MAPE
#Make a prediction for the last 6 months
outdata_eu_sales <- as.data.frame(eu_sales[43:48,])
timevals_out <- outdata_eu_sales$month

global_pred_outdata_eu_sales <- predict(lmfit_eu_sales,data.frame(Month =timevals_out))

#Since there is locally predictable series, making local predictions  
local_pred_outdata_eu_sales <- predict(eu_sales_armafit, n.ahead = 6)

#Combining local and global predictions
eu_sales_fcast <- global_pred_outdata_eu_sales + as.vector(local_pred_outdata_eu_sales$pred)

#Now, let's compare our prediction with the actual values, using MAPE

eu_sales_MAPE_classic <- accuracy(eu_sales_fcast, outdata_eu_sales[,5])[5]
eu_sales_MAPE_classic

#Hence MAPE classical = 9.17967

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

eu_sales_classic_pred <- ts(c(global_pred_eu_sales + eu_sales_armafit$fitted,
                              eu_sales_fcast), start = c(2011,1), frequency = 12)
plot(eu_sales_ts, col = "black", main = "EU, Consumer Sales",
     xlab = "Year", ylab = "Sales")
lines(eu_sales_classic_pred, col = "red")

#Forecast for next 6 months using classical decomposition model
eu_sales_forecast_classic <- forecast(eu_sales_classic_pred, h = 6)

eu_sales_forecast_classic

plot(eu_sales_forecast_classic, main = "EU, Consumer Sales Forecast (Classical Decompositon)",
     xlab = "Year", ylab = "Sales")


#--------------Auto Arima for EU,Consumer Sales Forecasting --------------
# ARIMA fit

eu_sales_autoarima <- auto.arima(indata_eu_sales_ts, allowdrift = FALSE)
eu_sales_autoarima
tsdiag(eu_sales_autoarima)
plot(eu_sales_autoarima$x, col="black")
lines(fitted(eu_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise

eu_sales_resi_auto_arima <- eu_sales_ts - fitted(eu_sales_autoarima)

adf.test(eu_sales_resi_auto_arima,alternative = "stationary")
kpss.test(eu_sales_resi_auto_arima)
Box.test(eu_sales_resi_auto_arima, lag = 20, type = "Ljung-Box")
#Since Both KPSS and Ljung-Box test indicate stationarity, we can take the 
#residual series as noise

#Also, let's evaluate the model using MAPE
eu_sales_fcast_auto_arima <- predict(eu_sales_autoarima, n.ahead = 6)

eu_sales_MAPE_auto_arima <- accuracy(eu_sales_fcast_auto_arima$pred,outdata_eu_sales[,5])[5]
eu_sales_MAPE_auto_arima

# MAPE for auto-arima = 25.14527

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
eu_sales_auto_arima_pred <- ts(c(fitted(eu_sales_autoarima),eu_sales_fcast_auto_arima$pred), start = c(2011,1),frequency = 12)
plot(eu_sales_ts, col = "black")
lines(eu_sales_auto_arima_pred, col = "red")

#Since classical decomposition has lower MAPE, hence classical decomposition
#model is better than Auto arima model

#Make forecats for the next 6 months using AutoArima

eu_sales_forecast_arima <- forecast(eu_sales_auto_arima_pred, h = 6)

eu_sales_forecast_arima

plot(eu_sales_forecast_arima, main = "EU, Consumer Sales Forecast (AUTO ARIMA Method)")

#------------------EU,Consumer Quantity Forecasting ---------------------
#------------------ Classical Decomposition Method ---------------------

#Creating the model using first 42 rows and the remaining 6 rows will be
#used for testing the model.

indata_eu_qty <- eu_qty[1:42,]

indata_eu_qty_ts <- ts(indata_eu_qty$monthly_qty, start = c(2011,1), frequency = 12)

plot.ts(indata_eu_qty_ts)

#Smoothing the series - Moving Average Smoothing
smoothed_eu_qty <- smoothing_ma(indata_eu_qty_ts, 1)

lines(smoothed_eu_qty, col="blue", lwd=2)

timevals_in <- indata_eu_qty$month

#Building a model on the smoothed time series using classical decomposition
#converting the smoothed time series to a dataframe.

smth_eu_qty_df <- as.data.frame(cbind(timevals_in, as.vector(smoothed_eu_qty)))
colnames(smth_eu_qty_df) <- c('Month', 'Quantity')

#Original model shows constant amplitude of peaks and troughs along with
#increasing trend. So, we will try to fit an additive model with trend
#and seasonality.
#Fitting a multiplictive model with trend and seasonality to the data. Seasonality
#will be modeled using a sinusoid function
#After several trials, the following combination gave the least MAPE

lmfit_eu_qty <- lm(Quantity ~ sin(0.5*Month) + poly(Month,4),  data=smth_eu_qty_df)
global_pred_eu_qty <- predict(lmfit_eu_qty, Month=timevals_in)
summary(global_pred_eu_qty)

global_pred_eu_qty_ts <- ts(global_pred_eu_qty, start = c(2011,1), frequency = 12)

lines(global_pred_eu_qty_ts, col='red', lwd=2)

#Locally predictable part as ARMA series
local_pred_eu_qty_ts <- indata_eu_qty_ts - global_pred_eu_qty_ts
plot(local_pred_eu_qty_ts, col='red', type = "l")
acf(local_pred_eu_qty_ts)
acf(local_pred_eu_qty_ts, type="partial")
eu_qty_armafit <- auto.arima(local_pred_eu_qty_ts)

tsdiag(eu_qty_armafit)
eu_qty_armafit
#ARIMA(0,0,0)(1,0,0)[12] indicates there is a locally predictable part as
#seasonal AR(1) model.

#We'll check if the residual series is white noise
eu_qty_resid <- local_pred_eu_qty_ts - fitted(eu_qty_armafit)

#Conducting stationarity tests for noise
adf.test(eu_qty_resid,alternative = "stationary")
kpss.test(eu_qty_resid)
Box.test(eu_qty_resid, lag = 20, type = "Ljung-Box")
#Since All 3 tests indicate stationarity, we can take the 
#residual series as noise

#Evaluating the model using MAPE
#Make a prediction for the last 6 months

outdata_eu_qty <- as.data.frame(eu_qty[43:48,])
timevals_out <- outdata_eu_qty$month

global_pred_outdata_eu_qty <- predict(lmfit_eu_qty,data.frame(Month=timevals_out))

#Since there is locally predictable series, making local predictions  
local_pred_outdata_eu_qty <- predict(eu_qty_armafit, n.ahead = 6)

#Combining local and global predictions
eu_qty_fcast <- global_pred_outdata_eu_qty +as.vector(local_pred_outdata_eu_qty$pred)

#Now, let's compare our prediction with the actual values, using MAPE
eu_qty_MAPE_classic <- accuracy(eu_qty_fcast, outdata_eu_qty[,5])[5]
eu_qty_MAPE_classic

#Hence MAPE classical = 12.07027

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

eu_qty_classic_pred <- ts(c(global_pred_eu_qty + eu_qty_armafit$fitted,
                            eu_qty_fcast), start = c(2011,1), frequency = 12)
plot(eu_qty_ts, col = "black", main = "EU, Consumer Quantity",
     xlab = "Year", ylab = "Quantity")
lines(eu_qty_classic_pred, col = "red")

#Forecast using Classical Decomposition model
eu_qty_forecast <- forecast(eu_qty_classic_pred, h = 6)

eu_qty_forecast

plot(eu_qty_forecast, main = "EU, Consumer Quantity Forecast (Classical Decompositon)",
     xlab = "Year", ylab = "Quantity")

#--------------Auto Arima for EU,Consumer Quantity Forecasting --------------
# ARIMA fit

eu_qty_autoarima <- auto.arima(indata_eu_qty_ts, allowdrift = FALSE)
eu_qty_autoarima
tsdiag(eu_qty_autoarima)
plot(eu_qty_autoarima$x, col="black")
lines(fitted(eu_qty_autoarima), col="red")

#Again, let's check if the residual series is white noise

eu_qty_resi_auto_arima <- eu_qty_ts - fitted(eu_qty_autoarima)

#Conducting stationarity tests for noise
adf.test(eu_qty_resi_auto_arima,alternative = "stationary")
kpss.test(eu_qty_resi_auto_arima)
Box.test(eu_qty_resi_auto_arima, lag = 20, type = "Ljung-Box")

#Also, let's evaluate the model using MAPE
eu_qty_fcast_auto_arima <- predict(eu_qty_autoarima, n.ahead = 6)

eu_qty_MAPE_auto_arima <- accuracy(eu_qty_fcast_auto_arima$pred,outdata_eu_qty[,5])[5]
eu_qty_MAPE_auto_arima

# MAPE for auto-arima = 35.53743

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

eu_qty_auto_arima_pred <- ts(c(fitted(eu_qty_autoarima),eu_qty_fcast_auto_arima$pred), start = c(2011,1),frequency = 12)
plot(eu_qty_ts, col = "black")
lines(eu_qty_auto_arima_pred, col = "red")

#Make forecasts for the next 6 months using AutoArima
eu_qty_forecast_arima <- forecast(eu_qty_auto_arima_pred, h = 6)
eu_qty_forecast_arima
plot(eu_qty_forecast_arima, main = "EU Quantity Forecast (AUTO ARIMA Method)")

#Since classical decomposition has lower MAPE, hence classical decomposition
#model is better than the auto arima model
