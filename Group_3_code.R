###############################
###############################



# # EU ETS spot volume analysis
# 
# # Authors: Flavio Salvatore Boccia, Ludovico Costa,Michele Facconi, Alessandro Pigato
# 
# # Politecnico di Milano
# 
# # 25 June 2024



###############################
###############################

# Sections
#
# 1 - Import the datasets
# 2 - Data cleaning and time series computing
# 3 - Building a unique object with volume, ice future prices, brent price, coal and vix
# 4 - Test for stationary data
# 5 - Test for arch effect
# 6 - Model selection and fitting
# 7 - Model selection
# 8 - Exogenous variable taken into account
# 9 - Forecasting
# 10 - Accuracy metrics

###############################
###############################

rm(list = ls())

# Load necessary libraries
install.packages("nortest") # To use Anderson-Darling test
install.packages("tseries") # To use Augmented Dickey-Fuller test
install.packages("rugarch") # Used in modelling
install.packages("fGarch") # Used in modelling
install.packages("FinTS") # Used in ARCH test
install.packages("dynlm") # To add lags in the model
install.packages("vars") # To use VAR
install.packages("nlWaldTest") # To use non-linear Wald test
install.packages("lmtest") # Used in BP test
install.packages("broom")
install.packages("car")
install.packages("sandwich")
install.packages("knitr")
install.packages("forecast")
install.packages("tsbox")
install.packages("stats")
install.packages("zoo")
install.packages("vrtest")
install.packages("strucchange")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("changepoint")
install.packages("stats")
install.packages("moments")
install.packages('nortest')
install.packages("urca")
install.packages("robustHD")
install.packages("stlplus")
install.packages("prophet")
install.packages("TTR")
install.packages("dplyr")
library(dplyr)
library(TTR)
library(prophet)
library(stlplus)
library(robustHD)
library(ggplot2)
library(MASS)
library(forecast)
library(tseries)
library(readr)
library(dplyr)
library(urca)
library(nortest)
library(DescTools)
library(readr)                                                                          
library(moments)                                                                        
library(nortest)                                                                        
library(ggplot2)                                                                        
library(tseries)                                                                        
library(rugarch)                                                                        
library(fGarch)                                                                        
library(FinTS)                                                                         
library(dynlm)                                                                          
library(vars)                                                                      
library(nlWaldTest)                                                                     
library(lmtest)                                                                         
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(FinTS)
library(strucchange)
library(tidyverse)
library(lubridate)
library(changepoint)                                                                   
library(stats)
library(moments)


#########################
# 1 - Import the datasets
#########################

# Spot transactions
transaction <- read.csv("transaction.csv")
transaction$date <- as.Date(transaction$date)

# ICE Future prices
future_prices <- read.csv("Daily_Future.csv")
future_prices$date <- as.Date(future_prices$Date)

# Brent prices
brent<- read.csv("Brent Oil Futures Historical Data.csv")
brent$Date <- as.Date(brent$Date, format = "%m/%d/%Y")

# Coal prices
coal<- read.csv("Coal.csv")
coal$date <- as.Date(paste0(substr(coal$Date, 1, 6), "20", substr(coal$Date, 7, 8)), format = "%m/%d/%Y")

# VIX index
vix<- read.csv("vix.csv")
vix$date <- as.Date(vix$DATE, format = "%Y-%m-%d")


#############################################
# 2 - Data cleaning and time series computing
#############################################

# Specify the date range
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2020-5-5")

transaction_subset <- subset(transaction, date >= start_date & date <= end_date)

future_prices_subset<- subset(future_prices, date >= start_date & date <= end_date)
future_prices_subset <- future_prices_subset[, c("CLOSE", "date")]

coal<- subset(coal, date >= start_date & date <= end_date)
coal<-coal[, c("date", "Close")]
names(coal) <- c("date", "coal_daily_returns")
coal$coal_daily_returns <- c(NA, diff(log(coal$coal_daily_returns)))
coal <- coal[-1, ]

vix<- subset(vix, date >= start_date & date <= end_date)
vix<-vix[, c("date", "VIXCLS")]
names(vix) <- c("date", "vix")
vix$vix<-as.numeric(vix$vix)
vix$vix <- na.approx(vix$vix, na.rm = FALSE)

brent_reversed<-subset(brent, Date >= start_date & Date <= end_date)
brent<- brent_reversed[nrow(brent_reversed):1, ]
brent_aux<-brent[, c("Date", "Price")]
names(brent_aux) <- c("date", "Price")


# Here we divide between in sample (01-01-2013 -> 15-5-2019) and out of sample (16-05-2019 -> 05-05-2020)
mid_end_date <- as.Date("2019-5-15")
transaction_in<- subset(transaction, date >= start_date & date <= mid_end_date)
transaction_out<- subset(transaction, date > mid_end_date & date <= end_date)
daily_transactions_in<- aggregate(amount ~ date, data = transaction_in, FUN = sum)
daily_transactions_out<- aggregate(amount ~ date, data = transaction_out, FUN = sum)

# Here we display some features of the transactions dataset
summary(daily_transactions_in)
summary(daily_transactions_out)
sd(daily_transactions_in$amount)
sd(daily_transactions_out$amount)
skewness((daily_transactions_in$amount))
skewness(daily_transactions_out$amount)
kurtosis(daily_transactions_in$amount)
kurtosis(daily_transactions_out$amount)

# Aggregate the total amount per day
daily_transactions<- aggregate(amount ~ date, data = transaction_subset, FUN = sum)

# Plot the daily amount
names(daily_transactions) <- c("date", "total_amount")
ggplot(daily_transactions, aes(x = date, y = (total_amount))) +
  geom_line(color = "blue") +
  labs(title = "Daily Transaction Volumes", x = "Date", y = "Total Amount") +
  theme_minimal()

# Plot the daily transaction volumes in log scale
ggplot(daily_transactions, aes(x = date, y = log(total_amount))) +
  geom_line(color = "blue") +
  labs(title = "Daily Transaction Volumes", x = "Date", y = "Total Amount") +
  theme_minimal()

# Adding the day of week feature
daily_transactions$day <- weekdays(as.Date(daily_transactions$date))

# We look for every Saturday and Sunday and eliminate those observation since they are not reliable
index <- which(daily_transactions$day != "sabato" & daily_transactions$day != "domenica")
daily_transaction_cleaned <- daily_transactions[index, ]
daily_transaction_cleaned$log_total_amount <- log(daily_transaction_cleaned$total_amount)
daily_transaction_cleaned$diff_log <- c(0, diff(daily_transaction_cleaned$log_total_amount))


# We eliminate outliers taking 14 and 21 as lower and upper bound for our data
dates_below_14 <- daily_transaction_cleaned$date[daily_transaction_cleaned$log_total_amount < 14]

dates_above_21 <- daily_transaction_cleaned$date[daily_transaction_cleaned$log_total_amount >21]

daily_transaction_cleaned <- daily_transaction_cleaned[!daily_transaction_cleaned$date %in% dates_below_14, ]
daily_transaction_cleaned <- daily_transaction_cleaned[!daily_transaction_cleaned$date %in% dates_above_21, ]

ggplot(daily_transaction_cleaned, aes(x = date, y = atan(log(total_amount)))) +
  geom_line(color = "blue") +
  labs(title = "Daily Transaction Volumes without outliers", x = "Date", y = "Total Amount") +
  theme_minimal()



########################################################################################
# 3 - Building a unique object with volume, ice future prices, brent price, coal and vix
########################################################################################

# We merge all the datasets and we interpolate for missing values
merged_data <- left_join(daily_transaction_cleaned, future_prices_subset,  by = "date")
merged_data <- left_join(merged_data, brent_aux, by = "date")
merged_data<-left_join(merged_data, coal, by="date")
merged_data<-left_join(merged_data, vix, by="date")
colnames(merged_data) <- c("date", "Total_Amount_Volume", "Weekday", "Log_Amount", "Diff_Log_Amount", "Daily_Ret_Future_Price",  "Brent_Price","coal_daily_returns", "vix")
merged_data$Daily_Ret_Future_Price<-c(NA,diff((merged_data$Daily_Ret_Future_Price)))
merged_data$Daily_Ret_Future_Price[1]<- 0
merged_data$Brent_Price<-c(NA,diff((merged_data$Brent_Price)))
merged_data$Brent_Price[1]<- merged_data$Brent_Price[3]
merged_data$Daily_Ret_Future_Price[1]<-merged_data$Daily_Ret_Future_Price[3]
merged_data$Brent_Daily_Ret[1]<-merged_data$Brent_Daily_Ret[3]
merged_data$Daily_Ret_Future_Price<- na.approx(merged_data$Daily_Ret_Future_Price)
merged_data$Brent_Price<- na.approx(merged_data$Brent_Price)
merged_data$coal_daily_returns[1]<-merged_data$coal_daily_returns[2]
merged_data$vix[1]<-merged_data$vix[2]
merged_data$coal_daily_returns<- na.approx(merged_data$coal_daily_returns)
merged_data$vix<- na.approx(merged_data$vix)
merged_data$vix<-c(NA,diff((merged_data$vix)))
merged_data$vix[1]<- merged_data$vix[2]
colnames(merged_data) <- c("date", "Total_Amount_Volume", "Weekday", "Log_Amount", "Diff_Log_Amount","Daily_Future_Price" ,"Brent_daily_ret", "coal_daily_returns", "vix")


##############################
# 4 - Test for stationary data
##############################

adf_test <- ur.df(merged_data$Log_Amount, lags = 0)
summary(adf_test)

pp_test_result <- kpss.test(merged_data$Log_Amount)
print(pp_test_result)


adf_test <- ur.df(atan(merged_data$Diff_Log_Amount), lags = 0)
summary(adf_test)

pp_test_result <- kpss.test(atan(merged_data$Diff_Log_Amount))
print(pp_test_result)


adf_test <- ur.df(merged_data$coal_daily_returns, lags = 0)
summary(adf_test)

pp_test_result <- kpss.test(merged_data$coal_daily_returns)
print(pp_test_result)

adf_test <- ur.df(merged_data$vix, lags = 0)
summary(adf_test)

pp_test_result <- kpss.test(merged_data$vix)
print(pp_test_result)

adf_test <- ur.df(merged_data$Brent_daily_ret, lags = 0)
summary(adf_test)

pp_test_result <- kpss.test(merged_data$Brent_daily_ret)
print(pp_test_result)

adf_test <- ur.df(merged_data$Daily_Future_Price, lags = 0)
summary(adf_test)

pp_test_result <- kpss.test(merged_data$Daily_Future_Price)
print(pp_test_result)


##########################
# 5 - Test for arch effect
##########################

arch_test <- ArchTest(daily_transaction_cleaned$diff_log, lags = 12) 

print(arch_test)



#################################
# 6 - Model selection and fitting
#################################

acf.X  <- acf(merged_data$Diff_Log_Amount, main = "ACF ", lag.max = 300) 
gpacf.X <- pacf(merged_data$Diff_Log_Amount, main = "PACF", lag.max = 300)

#########
# 1) AR(0)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )

garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 2) AR(1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria


#########
# 3) AR(2)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(2, 0), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 4) AR(3)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(3, 0), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 5) MA(1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria


#########
# 6) ARMA(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 7) ARMA(2,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(2, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 8) ARMA(3,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 0 )),
  mean.model = list(armaOrder = c(3, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 9) GARCH(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 10) AR(1)-GARCH(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria


#########
# 11) AR(2)-GARCH(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(2, 0), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 12) MA(1)-GARCH(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 13) ARMA(1,1)-GARCH(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria

#########
# 14) ARMA(2,1)-GARCH(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(2, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria


#########
# 15) ARMA(3,1)-GARCH(1,1)

# Define the specifics of the model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(3, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )
garch_model
info_criteria <- infocriteria(garch_model)
info_criteria




#####################
# 7 - Model selection
#####################

# we choose ARMA(1,1)-GARCH(1,1)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )


# Print the model summary and residuals diagnostics
print(garch_model)
garch_model
residuals_armax <- residuals(garch_model)                                                    
std_resid <- residuals_armax / sd(residuals_armax)                                                
mean(std_resid)                                                                     
var(std_resid)                                                                         
skewness(std_resid)                                                                    
kurtosis(std_resid)                                                                     
jarque.bera.test((std_resid) )

qqnorm((std_resid))                                                                              
qqline((std_resid))

Box.test(residuals_armax, lag = 10, type = "Ljung-Box")

###########################################
# 8 - Exogenous variable taken into account
###########################################

exog_vars <- cbind(merged_data$Daily_Future_Price,
                   merged_data$Brent_daily_ret,
                   merged_data$vix,
                   merged_data$coal_daily_returns)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE,external.regressors = exog_vars),  
  distribution.model = "std")

# Fit of the model
garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid" )

garch_model
info_criteria <- infocriteria(garch_model)
info_criteria




################
# 9- Forecasting
################

# Out of sample length 
pred_len<-293

# As exogenous variables we just take the Coal

# Model fitting
exog_vars <- cbind(merged_data$coal_daily_returns);
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE,external.regressors = exog_vars),  
  distribution.model = "std")

garch_model <- ugarchfit(
  spec = spec,
  data = atan(merged_data$Diff_Log_Amount),
  solver = "hybrid",
)
garch_model



# 1-Day rolling forecasting of the atan(diff(log_amount))
spec=getspec(garch_model)
setfixed(spec) <- as.list(coef(garch_model));
forecast <- ugarchforecast(
  spec,
  n.ahead = 1,
  n.roll = pred_len-1,
  data = atan(merged_data$Diff_Log_Amount[(length(merged_data$Diff_Log_Amount)-pred_len):(length(merged_data$Diff_Log_Amount))]),
  out.sample = pred_len
)

# Computing the predicted values
pred_values<-fitted(forecast)
pred_values<-t(tan(pred_values))

# We take the out of sample data to compare and analyze our forecasting
real_last_293_obs <- (tail(merged_data$Log_Amount, pred_len))

# Calculate 90% confidence interval bounds
alpha <- 0.1  # Adjusted to 0.1 for 90% CI
z_score <- qnorm(1 - alpha / 2)
se_residuals <- tail(sigma(garch_model), pred_len)

# Our predicted log amount will be the sum of the previous day observed log of the volume plus the predicted delta log volume
predicted_plus_diff <- c(merged_data$Log_Amount[1876-pred_len],real_last_293_obs[-length(real_last_293_obs)] ) +pred_values

# We compute the 90% confidence interval for every timestep
ci_upper <- (predicted_plus_diff + z_score * se_residuals)
ci_lower <- (predicted_plus_diff - z_score * se_residuals)

# Plot original and predicted values with confidence intervals
plot(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], real_last_293_obs, type = "l", col = "blue", xlab = "Date", ylab = "Values", main = "Original vs Predicted Values with 90% Confidence Interval")
lines(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], predicted_plus_diff, col = "red")
lines(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], ci_upper, col = "orange", lty = 2)
lines(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], ci_lower, col = "orange", lty = 2)

legend("topright", legend = c("Original", "Predicted", "90% CI Upper", "90% CI Lower"), col = c("blue", "red", "orange", "orange"), lty = c(1, 1, 2, 2))

# Plot without any transformation on data
plot(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], exp(real_last_293_obs), type = "l", col = "blue", xlab = "Date", ylab = "Values", main = "Original vs Predicted Values with 90% Confidence Interval")
lines(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], exp(predicted_plus_diff), col = "red")


# Plot the daily predicted and observed differences in log volumes 
real_last_293_diff <- (tail(merged_data$Diff_Log_Amount, pred_len))

plot(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], real_last_293_diff, type = "l", col = "blue", xlab = "Date", ylab = "Values", main = "Original vs Predicted Values with 90% Confidence Interval")
lines(merged_data$date[(nrow(merged_data) - (pred_len-1)):nrow(merged_data)], pred_values, col = "red")


#######################
# 10 - Accuracy metrics
#######################

sign_pred <- sign(pred_values)
sign_diff <- sign(real_last_293_diff)
num_same_sign <- sum(sign_pred == sign_diff, na.rm = TRUE)
cat("The percentage of correct sign of differences predicted:", num_same_sign/pred_len, "\n")

#######
# In real scale

# Mean Absolute Error (MAE)
mae <- mean(abs(exp(real_last_293_obs) - exp(predicted_plus_diff)))

# Mean Squared Error (MSE)
mse <- mean((exp(real_last_293_obs) - exp(predicted_plus_diff))^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((exp(real_last_293_obs) - exp(predicted_plus_diff)) / exp(real_last_293_obs))) * 100

# Print the results
cat("MAE: ", mae, "\n")
cat("MSE: ", mse, "\n")
cat("RMSE: ", rmse, "\n")
cat("MAPE: ", mape, "%\n")

######
# In log scale

# Mean Absolute Error (MAE)
mae <- mean(abs((real_last_293_obs) - (predicted_plus_diff)))

# Mean Squared Error (MSE)
mse <- mean(((real_last_293_obs) - (predicted_plus_diff))^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs(((real_last_293_obs) - (predicted_plus_diff)) / (real_last_293_obs))) * 100

# Print the results
cat("MAE: ", mae, "\n")
cat("MSE: ", mse, "\n")
cat("RMSE: ", rmse, "\n")
cat("MAPE: ", mape, "%\n")

