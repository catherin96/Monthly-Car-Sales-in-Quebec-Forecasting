# ---------------------------------------------------
# Forecast of Monthly Car Sales in Quebec Project
# ---------------------------------------------------

# Install necessary libraries
required_libraries <- c(
  "fpp3", "tidyverse", "stringr", "readr", "readxl", "dplyr", 
  "tsibble", "ggplot2", "forecast", "TSA", "seasonal", 
  "fable", "fabletools", "stats", "feasts", "tseries"
)

# Install libraries if not already installed
for (lib in required_libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
}

library(fpp3)
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tsibble)
library(ggplot2)
library(forecast)
library(TSA)
library(seasonal)
library(fable)
library(fabletools)
library(stats)
library(feasts)
library(tseries)

#Load monthly-car-sales.csv
Car_Sales <- read.csv("monthly-car-sales.csv")

#Data Preprocessing

#Rename column name 'Monthly car sales in Quebec 1960-1968' to 'CarSales'
names(Car_Sales)[names(Car_Sales) == "Monthly.car.sales.in.Quebec.1960.1968"] <- "CarSales"

#convert to tsibble
CarSales_tsibble <- Car_Sales %>%
  mutate(Month = as.Date(paste0(Month, "-01"))) %>%
  filter(year(Month) >= 1960) %>%
  as_tsibble(index = Month)

#Time Plot
CarSales_tsibble %>%
  autoplot(CarSales) +
  ggtitle("Monthly Car Sales") +
  labs(x = "Year-Month", y = "Number of Cars Sold") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma_format()) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2)

#Create Time Series Object
Car_Sales.ts <- ts(Car_Sales$CarSales, frequency = 12, start = c(1960, 1))

#ACF Plot
acf(Car_Sales.ts,lag.max = 36,main = "ACF Plot - Car Sales Time Series")

#PACF Plot
pacf(Car_Sales.ts,lag.max = 36,main = "PACF Plot - Car Sales Time Series")


# DECOMPOSITION
#Decompose using stl
decompose_stl <- decompose(Car_Sales.ts)
plot(decompose_stl)


#Transformation
#Histogram before Box-Cox Transformation
# Set smaller margins
par(mar = c(6, 6, 3, 3))
hist(Car_Sales.ts, col = "brown", main = "Histogram of Car Sales")

# Reset the margins to default values
par(mar = c(5, 4, 4, 2) + 0.1)

#the data is almost normally distributed so we can omit transformation

#DIFFERENCING
#stationary-no trend or seasonality
#not stationary-have trend or seasonality

diff.ts <- CarSales_tsibble %>%
  mutate(diff = difference(Car_Sales$CarSales))
head(diff.ts)
#omit na values in diff.ts
diff.ts <- na.omit(diff.ts)
head(diff.ts)

# Plot the differenced series
diff.ts%>%
  autoplot(diff)

#From the plot,we can see there is no trend or seasonality,everything is plotted around the mean

#ACF Plot for differenced series
acf(diff.ts$diff,lag.max = 36,main = "ACF-Changes in Car Sales")

#PACF Plot for differenced series
pacf(diff.ts$diff,lag.max = 36,main = "PACF-Changes in Car Sales")

#There are significant lags at frequent points.Data has now become stationary

#2)kpss
diff.ts %>% features(CarSales,unitroot_kpss) 
#pvalue=0.01, less than 0.05(significance level =0.5) hence Reject the null hypothesis,
#so the variable CarSales is not stationary

diff.ts %>% features(diff,unitroot_kpss)
#pvalue=0.1, greater than 0.05,hence Failed to reject the null hypothesis,
#so after differencing CarSales is stationary
#Therefore, first order differencing will be used to try to fit in the models.

#Model fitting and Forecasting

#1)Naive Model
CarSales_Naive <- naive(diff.ts$CarSales,h=24) 
summary(CarSales_Naive)


#Forecast of Naive model
CarSalesNaiveForecast <- forecast(CarSales_Naive)
plot(CarSalesNaiveForecast)

#2)Seasonal Naive Model
#seasonal naive means for eg,if we predict something on feb 2022,it will be the same on feb 2023
CarSales_SeasonalNaive <- snaive(diff.ts$CarSales, h=24)
summary(CarSales_SeasonalNaive)


#Forecast of the Seasonal Naive model
CarSalesSNaiveForecast <- forecast(CarSales_SeasonalNaive)
plot(CarSalesSNaiveForecast)

#3)Drift Model and Forecasting
CarSales_DriftForecast <- rwf(diff.ts$CarSales, h = 24, drift = TRUE)
summary(CarSales_DriftForecast)


# Plot the Drift Forecast
plot(CarSales_DriftForecast)

#4)ETS Model
CarSalesETS <- ets(diff.ts$CarSales)
summary(CarSalesETS)


#Forecast of ETS model
CarSalesETSForecast <- forecast(CarSalesETS,h=24)
plot(CarSalesETSForecast)


#5)ARIMA Model-1
#p= 3(in pacf the first statistically significant lag is at 3)
#d= 1 (one differencing)
#q = 3(in acf the first statistically significant lag is at 3)

CarSalesARIMA1 <- Arima(diff.ts$CarSales, order = c(3, 1, 3), include.mean = FALSE)

# Specify the training set
training_set <- window(diff.ts$CarSales, end = c(1968-12-01))
summary(CarSalesARIMA1, training.set = training_set)


#Forecast of ARIMA model
CarSalesARIMAForecast1 <- forecast(CarSalesARIMA1,h=24)
plot(CarSalesARIMAForecast1)

#6)ARIMA Model-2
#p= 4(in pacf the second statistically significant lag is at 4)
#d= 1 (one differencing)
#q = 4(in acf the second statistically significant lag is at 4)

CarSalesARIMA2 <- Arima(diff.ts$CarSales, order = c(4, 1, 4), include.mean = FALSE)

# Specify the training set
training_set <- window(diff.ts$CarSales, end = c(1968-12-01))
summary(CarSalesARIMA2, training.set = training_set)



#Forecast of ARIMA model
CarSalesARIMAForecast2 <- forecast(CarSalesARIMA2,h=24)
plot(CarSalesARIMAForecast2)

#7)AutoARIMA Model
set.seed(123)
CarSalesAutoARIMA <- auto.arima(diff.ts$CarSales, seasonal = TRUE)
summary(CarSalesAutoARIMA)


# Forecast  of Auto ARIMA model
CarSales_AutoARIMAForecast <- forecast(CarSalesAutoARIMA, h = 24)
plot(CarSales_AutoARIMAForecast, main = 'AutoARIMA Forecast')

#7)SARIMA Model
# Define a grid of parameters

parameter_combinations <- expand.grid(
  p = 1:3,
  d = 1,
  q = 1:3,
  P = 1:3,
  D = 1,
  Q = 1:3
)
# Initialize an empty data frame to store results
results <- data.frame()

# Loop through each parameter combination
for (i in 1:nrow(parameter_combinations)) {
  tryCatch({
    # Extract parameters for the current iteration
    current_params <- parameter_combinations[i, ]
    
    # Fit SARIMA model
    current_model <- Arima(
      diff.ts$CarSales,
      order = c(current_params$p, current_params$d, current_params$q),
      seasonal = list(order = c(current_params$P, current_params$D, current_params$Q), period = 24),
      include.mean = FALSE
    )
    
    # Extract AIC and add to results
    current_result <- data.frame(
      params = paste("(", current_params$p, ",", current_params$d, ",", current_params$q, ")(", current_params$P, ",", current_params$D, ",", current_params$Q, ")", sep = ""),
      AIC = AIC(current_model)
    )
    
    results <- rbind(results, current_result)
  }, error = function(e) {
    cat("Error for combination:", paste(current_params, collapse = "_"), "\n")
    cat("Error message:", conditionMessage(e), "\n")
    
    # If there's an error, add "failed" to results
    results <- rbind(results, data.frame(params = paste("(", current_params$p, ",", current_params$d, ",", current_params$q, ")(", current_params$P, ",", current_params$D, ",", current_params$Q, ")", sep = ""), AIC = "failed"))
  })
}

# View the results
print(results)

# Create a matrix with AIC values or "failed"
result_matrix <- acast(results, params ~ ., value.var = "AIC")
print(result_matrix)



CarSalesSARIMA <- Arima(diff.ts$CarSales, order = c(2,1,1), seasonal=list(order=c(1,1,1), period=24),include.mean = FALSE)

training_set <- window(diff.ts$CarSales, end = c(1968-12-01))
summary(CarSalesSARIMA, training.set = training_set)



#Forecast of SARIMA model
CarSalesSARIMAForecast <- forecast(CarSalesSARIMA,h=24)
plot(CarSalesSARIMAForecast)
checkresiduals(CarSalesSARIMA)



#Plotting residuals
#1)Naive residuals
Naive = residuals(CarSales_Naive)
hist(Naive)
#bell curve means the error is minimum and the forecasting is valid

#2)Seasonal Naive Residuals
Seasonal_Naive = residuals(CarSales_SeasonalNaive)
hist(Seasonal_Naive)

#3)Drift Residuals
Drift = residuals(CarSales_DriftForecast)
hist(Drift)

#4)ETS Residuals
ETS = residuals(CarSalesETS)
hist(ETS)

#5)Arima Model 1 Residuals
Arima_3_1_3 = residuals(CarSalesARIMA1)
hist(Arima_3_1_3)

#6)Arima Model 2 Residuals
Arima_4_1_4 = residuals(CarSalesARIMA2)
hist(Arima_4_1_4)

#7)Auto Arima Residuals
Auto_Arima = residuals(CarSalesAutoARIMA)
hist(Auto_Arima)

#8)Seasonal Arima Residuals
SARIMA = residuals(CarSalesSARIMA)
hist(SARIMA)
