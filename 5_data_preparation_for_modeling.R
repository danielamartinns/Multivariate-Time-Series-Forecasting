## Data preparation for final dataset

library(tseries)

data_final #retrieved from multi step

## final data
#Cut data until September 2024
#remove product B and total sales columns
# due to the time lags of the regressors, let's drop the rows where we have NAs - let's start from 2020 onwards

data_forecast <- data_final[c(1,2, 6)] %>% filter(Month >= '2019-01-01' & Month <= '2024-09-01')

#write.csv(data_forecast, "data_forecast_final.csv")

# Print the structure of the data
str(data_forecast)

#ACF plot
acf(data_forecast$category_interest_3, main = "ACF of Category Interest")


summary(data_forecast)

# Test for stationarity using Augmented Dickey-Fuller test

ts_data <- ts(data_forecast[,c("Product_A", 
                               "category_interest_3")], 
              start = c(2019, 1), frequency = 12)


adf_y <- adf.test(ts_data[, "Product_A"])
adf_x_reg1 <- adf.test(ts_data[, "category_interest_3"]) 

print("ADF test results for stationarity:")
cat("y:", adf_y$p.value, "\n") #0.027
cat("x_reg1:", adf_x_reg1$p.value, "\n") #0.33 --> higher than 0.05

# Differencing is necessary as the ADF test p-value is > 0.05
#then the series is non-stationary and needs differencing

data_forecast_for_stationarity <- data_final[c(1,2,5,6,7)] %>% 
  filter(Month >= '2018-12-01' & Month <= '2024-09-01')

ts_data_stat <- ts(data_forecast_for_stationarity[,c("Product_A", 
                                                     "category_interest_3")], 
                   start = c(2018, 12), frequency = 12)

diff_x_reg1 <- diff(ts_data_stat[, "category_interest_3"])

diff_y <- ts_data[, "Product_A"]

non_diff_x_reg1 <- ts_data_stat[, "category_interest_3"]


# Check stationarity of differenced series
adf_diff_x_reg1 <- adf.test(diff_x_reg1)

print("ADF test results after differencing:")
cat("Differenced x_reg1:", adf_diff_x_reg1$p.value, "\n") #0.01

# Create a differenced dataset for modeling
diff_ts_data <- cbind(diff_y, diff_x_reg1, non_diff_x_reg1)

View(diff_ts_data)

colnames(diff_ts_data) <- c("Product_A", 
                            "category_interest_3_diff",
                            "category_interest_3")

#remove first row

diff_ts_data <- diff_ts_data[-1,]

#use diff_ts_data in modeling phase
View(diff_ts_data)
class(diff_ts_data)

diff_ts_data <- as.data.frame(diff_ts_data)
summary(diff_ts_data)

acf(diff_ts_data$category_interest_3_diff, main = "ACF of diff(Category Interest)")

#regressor data for forecast 

data_reg_future <- data_final[c(1, 6)] %>% filter(Month >= '2019-01-01' & Month <= '2025-03-01')

data_reg_future <- tail(data_reg_future, 6)

ts_reg_future <- ts(data_reg_future[2], 
                    start = c(2024, 10), end = c(2025, 03), frequency = 12)

View(ts_reg_future)

ts_reg_future <- as.data.frame(ts_reg_future)

colnames(ts_reg_future) <- c("xreg1")
