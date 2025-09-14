# Time Series Feature Engineering using rminer's CasesSeries for modeling with tree-based models (Random Forest, LightGBM, XGBoost)

library(rminer)
library(lubridate)

# Step 1: Data Preparation
data_forecast

# Step 2: Define lag structure for CasesSeries
# Define which lags you want to create
lag_list <- c(1, 2, 3, 6, 12)  # Monthly lags

print("Step 2: Creating lagged features using CasesSeries")

# Step 3: Apply CasesSeries to target variable (y)

y_cases <- CasesSeries(data_forecast$Product_A, lag_list)
print(paste("CasesSeries output dimensions for y:", nrow(y_cases), "x", ncol(y_cases)))

# Convert to dataframe and set column names directly
y_lagged_df <- data.frame(y_cases)
# CasesSeries puts lags first, then current value (lag 0) as last column
colnames(y_lagged_df) <- c(paste0("y_lag_", lag_list), "y_current")

print("Target variable lagged features created:")
print(colnames(y_lagged_df))

# Step 4: Apply CasesSeries to external regressor (x_reg)
print("Creating lagged features for external regressor x_reg")
x_cases <- CasesSeries(data_forecast$category_interest_3, lag_list)
print(paste("CasesSeries output dimensions for x_reg:", nrow(x_cases), "x", ncol(x_cases)))

# Convert to dataframe and set column names directly
x_lagged_df <- data.frame(x_cases)
# CasesSeries puts lags first, then current value (lag 0) as last column
colnames(x_lagged_df) <- c(paste0("x_reg_lag_", lag_list), "x_reg_current")

print("External regressor lagged features created:")
print(colnames(x_lagged_df))

# Step 5: Combine original data with lagged features
# Note: CasesSeries removes the first max(lag_list) rows
max_lag <- max(lag_list)
print(paste("Maximum lag period:", max_lag))

# Get the corresponding rows from original data (excluding first max_lag rows)
original_subset <- data_forecast[(max_lag + 1):nrow(ts_data), ]

# Combine all features
final_data <- cbind(original_subset, y_lagged_df, x_lagged_df)

# Step 6: Create additional simple features
print("Step 6: Adding basic time features")

# Add simple time-based features
final_data$month_num <- month(final_data$Month)
final_data$quarter <- quarter(final_data$Month)
final_data$year <- year(final_data$Month)

# Step 7: Display results
print("Step 7: Feature engineering summary")
print("=== FINAL DATASET OVERVIEW ===")
print(paste("Rows:", nrow(final_data)))
print(paste("Columns:", ncol(final_data)))

print("=== FEATURE COLUMNS ===")
feature_cols <- colnames(final_data)[!colnames(final_data) %in% c("Month", "y")]
print(feature_cols)

print("=== FIRST FEW ROWS ===")
print(head(final_data))

print("=== DATA READY FOR ML MODELS ===")
print("Target variable: y")
print("Feature variables:")
print(feature_cols)

# Step 8: Prepare final objects for modeling
target <- final_data$y
features <- final_data[, feature_cols]

print("Feature engineering with CasesSeries completed successfully!")
print(paste("Training examples available:", nrow(features)))
print(paste("Features created:", ncol(features)))

#write.csv(final_data,"data_ml.csv")

final_data <- final_data[-c(10,16)]
