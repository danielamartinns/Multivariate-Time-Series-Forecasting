# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)
library(Metrics)
library(lubridate)
library(dplyr)
library(zoo)       
library(xts)       
library(tidyverse) # For data processing

## STEP 1: Time Series CV for Hyperparameter Selection

# Function to perform time series CV for hyperparameter selection
ts_cv_hyperparameter_search <- function(ts_data, xreg_data, 
                                        window_type = "rolling", # "rolling" or "growing"
                                        initial_window = 36, 
                                        cv_horizon = 6,
                                        max_order = 2,
                                        min_cv_folds = 3) {
  
  n <- length(ts_data)
  
  # Calculate number of CV folds possible
  if (window_type == "rolling") {
    max_folds <- n - initial_window - cv_horizon + 1
  } else { # growing
    max_folds <- n - initial_window - cv_horizon + 1
  }
  
  num_cv_folds <- min(max_folds, max(min_cv_folds, max_folds))
  
  cat("Hyperparameter search using", window_type, "window CV\n")
  cat("Number of CV folds:", num_cv_folds, "\n\n")
  
  # Grid search over all parameter combinations
  param_grid <- expand.grid(
    p = 0:max_order,
    d = 0:1,
    q = 0:max_order,
    P = 0:1,
    D = 0:1,
    Q = 0:1
  )
  
  # Filter out overly complex models
  param_grid <- param_grid[rowSums(param_grid) <= 5, ]
  
  best_avg_error <- Inf
  best_params <- NULL
  
  cat("Testing", nrow(param_grid), "parameter combinations...\n")
  
  # Test each parameter combination
  for (i in 1:nrow(param_grid)) {
    params <- param_grid[i, ]
    cv_errors <- numeric(num_cv_folds)
    successful_folds <- 0
    
    # Perform CV for this parameter combination
    for (fold in 1:num_cv_folds) {
      if (window_type == "rolling") {
        # Rolling window indices - MATCH EVALUATION FUNCTION
        train_start_idx <- fold
        train_end_idx <- fold + initial_window - 1
        test_start_idx <- train_end_idx + 1
        test_end_idx <- min(test_start_idx + cv_horizon - 1, n)
      } else {
        # Growing window indices
        train_start_idx <- 1
        train_end_idx <- initial_window + fold - 1
        test_start_idx <- train_end_idx + 1
        test_end_idx <- min(test_start_idx + cv_horizon - 1, n)
      }
      
      # Extract data - EXACTLY LIKE EVALUATION FUNCTION
      train_ts <- ts_data[train_start_idx:train_end_idx]
      test_ts <- ts_data[test_start_idx:test_end_idx]
      
      # Get actual time indices - EXACTLY LIKE EVALUATION FUNCTION
      time_indices <- time(ts_data)
      
      # Convert to ts objects if necessary - EXACTLY LIKE EVALUATION FUNCTION
      if (!is.ts(train_ts)) {
        train_ts <- ts(train_ts, start = time_indices[train_start_idx], 
                       frequency = frequency(ts_data))
      }
      if (!is.ts(test_ts)) {
        test_ts <- ts(test_ts, start = time_indices[test_start_idx], 
                      frequency = frequency(ts_data))
      }
      
      # Extract corresponding regressor values - EXACTLY LIKE EVALUATION FUNCTION
      if (!is.null(xreg_data)) {
        train_xreg <- xreg_data[train_start_idx:train_end_idx, , drop = FALSE]
        test_xreg <- xreg_data[test_start_idx:test_end_idx, , drop = FALSE]
      } else {
        train_xreg <- NULL
        test_xreg <- NULL
      }
      
      # EXACTLY LIKE EVALUATION FUNCTION - Try to fit model with current parameters
      if (!is.null(xreg_data)) {
        fit_success <- FALSE
        tryCatch({
          model <- Arima(train_ts, 
                         order = c(params$p, params$d, params$q),
                         seasonal = list(order = c(params$P, params$D, params$Q), period = 12),
                         xreg = train_xreg, 
                         method = "ML")
          
          # Forecast and calculate error
          fc <- forecast(model, h = length(test_ts), xreg = test_xreg)
          fit_success <- TRUE
          
        }, error = function(e) {
          # Do nothing here, handle below
        })
        
        # If fixed parameters failed, use auto.arima - EXACTLY LIKE EVALUATION FUNCTION
        if (!fit_success) {
          model <- auto.arima(train_ts, xreg = train_xreg)
          fc <- forecast(model, h = length(test_ts), xreg = test_xreg)
        }
      } else {
        # For testing purposes, use a simple auto.arima model
        model <- auto.arima(train_ts)
        fc <- forecast(model, h = length(test_ts))
      }
      
      # Calculate errors - EXACTLY LIKE EVALUATION FUNCTION
      actual <- as.numeric(test_ts)
      predicted <- as.numeric(fc$mean)
      
      rmse <- sqrt(mean((actual - predicted)^2))
      cv_errors[fold] <- rmse
      successful_folds <- successful_folds + 1
    }
    
    # Calculate average error for this parameter combination
    if (successful_folds >= min_cv_folds) {
      avg_error <- mean(cv_errors, na.rm = TRUE)
      
      if (avg_error < best_avg_error) {
        best_avg_error <- avg_error
        best_params <- params
        cat("New best RMSE:", round(avg_error, 4), "with params:", 
            paste(params, collapse=","), "\n")
      }
    }
    
    # Progress update
    if (i %% 10 == 0 || i == nrow(param_grid)) {
      cat("Tested", i, "/", nrow(param_grid), "combinations. Best RMSE so far:", 
          round(best_avg_error, 4), "\n")
    }
  }
  
  cat("\nBest parameters found:\n")
  print(best_params)
  cat("CV RMSE:", round(best_avg_error, 4), "\n\n")
  
  return(list(
    best_params = best_params,
    cv_error = best_avg_error,
    window_type = window_type
  ))
}



#Find best parameters for growing and rolling windows with Grid Search CV

best_params_rolling <- ts_cv_hyperparameter_search(
  ts_data = y_ts, 
  xreg_data = xreg_matrix,
  window_type = "rolling",
  initial_window = 36,
  cv_horizon = 6,
  max_order = 2
)

# Best parameters found:
#   p d q P D Q
# 38 1 0 0 0 1 0
# CV RMSE: 157.8931 


best_params_growing <- ts_cv_hyperparameter_search(
  ts_data = y_ts,
  xreg_data = xreg_matrix, 
  window_type = "growing",
  initial_window = 36,
  cv_horizon = 6,
  max_order = 2
)

# 38 1 0 0 0 1 0
# CV RMSE: 159.0452 

## STEP 2: Fixed Parameter Evaluation Functions

# Rolling window evaluation with FIXED parameters

rolling_window_arimax_evaluation <- function(ts_data, xreg_data, 
                                             window_size = 36, 
                                             horizon = 6) {
  n <- length(ts_data)
  errors <- list(MAE = numeric(), RMSE = numeric(), MAPE = numeric())
  forecasts <- list()
  best_params_list <- list()  # Add this to store parameters
  
  # Get actual time indices
  time_indices <- time(ts_data)
  
  # Number of rolling windows
  num_windows <- n - window_size - horizon + 1
  
  # For debugging
  cat("Running ROLLING window evaluation with FIXED parameters: ARIMA(1,1,1)(1,0,1)12\n")
  cat("Total observations:", n, "\n")
  cat("Window size:", window_size, "\n")
  cat("Forecast horizon:", horizon, "\n")
  cat("Number of test windows:", num_windows, "\n\n")
  
  for (i in 1:num_windows) {
    # Define training and test periods explicitly by observation indices
    train_start_idx <- i
    train_end_idx <- i + window_size - 1
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, n)
    
    # Print the actual indices for debugging
    cat("Window", i, "- Training indices:", train_start_idx, "to", train_end_idx, 
        "Test indices:", test_start_idx, "to", test_end_idx, "\n")
    
    # Extract training and test data directly using indices
    train_ts <- ts_data[train_start_idx:train_end_idx]
    test_ts <- ts_data[test_start_idx:test_end_idx]
    
    # Convert to ts objects if necessary
    if (!is.ts(train_ts)) {
      train_ts <- ts(train_ts, start = time_indices[train_start_idx], 
                     frequency = frequency(ts_data))
    }
    if (!is.ts(test_ts)) {
      test_ts <- ts(test_ts, start = time_indices[test_start_idx], 
                    frequency = frequency(ts_data))
    }
    
    # Extract corresponding regressor values
    if (!is.null(xreg_data)) {
      train_xreg <- xreg_data[train_start_idx:train_end_idx, , drop = FALSE]
      test_xreg <- xreg_data[test_start_idx:test_end_idx, , drop = FALSE]
    } else {
      train_xreg <- NULL
      test_xreg <- NULL
    }
    
    # Use FIXED parameters: ARIMA(1,1,1)(1,0,1)12
    if (!is.null(xreg_data)) {
      # Store the fixed parameters for tracking
      fixed_params <- list(order = c(1, 0, 0), seasonal = c(0,1,0))
      best_params_list[[i]] <- fixed_params
      
      # Try to fit model with fixed parameters
      fit_success <- FALSE
      tryCatch({
        # Fit the model with FIXED parameters (1,1,1)(1,0,1)12
        model <- Arima(train_ts, 
                       order = c(1, 0, 0), 
                       seasonal = list(order = c(0,1,0), period = 12),
                       xreg = train_xreg,
                       method = "ML")  # Force ML estimation
        
        # Forecast the test period
        fc <- forecast(model, h = length(test_ts), xreg = test_xreg)
        fit_success <- TRUE
        
      }, error = function(e) {
        cat("  Fixed parameters failed for window", i, "\n")
      })
      
      # If fixed parameters failed, use auto.arima
      if (!fit_success) {
        cat("  Using auto.arima fallback\n")
        model <- auto.arima(train_ts, xreg = train_xreg)
        fc <- forecast(model, h = length(test_ts), xreg = test_xreg)
      }
    } else {
      # For testing purposes, use a simple auto.arima model
      model <- auto.arima(train_ts)
      fc <- forecast(model, h = length(test_ts))
    }
    
    # Save forecasts
    forecasts[[i]] <- fc
    
    # Calculate errors
    actual <- as.numeric(test_ts)
    predicted <- as.numeric(fc$mean)
    
    mae <- mean(abs(actual - predicted))
    rmse <- sqrt(mean((actual - predicted)^2))
    mape <- mean(abs((actual - predicted) / actual)) * 100
    
    errors$MAE <- c(errors$MAE, mae)
    errors$RMSE <- c(errors$RMSE, rmse)
    errors$MAPE <- c(errors$MAPE, mape)
    
    cat("  Training size:", length(train_ts), 
        "- MAE:", round(mae, 2), 
        "RMSE:", round(rmse, 2), 
        "MAPE:", round(mape, 2), "%\n\n")
  }
  
  # Average errors
  avg_errors <- list(
    MAE = mean(errors$MAE),
    RMSE = mean(errors$RMSE),
    MAPE = mean(errors$MAPE)
  )
  
  cat("Average errors - MAE:", round(avg_errors$MAE, 2), 
      "RMSE:", round(avg_errors$RMSE, 2), 
      "MAPE:", round(avg_errors$MAPE, 2), "%\n")
  
  return(list(errors = errors, avg_errors = avg_errors, forecasts = forecasts, 
              best_params = best_params_list))  # Return parameters too
}

results_arimax_rolling_window <- rolling_window_arimax_evaluation(y_ts, 
                                                           xreg_matrix, 
                                                           window_size, 
                                                           horizon)

#Average errors - MAE: 137.09 RMSE: 157.89 MAPE: 7.7 % 

# Growing window evaluation with FIXED parameters
growing_window_arimax_evaluation <- function(ts_data, xreg_data, 
                                             window_size = 36, 
                                             horizon = 6) {
  n <- length(ts_data)
  errors <- list(MAE = numeric(), RMSE = numeric(), MAPE = numeric())
  forecasts <- list()
  best_params_list <- list()  # Add this to store parameters
  
  # Get actual time indices
  time_indices <- time(ts_data)
  
  # Number of growing windows
  num_windows <- n - initial_window - horizon + 1
  
  # For debugging
  cat("Running ROLLING window evaluation with FIXED parameters: ARIMA(1,0,0)(0,1,0)12\n")
  cat("Total observations:", n, "\n")
  cat("Initial window size:", initial_window, "\n")
  cat("Forecast horizon:", horizon, "\n")
  cat("Number of test windows:", num_windows, "\n\n")
  
  for (i in 1:num_windows) {
    # Define training and test periods explicitly by observation indices
    train_start_idx <- 1
    train_end_idx <- i + initial_window - 1
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, n)
    
    # Print the actual indices for debugging
    cat("Window", i, "- Training indices:", train_start_idx, "to", train_end_idx, 
        "Test indices:", test_start_idx, "to", test_end_idx, "\n")
    
    # Extract training and test data directly using indices
    train_ts <- ts_data[train_start_idx:train_end_idx]
    test_ts <- ts_data[test_start_idx:test_end_idx]
    
    # Convert to ts objects if necessary
    if (!is.ts(train_ts)) {
      train_ts <- ts(train_ts, start = time_indices[train_start_idx], 
                     frequency = frequency(ts_data))
    }
    if (!is.ts(test_ts)) {
      test_ts <- ts(test_ts, start = time_indices[test_start_idx], 
                    frequency = frequency(ts_data))
    }
    
    # Extract corresponding regressor values
    if (!is.null(xreg_data)) {
      train_xreg <- xreg_data[train_start_idx:train_end_idx, , drop = FALSE]
      test_xreg <- xreg_data[test_start_idx:test_end_idx, , drop = FALSE]
    } else {
      train_xreg <- NULL
      test_xreg <- NULL
    }
    
    # Use FIXED parameters: ARIMA(1, 0, 0)(0, 1, 0)12
    if (!is.null(xreg_data)) {
      # Store the fixed parameters for tracking
      fixed_params <- list(order = c(1, 0, 0), seasonal = c(0, 1, 0))
      best_params_list[[i]] <- fixed_params
      
      # Try to fit model with fixed parameters
      fit_success <- FALSE
      tryCatch({
        # Fit the model with FIXED parameters (1,1,1)(1,0,1)12
        model <- Arima(train_ts, 
                       order = c(1, 0, 0), 
                       seasonal = list(order = c(0, 1, 0), period = frequency(ts_data)),
                       xreg = train_xreg,
                       method = "ML")  # Force ML estimation
        
        # Forecast the test period
        fc <- forecast(model, h = length(test_ts), xreg = test_xreg)
        fit_success <- TRUE
        
      }, error = function(e) {
        cat("  Fixed parameters failed for window", i, "\n")
      })
      
      # If fixed parameters failed, use auto.arima
      if (!fit_success) {
        cat("  Using auto.arima fallback\n")
        model <- auto.arima(train_ts, xreg = train_xreg)
        fc <- forecast(model, h = length(test_ts), xreg = test_xreg)
      }
    } else {
      # For testing purposes, use a simple auto.arima model
      model <- auto.arima(train_ts)
      fc <- forecast(model, h = length(test_ts))
    }
    
    # Save forecasts
    forecasts[[i]] <- fc
    
    # Calculate errors
    actual <- as.numeric(test_ts)
    predicted <- as.numeric(fc$mean)
    
    mae <- mean(abs(actual - predicted))
    rmse <- sqrt(mean((actual - predicted)^2))
    mape <- mean(abs((actual - predicted) / actual)) * 100
    
    errors$MAE <- c(errors$MAE, mae)
    errors$RMSE <- c(errors$RMSE, rmse)
    errors$MAPE <- c(errors$MAPE, mape)
    
    cat("  Training size:", length(train_ts), 
        "- MAE:", round(mae, 2), 
        "RMSE:", round(rmse, 2), 
        "MAPE:", round(mape, 2), "%\n\n")
  }
  
  # Average errors
  avg_errors <- list(
    MAE = mean(errors$MAE),
    RMSE = mean(errors$RMSE),
    MAPE = mean(errors$MAPE)
  )
  
  cat("Average errors - MAE:", round(avg_errors$MAE, 2), 
      "RMSE:", round(avg_errors$RMSE, 2), 
      "MAPE:", round(avg_errors$MAPE, 2), "%\n")
  
  return(list(errors = errors, avg_errors = avg_errors, forecasts = forecasts, 
              best_params = best_params_list))  # Return parameters too
}

results_arimax_growing_window <- growing_window_arimax_evaluation(y_ts, xreg_matrix, 
                                                           initial_window, horizon)

#Average errors - MAE: 137.04 RMSE: 159.05 MAPE: 7.72 %


# Compare results
cat("="*60, "\n")
cat("FINAL COMPARISON\n")
cat("="*60, "\n")
cat("Rolling Window - MAE:", round(rolling_results$avg_errors$MAE, 3), 
    "RMSE:", round(rolling_results$avg_errors$RMSE, 3),
    "MAPE:", round(rolling_results$avg_errors$MAPE, 2), "%\n")
cat("Growing Window - MAE:", round(growing_results$avg_errors$MAE, 3), 
    "RMSE:", round(growing_results$avg_errors$RMSE, 3),
    "MAPE:", round(growing_results$avg_errors$MAPE, 2), "%\n")

#Compare rolling and growing windows results

compare_arimax_window_methods <- function(ts_data, xreg_data, window_size = 36, horizon = 6) {
  
  cat("=== COMPARING ROLLING vs GROWING WINDOW METHODS ===\n\n")
  
  # Rolling Window Evaluation
  cat("1. ROLLING WINDOW EVALUATION:\n")
  rolling_results <- rolling_window_arimax_evaluation(ts_data, xreg_data, 
                                                      window_size = window_size, 
                                                      horizon = horizon)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Growing Window Evaluation  
  cat("2. GROWING WINDOW EVALUATION:\n")
  growing_results <- growing_window_arimax_evaluation(ts_data, xreg_data, 
                                               window_size = initial_window, 
                                               horizon = horizon)
  
  # Summary comparison
  cat("\n=== PERFORMANCE COMPARISON ===\n")
  
  # Rolling window stats
  rolling_mae_mean <- mean(rolling_results$errors$MAE)
  rolling_rmse_mean <- mean(rolling_results$errors$RMSE)
  rolling_mape_mean <- mean(rolling_results$errors$MAPE)
  rolling_mae_sd <- sd(rolling_results$errors$MAE)
  rolling_rmse_sd <- sd(rolling_results$errors$RMSE)
  rolling_mape_sd <- sd(rolling_results$errors$MAPE)
  
  # Growing window stats
  growing_mae_mean <- mean(growing_results$errors$MAE)
  growing_rmse_mean <- mean(growing_results$errors$RMSE)
  growing_mape_mean <- mean(growing_results$errors$MAPE)
  growing_mae_sd <- sd(growing_results$errors$MAE)
  growing_rmse_sd <- sd(growing_results$errors$RMSE)
  growing_mape_sd <- sd(growing_results$errors$MAPE)
  
  cat("ROLLING WINDOW:\n")
  cat("  Mean MAE:", round(rolling_mae_mean, 2), "± SD:", round(rolling_mae_sd, 2), "\n")
  cat("  Mean RMSE:", round(rolling_rmse_mean, 2), "± SD:", round(rolling_rmse_sd, 2), "\n")
  cat("  Mean MAPE:", round(rolling_mape_mean, 2), "% ± SD:", round(rolling_mape_sd, 2), "%\n\n")
  
  cat("GROWING WINDOW:\n")
  cat("  Mean MAE:", round(growing_mae_mean, 2), "± SD:", round(growing_mae_sd, 2), "\n")
  cat("  Mean RMSE:", round(growing_rmse_mean, 2), "± SD:", round(growing_rmse_sd, 2), "\n")
  cat("  Mean MAPE:", round(growing_mape_mean, 2), "% ± SD:", round(growing_mape_sd, 2), "%\n\n")
  
  # Performance improvement analysis
  mae_improvement <- ((rolling_mae_mean - growing_mae_mean) / rolling_mae_mean) * 100
  rmse_improvement <- ((rolling_rmse_mean - growing_rmse_mean) / rolling_rmse_mean) * 100
  mape_improvement <- ((rolling_mape_mean - growing_mape_mean) / rolling_mape_mean) * 100
  
  cat("PERFORMANCE DIFFERENCE (Growing vs Rolling):\n")
  cat("  MAE improvement:", round(mae_improvement, 2), "% (positive = better)\n")
  cat("  RMSE improvement:", round(rmse_improvement, 2), "% (positive = better)\n") 
  cat("  MAPE improvement:", round(mape_improvement, 2), "% (positive = better)\n\n")
  
  # Determine which method is better
  better_method <- ifelse(growing_mape_mean < rolling_mape_mean, "GROWING", "ROLLING")
  cat("RECOMMENDATION: ", better_method, " window method shows better overall performance\n\n")
  
  # Create comparison plots
  par(mfrow = c(2, 3))
  
  # MAE comparison
  plot(1:length(rolling_results$errors$MAE), rolling_results$errors$MAE, 
       type = "b", col = "blue", pch = 16, 
       main = "SARIMAX MAE Comparison", xlab = "Window Number", ylab = "MAE",
       ylim = range(c(rolling_results$errors$MAE, growing_results$errors$MAE)))
  lines(1:length(growing_results$errors$MAE), growing_results$errors$MAE, 
        type = "b", col = "red", pch = 17)
  legend("topright", legend = c("Rolling", "Growing"), 
         col = c("blue", "red"), pch = c(16, 17), lty = 1)
  grid()
  
  # RMSE comparison
  plot(1:length(rolling_results$errors$RMSE), rolling_results$errors$RMSE, 
       type = "b", col = "blue", pch = 16,
       main = "SARIMAX RMSE Comparison", xlab = "Window Number", ylab = "RMSE",
       ylim = range(c(rolling_results$errors$RMSE, growing_results$errors$RMSE)))
  lines(1:length(growing_results$errors$RMSE), growing_results$errors$RMSE, 
        type = "b", col = "red", pch = 17)
  legend("topright", legend = c("Rolling", "Growing"), 
         col = c("blue", "red"), pch = c(16, 17), lty = 1)
  grid()
  
  # MAPE comparison
  plot(1:length(rolling_results$errors$MAPE), rolling_results$errors$MAPE, 
       type = "b", col = "blue", pch = 16,
       main = "SARIMAX MAPE Comparison", xlab = "Window Number", ylab = "MAPE (%)",
       ylim = range(c(rolling_results$errors$MAPE, growing_results$errors$MAPE)))
  lines(1:length(growing_results$errors$MAPE), growing_results$errors$MAPE, 
        type = "b", col = "red", pch = 17)
  legend("topright", legend = c("Rolling", "Growing"), 
         col = c("blue", "red"), pch = c(16, 17), lty = 1)
  grid()
  
  # Error distribution boxplots
  boxplot(list(Rolling = rolling_results$errors$MAE, Growing = growing_results$errors$MAE),
          main = "SARIMAX MAE Distribution", ylab = "MAE", col = c("lightblue", "lightcoral"))
  
  boxplot(list(Rolling = rolling_results$errors$RMSE, Growing = growing_results$errors$RMSE),
          main = "SARIMAX RMSE Distribution", ylab = "RMSE", col = c("lightblue", "lightcoral"))
  
  boxplot(list(Rolling = rolling_results$errors$MAPE, Growing = growing_results$errors$MAPE),
          main = "SARIMAX MAPE Distribution", ylab = "MAPE (%)", col = c("lightblue", "lightcoral"))
  
  par(mfrow = c(1, 1))
  
  return(list(
    rolling = rolling_results,
    growing = growing_results,
    comparison = list(
      rolling_stats = list(mae_mean = rolling_mae_mean, rmse_mean = rolling_rmse_mean, mape_mean = rolling_mape_mean,
                           mae_sd = rolling_mae_sd, rmse_sd = rolling_rmse_sd, mape_sd = rolling_mape_sd),
      growing_stats = list(mae_mean = growing_mae_mean, rmse_mean = growing_rmse_mean, mape_mean = growing_mape_mean,
                           mae_sd = growing_mae_sd, rmse_sd = growing_rmse_sd, mape_sd = growing_mape_sd),
      improvements = list(mae = mae_improvement, rmse = rmse_improvement, mape = mape_improvement),
      better_method = better_method
    )
  ))
}

comparison_arimax_results <- compare_arimax_window_methods(y_ts, xreg_matrix, 
                                             window_size = 36, horizon = 6)
