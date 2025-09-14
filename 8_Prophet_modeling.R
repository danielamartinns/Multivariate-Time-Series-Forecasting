# Prophet Rolling and Growing Window Evaluation Functions
# Load required libraries
library(prophet)
library(dplyr)
library(ggplot2)

View(y_ts)

# Create matrices for exogenous regressors for prophet
xreg_matrix_prophet <- cbind(xreg1 = diff_ts_data$category_interest_3)

View(xreg_matrix_prophet)

# Rolling Window Evaluation for Prophet
rolling_window_evaluation_prophet <- function(ts_data, xreg_data = NULL, window_size = 36, horizon = 6) {
  n <- length(ts_data)
  errors <- list(MAE = numeric(), RMSE = numeric(), MAPE = numeric())
  forecasts <- list()
  
  # Get actual time indices and convert to dates
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  # Number of rolling windows
  num_windows <- n - window_size - horizon + 1
  
  # For debugging
  cat("Running ROLLING window evaluation with PROPHET\n")
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
    
    # Extract training and test data
    train_dates <- dates[train_start_idx:train_end_idx]
    train_values <- as.numeric(ts_data[train_start_idx:train_end_idx])
    test_dates <- dates[test_start_idx:test_end_idx]
    test_values <- as.numeric(ts_data[test_start_idx:test_end_idx])
    
    # Create Prophet dataframe for training
    train_df <- data.frame(
      ds = train_dates,
      y = train_values
    )
    
    # Add regressors if provided
    if (!is.null(xreg_data)) {
      for (j in 1:ncol(xreg_data)) {
        train_df[[paste0("regressor_", j)]] <- xreg_data[train_start_idx:train_end_idx, j]
      }
    }
    
    # Create Prophet dataframe for testing (needed for regressors)
    test_df <- data.frame(
      ds = test_dates
    )
    
    if (!is.null(xreg_data)) {
      for (j in 1:ncol(xreg_data)) {
        test_df[[paste0("regressor_", j)]] <- xreg_data[test_start_idx:test_end_idx, j]
      }
    }
    
    # Fit Prophet model
    tryCatch({
      # Initialize Prophet model
      m <- prophet(
        yearly_seasonality = TRUE,
        weekly.seasonality = FALSE,  # Monthly data doesn't need weekly
        daily.seasonality = FALSE,   # Monthly data doesn't need daily
        changepoint_prior_scale = 0.5,
        seasonality_mode = "multiplicative", 
        seasonality_prior_scale = 50
        )
      
      # Add regressors if provided
      if (!is.null(xreg_data)) {
        for (j in 1:ncol(xreg_data)) {
          regressor_name <- paste0("regressor_", j)
          m <- add_regressor(m, regressor_name)
        }
      }
      
      # Fit the model
      m <- fit.prophet(m, train_df)
      
      # Create future dataframe for forecasting
      future <- test_df
      
      # Make forecast
      forecast_result <- predict(m, future)
      
      # Extract forecasted values
      predicted <- forecast_result$yhat
      
    }, error = function(e) {
      cat("Error in Prophet model fitting:", e$message, "\n")
      # Use simple mean as fallback
      predicted <- rep(mean(train_values), length(test_values))
    })
    
    # Save forecasts
    forecasts[[i]] <- list(
      predicted = predicted,
      actual = test_values,
      dates = test_dates
    )
    
    # Calculate errors
    actual <- test_values
    
    mae <- mean(abs(actual - predicted))
    rmse <- sqrt(mean((actual - predicted)^2))
    mape <- mean(abs((actual - predicted) / actual)) * 100
    
    errors$MAE <- c(errors$MAE, mae)
    errors$RMSE <- c(errors$RMSE, rmse)
    errors$MAPE <- c(errors$MAPE, mape)
    
    cat("  Training size:", length(train_values), 
        "- MAE:", round(mae, 2), 
        "RMSE:", round(rmse, 2), 
        "MAPE:", round(mape, 2), "%\n\n")
  }
  
  return(list(errors = errors, forecasts = forecasts))
}

# Run rolling window evaluation

results_rolling_window_prophet <- rolling_window_evaluation_prophet(y_ts, xreg_matrix_prophet, window_size, horizon)

# Growing Window Evaluation for Prophet
growing_window_evaluation_prophet <- function(ts_data, xreg_data = NULL, initial_window = 36, horizon = 6) {
  n <- length(ts_data)
  errors <- list(MAE = numeric(), RMSE = numeric(), MAPE = numeric())
  forecasts <- list()
  
  # Get actual time indices and convert to dates
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  # Number of growing windows
  num_windows <- n - initial_window - horizon + 1
  
  # For debugging
  cat("Running GROWING window evaluation with PROPHET\n")
  cat("Total observations:", n, "\n")
  cat("Initial window size:", initial_window, "\n")
  cat("Forecast horizon:", horizon, "\n")
  cat("Number of test windows:", num_windows, "\n\n")
  
  for (i in 1:num_windows) {
    # Define training and test periods explicitly by observation indices
    train_start_idx <- 1  # Always start from the beginning
    train_end_idx <- initial_window + i - 1  # Growing window size
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, n)
    
    # Print the actual indices for debugging
    cat("Window", i, "- Training indices:", train_start_idx, "to", train_end_idx, 
        "(size:", train_end_idx - train_start_idx + 1, ")",
        "Test indices:", test_start_idx, "to", test_end_idx, "\n")
    
    # Extract training and test data
    train_dates <- dates[train_start_idx:train_end_idx]
    train_values <- as.numeric(ts_data[train_start_idx:train_end_idx])
    test_dates <- dates[test_start_idx:test_end_idx]
    test_values <- as.numeric(ts_data[test_start_idx:test_end_idx])
    
    # Create Prophet dataframe for training
    train_df <- data.frame(
      ds = train_dates,
      y = train_values
    )
    
    # Add regressors if provided
    if (!is.null(xreg_data)) {
      for (j in 1:ncol(xreg_data)) {
        train_df[[paste0("regressor_", j)]] <- xreg_data[train_start_idx:train_end_idx, j]
      }
    }
    
    # Create Prophet dataframe for testing (needed for regressors)
    test_df <- data.frame(
      ds = test_dates
    )
    
    if (!is.null(xreg_data)) {
      for (j in 1:ncol(xreg_data)) {
        test_df[[paste0("regressor_", j)]] <- xreg_data[test_start_idx:test_end_idx, j]
      }
    }
    
    # Fit Prophet model
    tryCatch({
      # Initialize Prophet model
      m <- prophet(
        yearly_seasonality = TRUE,
        weekly.seasonality = FALSE,  # Monthly data doesn't need weekly
        daily.seasonality = FALSE,   # Monthly data doesn't need daily
        changepoint_prior_scale = 0.5,
        seasonality_mode = "multiplicative", 
        seasonality_prior_scale = 50
      )
      
      # Add regressors if provided
      if (!is.null(xreg_data)) {
        for (j in 1:ncol(xreg_data)) {
          regressor_name <- paste0("regressor_", j)
          m <- add_regressor(m, regressor_name)
        }
      }
      
      # Fit the model
      m <- fit.prophet(m, train_df)
      
      # Create future dataframe for forecasting
      future <- test_df
      
      # Make forecast
      forecast_result <- predict(m, future)
      
      # Extract forecasted values
      predicted <- forecast_result$yhat
      
    }, error = function(e) {
      cat("Error in Prophet model fitting:", e$message, "\n")
      # Use simple mean as fallback
      predicted <- rep(mean(train_values), length(test_values))
    })
    
    # Save forecasts
    forecasts[[i]] <- list(
      predicted = predicted,
      actual = test_values,
      dates = test_dates
    )
    
    # Calculate errors
    actual <- test_values
    
    mae <- mean(abs(actual - predicted))
    rmse <- sqrt(mean((actual - predicted)^2))
    mape <- mean(abs((actual - predicted) / actual)) * 100
    
    errors$MAE <- c(errors$MAE, mae)
    errors$RMSE <- c(errors$RMSE, rmse)
    errors$MAPE <- c(errors$MAPE, mape)
    
    cat("  Training size:", length(train_values), 
        "- MAE:", round(mae, 2), 
        "RMSE:", round(rmse, 2), 
        "MAPE:", round(mape, 2), "%\n\n")
  }
  
  return(list(errors = errors, forecasts = forecasts))
}

# Run rolling growing evaluation

results_growing_window_prophet <- growing_window_evaluation_prophet(y_ts, 
                                                                    xreg_matrix_prophet, 
                                                                    window_size, 
                                                                    horizon)

# Function to compare Rolling vs Growing Window performance with Prophet
compare_window_methods_prophet <- function(ts_data, xreg_data = NULL, window_size = 36, horizon = 6) {
  
  cat("=== COMPARING ROLLING vs GROWING WINDOW METHODS WITH PROPHET ===\n\n")
  
  # Rolling Window Evaluation
  cat("1. ROLLING WINDOW EVALUATION (PROPHET):\n")
  rolling_results <- rolling_window_evaluation_prophet(ts_data, xreg_data, 
                                                       window_size = window_size, 
                                                       horizon = horizon)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Growing Window Evaluation  
  cat("2. GROWING WINDOW EVALUATION (PROPHET):\n")
  growing_results <- growing_window_evaluation_prophet(ts_data, xreg_data, 
                                                       initial_window = window_size, 
                                                       horizon = horizon)
  
  # Summary comparison
  cat("\n=== PROPHET PERFORMANCE COMPARISON ===\n")
  
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
  
  cat("ROLLING WINDOW (PROPHET):\n")
  cat("  Mean MAE:", round(rolling_mae_mean, 2), "± SD:", round(rolling_mae_sd, 2), "\n")
  cat("  Mean RMSE:", round(rolling_rmse_mean, 2), "± SD:", round(rolling_rmse_sd, 2), "\n")
  cat("  Mean MAPE:", round(rolling_mape_mean, 2), "% ± SD:", round(rolling_mape_sd, 2), "%\n\n")
  
  cat("GROWING WINDOW (PROPHET):\n")
  cat("  Mean MAE:", round(growing_mae_mean, 2), "± SD:", round(growing_mae_sd, 2), "\n")
  cat("  Mean RMSE:", round(growing_rmse_mean, 2), "± SD:", round(growing_rmse_sd, 2), "\n")
  cat("  Mean MAPE:", round(growing_mape_mean, 2), "% ± SD:", round(growing_mape_sd, 2), "%\n\n")
  
  # Performance improvement analysis
  mae_improvement <- ((rolling_mae_mean - growing_mae_mean) / rolling_mae_mean) * 100
  rmse_improvement <- ((rolling_rmse_mean - growing_rmse_mean) / rolling_rmse_mean) * 100
  mape_improvement <- ((rolling_mape_mean - growing_mape_mean) / rolling_mape_mean) * 100
  
  cat("PERFORMANCE DIFFERENCE (Growing vs Rolling with PROPHET):\n")
  cat("  MAE improvement:", round(mae_improvement, 2), "% (positive = better)\n")
  cat("  RMSE improvement:", round(rmse_improvement, 2), "% (positive = better)\n") 
  cat("  MAPE improvement:", round(mape_improvement, 2), "% (positive = better)\n\n")
  
  # Determine which method is better
  better_method <- ifelse(growing_mape_mean < rolling_mape_mean, "GROWING", "ROLLING")
  cat("RECOMMENDATION: ", better_method, " window method shows better overall performance for PROPHET\n\n")
  
  # Create comparison plots
  par(mfrow = c(2, 3))
  
  # MAE comparison
  plot(1:length(rolling_results$errors$MAE), rolling_results$errors$MAE, 
       type = "b", col = "blue", pch = 16, 
       main = "Prophet MAE Comparison", xlab = "Window Number", ylab = "MAE",
       ylim = range(c(rolling_results$errors$MAE, growing_results$errors$MAE)))
  lines(1:length(growing_results$errors$MAE), growing_results$errors$MAE, 
        type = "b", col = "red", pch = 17)
  legend("topright", legend = c("Rolling", "Growing"), 
         col = c("blue", "red"), pch = c(16, 17), lty = 1)
  grid()
  
  # RMSE comparison
  plot(1:length(rolling_results$errors$RMSE), rolling_results$errors$RMSE, 
       type = "b", col = "blue", pch = 16,
       main = "Prophet RMSE Comparison", xlab = "Window Number", ylab = "RMSE",
       ylim = range(c(rolling_results$errors$RMSE, growing_results$errors$RMSE)))
  lines(1:length(growing_results$errors$RMSE), growing_results$errors$RMSE, 
        type = "b", col = "red", pch = 17)
  legend("topright", legend = c("Rolling", "Growing"), 
         col = c("blue", "red"), pch = c(16, 17), lty = 1)
  grid()
  
  # MAPE comparison
  plot(1:length(rolling_results$errors$MAPE), rolling_results$errors$MAPE, 
       type = "b", col = "blue", pch = 16,
       main = "Prophet MAPE Comparison", xlab = "Window Number", ylab = "MAPE (%)",
       ylim = range(c(rolling_results$errors$MAPE, growing_results$errors$MAPE)))
  lines(1:length(growing_results$errors$MAPE), growing_results$errors$MAPE, 
        type = "b", col = "red", pch = 17)
  legend("topright", legend = c("Rolling", "Growing"), 
         col = c("blue", "red"), pch = c(16, 17), lty = 1)
  grid()
  
  # Error distribution boxplots
  boxplot(list(Rolling = rolling_results$errors$MAE, Growing = growing_results$errors$MAE),
          main = "Prophet MAE Distribution", ylab = "MAE", col = c("lightblue", "lightcoral"))
  
  boxplot(list(Rolling = rolling_results$errors$RMSE, Growing = growing_results$errors$RMSE),
          main = "Prophet RMSE Distribution", ylab = "RMSE", col = c("lightblue", "lightcoral"))
  
  boxplot(list(Rolling = rolling_results$errors$MAPE, Growing = growing_results$errors$MAPE),
          main = "Prophet MAPE Distribution", ylab = "MAPE (%)", col = c("lightblue", "lightcoral"))
  
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

# Complete Prophet analysis function
complete_prophet_analysis <- function(data, y_col = "y", regressor_cols = NULL, 
                                      window_size = 36, horizon = 6) {
  
  cat("=== COMPLETE PROPHET ANALYSIS ===\n\n")
  
  # Create time series object
  ts_data <- ts(data[[y_col]], start = c(2019, 1), frequency = 12)
  
  # Create regressor matrix if specified
  xreg_data <- NULL
  if (!is.null(regressor_cols)) {
    xreg_data <- as.matrix(data[, regressor_cols, drop = FALSE])
    cat("Using regressors:", paste(regressor_cols, collapse = ", "), "\n")
  } else {
    cat("No regressors specified - using Prophet with seasonality only\n")
  }
  
  cat("Time series length:", length(ts_data), "\n")
  cat("Window size:", window_size, "\n")
  cat("Forecast horizon:", horizon, "\n\n")
  
  # Run comparison
  results <- compare_window_methods_prophet(ts_data, xreg_data, window_size, horizon)
  
  return(results)
}

results_prophet <- complete_prophet_analysis(
  data = data_forecast,
  y_col = "Product_A",
  regressor_cols = c("category_interest_3"),  # or NULL for no regressors
  window_size = 36,
  horizon = 6
)

cat("Prophet window evaluation functions created successfully!\n")
cat("Key features:\n")
cat("- Handles external regressors automatically\n")
cat("- Built-in seasonality modeling (yearly for monthly data)\n")
cat("- Robust error handling with fallback predictions\n")
cat("- Comprehensive comparison between rolling and growing windows\n")
cat("- Visualization of forecast performance\n\n")
cat("Use complete_prophet_analysis() for end-to-end Prophet evaluation.\n")

# Functions to plot PROPHET forecasts with actual time series data

plot_prophet_forecasts <- function(ts_data, results, window_type = "Rolling", 
                                   window_size = 36, horizon = 6, 
                                   show_all_forecasts = TRUE, highlight_window = NULL) {
  
  # Convert time series to data frame
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  ts_df <- data.frame(
    Date = dates,
    Actual = as.numeric(ts_data)
  )
  
  # Initialize forecast data frame
  forecast_df <- data.frame()
  
  # Calculate prediction intervals based on historical errors (since Prophet doesn't provide CI in your setup)
  all_errors <- unlist(lapply(results$forecasts, function(x) x$actual - x$predicted))
  error_sd <- sd(all_errors, na.rm = TRUE)
  z_score_95 <- qnorm(0.975)  # 95% confidence interval
  z_score_80 <- qnorm(0.90)   # 80% confidence interval
  
  # Process each forecast window
  for(i in 1:length(results$forecasts)) {
    
    forecast_data <- results$forecasts[[i]]
    
    if(window_type == "Rolling") {
      # For rolling window - calculate indices based on your original function logic
      train_start_idx <- i
      train_end_idx <- i + window_size - 1
      test_start_idx <- train_end_idx + 1
      test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
    } else {
      # For growing window (if you want to implement this later)
      train_start_idx <- 1
      train_end_idx <- window_size + i - 1
      test_start_idx <- train_end_idx + 1
      test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
    }
    
    # Skip if indices are invalid
    if(test_start_idx > length(ts_data)) next
    
    # Get forecast dates (from your results structure)
    forecast_dates <- forecast_data$dates
    
    # Get forecast values
    forecast_mean <- forecast_data$predicted
    
    # Calculate confidence intervals based on prediction error standard deviation
    forecast_lower_95 <- forecast_mean - z_score_95 * error_sd
    forecast_upper_95 <- forecast_mean + z_score_95 * error_sd
    forecast_lower_80 <- forecast_mean - z_score_80 * error_sd
    forecast_upper_80 <- forecast_mean + z_score_80 * error_sd
    
    # Adjust length if needed
    n_forecast <- length(forecast_dates)
    forecast_mean <- forecast_mean[1:n_forecast]
    forecast_lower_95 <- forecast_lower_95[1:n_forecast]
    forecast_upper_95 <- forecast_upper_95[1:n_forecast]
    forecast_lower_80 <- forecast_lower_80[1:n_forecast]
    forecast_upper_80 <- forecast_upper_80[1:n_forecast]
    
    # Create forecast data frame for this window
    temp_df <- data.frame(
      Date = forecast_dates,
      Forecast = forecast_mean,
      Actual_Test = forecast_data$actual[1:n_forecast],
      Lower_CI_95 = forecast_lower_95,
      Upper_CI_95 = forecast_upper_95,
      Lower_CI_80 = forecast_lower_80,
      Upper_CI_80 = forecast_upper_80,
      Window = i,
      TrainStart = dates[train_start_idx],
      TrainEnd = dates[train_end_idx]
    )
    
    forecast_df <- rbind(forecast_df, temp_df)
  }
  
  # Create the base plot
  p <- ggplot() +
    # Plot actual time series
    geom_line(data = ts_df, aes(x = Date, y = Actual), 
              color = "black", size = 0.8, alpha = 0.8) +
    labs(title = paste(window_type, "Window Prophet Forecasting"),
         subtitle = paste("Training Window:", window_size, "months | Forecast Horizon:", horizon, "months"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add forecasts
  if(show_all_forecasts) {
    # Show all forecast windows
    if(!is.null(highlight_window) && highlight_window <= max(forecast_df$Window)) {
      # Highlight specific window
      other_windows <- forecast_df[forecast_df$Window != highlight_window, ]
      highlight_data <- forecast_df[forecast_df$Window == highlight_window, ]
      
      # Other windows (less prominent)
      if(nrow(other_windows) > 0) {
        if(window_type == "Growing") {
          p <- p + 
            # 95% CI for other windows
            geom_ribbon(data = other_windows, 
                        aes(x = Date, ymin = Lower_CI_95, ymax = Upper_CI_95, group = Window),
                        alpha = 0.08, fill = "blue") +
            # 80% CI for other windows
            geom_ribbon(data = other_windows, 
                        aes(x = Date, ymin = Lower_CI_80, ymax = Upper_CI_80, group = Window),
                        alpha = 0.12, fill = "blue") +
            geom_point(data = other_windows, 
                       aes(x = Date, y = Forecast),
                       color = "blue", alpha = 0.4, size = 1) +
            geom_line(data = other_windows, 
                      aes(x = Date, y = Forecast, group = Window),
                      color = "blue", alpha = 0.3, size = 0.5) +
            # Add actual test points
            geom_point(data = other_windows, 
                       aes(x = Date, y = Actual_Test),
                       color = "darkblue", alpha = 0.6, size = 1.2, shape = 1)
        } else {
          p <- p + 
            # 95% CI for other windows
            geom_ribbon(data = other_windows, 
                        aes(x = Date, ymin = Lower_CI_95, ymax = Upper_CI_95, group = Window),
                        alpha = 0.08, fill = "blue") +
            # 80% CI for other windows
            geom_ribbon(data = other_windows, 
                        aes(x = Date, ymin = Lower_CI_80, ymax = Upper_CI_80, group = Window),
                        alpha = 0.12, fill = "blue") +
            geom_line(data = other_windows, 
                      aes(x = Date, y = Forecast, group = Window),
                      color = "blue", alpha = 0.3, size = 0.5) +
            # Add actual test points
            geom_point(data = other_windows, 
                       aes(x = Date, y = Actual_Test),
                       color = "darkblue", alpha = 0.6, size = 1.2, shape = 1)
        }
      }
      
      # Highlighted window
      if(nrow(highlight_data) > 0) {
        p <- p + 
          # 95% CI for highlighted window
          geom_ribbon(data = highlight_data, 
                      aes(x = Date, ymin = Lower_CI_95, ymax = Upper_CI_95),
                      alpha = 0.15, fill = "red") +
          # 80% CI for highlighted window
          geom_ribbon(data = highlight_data, 
                      aes(x = Date, ymin = Lower_CI_80, ymax = Upper_CI_80),
                      alpha = 0.25, fill = "red") +
          geom_line(data = highlight_data, 
                    aes(x = Date, y = Forecast),
                    color = "red", size = 1.2) +
          geom_point(data = highlight_data, 
                     aes(x = Date, y = Forecast),
                     color = "red", size = 2) +
          # Add actual test points for highlighted window
          geom_point(data = highlight_data, 
                     aes(x = Date, y = Actual_Test),
                     color = "darkred", size = 2.5, shape = 1, stroke = 1.5)
      }
      
    } else {
      # Show all windows equally
      if(window_type == "Growing") {
        # For growing windows, emphasize the individual forecast segments
        p <- p + 
          # 95% CI
          geom_ribbon(data = forecast_df, 
                      aes(x = Date, ymin = Lower_CI_95, ymax = Upper_CI_95, group = Window),
                      alpha = 0.1, fill = "blue") +
          # 80% CI (darker)
          geom_ribbon(data = forecast_df, 
                      aes(x = Date, ymin = Lower_CI_80, ymax = Upper_CI_80, group = Window),
                      alpha = 0.15, fill = "blue") +
          geom_point(data = forecast_df, 
                     aes(x = Date, y = Forecast),
                     color = "blue", alpha = 0.7, size = 1.5) +
          geom_line(data = forecast_df, 
                    aes(x = Date, y = Forecast, group = Window),
                    color = "blue", alpha = 0.6, size = 0.7) +
          # Add actual test points
          geom_point(data = forecast_df, 
                     aes(x = Date, y = Actual_Test),
                     color = "darkblue", alpha = 0.8, size = 1.5, shape = 1)
      } else {
        # For rolling windows, show continuous lines
        p <- p + 
          # 95% CI
          geom_ribbon(data = forecast_df, 
                      aes(x = Date, ymin = Lower_CI_95, ymax = Upper_CI_95, group = Window),
                      alpha = 0.1, fill = "blue") +
          # 80% CI (darker)
          geom_ribbon(data = forecast_df, 
                      aes(x = Date, ymin = Lower_CI_80, ymax = Upper_CI_80, group = Window),
                      alpha = 0.15, fill = "blue") +
          geom_line(data = forecast_df, 
                    aes(x = Date, y = Forecast, group = Window),
                    color = "blue", alpha = 0.6, size = 0.7) +
          # Add actual test points
          geom_point(data = forecast_df, 
                     aes(x = Date, y = Actual_Test),
                     color = "darkblue", alpha = 0.8, size = 1.5, shape = 1)
      }
    }
  } else {
    # Show only the last forecast (most recent)
    last_forecast <- forecast_df[forecast_df$Window == max(forecast_df$Window), ]
    
    p <- p + 
      # 95% CI
      geom_ribbon(data = last_forecast, 
                  aes(x = Date, ymin = Lower_CI_95, ymax = Upper_CI_95),
                  alpha = 0.2, fill = "blue") +
      # 80% CI (darker)
      geom_ribbon(data = last_forecast, 
                  aes(x = Date, ymin = Lower_CI_80, ymax = Upper_CI_80),
                  alpha = 0.3, fill = "blue") +
      geom_line(data = last_forecast, 
                aes(x = Date, y = Forecast),
                color = "blue", size = 1.2) +
      geom_point(data = last_forecast, 
                 aes(x = Date, y = Forecast),
                 color = "blue", size = 2) +
      # Add actual test points
      geom_point(data = last_forecast, 
                 aes(x = Date, y = Actual_Test),
                 color = "darkblue", size = 2.5, shape = 1, stroke = 1.5)
  }
  
  # Format x-axis for monthly data
  p <- p + scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months")
  
  return(p)
}

# Additional function for error metrics dashboard (ggplot2 style)
plot_prophet_error_dashboard <- function(results, ts_data, window_size, horizon) {
  library(gridExtra)
  
  # Create error metrics data frame
  error_df <- data.frame(
    Window = 1:length(results$errors$MAE),
    MAE = results$errors$MAE,
    RMSE = results$errors$RMSE,
    MAPE = results$errors$MAPE
  )
  
  # MAE over time
  p1 <- ggplot(error_df, aes(x = Window, y = MAE)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "red", size = 1.5) +
    labs(title = "MAE Over Rolling Windows", x = "Window Number", y = "MAE") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  # RMSE over time
  p2 <- ggplot(error_df, aes(x = Window, y = RMSE)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "blue", size = 1.5) +
    labs(title = "RMSE Over Rolling Windows", x = "Window Number", y = "RMSE") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  # MAPE over time
  p3 <- ggplot(error_df, aes(x = Window, y = MAPE)) +
    geom_line(color = "green", size = 1) +
    geom_point(color = "green", size = 1.5) +
    labs(title = "MAPE Over Rolling Windows", x = "Window Number", y = "MAPE (%)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  # Combined error metrics
  error_long <- reshape2::melt(error_df, id.vars = "Window", 
                               measure.vars = c("MAE", "RMSE"), 
                               variable.name = "Metric", value.name = "Value")
  
  p4 <- ggplot(error_long, aes(x = Window, y = Value, color = Metric)) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    labs(title = "Error Metrics Comparison", x = "Window Number", y = "Error Value") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          legend.position = "bottom") +
    scale_color_manual(values = c("MAE" = "red", "RMSE" = "blue"))
  
  # Combine plots
  grid.arrange(p1, p2, p3, p4, ncol = 2, 
               top = paste("Prophet Rolling Window Performance -", 
                           "Window Size:", window_size, 
                           "| Horizon:", horizon))
}

# Function to create side-by-side comparison
plot_rolling_vs_growing_prophet <- function(ts_data, rolling_results, growing_results, 
                                            window_size = 36, horizon = 6) {
  
  # Create rolling window plot
  p1 <- plot_prophet_forecasts(ts_data, rolling_results, "Rolling", 
                               window_size, horizon, show_all_forecasts = TRUE)
  
  # Create growing window plot  
  p2 <- plot_prophet_forecasts(ts_data, growing_results, "Growing", 
                               window_size, horizon, show_all_forecasts = TRUE)
  
  # Display plots side by side
  library(gridExtra)
  combined_plot <- grid.arrange(p1, p2, nrow = 2)
  
  return(combined_plot)
}

# Function to plot specific forecast window in detail
plot_detailed_forecast_prophet <- function(ts_data, results, window_num, window_type = "Rolling",
                                           window_size = 36, horizon = 6) {
  
  if(window_num > length(results$forecasts)) {
    stop("Window number exceeds available forecasts")
  }
  
  # Get time indices and convert to dates
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  # Calculate window indices
  if(window_type == "Rolling") {
    train_start_idx <- window_num
    train_end_idx <- window_num + window_size - 1
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
  } else {
    train_start_idx <- 1
    train_end_idx <- window_size + window_num - 1
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
  }
  
  # Create detailed plot data
  plot_start_idx <- max(1, train_start_idx - 12)  # Show 12 months before training
  plot_end_idx <- min(length(ts_data), test_end_idx + 6)  # Show 6 months after forecast
  
  plot_dates <- dates[plot_start_idx:plot_end_idx]
  plot_actual <- as.numeric(ts_data)[plot_start_idx:plot_end_idx]
  
  # Create base data frame
  plot_df <- data.frame(
    Date = plot_dates,
    Actual = plot_actual,
    Type = "Historical"
  )
  
  # Mark training period
  plot_df$Type[plot_df$Date >= dates[train_start_idx] & 
                 plot_df$Date <= dates[train_end_idx]] <- "Training"
  
  # Mark actual test period
  plot_df$Type[plot_df$Date >= dates[test_start_idx] & 
                 plot_df$Date <= dates[test_end_idx]] <- "Test"
  
  # Add forecast data (from Prophet results structure)
  forecast_data <- results$forecasts[[window_num]]
  forecast_dates <- forecast_data$dates
  forecast_mean <- forecast_data$predicted
  
  # Calculate confidence intervals
  all_errors <- unlist(lapply(results$forecasts, function(x) x$actual - x$predicted))
  error_sd <- sd(all_errors, na.rm = TRUE)
  z_score <- qnorm(0.975)  # 95% CI
  
  forecast_lower <- forecast_mean - z_score * error_sd
  forecast_upper <- forecast_mean + z_score * error_sd
  
  forecast_df <- data.frame(
    Date = forecast_dates,
    Forecast = forecast_mean,
    Lower_CI = forecast_lower,
    Upper_CI = forecast_upper
  )
  
  # Create the plot
  p <- ggplot() +
    geom_line(data = plot_df, aes(x = Date, y = Actual, color = Type), size = 1) +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                alpha = 0.3, fill = "red") +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast), 
              color = "red", size = 1.2, linetype = "dashed") +
    geom_point(data = forecast_df, aes(x = Date, y = Forecast), 
               color = "red", size = 2) +
    # Add actual test points
    geom_point(data = data.frame(Date = forecast_dates, Actual = forecast_data$actual), 
               aes(x = Date, y = Actual), color = "darkred", size = 2.5, shape = 1, stroke = 1.5) +
    scale_color_manual(values = c("Historical" = "gray50", 
                                  "Training" = "blue", 
                                  "Test" = "black")) +
    labs(title = paste("Detailed", window_type, "Window Prophet Forecast - Window", window_num),
         subtitle = paste("Training:", dates[train_start_idx], "to", 
                          dates[train_end_idx], "| Forecast:", dates[test_start_idx], "to", 
                          dates[test_end_idx]),
         x = "Date", y = "Value", color = "Data Type") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")
  
  return(p)
}

# Function to create a clearer rolling window visualization
plot_rolling_window_progression_prophet <- function(ts_data, results, window_size = 36, horizon = 6) {
  
  # Get time indices and convert to dates
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  # Convert time series to data frame
  ts_df <- data.frame(
    Date = dates,
    Actual = as.numeric(ts_data)
  )
  
  # Create the base plot
  p <- ggplot() +
    geom_line(data = ts_df, aes(x = Date, y = Actual), 
              color = "black", size = 1, alpha = 0.8) +
    labs(title = "Rolling Window Prophet: Overlapping Forecasts",
         subtitle = paste("Each forecast uses a fixed", window_size, "month training window |", 
                          "Forecast Horizon:", horizon, "months"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Create gradient colors from light to dark blue to show progression
  n_windows <- length(results$forecasts)
  colors <- colorRampPalette(c("lightblue", "darkblue"))(n_windows)
  alphas <- seq(0.3, 0.8, length.out = n_windows)
  
  # Add training window indicators for first few and last few windows
  show_windows <- c(1, 2, 3, n_windows-2, n_windows-1, n_windows)
  show_windows <- show_windows[show_windows <= n_windows & show_windows > 0]
  
  # Calculate prediction intervals
  all_errors <- unlist(lapply(results$forecasts, function(x) x$actual - x$predicted))
  error_sd <- sd(all_errors, na.rm = TRUE)
  z_score <- qnorm(0.975)  # 95% CI
  
  for(i in 1:length(results$forecasts)) {
    # Calculate indices for this rolling window
    train_start_idx <- i
    train_end_idx <- i + window_size - 1
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
    
    if(test_start_idx > length(ts_data)) next
    
    # Get forecast data from Prophet results
    forecast_data <- results$forecasts[[i]]
    forecast_dates <- forecast_data$dates
    forecast_mean <- forecast_data$predicted
    forecast_lower <- forecast_mean - z_score * error_sd
    forecast_upper <- forecast_mean + z_score * error_sd
    
    # Create forecast data frame
    temp_df <- data.frame(
      Date = forecast_dates,
      Forecast = forecast_mean,
      Lower_CI = forecast_lower,
      Upper_CI = forecast_upper
    )
    
    # Determine line properties based on window position
    if(i %in% show_windows) {
      # Highlighted windows (first few and last few)
      line_size <- 1.2
      point_size <- 2
      alpha_ribbon <- 0.3
      alpha_line <- 0.9
    } else {
      # Other windows (more subtle)
      line_size <- 0.6
      point_size <- 1
      alpha_ribbon <- 0.1
      alpha_line <- 0.4
    }
    
    # Add forecast to plot
    p <- p + 
      geom_ribbon(data = temp_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                  alpha = alpha_ribbon, fill = colors[i]) +
      geom_line(data = temp_df, aes(x = Date, y = Forecast),
                color = colors[i], size = line_size, alpha = alpha_line) +
      geom_point(data = temp_df, aes(x = Date, y = Forecast),
                 color = colors[i], size = point_size, alpha = alpha_line)
  }
  
  # Add training window indicators for selected windows
  if(length(show_windows) > 0) {
    for(window_num in show_windows[1:min(3, length(show_windows))]) {
      train_start_date <- dates[window_num]
      train_end_date <- dates[window_num + window_size - 1]
      
      # Add subtle vertical lines to show training windows
      p <- p + 
        geom_vline(xintercept = train_start_date, linetype = "dotted", 
                   color = colors[window_num], alpha = 0.5) +
        geom_vline(xintercept = train_end_date, linetype = "dashed", 
                   color = colors[window_num], alpha = 0.7)
    }
  }
  
  # Add legend annotations
  legend_y <- max(ts_df$Actual) * 0.95
  p <- p + 
    annotate("text", x = min(ts_df$Date), y = legend_y, 
             label = "Lighter = Earlier Windows\nDarker = Later Windows", 
             hjust = 0, size = 3.5, color = "gray30")
  
  # Format x-axis
  p <- p + scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create an animated-style rolling window plot (shows window movement)
plot_rolling_window_movement_prophet <- function(ts_data, results, window_size = 36, horizon = 6, 
                                                 show_window = NULL) {
  
  if(is.null(show_window)) {
    show_window <- ceiling(length(results$forecasts) / 2)  # Show middle window by default
  }
  
  # Get time indices and convert to dates
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  # Convert time series to data frame
  ts_df <- data.frame(
    Date = dates,
    Actual = as.numeric(ts_data)
  )
  
  # Calculate window indices
  train_start_idx <- show_window
  train_end_idx <- show_window + window_size - 1
  test_start_idx <- train_end_idx + 1
  test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
  
  # Create data segments
  before_train <- ts_df[1:(train_start_idx-1), ]
  train_data <- ts_df[train_start_idx:train_end_idx, ]
  test_data <- ts_df[test_start_idx:test_end_idx, ]
  after_test <- if(test_end_idx < nrow(ts_df)) ts_df[(test_end_idx+1):nrow(ts_df), ] else data.frame()
  
  # Get forecast data from Prophet results
  forecast_data <- results$forecasts[[show_window]]
  forecast_dates <- forecast_data$dates
  forecast_mean <- forecast_data$predicted
  
  # Calculate confidence intervals
  all_errors <- unlist(lapply(results$forecasts, function(x) x$actual - x$predicted))
  error_sd <- sd(all_errors, na.rm = TRUE)
  z_score <- qnorm(0.975)  # 95% CI
  
  forecast_lower <- forecast_mean - z_score * error_sd
  forecast_upper <- forecast_mean + z_score * error_sd
  
  forecast_df <- data.frame(
    Date = forecast_dates,
    Forecast = forecast_mean,
    Lower_CI = forecast_lower,
    Upper_CI = forecast_upper,
    Actual_Test = forecast_data$actual
  )
  
  # Create the plot
  p <- ggplot() +
    # Historical data (before training)
    geom_line(data = before_train, aes(x = Date, y = Actual), 
              color = "gray70", size = 0.8, alpha = 0.6) +
    # Training data (highlighted)
    geom_line(data = train_data, aes(x = Date, y = Actual), 
              color = "blue", size = 1.2) +
    # Test data (actual values to compare against)
    geom_line(data = test_data, aes(x = Date, y = Actual), 
              color = "black", size = 1.2) +
    # Future data (if any)
    {if(nrow(after_test) > 0) geom_line(data = after_test, aes(x = Date, y = Actual), 
                                        color = "gray70", size = 0.8, alpha = 0.6)} +
    # Forecast with confidence interval
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                alpha = 0.3, fill = "red") +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast),
              color = "red", size = 1.5, linetype = "dashed") +
    geom_point(data = forecast_df, aes(x = Date, y = Forecast),
               color = "red", size = 2.5) +
    # Add actual test points
    geom_point(data = forecast_df, aes(x = Date, y = Actual_Test),
               color = "darkred", size = 2.5, shape = 1, stroke = 1.5) +
    # Add vertical separators
    geom_vline(xintercept = train_data$Date[1], linetype = "solid", color = "blue", alpha = 0.7) +
    geom_vline(xintercept = train_data$Date[nrow(train_data)], linetype = "solid", color = "blue", alpha = 0.7) +
    geom_vline(xintercept = test_data$Date[1], linetype = "solid", color = "red", alpha = 0.7) +
    # Labels and theme
    labs(title = paste("Rolling Window", show_window, "- Prophet Training and Forecasting"),
         subtitle = paste("Blue: Training Data (", window_size, "months) | Red: Forecast (", horizon, "months) | Black: Actual Test Values"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")
  
  return(p)
}

# Function to create a clearer growing window visualization
plot_growing_window_progression_prophet <- function(ts_data, results, initial_window = 36, horizon = 6) {
  
  # Get time indices and convert to dates
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  # Convert time series to data frame
  ts_df <- data.frame(
    Date = dates,
    Actual = as.numeric(ts_data)
  )
  
  # Create the base plot
  p <- ggplot() +
    geom_line(data = ts_df, aes(x = Date, y = Actual), 
              color = "black", size = 1, alpha = 0.8) +
    labs(title = "Growing Window Prophet: Progressive Forecasting",
         subtitle = paste("Each forecast uses progressively more training data |", 
                          "Initial Window:", initial_window, "months | Horizon:", horizon, "months"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Add forecasts with different colors for progression
  colors <- rainbow(length(results$forecasts), start = 0.6, end = 0.9)
  
  # Calculate prediction intervals
  all_errors <- unlist(lapply(results$forecasts, function(x) x$actual - x$predicted))
  error_sd <- sd(all_errors, na.rm = TRUE)
  z_score <- qnorm(0.975)  # 95% CI
  
  for(i in 1:length(results$forecasts)) {
    # Calculate indices for this window
    train_end_idx <- initial_window + i - 1
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
    
    if(test_start_idx > length(ts_data)) next
    
    # Get forecast data from Prophet results
    forecast_data <- results$forecasts[[i]]
    forecast_dates <- forecast_data$dates
    forecast_mean <- forecast_data$predicted
    forecast_lower <- forecast_mean - z_score * error_sd
    forecast_upper <- forecast_mean + z_score * error_sd
    
    # Add forecast to plot
    temp_df <- data.frame(
      Date = forecast_dates,
      Forecast = forecast_mean,
      Lower_CI = forecast_lower,
      Upper_CI = forecast_upper
    )
    
    p <- p + 
      geom_ribbon(data = temp_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                  alpha = 0.2, fill = colors[i]) +
      geom_line(data = temp_df, aes(x = Date, y = Forecast),
                color = colors[i], size = 1) +
      geom_point(data = temp_df, aes(x = Date, y = Forecast),
                 color = colors[i], size = 1.5)
  }
  
  # Add training period indicators for first and last windows
  first_train_end <- dates[initial_window]
  last_train_end <- dates[initial_window + length(results$forecasts) - 1]
  
  p <- p + 
    geom_vline(xintercept = first_train_end, linetype = "dashed", color = "gray50", alpha = 0.7) +
    geom_vline(xintercept = last_train_end, linetype = "dashed", color = "gray50", alpha = 0.7) +
    annotate("text", x = first_train_end, y = max(ts_df$Actual) * 0.9, 
             label = "First Training End", angle = 90, vjust = -0.5, size = 3) +
    annotate("text", x = last_train_end, y = max(ts_df$Actual) * 0.9, 
             label = "Last Training End", angle = 90, vjust = -0.5, size = 3)
  
  # Format x-axis
  p <- p + scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create an animated-style growing window plot (shows window expansion)
plot_growing_window_movement_prophet <- function(ts_data, results, initial_window = 36, horizon = 6, 
                                                 show_window = NULL) {
  
  if(is.null(show_window)) {
    show_window <- ceiling(length(results$forecasts) / 2)  # Show middle window by default
  }
  
  # Get time indices and convert to dates
  time_indices <- time(ts_data)
  dates <- as.Date(paste(floor(time_indices), round((time_indices - floor(time_indices)) * 12) + 1, "01", sep = "-"))
  
  # Convert time series to data frame
  ts_df <- data.frame(
    Date = dates,
    Actual = as.numeric(ts_data)
  )
  
  # Calculate window indices for growing window
  train_start_idx <- 1  # Always starts from beginning
  train_end_idx <- initial_window + show_window - 1  # Growing window size
  test_start_idx <- train_end_idx + 1
  test_end_idx <- min(test_start_idx + horizon - 1, length(ts_data))
  
  # Create data segments
  train_data <- ts_df[train_start_idx:train_end_idx, ]
  test_data <- ts_df[test_start_idx:test_end_idx, ]
  after_test <- if(test_end_idx < nrow(ts_df)) ts_df[(test_end_idx+1):nrow(ts_df), ] else data.frame()
  
  # Highlight the initial window vs the additional data
  initial_train <- ts_df[train_start_idx:initial_window, ]
  additional_train <- if(train_end_idx > initial_window) ts_df[(initial_window+1):train_end_idx, ] else data.frame()
  
  # Get forecast data from Prophet results
  forecast_data <- results$forecasts[[show_window]]
  forecast_dates <- forecast_data$dates
  forecast_mean <- forecast_data$predicted
  
  # Calculate confidence intervals
  all_errors <- unlist(lapply(results$forecasts, function(x) x$actual - x$predicted))
  error_sd <- sd(all_errors, na.rm = TRUE)
  z_score <- qnorm(0.975)  # 95% CI
  
  forecast_lower <- forecast_mean - z_score * error_sd
  forecast_upper <- forecast_mean + z_score * error_sd
  
  forecast_df <- data.frame(
    Date = forecast_dates,
    Forecast = forecast_mean,
    Lower_CI = forecast_lower,
    Upper_CI = forecast_upper,
    Actual_Test = forecast_data$actual
  )
  
  # Create the plot
  p <- ggplot() +
    # Initial training window (highlighted differently)
    geom_line(data = initial_train, aes(x = Date, y = Actual), 
              color = "darkblue", size = 1.0, alpha = 0.7) +
    # Additional training data (showing growth)
    {if(nrow(additional_train) > 0) geom_line(data = additional_train, aes(x = Date, y = Actual), 
                                              color = "lightblue", size = 1.2)} +
    # Test data (actual values to compare against)
    geom_line(data = test_data, aes(x = Date, y = Actual), 
              color = "black", size = 1.2) +
    # Future data (if any)
    {if(nrow(after_test) > 0) geom_line(data = after_test, aes(x = Date, y = Actual), 
                                        color = "gray70", size = 0.8, alpha = 0.6)} +
    # Forecast with confidence interval
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                alpha = 0.3, fill = "red") +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast),
              color = "red", size = 1.5, linetype = "dashed") +
    geom_point(data = forecast_df, aes(x = Date, y = Forecast),
               color = "red", size = 2.5) +
    # Add actual test points
    geom_point(data = forecast_df, aes(x = Date, y = Actual_Test),
               color = "darkred", size = 2.5, shape = 1, stroke = 1.5) +
    # Add vertical separators
    geom_vline(xintercept = initial_train$Date[nrow(initial_train)], 
               linetype = "dotted", color = "darkblue", alpha = 0.8, size = 1) +
    geom_vline(xintercept = train_data$Date[nrow(train_data)], 
               linetype = "solid", color = "lightblue", alpha = 0.8, size = 1) +
    geom_vline(xintercept = test_data$Date[1], 
               linetype = "solid", color = "red", alpha = 0.7) +
    # Add annotations
    annotate("text", x = initial_train$Date[ceiling(nrow(initial_train)/2)], 
             y = max(ts_df$Actual) * 0.95, 
             label = paste("Initial Window\n(", initial_window, " months)"), 
             color = "darkblue", size = 3, hjust = 0.5) +
    {if(nrow(additional_train) > 0) 
      annotate("text", x = additional_train$Date[ceiling(nrow(additional_train)/2)], 
               y = max(ts_df$Actual) * 0.85, 
               label = paste("Added Data\n(+", nrow(additional_train), " months)"), 
               color = "lightblue", size = 3, hjust = 0.5)} +
    # Labels and theme
    labs(title = paste("Growing Window", show_window, "- Prophet Training Expansion"),
         subtitle = paste("Dark Blue: Initial Window (", initial_window, "months) | Light Blue: Added Data | Red: Forecast (", horizon, "months) | Black: Actual Test"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "4 months")
  
  # Add summary info
  total_train_months <- train_end_idx - train_start_idx + 1
  p <- p + 
    annotate("text", x = min(ts_df$Date), y = min(ts_df$Actual), 
             label = paste("Total Training:", total_train_months, "months"), 
             hjust = 0, vjust = 0, size = 3.5, color = "gray30",
             fontface = "bold")
  
  return(p)
}

# Rolling window process
plot_prophet_forecasts(y_ts, results_rolling_window_prophet,
                      window_type = "Rolling", window_size = window_size, horizon = horizon)

# Show only recent forecasts
plot_prophet_forecasts(y_ts, results_rolling_window_prophet,
                      show_all_forecasts = FALSE)

# Highlight a specific window (window 15)
plot_prophet_forecasts(y_ts, results_rolling_window_prophet,
                      highlight_window = 15)

# Error dashboard
plot_prophet_error_dashboard(results_rolling_window_prophet, y_ts, window_size, horizon)

# Side-by-side comparison (if you have both rolling and growing results)
plot_rolling_vs_growing_prophet(y_ts, results_rolling_window_prophet,
                                results_growing_window_prophet, window_size, horizon)

# Detailed window view
plot_detailed_forecast_prophet(y_ts, results_rolling_window_prophet, window_num = 5)

# Rolling window progression
plot_rolling_window_progression_prophet(y_ts, results_rolling_window_prophet, window_size, horizon)

# Rolling window movement (animated-style)
plot_rolling_window_movement_prophet(y_ts, results_rolling_window_prophet, 
                                     window_size, horizon, show_window = 15)

# Growing window progression (if you implement growing windows)
plot_growing_window_progression_prophet(y_ts, results_growing_window_prophet, window_size, horizon)

# Growing window movement (if you implement growing windows)
plot_growing_window_movement_prophet(y_ts, results_growing_window_prophet, 
                                     window_size, horizon, show_window = 15)
