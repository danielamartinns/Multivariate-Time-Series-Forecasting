# Updated rolling window progression function for Prophet 
plot_rolling_window_progression_prophet <- function(df_data, results, window_size = 36, horizon = 6, debug = FALSE) {
  
  # Ensure ds column is Date format
  df_data$ds <- as.Date(df_data$ds)
  
  # Create the base plot
  p <- ggplot() +
    geom_line(data = df_data, aes(x = ds, y = y), 
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
  
  # Debug info
  if(debug) {
    cat("Number of windows:", n_windows, "\n")
    cat("First forecast columns:", names(results$forecasts[[1]]), "\n")
    cat("First forecast nrows:", nrow(results$forecasts[[1]]), "\n")
  }
  
  # Add training window indicators for first few and last few windows
  show_windows <- c(1, 2, 3, n_windows-2, n_windows-1, n_windows)
  show_windows <- show_windows[show_windows <= n_windows & show_windows > 0]
  
  for(i in 1:length(results$forecasts)) {
    # Calculate indices for this rolling window
    train_start_idx <- i
    train_end_idx <- i + window_size - 1
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, nrow(df_data))
    
    if(test_start_idx > nrow(df_data)) next
    
    # Get forecast data from Prophet results
    forecast_full <- results$forecasts[[i]]
    # Extract only the forecast period (after training data)
    training_size <- train_end_idx - train_start_idx + 1
    forecast_start_row <- training_size + 1
    forecast_end_row <- min(forecast_start_row + horizon - 1, nrow(forecast_full))
    
    if(forecast_start_row > nrow(forecast_full)) next
    
    forecast_subset <- forecast_full[forecast_start_row:forecast_end_row, ]
    
    # Debug info for first window
    if(debug && i == 1) {
      cat("Window 1 - Training size:", training_size, "\n")
      cat("Forecast start row:", forecast_start_row, "\n")
      cat("Forecast end row:", forecast_end_row, "\n")
      cat("Forecast subset nrows:", nrow(forecast_subset), "\n")
      cat("Forecast subset columns:", names(forecast_subset), "\n")
      print(head(forecast_subset))
    }
    
    # Create forecast data frame - check for different possible column names
    if("yhat_lower" %in% names(forecast_subset) && "yhat_upper" %in% names(forecast_subset)) {
      temp_df <- data.frame(
        Date = as.Date(forecast_subset$ds),
        Forecast = forecast_subset$yhat,
        Lower_CI = forecast_subset$yhat_lower,
        Upper_CI = forecast_subset$yhat_upper
      )
    } else if("lower" %in% names(forecast_subset) && "upper" %in% names(forecast_subset)) {
      temp_df <- data.frame(
        Date = as.Date(forecast_subset$ds),
        Forecast = forecast_subset$yhat,
        Lower_CI = forecast_subset$lower,
        Upper_CI = forecast_subset$upper
      )
    } else {
      # No confidence intervals available
      temp_df <- data.frame(
        Date = as.Date(forecast_subset$ds),
        Forecast = forecast_subset$yhat,
        Lower_CI = forecast_subset$yhat,  # Use forecast as placeholder
        Upper_CI = forecast_subset$yhat   # Use forecast as placeholder
      )
      if(debug && i == 1) {
        cat("Warning: No confidence interval columns found!\n")
      }
    }
    
    # Determine line properties based on window position
    if(i %in% show_windows) {
      # Highlighted windows (first few and last few)
      line_size <- 1.2
      point_size <- 2
      alpha_ribbon <- 0.4  # Increased from 0.3
      alpha_line <- 0.9
    } else {
      # Other windows (more subtle)
      line_size <- 0.6
      point_size <- 1
      alpha_ribbon <- 0.2  # Increased from 0.1
      alpha_line <- 0.4
    }
    
    # Debug info for confidence intervals
    if(debug && i <= 3) {
      cat("Window", i, "CI range:", 
          "Min Lower:", min(temp_df$Lower_CI), 
          "Max Upper:", max(temp_df$Upper_CI), 
          "Forecast range:", min(temp_df$Forecast), "to", max(temp_df$Forecast), "\n")
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
      train_start_date <- df_data$ds[window_num]
      train_end_date <- df_data$ds[window_num + window_size - 1]
      
      # Add subtle vertical lines to show training windows
      p <- p + 
        geom_vline(xintercept = train_start_date, linetype = "dotted", 
                   color = colors[window_num], alpha = 0.5) +
        geom_vline(xintercept = train_end_date, linetype = "dashed", 
                   color = colors[window_num], alpha = 0.7)
    }
  }
  
  # Add legend annotations
  legend_y <- max(df_data$y) * 0.95
  p <- p + 
    annotate("text", x = min(df_data$ds), y = legend_y, 
             label = "Lighter = Earlier Windows\nDarker = Later Windows", 
             hjust = 0, size = 3.5, color = "gray30")
  
  # Format x-axis
  p <- p + scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Updated growing window progression function for Prophet
plot_growing_window_progression_prophet <- function(df_data, results, initial_window = 36, horizon = 6) {
  
  # Ensure ds column is Date format
  df_data$ds <- as.Date(df_data$ds)
  
  # Create the base plot
  p <- ggplot() +
    geom_line(data = df_data, aes(x = ds, y = y), 
              color = "black", size = 1, alpha = 0.8) +
    labs(title = "Growing Window Prophet: Progressive Forecasting",
         subtitle = paste("Each forecast uses progressively more training data |", 
                          "Initial Window:", initial_window, "months | Horizon:", horizon, "months"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Add forecasts with different colors for progression
  colors <- rainbow(length(results$forecasts), start = 0.6, end = 0.9)
  
  for(i in 1:length(results$forecasts)) {
    # Calculate indices for this window
    train_start_idx <- 1  # Always start from beginning
    train_end_idx <- initial_window + i - 1  # Growing window size
    test_start_idx <- train_end_idx + 1
    test_end_idx <- min(test_start_idx + horizon - 1, nrow(df_data))
    
    if(test_start_idx > nrow(df_data)) next
    
    # Get forecast data from Prophet results
    forecast_full <- results$forecasts[[i]]
    # Extract only the forecast period (after training data)
    training_size <- train_end_idx - train_start_idx + 1
    forecast_start_row <- training_size + 1
    forecast_end_row <- min(forecast_start_row + horizon - 1, nrow(forecast_full))
    
    if(forecast_start_row > nrow(forecast_full)) next
    
    forecast_subset <- forecast_full[forecast_start_row:forecast_end_row, ]
    
    # Add forecast to plot
    temp_df <- data.frame(
      Date = as.Date(forecast_subset$ds),
      Forecast = forecast_subset$yhat,
      Lower_CI = forecast_subset$yhat_lower,
      Upper_CI = forecast_subset$yhat_upper
    )
    
    p <- p + 
      geom_ribbon(data = temp_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                  alpha = 0.3, fill = colors[i]) +  # Increased from 0.2
      geom_line(data = temp_df, aes(x = Date, y = Forecast),
                color = colors[i], size = 1) +
      geom_point(data = temp_df, aes(x = Date, y = Forecast),
                 color = colors[i], size = 1.5)
  }
  
  # Add training period indicators for first and last windows
  first_train_end <- df_data$ds[initial_window]
  last_train_end <- df_data$ds[initial_window + length(results$forecasts) - 1]
  
  p <- p + 
    geom_vline(xintercept = first_train_end, linetype = "dashed", color = "gray50", alpha = 0.7) +
    geom_vline(xintercept = last_train_end, linetype = "dashed", color = "gray50", alpha = 0.7) +
    annotate("text", x = first_train_end, y = max(df_data$y) * 0.9, 
             label = "First Training End", angle = 90, vjust = -0.5, size = 3) +
    annotate("text", x = last_train_end, y = max(df_data$y) * 0.9, 
             label = "Last Training End", angle = 90, vjust = -0.5, size = 3)
  
  # Format x-axis
  p <- p + scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create an animated-style rolling window plot for Prophet (shows window movement)
plot_rolling_window_movement_prophet <- function(df_data, results, window_size = 36, horizon = 6, 
                                                 show_window = NULL) {
  
  if(is.null(show_window)) {
    show_window <- ceiling(length(results$forecasts) / 2)  # Show middle window by default
  }
  
  # Ensure ds column is Date format
  df_data$ds <- as.Date(df_data$ds)
  
  # Calculate window indices
  train_start_idx <- show_window
  train_end_idx <- show_window + window_size - 1
  test_start_idx <- train_end_idx + 1
  test_end_idx <- min(test_start_idx + horizon - 1, nrow(df_data))
  
  # Create data segments
  before_train <- df_data[1:(train_start_idx-1), ]
  train_data <- df_data[train_start_idx:train_end_idx, ]
  test_data <- df_data[test_start_idx:test_end_idx, ]
  after_test <- if(test_end_idx < nrow(df_data)) df_data[(test_end_idx+1):nrow(df_data), ] else data.frame()
  
  # Get forecast data from Prophet results
  forecast_full <- results$forecasts[[show_window]]
  training_size <- train_end_idx - train_start_idx + 1
  forecast_start_row <- training_size + 1
  forecast_end_row <- min(forecast_start_row + horizon - 1, nrow(forecast_full))
  
  forecast_subset <- forecast_full[forecast_start_row:forecast_end_row, ]
  
  forecast_df <- data.frame(
    Date = as.Date(forecast_subset$ds),
    Forecast = forecast_subset$yhat,
    Lower_CI = forecast_subset$yhat_lower,
    Upper_CI = forecast_subset$yhat_upper
  )
  
  # Create the plot
  p <- ggplot() +
    # Historical data (before training)
    geom_line(data = before_train, aes(x = ds, y = y), 
              color = "gray70", size = 0.8, alpha = 0.6) +
    # Training data (highlighted)
    geom_line(data = train_data, aes(x = ds, y = y), 
              color = "blue", size = 1.2) +
    # Test data (actual values to compare against)
    geom_line(data = test_data, aes(x = ds, y = y), 
              color = "black", size = 1.2) +
    # Future data (if any)
    {if(nrow(after_test) > 0) geom_line(data = after_test, aes(x = ds, y = y), 
                                        color = "gray70", size = 0.8, alpha = 0.6)} +
    # Forecast with confidence interval
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                alpha = 0.3, fill = "red") +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast),
              color = "red", size = 1.5, linetype = "dashed") +
    geom_point(data = forecast_df, aes(x = Date, y = Forecast),
               color = "red", size = 2.5) +
    # Add vertical separators
    geom_vline(xintercept = train_data$ds[1], linetype = "solid", color = "blue", alpha = 0.7) +
    geom_vline(xintercept = train_data$ds[nrow(train_data)], linetype = "solid", color = "blue", alpha = 0.7) +
    geom_vline(xintercept = test_data$ds[1], linetype = "solid", color = "red", alpha = 0.7) +
    # Labels and theme
    labs(title = paste("Rolling Window", show_window, "- Prophet Training and Forecasting"),
         subtitle = paste("Blue: Training Data (", window_size, "months) | Red: Forecast (", horizon, "months) | Black: Actual Test Values"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")
  
  return(p)
}

# Function to create an animated-style growing window plot for Prophet (shows window expansion)
plot_growing_window_movement_prophet <- function(df_data, results, initial_window = 36, horizon = 6, 
                                                 show_window = NULL) {
  
  if(is.null(show_window)) {
    show_window <- ceiling(length(results$forecasts) / 2)  # Show middle window by default
  }
  
  # Ensure ds column is Date format
  df_data$ds <- as.Date(df_data$ds)
  
  # Calculate window indices for growing window
  train_start_idx <- 1  # Always starts from beginning
  train_end_idx <- initial_window + show_window - 1  # Growing window size
  test_start_idx <- train_end_idx + 1
  test_end_idx <- min(test_start_idx + horizon - 1, nrow(df_data))
  
  # Create data segments
  train_data <- df_data[train_start_idx:train_end_idx, ]
  test_data <- df_data[test_start_idx:test_end_idx, ]
  after_test <- if(test_end_idx < nrow(df_data)) df_data[(test_end_idx+1):nrow(df_data), ] else data.frame()
  
  # Highlight the initial window vs the additional data
  initial_train <- df_data[train_start_idx:initial_window, ]
  additional_train <- if(train_end_idx > initial_window) df_data[(initial_window+1):train_end_idx, ] else data.frame()
  
  # Get forecast data from Prophet results
  forecast_full <- results$forecasts[[show_window]]
  training_size <- train_end_idx - train_start_idx + 1
  forecast_start_row <- training_size + 1
  forecast_end_row <- min(forecast_start_row + horizon - 1, nrow(forecast_full))
  
  forecast_subset <- forecast_full[forecast_start_row:forecast_end_row, ]
  
  forecast_df <- data.frame(
    Date = as.Date(forecast_subset$ds),
    Forecast = forecast_subset$yhat,
    Lower_CI = forecast_subset$yhat_lower,
    Upper_CI = forecast_subset$yhat_upper
  )
  
  # Create the plot
  p <- ggplot() +
    # Initial training window (highlighted differently)
    geom_line(data = initial_train, aes(x = ds, y = y), 
              color = "darkblue", size = 1.0, alpha = 0.7) +
    # Additional training data (showing growth)
    {if(nrow(additional_train) > 0) geom_line(data = additional_train, aes(x = ds, y = y), 
                                              color = "lightblue", size = 1.2)} +
    # Test data (actual values to compare against)
    geom_line(data = test_data, aes(x = ds, y = y), 
              color = "black", size = 1.2) +
    # Future data (if any)
    {if(nrow(after_test) > 0) geom_line(data = after_test, aes(x = ds, y = y), 
                                        color = "gray70", size = 0.8, alpha = 0.6)} +
    # Forecast with confidence interval
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI),
                alpha = 0.3, fill = "red") +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast),
              color = "red", size = 1.5, linetype = "dashed") +
    geom_point(data = forecast_df, aes(x = Date, y = Forecast),
               color = "red", size = 2.5) +
    # Add vertical separators
    geom_vline(xintercept = initial_train$ds[nrow(initial_train)], 
               linetype = "dotted", color = "darkblue", alpha = 0.8, size = 1) +
    geom_vline(xintercept = train_data$ds[nrow(train_data)], 
               linetype = "solid", color = "lightblue", alpha = 0.8, size = 1) +
    geom_vline(xintercept = test_data$ds[1], 
               linetype = "solid", color = "red", alpha = 0.7) +
    # Add annotations
    annotate("text", x = initial_train$ds[ceiling(nrow(initial_train)/2)], 
             y = max(df_data$y) * 0.95, 
             label = paste("Initial Window\n(", initial_window, " months)"), 
             color = "darkblue", size = 3, hjust = 0.5) +
    {if(nrow(additional_train) > 0) 
      annotate("text", x = additional_train$ds[ceiling(nrow(additional_train)/2)], 
               y = max(df_data$y) * 0.85, 
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
    annotate("text", x = min(df_data$ds), y = min(df_data$y), 
             label = paste("Total Training:", total_train_months, "months"), 
             hjust = 0, vjust = 0, size = 3.5, color = "gray30",
             fontface = "bold")
  
  return(p)
}

# usage:
plot_rolling_window_progression_prophet(df_data, results_prophet_rolling_window)

plot_growing_window_progression_prophet(df_data, results_prophet_growing_window)

plot_rolling_window_movement_prophet(df_data, results_prophet_rolling_window, show_window = 5)

plot_growing_window_movement_prophet(df_data, results_prophet_growing_window, show_window = 5)
