# --- Load Required Libraries ---
# These packages are essential for data manipulation, visualization,
# forecasting, and modeling.

library(shiny) # Shiny web framework for building interactive apps
library(dplyr) # Data manipulation (filter, mutate, group_by, etc.)
library(ggplot2) # Data visualization using the Grammar of Graphics
library(scales) # Scaling functions for ggplot2 (e.g., percent_format)
library(tidyr) # Data reshaping and tidying functions (spread, gather)
library(readr) # Efficient CSV file reading and writing

# --- Time Series & Forecasting Libraries ---
library(forecast) # Classical time series forecasting (ARIMA, ETS, STL)
library(prophet) # Facebook Prophet for time series forecasting
library(modeltime) # Unified framework for multiple time series models
library(tidymodels) # Machine learning framework, including time series models
library(timetk) # Time series feature engineering and visualization
library(lubridate) # Working with date/time objects (e.g., parsing, manipulation)
library(timeDate) # Additional date/time computations for financial analysis

# --- Visualization & Interactive Elements ---
library(gridExtra) # Arrange multiple ggplot2 plots in a grid layout
library(mgcv) # Generalized Additive Models (GAM) for smoothing time series
library(plotly) # Interactive visualizations with ggplot2 compatibility
library(DT) # Interactive tables for displaying data in Shiny apps

# --- OpenAI & Performance Optimization ---
library(openai) # Integration with OpenAI API for AI-driven insights
library(memoise) # Caching computations to improve performance

# --- Missing Value Handling & Data Cleaning ---
library(imputeTS) # Impute missing values in time series data (e.g., Kalman filter)
library(zoo) # Moving averages, rolling functions, and time series operations

# --- Shiny Enhancements ---
library(shinyjs) # JavaScript integration for dynamic UI changes in Shiny
library(shinycssloaders) # Loading animations for UI elements in Shiny
library(shinyWidgets) # Additional UI widgets for enhanced interactivity

# --- Parallel Processing & Machine Learning ---
library(furrr) # Parallelized functions using 'future' package for faster execution
library(Metrics) # Evaluation metrics for model performance (MAE, RMSE, etc.)
library(caret) # Machine learning model training, tuning, and validation


# --- Load API Key for AI-Generated Insights ---
openai_api_key <- Sys.getenv("OPENAI_API_KEY") # Retrieve API key securely

# --- Load Datasets ---
# 1. Historical Booking Data (Past flights)
dataset <- read.csv("data/dataset.csv") %>%
  mutate(departure_Date = as.Date(departure_Date)) %>%
  arrange(departure_Date)

# 2. Future Flights Requiring Forecasting
output <- read.csv("data/output.csv") %>%
  mutate(departure_Date = as.Date(departure_Date)) %>%
  arrange(departure_Date)

# --- Compute Pickup (Seats Left to Sell) ---
weekend_definition <- c("Saturday", "Sunday")

pickup_info_weekend <- dataset %>%
  group_by(
    Origin_Destination,
    WeekendDeparture = ifelse(
      test = wday(
        departure_Date,
        label = TRUE,
        abbr = FALSE
      ) %in% weekend_definition,
      yes = 1,
      no = 0
    )
  ) %>%
  summarise(
    .groups = "drop",
    across(-departure_Date, ~ round(mean(.x, na.rm = TRUE)),
      .names = "{.col}"
    )
  ) %>%
  ungroup() %>%
  mutate(
    across(
      -c(
        Origin_Destination,
        Target,
        WeekendDeparture
      ),
      ~ Target - .,
      .names = "{.col}"
    )
  ) %>%
  select(-Target) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "AvgPickUp"
  ) %>%
  mutate(
    `Days Before Departure` = as.numeric(
      gsub("[^0-9]", "", `Days Before Departure`)
    )
  ) %>%
  drop_na()

# --- Reshape Historical Data for Time-Series Analysis ---
dataset_long <- dataset %>%
  # Convert wide format to long format for time series processing
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "Seats Sold"
  ) %>%
  # Identify if the departure date falls on a weekend
  mutate(
    WeekendDeparture = ifelse(
      wday(
        departure_Date,
        label = TRUE,
        abbr = FALSE
      ) %in% weekend_definition,
      yes = 1, # Weekend departure
      no = 0 # Weekday departure
    ),

    # Extract numerical values from column names
    `Days Before Departure` = as.numeric(
      gsub("[^0-9]", "", `Days Before Departure`)
    )
  ) %>%
  # Ensure the dataset is ordered correctly for time series analysis
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>%
  # Fill in missing days before departure to maintain a continuous time series
  group_by(departure_Date, Origin_Destination) %>%
  complete(`Days Before Departure` = full_seq(`Days Before Departure`, 1)) %>%
  # Remove negative or zero seat counts (ensuring valid values)
  mutate(`Seats Sold` = ifelse(`Seats Sold` <= 0, 0, `Seats Sold`)) %>%
  ungroup() %>%
  # --- Outlier Detection using Rolling Mean and Residuals ---
  group_by(Origin_Destination, departure_Date) %>%
  arrange(Origin_Destination, departure_Date, `Days Before Departure`) %>%
  # Compute a rolling mean (window size = 3) to detect anomalies
  mutate(
    rolling_mean = rollmean(`Seats Sold`, k = 3, fill = NA, align = "right"),
    residual = `Seats Sold` - rolling_mean # Compute residuals
  ) %>%
  # Define an outlier threshold based on standard deviation
  mutate(
    threshold = 2 * sd(residual, na.rm = TRUE) # Outliers are 2 SDs away
  ) %>%
  # Identify and remove extreme outliers only for dates far from departure (>30 days)
  mutate(
    `Seats Sold Outliers Removed` = ifelse(
      residual > threshold & `Days Before Departure` > 30,
      NA, # Remove extreme values as NA
      `Seats Sold`
    )
  ) %>%
  ungroup() %>%
  # --- Missing Value Imputation using Kalman Filter (StructTS Model) ---
  mutate(`Seats Sold` = round(
    na_kalman(
      `Seats Sold`, # Impute missing values
      model = "StructTS" # Use Structural Time Series Model
    )
  )) %>%
  # --- Compute Booking Dynamics for Forecasting ---
  ungroup() %>%
  group_by(Origin_Destination, departure_Date) %>%
  arrange(Origin_Destination, departure_Date, `Days Before Departure`) %>%
  # Compute daily booking rates and acceleration
  mutate(
    DailyBookingRate = `Seats Sold` - lead(`Seats Sold`),
    BookingRateAccelaration = DailyBookingRate - lead(DailyBookingRate),

    # Calculate the percentage of the target seat capacity reached
    PercentageTargetReached = `Seats Sold` / Target
  ) %>%
  ungroup() %>%
  # Ensure percentage target reached does not exceed 100% (1.0)
  mutate(
    PercentageTargetReached = ifelse(
      PercentageTargetReached > 1,
      1,
      PercentageTargetReached
    )
  ) %>%
  # --- Compute Load Factor Dynamics ---
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>%
  group_by(departure_Date, Origin_Destination) %>%
  # Lead the percentage target reached to compute next period's load factor
  mutate(LF_PercentageTargetReached = lead(PercentageTargetReached)) %>%
  ungroup() %>%
  # --- Merge with Additional Historical Pickup Data ---
  left_join(
    pickup_info_weekend, # Contains historical pickup trends for weekends
    by = c("Origin_Destination", "Days Before Departure", "WeekendDeparture")
  )


# --- Reshape Output Data for Forecasting ---
output_long <- output %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "Seats Sold"
  ) %>%
  mutate(
    WeekendDeparture = ifelse(
      test = wday(
        departure_Date,
        label = TRUE,
        abbr = FALSE
      ) %in% weekend_definition,
      yes = 1,
      no = 0
    ),
    `Days Before Departure` = as.numeric(
      gsub("[^0-9]", "", `Days Before Departure`)
    )
  ) %>%
  mutate(`Seats Sold` = ifelse(`Seats Sold` <= 0, 0, `Seats Sold`)) %>%
  group_by(departure_Date, Origin_Destination) %>%
  filter(
    `Days Before Departure` >= min(
      `Days Before Departure`[!is.na(`Seats Sold`)],
      na.rm = TRUE
    )
  ) %>%
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>%
  group_by(departure_Date, Origin_Destination) %>%
  complete(`Days Before Departure` = full_seq(`Days Before Departure`, 1)) %>%
  mutate(`Seats Sold` = round(
    na_kalman(
      `Seats Sold`,
      model = "StructTS"
    )
  )) %>%
  ungroup() %>%
  group_by(Origin_Destination, departure_Date) %>%
  arrange(
    Origin_Destination,
    departure_Date,
    `Days Before Departure`
  ) %>%
  mutate(
    DailyBookingRate = `Seats Sold` - lead(`Seats Sold`),
    BookingRateAccelaration = DailyBookingRate - lead(DailyBookingRate),
    PercentageTargetReached = `Seats Sold` / Target
  ) %>%
  ungroup() %>%
  mutate(
    PercentageTargetReached = ifelse(
      PercentageTargetReached > 1, 1, PercentageTargetReached
    )
  ) %>%
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>%
  group_by(departure_Date, Origin_Destination) %>%
  mutate(LF_PercentageTargetReached = lead(PercentageTargetReached)) %>%
  ungroup() %>%
  drop_na() %>%
  left_join(
    pickup_info_weekend,
    by = c("Origin_Destination", "Days Before Departure", "WeekendDeparture")
  ) %>%
  mutate(
    `Traditional Pick-Up Forecast` = `Seats Sold` + AvgPickUp
  )

# --- Generate General Historical Summary Statistics ---
historical_summary <- dataset_long %>%
  group_by(Origin_Destination, `Days Before Departure`) %>%
  summarise(
    .groups = "drop",
    DailyBookingRate = mean(DailyBookingRate, na.rm = TRUE),
    BookingRateAccelaration = mean(BookingRateAccelaration, na.rm = TRUE),
    PercentageTargetReached = mean(PercentageTargetReached, na.rm = TRUE),
    LF_PercentageTargetReached = mean(LF_PercentageTargetReached, na.rm = TRUE),
    AvgPickUp = mean(AvgPickUp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  drop_na()

# --- Generate Weekend/Weekday Based Historical Summary Statistics ---
historical_summary_weekend <- dataset_long %>%
  mutate(
    WeekendDeparture = ifelse(
      test = wday(
        departure_Date,
        label = TRUE,
        abbr = FALSE
      ) %in% weekend_definition,
      yes = 1,
      no = 0
    )
  ) %>%
  group_by(Origin_Destination, `Days Before Departure`, WeekendDeparture) %>%
  summarise(
    .groups = "drop",
    DailyBookingRate = mean(DailyBookingRate, na.rm = TRUE),
    BookingRateAccelaration = mean(BookingRateAccelaration, na.rm = TRUE),
    PercentageTargetReached = mean(PercentageTargetReached, na.rm = TRUE),
    LF_PercentageTargetReached = mean(LF_PercentageTargetReached, na.rm = TRUE),
    AvgPickUp = mean(AvgPickUp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  drop_na()

# --- Extract Unique Departure Dates and Routes for Dropdowns ---
departure_dates <- unique(output_long$departure_Date) # Unique departure dates
routes <- unique(output_long$Origin_Destination) # Unique flight routes
