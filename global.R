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

library(future)
library(progress)

weekend_definition <- c("Saturday", "Sunday")

historical_summary_weekend <- readRDS(
  file = "ouptut/historical_summary_weekend.rds"
)

nested_forecasts_insights <- readRDS(
  file = "ouptut/nested_forecasts_insights.rds"
)

queries_df <- readRDS(
  file = "ouptut/queries_df.rds"
)

# # --- Extract Unique Departure Dates and Routes for Dropdowns ---
departure_dates <- unique(
  sapply(
    strsplit(
      names(nested_forecasts_insights), "__"
    ), `[`, 1
  )
)

routes <- unique(
  sapply(
    strsplit(
      names(nested_forecasts_insights), "__"
    ), `[`, 2
  )
)
