# --- Load Required Libraries ---
# These packages are essential for data manipulation, visualization, forecasting, and modeling.
library(shiny)        # Shiny web framework
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(scales)       # Scaling functions for ggplot2
library(tidyr)        # Data reshaping
library(readr)        # Reading CSV files
library(forecast)     # Time series forecasting
library(prophet)      # Facebook Prophet for forecasting
library(modeltime)    # Time series modeling
library(tidymodels)   # Machine learning framework
library(tidyverse)    # Collection of R packages for data science
library(timetk)       # Time series feature engineering
library(lubridate)    # Working with date/time objects
library(timeDate)     # Date/time computations
library(gridExtra)    # Arrange multiple ggplot2 plots
library(mgcv)         # Generalized additive models
library(plotly)       # Interactive visualizations
library(DT)           # Interactive data tables

# --- Load API Key for AI-Generated Insights ---
openai_api_key <- Sys.getenv("OPENAI_API_KEY")  # Retrieve API key securely

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
pickup_info <- dataset %>%
  group_by(Origin_Destination) %>%
  summarise(
    across(-departure_Date, ~ round(mean(.x, na.rm = TRUE)), .names = "{.col}")
  ) %>%
  mutate(
    across(-c(Origin_Destination, Target), ~ Target - ., .names = "{.col}")
  ) %>%
  select(-Target) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "AvgPickUp"
  ) %>%
  mutate(
    `Days Before Departure` = as.numeric(gsub("[^0-9]", "", `Days Before Departure`))
  ) %>%
  drop_na()

# --- Reshape Historical Data for Time-Series Analysis ---
dataset_long <- dataset %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "Seats Sold"
  ) %>%
  mutate(
    `Days Before Departure` = as.numeric(gsub("[^0-9]", "", `Days Before Departure`))
  ) %>%
  group_by(departure_Date, Origin_Destination) %>%
  filter(
    `Days Before Departure` <= max(`Days Before Departure`[!is.na(`Seats Sold`)], na.rm = TRUE)
  ) %>%
  mutate(
    `Seats Sold` = round(ifelse(is.na(`Seats Sold`), 
                                (lead(`Seats Sold`) + lag(`Seats Sold`)) / 2, 
                                `Seats Sold`))
  ) %>%
  ungroup() %>%
  group_by(Origin_Destination, departure_Date) %>%
  arrange(Origin_Destination, departure_Date, `Days Before Departure`) %>%
  mutate(
    DailyBookingRate = `Seats Sold` - lead(`Seats Sold`),
    BookingRateAccelaration = DailyBookingRate - lead(DailyBookingRate),
    PercentageTargetReached = `Seats Sold` / Target
  ) %>%
  ungroup() %>%
  mutate(
    PercentageTargetReached = ifelse(PercentageTargetReached > 1, 1, PercentageTargetReached)
  ) %>%
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>%
  group_by(departure_Date, Origin_Destination) %>%
  mutate(LF_PercentageTargetReached = lead(PercentageTargetReached)) %>%
  ungroup() %>%
  left_join(pickup_info, by = c("Origin_Destination", "Days Before Departure"))

# --- Reshape Output Data for Forecasting ---
output_long <- output %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "Seats Sold"
  ) %>%
  mutate(
    `Days Before Departure` = as.numeric(gsub("[^0-9]", "", `Days Before Departure`))
  ) %>%
  group_by(departure_Date, Origin_Destination) %>%
  filter(
    `Days Before Departure` >= min(`Days Before Departure`[!is.na(`Seats Sold`)], na.rm = TRUE)
  ) %>%
  mutate(
    `Seats Sold` = round(ifelse(is.na(`Seats Sold`), 
                                (lead(`Seats Sold`) + lag(`Seats Sold`)) / 2, 
                                `Seats Sold`))
  ) %>%
  ungroup() %>%
  group_by(Origin_Destination, departure_Date) %>%
  arrange(Origin_Destination, departure_Date, `Days Before Departure`) %>%
  mutate(
    DailyBookingRate = `Seats Sold` - lead(`Seats Sold`),
    BookingRateAccelaration = DailyBookingRate - lead(DailyBookingRate),
    PercentageTargetReached = `Seats Sold` / Target
  ) %>%
  ungroup() %>%
  mutate(
    PercentageTargetReached = ifelse(PercentageTargetReached > 1, 1, PercentageTargetReached)
  ) %>%
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>%
  group_by(departure_Date, Origin_Destination) %>%
  mutate(LF_PercentageTargetReached = lead(PercentageTargetReached)) %>%
  ungroup() %>%
  drop_na() %>%
  left_join(pickup_info, by = c("Origin_Destination", "Days Before Departure"))

# --- Generate Historical Summary Statistics ---
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

# --- Extract Unique Departure Dates and Routes for Dropdowns ---
departure_dates <- unique(output_long$departure_Date)  # Unique departure dates
routes <- unique(output_long$Origin_Destination)  # Unique flight routes
