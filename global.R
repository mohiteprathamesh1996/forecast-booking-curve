# # --- Install Required Libraries ---
# # Define the required packages
# required_packages <- c(
#   "rsconnect", "shiny", "dplyr", "ggplot2", "scales", "tidyr", "readr",
#   "forecast", "prophet", "modeltime", "tidymodels", "tidyverse",
#   "timetk", "lubridate", "timeDate", "gridExtra", "mgcv",
#   "plotly", "DT", "openai"
# )
#
# # Identify missing packages
# missing_packages <- required_packages[
#   !(required_packages %in% installed.packages()[, "Package"])
# ]
#
# # Install missing packages
# if (length(missing_packages) > 0) {
#   install.packages(missing_packages, dependencies = TRUE)
# }

# --- Load Required Libraries ---
# These packages are essential for data manipulation, visualization,
# forecasting, and modeling.
library(shiny) # Shiny web framework
library(dplyr) # Data manipulation
library(ggplot2) # Data visualization
library(scales) # Scaling functions for ggplot2
library(tidyr) # Data reshaping
library(readr) # Reading CSV files
library(forecast) # Time series forecasting
library(prophet) # Facebook Prophet for forecasting
library(modeltime) # Time series modeling
library(tidymodels) # Machine learning framework
library(tidyverse) # Collection of R packages for data science
library(timetk) # Time series feature engineering
library(lubridate) # Working with date/time objects
library(timeDate) # Date/time computations
library(gridExtra) # Arrange multiple ggplot2 plots
library(mgcv) # Generalized additive models
library(plotly) # Interactive visualizations
library(DT) # Interactive data tables
library(openai) # OpenAI module
library(memoise) # Caching for better performance
library(imputeTS)
library(zoo)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)

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
  left_join(
    pickup_info_weekend,
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
