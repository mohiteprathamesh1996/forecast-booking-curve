library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(readr)
library(forecast)
library(prophet)
library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(timeDate)
library(gridExtra)
library(mgcv)
library(plotly)
library(DT)

# Load dataset (Historical booking curves)
dataset <- read.csv("data/dataset.csv") %>%
  mutate(departure_Date = as.Date(departure_Date)) %>%
  arrange(departure_Date)

# Load dataset (Flights needing forecast)
output <- read.csv("data/output.csv") %>%
  mutate(departure_Date = as.Date(departure_Date)) %>%
  arrange(departure_Date)

# Compute pickup
pickup_info <- dataset %>% 
  group_by(Origin_Destination) %>% 
  summarise(across(-departure_Date, ~ round(mean(.x, na.rm = TRUE)), .names = "{.col}")) %>%
  mutate(across(-c(Origin_Destination, Target), ~ Target - ., .names = "{.col}")) %>%
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

# Reshaped dataset in long format for time-series analysis
dataset_long <- dataset %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "Seats Sold"
  ) %>%
  mutate(
    `Days Before Departure` = as.numeric(
      gsub("[^0-9]", "", `Days Before Departure`)
    )
  ) %>%
  group_by(departure_Date, Origin_Destination) %>%
  filter(
    `Days Before Departure` <= max(
      `Days Before Departure`[!is.na(`Seats Sold`)],
      na.rm = TRUE
    )
  ) %>%
  mutate(
    `Seats Sold` = round(ifelse(
      test = is.na(`Seats Sold`),
      yes = (lead(`Seats Sold`) + lag(`Seats Sold`)) / 2,
      no = `Seats Sold`
    ))
  ) %>%
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
      PercentageTargetReached>1, 1, PercentageTargetReached
    )
  ) %>% 
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>% 
  group_by(departure_Date, Origin_Destination) %>% 
  mutate(LF_PercentageTargetReached = lead(PercentageTargetReached)) %>% 
  ungroup() %>% 
  left_join(
    pickup_info,
    by = c("Origin_Destination", "Days Before Departure")
  )

output_long <- output %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Days Before Departure",
    values_to = "Seats Sold"
  ) %>%
  mutate(
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
  mutate(
    `Seats Sold` = round(ifelse(
      test = is.na(`Seats Sold`),
      yes = (lead(`Seats Sold`) + lag(`Seats Sold`)) / 2,
      no = `Seats Sold`
    ))
  ) %>%
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
      PercentageTargetReached>1, 1, PercentageTargetReached
    )
  ) %>% 
  arrange(departure_Date, Origin_Destination, `Days Before Departure`) %>% 
  group_by(departure_Date, Origin_Destination) %>% 
  mutate(LF_PercentageTargetReached = lead(PercentageTargetReached)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  left_join(
    pickup_info,
    by = c("Origin_Destination", "Days Before Departure")
  )

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


# Extract unique departure dates and routes for dropdowns
departure_dates <- unique(output_long$departure_Date)
routes <- unique(output_long$Origin_Destination)