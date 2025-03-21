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
library(log4r)


#  --- Logging and error handling ---
log_dir <- "logs"

# Create the log directory if it doesn't exist
if (!dir.exists(log_dir)) {
  dir.create(log_dir)
}

log_file <- file.path(log_dir, "app_logs.txt") # Define log file path
logger <- logger(threshold = "INFO", appenders = file_appender(log_file))



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
  drop_na() %>%
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

# --- Forecast Stability & Risk Assessment Table ---
forecast_risk_summary <- output_long %>%
  group_by(departure_Date, Origin_Destination, Target) %>%
  summarise(
    .groups = "drop",
    `Training Points` = 1 + max(`Days Before Departure`) - min(`Days Before Departure`),
    `Prediction Ahead` = min(`Days Before Departure`)
  ) %>%
  mutate(
    `Prediction Ratio` = round(100 * `Prediction Ahead` / `Training Points`, 2),

    # Revamped Risk Logic
    Risk = case_when(
      `Prediction Ratio` <= 50 ~ "🟢 Low Risk - Reliable Forecasting",
      `Prediction Ratio` <= 100 ~ "🟡 Medium Risk - Monitor Accuracy",
      `Prediction Ratio` <= 140 ~ "🟠 High Risk - Unstable Forecasting",
      TRUE ~ "🔴 Very High Risk - Limited Data"
    )
  ) %>%
  filter(
    Risk %in% c(
      "🟢 Low Risk - Reliable Forecasting",
      "🟡 Medium Risk - Monitor Accuracy",
      "🟠 High Risk - Unstable Forecasting",
      "🔴 Very High Risk - Limited Data"
    )
  )

pb <- progress_bar$new(
  format = "  Processing [:bar] :percent ETA: :eta",
  total = length(unique(output_long$departure_Date)),
  clear = FALSE,
  width = 60
)

nested_forecasts_insights <- list()

for (dep_date in unique(output_long$departure_Date)) {
  pb$tick()
  for (route in unique(output_long$Origin_Destination)) {
    tryCatch(
      {
        # Filter and prepare training data
        train <- output_long %>%
          filter(
            departure_Date == dep_date &
              Origin_Destination == route
          ) %>%
          mutate(
            `Date Before Departure` = departure_Date - days(`Days Before Departure`)
          ) %>%
          select(
            `Date Before Departure`,
            `Seats Sold`,
            DailyBookingRate,
            BookingRateAccelaration,
            PercentageTargetReached,
            LF_PercentageTargetReached,
            AvgPickUp
          ) %>%
          arrange(`Date Before Departure`) %>%
          drop_na()

        target_cap <- forecast_risk_summary %>%
          filter(departure_Date == dep_date & Origin_Destination == route) %>%
          pull(Target)

        days_ahead <- forecast_risk_summary %>%
          filter(departure_Date == dep_date & Origin_Destination == route) %>%
          pull(`Prediction Ahead`)

        risk_msg <- forecast_risk_summary %>%
          filter(departure_Date == dep_date & Origin_Destination == route) %>%
          pull(Risk)

        # Enable parallel processing
        plan(multisession)

        assess_size <- 30

        while (assess_size >= 5) {
          try(
            {
              splits <- rolling_origin(
                data = train,
                initial = round(0.80 * nrow(train)),
                assess = assess_size,
                cumulative = FALSE
              )
              # If rolling_origin() succeeds, break out of the loop
              break
            },
            silent = TRUE
          )

          # Reduce assess_size by 5 if an error occurs
          assess_size <- assess_size - 1
        }

        # Function to Train and Forecast for Each Split
        walk_forward_results <- future_map_dfr(splits$splits, function(split) {
          if (nrow(splits) == 1) {
            train_data <- training(splits$splits[[1]])
            test_data <- testing(splits$splits[[1]])
          } else {
            train_data <- training(split)
            test_data <- testing(split)
          }

          # Train ARIMA Model
          model_arima <- arima_reg() %>%
            set_engine("auto_arima") %>%
            fit(
              `Seats Sold` ~ `Date Before Departure`,
              train_data
            )

          # Train Prophet Model (Basic)
          model_prophet <- prophet_reg(
            growth = "logistic",
            logistic_cap = target_cap
          ) %>%
            set_engine("prophet") %>%
            fit(
              `Seats Sold` ~ `Date Before Departure`,
              train_data
            )

          # Train Prophet Model with Regressors (Optimized)
          model_prophet_with_reg <- tryCatch(
            {
              prophet_reg(
                growth = "logistic",
                season = "multiplicative", # Primary model with seasonality
                logistic_cap = target_cap,
                changepoint_num = 1 # Less sensitivity to noise
              ) %>%
                set_engine("prophet") %>%
                fit(
                  `Seats Sold` ~ `Date Before Departure`
                    + DailyBookingRate
                    + LF_PercentageTargetReached,
                  train_data
                )
            },
            error = function(e) {
              message("Error with seasonality: ", e$message)
              message("Retrying without seasonality...")

              # Fallback model without seasonality
              prophet_reg(
                growth = "logistic",
                logistic_cap = target_cap,
                changepoint_num = 1 # Less sensitivity to noise
              ) %>%
                set_engine("prophet") %>%
                fit(
                  `Seats Sold` ~ `Date Before Departure`
                    + DailyBookingRate
                    + LF_PercentageTargetReached,
                  train_data
                )
            }
          )


          # Create Model Table for the Current Window
          model_tbl <- modeltime_table(
            model_arima,
            model_prophet,
            model_prophet_with_reg
          )

          # Forecast for the Test Set of Current Split
          forecast_tbl <- model_tbl %>%
            modeltime_calibrate(test_data) %>%
            modeltime_forecast(new_data = test_data, actual_data = train) %>%
            mutate(Window_ID = split$id) # Track which window this belongs to

          return(forecast_tbl)
        })

        walk_forward_results_summary <- walk_forward_results %>%
          filter(.model_desc != "ACTUAL") %>%
          left_join(
            walk_forward_results %>%
              filter(.model_desc == "ACTUAL") %>%
              select(.index, .actual = .value) %>%
              distinct(.keep_all = TRUE),
            by = ".index"
          ) %>%
          group_by(.model_desc) %>%
          summarise(
            MAE = round(mae(actual = .actual, predicted = .value), 2),
            MAPE = round(mape(actual = .actual, predicted = .value), 2),
            MASE = round(mase(actual = .actual, predicted = .value), 2),
            SMAPE = round(smape(actual = .actual, predicted = .value), 2),
            RMSE = round(rmse(actual = .actual, predicted = .value), 2),
            RSQ = round(postResample(pred = .value, obs = .actual)["Rsquared"], 2)
          )

        advance_pickup_df <- output_long %>%
          filter(
            Origin_Destination == route &
              departure_Date == dep_date
          ) %>%
          filter(
            `Days Before Departure` == min(`Days Before Departure`, na.rm = TRUE)
          )

        # Generate future dates for forecasting
        future_data <- future_frame(
          .data = train,
          .date_var = `Date Before Departure`,
          .length_out = paste(days_ahead, "days")
        ) %>%
          mutate(
            `Days Before Departure` = as.integer(
              as.Date(dep_date) - `Date Before Departure`
            )
          ) %>%
          left_join(
            historical_summary_weekend %>%
              filter(
                WeekendDeparture == ifelse(
                  test = wday(
                    dep_date,
                    label = TRUE,
                    abbr = FALSE
                  ) %in% weekend_definition,
                  yes = 1,
                  no = 0
                )
              ) %>%
              filter(Origin_Destination == route) %>%
              select(
                `Days Before Departure`,
                DailyBookingRate,
                BookingRateAccelaration,
                PercentageTargetReached,
                LF_PercentageTargetReached,
                AvgPickUp
              ),
            by = "Days Before Departure"
          )

        model_tbl <- modeltime_table(
          arima_reg(
            non_seasonal_ar = 2,
            non_seasonal_differences = 1,
            non_seasonal_ma = 1
          ) %>%
            set_engine("auto_arima") %>%
            fit(
              `Seats Sold` ~ `Date Before Departure`,
              train
            ),
          prophet_reg(
            growth = "logistic",
            logistic_cap = target_cap
          ) %>%
            set_engine("prophet") %>%
            fit(
              `Seats Sold` ~ `Date Before Departure`,
              train
            ),
          prophet_reg(
            growth = "logistic",
            season = "multiplicative",
            logistic_cap = target_cap,
            changepoint_num = 1
          ) %>%
            set_engine("prophet") %>%
            fit(
              `Seats Sold` ~ `Date Before Departure`
                + DailyBookingRate
                + LF_PercentageTargetReached,
              train
            )
        )

        dynamic_plot <- model_tbl %>%
          modeltime_forecast(
            new_data = future_data,
            actual_data = train
          ) %>%
          left_join(
            walk_forward_results %>%
              filter(.model_desc != "ACTUAL") %>%
              left_join(
                walk_forward_results %>%
                  filter(.model_desc == "ACTUAL") %>%
                  select(.index, .actual = .value) %>%
                  distinct(.keep_all = TRUE),
                by = ".index"
              ) %>%
              mutate(error = .actual - .value) %>%
              group_by(.model_desc) %>%
              summarise(
                mean_error = mean(error, na.rm = TRUE),
                sd_error = sd(error, na.rm = TRUE) # Standard deviation of errors
              ),
            by = ".model_desc"
          ) %>%
          mutate(
            .value = round(.value),
            .conf_lo = .value - (1.96 * sd_error), # Lower bound
            .conf_hi = .value + (1.96 * sd_error) # Upper bound
          ) %>%
          select(-c(mean_error, sd_error)) %>%
          rbind(
            data.frame(
              .model_id = rep(NA, days_ahead),
              .model_desc = rep("Traditional PickUp Model", days_ahead),
              .key = rep("prediction", days_ahead),
              .index = future_data %>% pull(`Date Before Departure`) %>% sort(),
              .value = seq(
                advance_pickup_df$`Seats Sold`,
                advance_pickup_df$`Traditional Pick-Up Forecast`,
                length.out = days_ahead
              ),
              .conf_lo = rep(NA, days_ahead),
              .conf_hi = rep(NA, days_ahead)
            )
          ) %>%
          mutate(
            .value = round(.value),
            .conf_lo = round(.conf_lo),
            .conf_hi = round(.conf_hi)
          ) %>%
          plot_modeltime_forecast(
            .x_lab = "Date before Departure",
            .y_lab = "Seats Sold",
            .title = paste(
              "Booking Curve for", route, "on", as.Date(dep_date),
              paste(
                "[Target = ", target_cap,
                " seats; Prediction Window = ", days_ahead,
                " days; ", risk_msg, "]",
                sep = ""
              )
            )
          )

        forecast_summary <- model_tbl %>%
          modeltime_forecast(
            new_data = future_data,
            actual_data = train
          ) %>%
          filter(
            .model_desc %in% c("ACTUAL", "PROPHET W/ REGRESSORS")
          )

        nested_forecasts_insights[[
          paste(as.Date(dep_date), route, sep = "__")
        ]] <- list(
          dep_date = as.Date(dep_date),
          route = route,
          walk_forward_results_summary = walk_forward_results_summary,
          dynamic_plot = dynamic_plot,
          query = paste(
            "As an airline revenue management expert, analyze the forecasted booking
        curve including the confidence intervals for an upcoming flight,
        detailing expected demand patterns and commercial implications
        leading up to departure.",
            "\n\n**Booking Data Analysis:**",
            "The ACTUAL seat bookings over time:",
            paste(
              forecast_summary %>%
                filter(.key == "actual") %>%
                arrange(.index) %>%
                pull(.value),
              collapse = ";"
            ),
            "\nhave their FORECASTED seat bookings over time:",
            paste(
              forecast_summary %>%
                filter(.key == "prediction") %>%
                arrange(.index) %>%
                pull(.value) %>%
                round(),
              collapse = ";"
            ),
            "\n for the corresponding booking dates:",
            paste(
              forecast_summary %>%
                filter(.key == "prediction") %>%
                arrange(.index) %>%
                pull(.index),
              collapse = ";"
            ),
            ".",
            "\n\nThe flight operates on the",
            route, "route with a target capacity of",
            target_cap, "seats.",
            "\nThis forecast is contextualized against historical
        booking trends for similar flights operating on",
            ifelse(
              test = wday(
                as.Date(dep_date),
                label = TRUE,
                abbr = FALSE
              ) %in% weekend_definition,
              yes = "WEEKENDS.",
              no = "WEEKDAYS."
            ),
            "\n\n**Historical Booking Insights:**",
            "\nDays Before Departure: ",
            paste(
              historical_summary_weekend %>%
                filter(
                  Origin_Destination == route &
                    WeekendDeparture == ifelse(
                      test = wday(
                        as.Date(dep_date),
                        label = TRUE,
                        abbr = FALSE
                      ) %in% weekend_definition,
                      yes = 1,
                      no = 0
                    )
                ) %>%
                arrange(`Days Before Departure`) %>%
                pull(`Days Before Departure`),
              collapse = ", "
            ),
            "\nAverage Pickup Rate (Seats): ",
            paste(
              historical_summary_weekend %>%
                filter(
                  Origin_Destination == route &
                    WeekendDeparture == ifelse(
                      test = wday(
                        as.Date(dep_date),
                        label = TRUE,
                        abbr = FALSE
                      ) %in% weekend_definition,
                      yes = 1,
                      no = 0
                    )
                ) %>%
                arrange(`Days Before Departure`) %>%
                pull(AvgPickUp) %>% 
                round(2),
              collapse = ", "
            ),
            "\nDaily Booking Rate (Percentage): ",
            paste(
              historical_summary_weekend %>%
                filter(
                  Origin_Destination == route &
                    WeekendDeparture == ifelse(
                      test = wday(
                        as.Date(dep_date),
                        label = TRUE,
                        abbr = FALSE
                      ) %in% weekend_definition,
                      yes = 1,
                      no = 0
                    )
                ) %>%
                arrange(`Days Before Departure`) %>%
                pull(DailyBookingRate) %>% 
                round(2),
              collapse = ", "
            ),
            "\n\n**Strategic Revenue Management Request:**",
            "Based on the forecast and historical trends, provide a structured
        assessment of the booking trajectory. Identify revenue optimization
        opportunities, including demand-stimulation tactics if projected bookings
        fall short. Include an approximate date (format: Month, Day, Year)
        when booking momentum typically accelerates closer to departure.",
            "Deliver the response in structured paragraphs with a fact-based
        approach, avoiding bullet points or bold text. Use 'we' instead of
        'I'. Focus on data-driven insights and strategic airline
        pricing adjustments. As a side note, if projections fall
        short by 3 or 4 points don't call it a significant lag."
          )
        )
      },
      error = function(e) {
        log4r::error(
          logger,
          paste(
            Sys.time(), # Include timestamp
            "- Error in iteration with dep_date:",
            as.Date(dep_date),
            "and route:",
            route,
            ":",
            e$message
          )
        )
      }
    )
  }
}


# --- Extract Unique Departure Dates and Routes for Dropdowns ---
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


ai_insights <- function(
    query,
    max_tokens = 500,
    min_tokens = 50,
    decrement = 150
    ) {
  while (max_tokens >= min_tokens) {
    tryCatch(
      {
        response <- openai::create_chat_completion(
          model = "gpt-4",
          messages = list(
            list(
              role = "system",
              content = paste(
                readLines("prompt/system_prompt.txt"),
                collapse = " "
              )
            ),
            list(
              role = "user",
              content = query
            )
          ),
          temperature = 0.5,
          max_tokens = max_tokens
        )$choices$message.content

        return(response)
      },
      error = function(e) {
        if (
          grepl("maximum context length|token limit", e$message)
        ) {
          message(
            paste(
              "Reducing max_tokens to",
              max_tokens - decrement,
              "due to token limit error..."
            )
          )

          # Reduce token limit and retry
          max_tokens <- max_tokens - decrement
        } else {
          stop(e)
        }
      }
    )
  }

  message("Failed to get a response after reducing max_tokens.")
  return(NA)
}

# Apply to dataframe
queries_df <- do.call(
  rbind,
  lapply(
    lapply(names(nested_forecasts_insights), function(id) {
      query <- nested_forecasts_insights[[id]]$query
      return(list(id = id, query = query))
    }),
    function(x) data.frame(id = x$id, query = x$query)
  )
) %>%
  mutate(
    analysis = unlist(map(query, ~ ai_insights(.x)))
  )


saveRDS(
  object = historical_summary_weekend,
  file = "ouptut/historical_summary_weekend.rds"
)

saveRDS(
  object = nested_forecasts_insights,
  file = "ouptut/nested_forecasts_insights.rds"
)

saveRDS(
  object = queries_df, file = "ouptut/queries_df.rds"
)

rm(list = ls())
