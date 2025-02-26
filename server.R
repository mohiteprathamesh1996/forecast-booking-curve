server <- function(input, output) {
  # --- Reactive Expression: Filter Dataset Based on User Selection ---
  filtered_data <- eventReactive(
    input$apply_filters,
    {
      req(input$dep_date, input$route)

      output_long %>%
        filter(
          departure_Date == as.Date(input$dep_date) &
            Origin_Destination == input$route
        )
    }
  )


  # --- Dynamic UI: Historical Trends Title ---
  # This dynamically generates the title based on user-selected route.
  output$historical_title <- renderUI({
    req(filtered_data())

    h3(
      paste(
        "Historical Trends Along",
        input$route,
        "Sector on",
        ifelse(
          test = wday(
            input$dep_date,
            label = TRUE,
            abbr = FALSE
          ) %in% weekend_definition,
          yes = "Weekend Departures",
          no = "Weekday Departures"
        )
      )
    )
  })

  # --- Generate Historical Trend Plots ---
  # This creates two line plots:
  # 1. Average Pickup (Seats Sold)
  # 2. Daily Booking Rate
  # The `grid.arrange()` function arranges them in a single view.

  output$historical_plots <- renderPlot({
    req(filtered_data())
    grid.arrange(
      historical_summary_weekend %>%
        filter(
          Origin_Destination == input$route &
            WeekendDeparture == ifelse(
              test = wday(
                input$dep_date,
                label = TRUE,
                abbr = FALSE
              ) %in% weekend_definition,
              yes = 1,
              no = 0
            )
        ) %>%
        mutate(`Days Before Departure` = -1 * `Days Before Departure`) %>%
        ggplot(
          aes(
            x = `Days Before Departure`,
            y = AvgPickUp
          )
        ) +
        geom_line(na.rm = TRUE, color = "#D71920", linewidth = 0.8) +
        theme_bw(),
      historical_summary_weekend %>%
        filter(
          Origin_Destination == input$route &
            WeekendDeparture == ifelse(
              test = wday(
                input$dep_date,
                label = TRUE,
                abbr = FALSE
              ) %in% weekend_definition,
              yes = 1,
              no = 0
            )
        ) %>%
        mutate(`Days Before Departure` = -1 * `Days Before Departure`) %>%
        ggplot(
          aes(
            x = `Days Before Departure`,
            y = DailyBookingRate
          )
        ) +
        geom_line(na.rm = TRUE, color = "darkgreen", linewidth = 0.8) +
        theme_bw()
    )
  })

  # --- Forecasting Model for Seat Sales ---
  # This section builds predictive models (ARIMA & Prophet) based on past booking data.
  output$forecast_plot <- renderPlotly({
    req(filtered_data()) # Ensure filtered dataset is available

    # Capture user inputs
    dep_date <- as.character(input$dep_date)
    route <- input$route

    # --- Prepare Training Data ---
    train <- filtered_data() %>%
      mutate(
        `Date Before Departure` = departure_Date - days(`Days Before Departure`)
      ) %>%
      select(
        `Date Before Departure`,
        `Seats Sold`,
        DailyBookingRate,
        LF_PercentageTargetReached,
        AvgPickUp
      ) %>%
      arrange(`Date Before Departure`) %>%
      drop_na() %>%
      filter_all(all_vars(!is.infinite(.)))

    # Keep the most recent data based on the user-selected training percentage
    train <- train %>%
      arrange(`Date Before Departure`) %>%
      tail(round(
        ((input$top_n_train) / 100) * nrow(train)
      ))

    # --- Extract Forecasting Parameters ---
    target_cap <- filtered_data() %>%
      pull(Target) %>%
      unique()

    days_ahead <- filtered_data() %>%
      pull(`Days Before Departure`) %>%
      min() # Find the minimum "Days Before Departure" for forecasting


    # Enable parallel processing
    plan(multisession)

    # --- Time Series Split for Model Training ---
    splits <- rolling_origin(
      data = train,
      initial = round(0.80 * nrow(train)),
      assess = input$assess,
      cumulative = FALSE
    )


    # Function to Train and Forecast for Each Split
    walk_forward_results <- future_map_dfr(splits$splits, function(split) {
      # Train ARIMA Model
      model_arima <- arima_reg() %>%
        set_engine("auto_arima") %>%
        fit(
          `Seats Sold` ~ `Date Before Departure`,
          training(split)
        )

      # Train Prophet Model (Basic)
      model_prophet <- prophet_reg(
        growth = "logistic",
        logistic_cap = target_cap
      ) %>%
        set_engine("prophet") %>%
        fit(
          `Seats Sold` ~ `Date Before Departure`,
          training(split)
        )

      # Train Prophet Model with Regressors (Optimized)
      model_prophet_with_reg <- prophet_reg(
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
          training(split)
        )

      # Create Model Table for the Current Window
      model_tbl <- modeltime_table(
        model_arima,
        model_prophet,
        model_prophet_with_reg
      )

      # Forecast for the Test Set of Current Split
      forecast_tbl <- model_tbl %>%
        modeltime_calibrate(testing(split)) %>%
        modeltime_forecast(new_data = testing(split), actual_data = train) %>%
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
      group_by(`Model Description` = .model_desc) %>%
      summarise(
        MAE = round(mae(actual = .actual, predicted = .value), 2),
        MAPE = round(mape(actual = .actual, predicted = .value), 2),
        MASE = round(mase(actual = .actual, predicted = .value), 2),
        SMAPE = round(smape(actual = .actual, predicted = .value), 2),
        RMSE = round(rmse(actual = .actual, predicted = .value), 2),
        RSQ = round(postResample(pred = .value, obs = .actual)["Rsquared"], 2)
      )

    # --- Display Model Accuracy in a Table ---
    output$accuracy_table <- renderDT({
      walk_forward_results_summary
    })

    # --- Generate Future Forecast Data ---
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

    advance_pickup_df <- output_long %>%
      filter(
        Origin_Destination == route &
          departure_Date == dep_date
      ) %>%
      filter(
        `Days Before Departure` == min(`Days Before Departure`, na.rm = TRUE)
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

    # --- Generate AI Insights (OpenAI API Call) ---
    output$ai_insights <- renderUI({
      # Extract forecasted seat bookings
      forecast_summary <- model_tbl %>%
        modeltime_forecast(
          new_data = future_data,
          actual_data = train
        ) %>%
        filter(
          .model_desc == c("ACTUAL", "PROPHET W/ REGRESSORS")
        )

      # Construct AI query
      query <- paste(
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
            pull(.value),
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
        input$route, "route with a target capacity of",
        target_cap, "seats.",
        "\nThis forecast is contextualized against historical
        booking trends for similar flights operating on",
        ifelse(
          test = wday(
            input$dep_date,
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
              Origin_Destination == input$route &
                WeekendDeparture == ifelse(
                  test = wday(
                    input$dep_date,
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
              Origin_Destination == input$route &
                WeekendDeparture == ifelse(
                  test = wday(
                    input$dep_date,
                    label = TRUE,
                    abbr = FALSE
                  ) %in% weekend_definition,
                  yes = 1,
                  no = 0
                )
            ) %>%
            arrange(`Days Before Departure`) %>%
            pull(AvgPickUp),
          collapse = ", "
        ),
        "\nDaily Booking Rate (Percentage): ",
        paste(
          historical_summary_weekend %>%
            filter(
              Origin_Destination == input$route &
                WeekendDeparture == ifelse(
                  test = wday(
                    input$dep_date,
                    label = TRUE,
                    abbr = FALSE
                  ) %in% weekend_definition,
                  yes = 1,
                  no = 0
                )
            ) %>%
            arrange(`Days Before Departure`) %>%
            pull(DailyBookingRate),
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


      # Send query to OpenAI API
      response <- openai::create_chat_completion(
        model = "gpt-4",
        messages = list(
          list(
            role = "system",
            content = "
            You are an expert in airline revenue management and
            commercial strategy. Answer questions with clear, structured
            insights using short paragraphs. Focus on key metrics, trends,
            and industry best practices. Limit responses to relevant
            data points and avoid speculation.
            "
          ),
          list(role = "user", content = query)
        ),
        temperature = 0.5, # Keeps responses focused and fact-based
        max_tokens = 1000 # Ensures detailed yet concise output
      )

      # Extract AI-generated insights
      insights <- response$choices$message.content

      formatted_insights <- paste(
        "<p>", insights, "</p>",
        sep = ""
      )

      HTML(formatted_insights)
    })

    # --- Generate Forecast Plot ---
    forecast_plot <- model_tbl %>%
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
          "Booking Curve for", input$route, "on", dep_date,
          paste(
            "[Target = ", target_cap,
            " seats; Prediction Days Ahead = ", days_ahead,
            sep = ""
          )
        )
      )


    # Convert GGPlot to Interactive Plotly Graph
    ggplotly(forecast_plot)
  })
}
