server <- function(input, output) {
  # --- Reactive Expression: Filter Dataset Based on User Selection ---
  # This ensures the user has selected a departure date and route before filtering.
  filtered_data <- reactive({
    req(input$dep_date, input$route) # Ensure required inputs are provided

    output_long %>%
      filter(
        departure_Date == as.Date(input$dep_date) & # Match selected departure date
          Origin_Destination == input$route # Match selected route
      )
  })

  # --- Dynamic UI: Historical Trends Title ---
  # This dynamically generates the title based on user-selected route.
  output$historical_title <- renderUI({
    req(input$route) # Ensure a route is selected

    h3(
      paste(
        "Historical Trends Along",
        input$route,
        "Sector"
      )
    )
  })

  # --- Generate Historical Trend Plots ---
  # This creates two line plots:
  # 1. Average Pickup (Seats Sold)
  # 2. Daily Booking Rate
  # The `grid.arrange()` function arranges them in a single view.
  output$historical_plots <- renderPlot({
    grid.arrange(
      historical_summary %>%
        filter(Origin_Destination == input$route) %>%
        mutate(`Days Before Departure` = -1 * `Days Before Departure`) %>%
        ggplot(
          aes(
            x = `Days Before Departure`,
            y = AvgPickUp
          )
        ) +
        geom_line(na.rm = TRUE, color = "#D71920", size = 0.8) + # Red line for visual emphasis
        theme_bw(),
      historical_summary %>%
        filter(Origin_Destination == input$route) %>%
        mutate(`Days Before Departure` = -1 * `Days Before Departure`) %>%
        ggplot(
          aes(
            x = `Days Before Departure`,
            y = DailyBookingRate
          )
        ) +
        geom_line(na.rm = TRUE, color = "darkgreen", size = 0.8) + # Green line for contrast
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
      drop_na() # Remove missing values

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

    # --- Time Series Split for Model Training ---
    splits <- time_series_split(
      data = train,
      assess = round(((input$test_slice) / 100) * nrow(train)), # Define test data size
      cumulative = TRUE
    )

    test_start_date <- min(testing(splits)$`Date Before Departure`) # Identify test start date

    # --- Model Definitions ---
    # 1. ARIMA Model
    model_arima <- arima_reg() %>%
      set_engine("auto_arima") %>%
      fit(`Seats Sold` ~ `Date Before Departure`, training(splits))

    # 2. Prophet Model (Logistic Growth)
    model_prophet <- prophet_reg(
      growth = "logistic",
      logistic_cap = target_cap
    ) %>%
      set_engine("prophet") %>%
      fit(`Seats Sold` ~ `Date Before Departure`, training(splits))

    # 3. Prophet Model with Additional Regressors
    model_prophet_with_reg <- prophet_reg(
      growth = "logistic",
      season = "multiplicative",
      logistic_cap = target_cap,
      changepoint_num = input$proph_changepoint_num
    ) %>%
      set_engine("prophet") %>%
      fit(
        `Seats Sold` ~ `Date Before Departure` +
          DailyBookingRate +
          LF_PercentageTargetReached +
          AvgPickUp,
        training(splits)
      )

    # --- Model Calibration & Accuracy Assessment ---
    model_tbl <- modeltime_table(
      model_arima,
      model_prophet,
      model_prophet_with_reg
    )

    calib_tbl <- model_tbl %>%
      modeltime_calibrate(testing(splits))

    # --- Display Model Accuracy in a Table ---
    output$accuracy_table <- renderDT({
      calib_tbl %>%
        modeltime_accuracy() %>%
        mutate(
          across(
            where(is.numeric) &
              !all_of(c(".model_id")),
            round, 2
          )
        ) %>%
        datatable(
          options = list(pageLength = 5, autoWidth = TRUE)
        )
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
              ) %in% c("Friday", "Saturday"),
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

    # --- Generate AI Insights (OpenAI API Call) ---
    output$ai_insights <- renderUI({
      # Extract forecasted seat bookings
      forecast_summary <- calib_tbl %>%
        modeltime_refit(
          data = train
        ) %>%
        modeltime_forecast(
          new_data = future_data,
          actual_data = train
        ) %>%
        mutate(
          .value = round(.value),
          .conf_lo = round(.conf_lo),
          .conf_hi = round(.conf_hi)
        ) %>%
        filter(
          .model_desc == c("ACTUAL", "PROPHET W/ REGRESSORS")
        )

      # Construct AI query
      query <- paste(
        "This report provides an analysis of the forecasted booking curve for an upcoming flight,
  detailing expected demand and commercial implications until departure.\n\n",
        "Actual seat bookings over time are:\n",
        paste(forecast_summary %>% filter(.key == "actual") %>% arrange(.index) %>% pull(.value), collapse = ";"),
        "and, the model forecasted seat bookings over time are:\n",
        paste(forecast_summary %>% filter(.key == "prediction") %>% arrange(.index) %>% pull(.value), collapse = ";"),
        "and their, corresponding dates are:\n",
        paste(forecast_summary %>% filter(.key == "prediction") %>% arrange(.index) %>% pull(.index), collapse = ";"),
        " respectively.",
        "\n\nThe flight has a target capacity of ", target_cap, " seats.",
        "This analysis focuses on the",
        input$route, "route, which has the following historical booking trends:\n\n",
        "- **Days Before Departure:** ",
        paste(
          historical_summary %>%
            filter(Origin_Destination == input$route) %>%
            arrange(`Days Before Departure`) %>%
            pull(`Days Before Departure`),
          collapse = ", "
        ),
        "\n- **Average Pickup Rate (Seats):** ",
        paste(
          historical_summary %>%
            filter(Origin_Destination == input$route) %>%
            arrange(`Days Before Departure`) %>%
            pull(AvgPickUp),
          collapse = ", "
        ),
        "\n- **Daily Booking Rate (Percentage):** ",
        paste(
          historical_summary %>%
            filter(Origin_Destination == input$route) %>%
            arrange(`Days Before Departure`) %>%
            pull(DailyBookingRate),
          collapse = ", "
        ),
        "\n\n**Business Request:**",
        "Given this forecast and historical trends, please provide a strategic
        assessment of the booking trajectory, highlighting revenue opportunities
        and any recommended ideas (based on your in depth knowledge of
        aviation sector) to stimulate demand if it falls short.
        Also do mention the approximate date (format Month, day, year) when the
        bookings begin to pick up closer to departure date. Jump straight to the
        facts and avoid making text bold. Return paragarph without bullet
        points. Avoid using the word I and say 'we' instead."
      )

      # Send query to OpenAI
      response <- openai::create_chat_completion(
        model = "gpt-4",
        messages = list(
          list(
            role = "system",
            content = "You are an expert an expert in airline revenue management
            and commercial strategy."
          ),
          list(role = "user", content = query)
        )
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
    forecast_plot <- calib_tbl %>%
      modeltime_refit(data = train) %>%
      modeltime_forecast(
        new_data = future_data,
        actual_data = train
      ) %>%
      mutate(
        .value = round(.value),
        .conf_lo = round(.conf_lo),
        .conf_hi = round(.conf_hi)
      ) %>%
      plot_modeltime_forecast(
        .x_lab = "Date Before Departure",
        .y_lab = "Seats Sold",
        .title = paste(
          "Booking Curve for", route, "on", dep_date,
          paste("[Target =", target_cap, " seats; Train Obs = ", nrow(train),
            "; ", "Predict days = ", days_ahead, "]",
            sep = ""
          )
        )
      )

    # Convert GGPlot to Interactive Plotly Graph
    ggplotly(forecast_plot) %>%
      layout(
        shapes = list(
          list(
            type = "line",
            x0 = test_start_date, x1 = test_start_date,
            y0 = 0, y1 = max(train$`Seats Sold`, na.rm = TRUE),
            line = list(color = "red", width = 2, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = test_start_date, y = max(train$`Seats Sold`, na.rm = TRUE),
            text = "Test Start Date",
            showarrow = TRUE,
            arrowhead = 2,
            ax = 20, ay = -40
          )
        )
      )
  })
}
