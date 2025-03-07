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

    walk_forward_results_summary <- nested_forecasts_insights[[
      paste(as.Date(input$dep_date), input$route, sep = "__")
    ]]$walk_forward_results_summary

    # --- Display Model Accuracy in a Table ---
    output$accuracy_table <- renderDT({
      walk_forward_results_summary
    })

    # --- Generate AI Insights (OpenAI API Call) ---
    output$ai_insights <- renderUI({
      # Extract AI-generated insights
      insights <- queries_df %>%
        filter(id == paste(as.Date(input$dep_date), input$route, sep = "__")) %>%
        pull(analysis)

      formatted_insights <- paste(
        "<p>", insights, "</p>",
        sep = ""
      )

      HTML(formatted_insights)
    })

    # --- Generate Forecast Plot ---
    forecast_plot <- nested_forecasts_insights[[
      paste(as.Date(input$dep_date), input$route, sep = "__")
    ]]$dynamic_plot


    # Convert GGPlot to Interactive Plotly Graph
    ggplotly(forecast_plot)
  })
}
