server <- function(input, output) {
  # --- Reactive Expression for Filtering ---
  filtered_inputs <- eventReactive(input$apply_filters, {
    req(input$dep_date, input$route) # Ensure inputs are selected
    list(dep_date = as.character(input$dep_date), route = input$route)
  })

  # --- Dynamic UI: Historical Trends Title ---
  output$historical_title <- renderUI({
    req(filtered_inputs()) # Wait for button press

    h3(
      paste(
        "Historical Trends Along",
        filtered_inputs()$route,
        "Sector on",
        ifelse(
          wday(filtered_inputs()$dep_date, label = TRUE, abbr = FALSE) %in% weekend_definition,
          "Weekend Departures",
          "Weekday Departures"
        )
      )
    )
  })

  # --- Generate Historical Trend Plots ---
  output$historical_plots <- renderPlot({
    req(filtered_inputs()) # Wait for button press

    grid.arrange(
      historical_summary_weekend %>%
        filter(
          Origin_Destination == filtered_inputs()$route &
            WeekendDeparture == ifelse(
              wday(filtered_inputs()$dep_date, label = TRUE, abbr = FALSE) %in% weekend_definition,
              1, 0
            )
        ) %>%
        mutate(`Days Before Departure` = -1 * `Days Before Departure`) %>%
        ggplot(aes(x = `Days Before Departure`, y = AvgPickUp)) +
        geom_line(na.rm = TRUE, color = "#D71920", linewidth = 0.8) +
        theme_bw(),
      historical_summary_weekend %>%
        filter(
          Origin_Destination == filtered_inputs()$route &
            WeekendDeparture == ifelse(
              wday(filtered_inputs()$dep_date, label = TRUE, abbr = FALSE) %in% weekend_definition,
              1, 0
            )
        ) %>%
        mutate(`Days Before Departure` = -1 * `Days Before Departure`) %>%
        ggplot(aes(x = `Days Before Departure`, y = DailyBookingRate)) +
        geom_line(na.rm = TRUE, color = "darkgreen", linewidth = 0.8) +
        theme_bw()
    )
  })

  # --- Forecasting Model for Seat Sales ---
  output$forecast_plot <- renderPlotly({
    req(filtered_inputs()) # Wait for button press

    walk_forward_results_summary <- nested_forecasts_insights[[
      paste(as.Date(filtered_inputs()$dep_date), filtered_inputs()$route, sep = "__")
    ]]$walk_forward_results_summary

    # --- Display Model Accuracy in a Table ---
    output$accuracy_table <- renderDT({
      req(filtered_inputs())
      walk_forward_results_summary
    })

    # --- Generate AI Insights ---
    output$ai_insights <- renderUI({
      req(filtered_inputs())

      insights <- queries_df %>%
        filter(id == paste(as.Date(filtered_inputs()$dep_date), filtered_inputs()$route, sep = "__")) %>%
        pull(analysis)

      HTML(paste("<p>", insights, "</p>", sep = ""))
    })

    # --- Generate Forecast Plot ---
    forecast_plot <- nested_forecasts_insights[[
      paste(as.Date(filtered_inputs()$dep_date), filtered_inputs()$route, sep = "__")
    ]]$dynamic_plot

    ggplotly(forecast_plot)
  })
}
