# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Booking Curve Insights"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        "dep_date",
        "Select Departure Date:",
        choices = departure_dates
      ),
      selectInput("route", "Select Sector:", choices = routes),
      sliderInput(
        "test_slice",
        "Set Testing Slice (%):",
        min = 0,
        max = 100,
        value = 20,
        step = 1
      ),
      sliderInput(
        "proph_changepoint_num",
        "Set Prophet Changepoints:",
        min = 1,
        max = 7,
        value = 1,
        step = 1
      )
    ),
    mainPanel(
      # h3("Prediction Plot"),
      div(plotlyOutput("forecast_plot"), style = "width: 90%; margin: auto;"),
      
      uiOutput("historical_title"),
      plotOutput("historical_plots"),
      
      h3("Model Performance Metrics"),
      DTOutput("accuracy_table")
    )
  )
)