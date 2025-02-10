# --- Define User Interface (UI) for Shiny App ---
ui <- fluidPage(

  # --- Title Panel ---
  # Displays the application title prominently at the top.
  titlePanel("Dynamic Booking Curve Insights"),

  # --- Layout Structure: Sidebar + Main Panel ---
  sidebarLayout(

    # --- Sidebar Panel (User Inputs) ---
    sidebarPanel(
      width = 2, # Set sidebar width for a compact layout

      # --- Dropdown: Select Departure Date ---
      # Allows users to choose a date for forecasting.
      selectInput(
        "dep_date",
        "Select Departure Date:",
        choices = departure_dates
      ),

      # --- Dropdown: Select Flight Route ---
      # Users select a flight sector (Origin-Destination).
      selectInput(
        "route",
        "Select Sector:",
        choices = routes
      ),

      # --- Slider: Training Window Size ---
      # Controls how much historical data is used for training (in percentage).
      sliderInput(
        "top_n_train",
        "Most Recent Training Window (%):",
        min = 0,
        max = 100,
        value = 100, # Default to 100% of available data
        step = 1
      ),

      # --- Slider: Testing Data Slice ---
      # Defines the proportion of data reserved for testing the model.
      sliderInput(
        "test_slice",
        "Set Testing Slice (%):",
        min = 0,
        max = 100,
        value = 20, # Default 20% for testing
        step = 1
      ),

      # --- Slider: Prophet Changepoint Configuration ---
      # Adjusts the number of changepoints in the Prophet model (affects trend flexibility).
      sliderInput(
        "proph_changepoint_num",
        "Set Prophet Changepoints:",
        min = 0,
        max = 7,
        value = 1, # Default to 1 changepoint for a balanced trend fit
        step = 1
      )
    ),

    # --- Main Panel (Output Display) ---
    mainPanel(

      # --- Forecast Plot ---
      # Displays the forecasted booking curve (Interactive Plotly).
      # div(plotlyOutput("forecast_plot"), style = "width: 90%; margin: auto;"),
      div(
        plotlyOutput(
          "forecast_plot", 
          width = "1550px", 
          # height = "600px"
          ), 
        style = "width: 95%; margin: auto;"
        ),

      # --- Historical Trends Title ---
      # Dynamically updates based on the selected flight sector.
      uiOutput("historical_title"),

      # --- Historical Trend Plots ---
      # Shows past trends for bookings and pickup rates.
      plotOutput("historical_plots"),

      # --- AI-Generated Insights Section ---
      # This section provides AI-driven revenue management insights.
      h3("Analysis of Trajectory"),
      uiOutput("ai_insights"),

      # --- Model Accuracy Metrics Table ---
      # Displays the predictive model’s performance evaluation.
      h3("Model Performance Metrics"),
      DTOutput("accuracy_table")
    )
  )
)
