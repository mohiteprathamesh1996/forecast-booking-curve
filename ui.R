ui <- fluidPage(

  # --- Load External HTML & CSS ---
  includeHTML("www/custom_ui.html"),
  includeCSS("www/styles.css"),

  # --- Sidebar & Main Panel Layout ---
  fluidRow(

    # --- Sidebar Panel (User Inputs) ---
    column(
      width = 4, # Sidebar takes 4 columns
      class = "sticky-sidebar",
      div(
        selectInput(
          "dep_date",
          "Select Departure Date:",
          choices = departure_dates
        ),
        selectInput(
          "route",
          "Select Sector:",
          choices = routes
        ),
        actionButton(
          "apply_filters",
          "Apply Filters",
          class = "btn-primary",
          style = "
          width:100%;
          background-color:#D71920;
          border: 2px solid black;
          font-size:16px;
          font-weight:600;
          padding:10px;
          margin-top:15px;
          "
        )
      )
    ),

    # --- Main Panel with Tabs ---
    column(
      width = 8, # Main panel takes 8 columns
      tabsetPanel(

        # --- First Tab: Forecast & Historical Analysis ---
        tabPanel(
          "Forecasting Dashboard",
          div(
            plotlyOutput("forecast_plot", width = "100%"),
            style = "
            padding: 10px;
            background-color: #fff;
            border-radius: 10px;
            box-shadow: 3px 3px 15px rgba(0,0,0,0.1);
            "
          ),

          # --- Historical Trends Title ---
          div(
            uiOutput("historical_title"),
            style = "
            font-family: 'Oswald', sans-serif;
            font-weight: 600;
            font-size: 20px;
            color: #333;
            text-align: center;
            padding-top: 15px;
            "
          ),

          # --- Historical Trend Plots ---
          plotOutput("historical_plots"),

          # --- AI-Generated Insights Section ---
          h3(
            "Analysis of Projections",
            style = "
            font-family: 'Oswald', sans-serif;
            font-weight: 600;
            text-align: center;
            color: #333;
            "
          ),
          div(
            uiOutput("ai_insights"),
            style = "
            font-family: 'Playfair Display', serif;
            font-size: 18px;
            font-weight: 500;
            color: #333;
            text-align: justify;
            padding: 15px;
            "
          ),

          # --- Model Accuracy Metrics Table ---
          h3(
            "Model Performance Metrics (Walk-Forward Validation)",
            style = "
            font-family: 'Oswald', sans-serif;
            font-weight: 600;
            text-align: center;
            color: #333;
            "
          ),
          DTOutput("accuracy_table")
        ),

        # --- Second Tab: Additional Analysis ---
        tabPanel(
          "Additional Analysis",
          plotlyOutput("additional_plot", height = "900px")
        )
      )
    )
  )
)
