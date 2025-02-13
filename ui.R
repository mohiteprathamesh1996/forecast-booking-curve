# --- Define User Interface (UI) for Shiny App ---
ui <- fluidPage(

  # --- Load Custom Fonts & Styles ---
  tags$head(
    tags$style(
    HTML("
    /* Styling for the slider */
    .irs-bar {
      background: green !important;  /* Changed to match Emirates red */
      border-top: 1px solid black !important;
      border-bottom: 1px solid black !important;
    }

    .irs-line {
      background: #f5f5f5 !important;
    }

    .irs-handle {
      border: 2px solid black !important; /* Keeps black border */
      background: #D71920 !important; /* Inside remains red */
      border-radius: 50% !important; /* Ensure circular shape */
    }

    .irs-single, .irs-from, .irs-to {
      background: black !important;
      color: white !important;
      font-weight: bold;
    }

    .irs-handle:hover {
      background: black !important;
      border: 2px solid black !important;
    }

    /* Hourglass Loader Animation */
    @keyframes rotateHourglass {
      0% { transform: rotate(0deg); }
      50% { transform: rotate(180deg); }
      100% { transform: rotate(360deg); }
    }

    .hourglass {
      font-size: 50px;
      color: #D71920;
      animation: rotateHourglass 2s linear infinite;
      display: none; /* Initially hidden */
      text-align: center;
      margin-top: 20px;
    }

    /* Sticky Sidebar */
    .sticky-sidebar {
      position: fixed !important;
      top: 10px !important;
      left: 10px !important;
      width: 400px !important; /* Match original sidebar width */
      height: 100vh !important;
      overflow-y: auto !important;
      background: white !important;
      padding: 15px !important;
      border-radius: 10px !important;
      box-shadow: 2px 2px 10px rgba(0,0,0,0.1) !important;
      z-index: 1000 !important;
    }

    /* Main panel content to prevent overlap with fixed sidebar */
    .main-panel {
      margin-left: 270px !important;
      padding: 20px;
    }
  "
         )
    ),
    
    tags$link(
      href = "
      https://fonts.googleapis.com/css2?family=Oswald:wght@300;400;500;700&display=swap
      ",
      rel = "stylesheet"
    ),
    
    tags$link(rel = "stylesheet", type = "text/css")
    ),

  # --- Title Panel ---
  div(
    h1(
      "Dynamic Booking Curve Insights",
      style = "
      font-family: 'Oswald', sans-serif;
      font-weight: 700;
      color: #D71920;
      text-align: center;
      "
    ),
    style = "
    padding: 20px;
    background: linear-gradient(to right, #E7E3D4, white);
    border-radius: 10px;
    "
  ),

  # --- Layout Structure: Sidebar + Main Panel ---
  sidebarLayout(

    # --- Sidebar Panel (User Inputs) ---
    sidebarPanel(
      width = 3, # Slightly wider for better spacing
      class = "sticky-sidebar",
      style = "
      background-color: #FAFAFA;
      padding: 15px;
      border-radius: 10px;
      box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      ",

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
      
      sliderInput(
        "top_n_train",
        "Most Recent Training Window (%):",
        min = 0,
        max = 100,
        value = 100,
        step = 1
      ),
      
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
        min = 0,
        max = 7,
        value = 1,
        step = 1
      ),

      # --- Add Apply Filters Button ---
      actionButton(
        "apply_filters",
        "Apply Filters",
        class = "btn-primary",
        style = "
        width:100%;
        background-color:#D71920;
        border: 2px solid black !important;
        font-size:16px;
        font-weight:600;
        padding:10px;
        margin-top:15px;
        "
      )
    ),

    # --- Main Panel (Output Display) ---
    mainPanel(
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
      uiOutput("ai_insights"),

      # --- Model Accuracy Metrics Table ---
      h3(
        "Model Performance Metrics",
        style = "
        font-family: 'Oswald', sans-serif;
        font-weight: 600;
        text-align: center;
        color: #333;
        "
      ),
      DTOutput("accuracy_table")
    )
  )
)
