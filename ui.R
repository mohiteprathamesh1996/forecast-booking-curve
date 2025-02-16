ui <- fluidPage(
  
  # --- Load Custom Fonts & Styles ---
  tags$head(
    tags$style(
      HTML("
    /* Styling for the slider */
    .irs-bar {
      background: green !important;
      border-top: 1px solid black !important;
      border-bottom: 1px solid black !important;
    }

    .irs-line {
      background: #f5f5f5 !important;
    }

    .irs-handle {
      border: 2px solid black !important;
      background: #D71920 !important;
      border-radius: 50% !important;
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

    /* Sticky Sidebar - Becomes fixed on larger screens but adapts on smaller ones */
    .sticky-sidebar {
      position: sticky;
      top: 10px;
      background-color: #FAF6F0;
      padding: 15px;
      border-radius: 10px;
      box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      z-index: 1000;
      width: 100%; /* Adjust width dynamically */
      max-width: 400px;
    }

    /* Responsive Sidebar Behavior */
    @media (max-width: 1024px) {
      .sticky-sidebar {
        position: relative !important; 
        width: 100% !important;
        max-width: 100% !important;
      }
    }

    /* Main Panel Adjustments */
    .main-panel {
      width: 100%;
    }

    /* Responsive Adjustments */
    @media (max-width: 768px) {
      .main-panel {
        padding: 10px;
      }
    }
  ")
    ),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Oswald:wght@300;400;500;700&display=swap",
      rel = "stylesheet"
    ),
    tags$link(rel = "stylesheet", type = "text/css")
  ),
  
  # --- Title Panel ---
  fluidRow(
    column(
      width = 12,
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
        text-align: center;
        "
      )
    )
  ),
  
  # --- Sidebar & Main Panel Layout ---
  fluidRow(
    
    # --- Sidebar Panel (User Inputs) ---
    column(
      width = 4,  # Sidebar takes 4 columns (1/3 of screen)
      class = "sticky-sidebar",
      div(
        selectInput("dep_date", "Select Departure Date:", choices = departure_dates),
        selectInput("route", "Select Sector:", choices = routes),
        sliderInput("top_n_train", "Most Recent Training Window (%):", min = 0, max = 100, value = 100, step = 1),
        sliderInput("test_slice", "Set Testing Slice (%):", min = 0, max = 100, value = 20, step = 1),
        sliderInput("proph_changepoint_num", "Set Prophet Changepoints:", min = 0, max = 7, value = 1, step = 1),
        actionButton("apply_filters", "Apply Filters", class = "btn-primary", 
                     style = "width:100%; background-color:#D71920; border: 2px solid black; font-size:16px; font-weight:600; padding:10px; margin-top:15px;")
      )
    ),
    
    # --- Main Panel (Output Display) ---
    column(
      width = 8,  # Main panel takes 8 columns (2/3 of screen)
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