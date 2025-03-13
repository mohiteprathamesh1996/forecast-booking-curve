# ✈ **Dynamic Booking Curve Forecasting**  
### **A Shiny Application for Intelligent Airline Demand Prediction**  

---

## **🚀 Overview**  
This **R Shiny** application dynamically forecasts airline **booking curves**, leveraging advanced **time-series models (ARIMA, Prophet, and ML-based regressors)**. It provides **AI-driven revenue insights**, helping airlines optimize seat allocation and pricing strategies.  

<div style="display: flex; justify-content: center; align-items: center; gap: 20px;"> <img src="https://github.com/mohiteprathamesh1996/forecast-booking-curve/blob/mohiteprathamesh1996-patch-1/www/DXB-XXX.png" alt="Booking Curve Forecast" width="45%"> <img src="https://github.com/mohiteprathamesh1996/forecast-booking-curve/blob/mohiteprathamesh1996-patch-1/www/XXX-DXB.png" alt="Model Performance Comparison" width="45%"> </div>

### **🔍 Key Capabilities**  
✔ **Visualize Historical Booking Trends** with interactive graphs.  
✔ **Predict Seat Demand** using machine learning models.  
✔ **Analyze Market Dynamics** through data-driven insights.  
✔ **Optimize Revenue Strategies** with AI-generated commercial intelligence.  
✔ **Evaluate Model Performance** with real-time accuracy metrics.  

🔗 **Live Demo**: [Flight Booking Curve](https://prathameshmohite.shinyapps.io/flight-booking-curve/)  

---

## **✨ Features at a Glance**  

- 🛠 **Data-Driven Insights** – Explore demand trends with historical flight booking data.  
- 📊 **Multi-Model Forecasting** – ARIMA, Prophet, and machine learning-based regressors.  
- 📈 **Interactive Visualizations** – Built with `ggplot2`, `plotly`, and `shiny`.  
- 🤖 **AI-Powered Revenue Strategy** – OpenAI-driven commercial insights.  
- ⚡ **Fast & Scalable** – Optimized with `furrr` for parallel processing.  
- 🎯 **Robust Performance Metrics** – Evaluate forecasts with RMSE, MAE, and confidence intervals.  

---

## **🛠 Technologies Used**  

| **Technology**    | **Purpose**  |  
|------------------|-------------|  
| 🖥 `R Shiny`    | Interactive UI for real-time analytics  |  
| 📊 `ggplot2`    | Static data visualizations  |  
| 🎥 `plotly`     | Interactive forecasting plots  |  
| 🔮 `prophet`    | Advanced time-series forecasting  |  
| 📉 `modeltime`  | Machine learning for demand forecasting  |  
| 📅 `lubridate`  | Date-time manipulation & processing  |  
| 🚀 `furrr`      | Parallel computing for fast execution  |  
| 💾 `tidyverse`  | Data wrangling and transformation  |  
| 🌎 `shinyapps.io` | Hosting and deploying the Shiny app  |  
| 🤖 `OpenAI API` | AI-generated revenue insights  |  

---

## **📁 Project Structure**  

```plaintext
forecast-booking-curve (Root Directory)
├── data/
│   ├── dataset.csv                    # Historical booking data
│   ├── output.csv                     # Flights requiring forecasting
├── prompt
    ├── system_prompt.txt              # System Prompt (ARM)
│── www/                               # Static assets (CSS, JS, HTML, images)
│   │── custom_ui.html                 # External HTML content
│   │── styles.css                     # External CSS for styling
├── output
    ├── historical_summary_weekend.rds # Historical Summary Statistics of Weekend/Weekday Departures
    ├── nested_forecasts_insights.rds  # All Predictions
    ├── queries_df.rds                 # AI-driven insights of forecasts
├── server.R                           # Backend logic (data processing, forecasting, AI insights)
├── ui.R                               # Frontend interface for interactive analysis
├── global.R                           # Data preprocessing & global variables
├── Explore.Rmd                        # Notebook for model experimentation
├── .github/workflows/
│   ├── main.yml                       # GitHub Actions for automated deployment
├── README.md                          # Project documentation
```


## 📦 Installation & Setup
### 1️⃣ Clone the Repository
```
git clone https://github.com/mohiteprathamesh1996/forecast-booking-curve.git
cd forecast-booking-curve
```

### 2️⃣ Install Required R Packages
Open RStudio and run:
```r
install.packages(c(
  "shiny", "dplyr", "ggplot2", "plotly", "tidyverse",
  "forecast", "prophet", "modeltime", "lubridate",
  "timetk", "timeDate", "gridExtra", "DT", "furrr",
  "shinyjs", "shinyWidgets", "shinycssloaders"
))
```

### 3️⃣ Configure OpenAI API Key
To enable AI-generated revenue insights, you need to store your OpenAI API key in `.Renviron`:
1. In the R terminal run this command:
```r
usethis::edit_r_environ()
```

2. And then add the following line in the `.Renviron` file:
```r
OPENAI_API_KEY="your-api-key-here"
```
### 4️⃣️ Get all forecasts
This will execute the script, which likely contains the logic to generate the forecasts, update any existing forecast data, and store the results for later use.

```r
source(run_output.R)
```

### 5️⃣ Run the Application
Open app.R and run the following command:
```r
shiny::runApp()
```

For deployment:
```r
rsconnect::deployApp()
```

---
## 📜 License
This project is licensed under the MIT License.

## 👥 Contributors
Prathamesh Mohite – Data Scientist

## 📩 Support & Feedback
Have questions, feature requests, or feedback? Let’s connect!

📧 Email: mohite.p@northeastern.edu

🔗 [LinkedIn Profile](https://www.linkedin.com/in/prathameshmohite96/)
