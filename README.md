# Dynamic Booking Curve Forecasting
### A Shiny Application for Forecasting Airline Booking Trends

---

## Overview
This repository contains an **R Shiny** application designed to analyze and forecast airline **booking curves** dynamically. It leverages **time-series forecasting models (ARIMA, Prophet, Modeltime)** to predict seat sales and provides **AI-driven revenue insights** using **OpenAI's API**.

The application allows users to:
- **Visualize historical booking trends** for specific routes  
- **Analyze demand patterns** using interactive plots  
- **Forecast future bookings** with machine learning models  
- **Assess model performance** with accuracy metrics  
- **Generate AI-based commercial insights** for revenue management  

---

## Features
- **Dynamic Filtering**: Select departure dates and routes for analysis.
- **Historical Data Insights**: Visualizes past booking trends.
- **Forecasting Models**: ARIMA, Prophet (with and without regressors).
- **Interactive Visualizations**: Powered by `ggplot2` & `plotly`.
- **AI-Generated Insights**: OpenAI integration for strategic recommendations.
- **Model Performance Metrics**: Accuracy assessment using `modeltime`.

---

## Technologies Used

| Technology   | Purpose |
|-------------|---------|
| `R Shiny` | Web framework for interactive UI |
| `tidyverse` | Data manipulation & visualization |
| `ggplot2` | Static plots for historical data |
| `plotly` | Interactive forecasting plots |
| `prophet` | Advanced time-series forecasting |
| `modeltime` | Automated machine learning for forecasting |
| `lubridate` | Handling and manipulating date-time data |
| `shinyapps.io` | Hosting and deploying the Shiny app |
| `OpenAI API` | AI-generated revenue insights |

---

## Project Structure
📦 Project Root │-- 📁 data/ │ ├── dataset.csv # Historical booking data │ ├── output.csv # Flights requiring forecasting │-- 📁 R/ │ ├── server.R # Backend logic (data filtering, forecasting models, AI insights) │ ├── ui.R # User interface layout │ ├── global.R # Load datasets and preprocess data │-- 📁 www/ │ ├── style.css # Custom styles (if any) │-- .github/workflows/ │ ├── deploy.yml # GitHub Actions for automatic deployment │-- 📄 README.md # Project documentation