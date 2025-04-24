library(shiny)
library(plotly)  # This is crucial for plotlyOutput()

# Custom CSS for dark theme
dark_theme <- "
  body {
    background-color: #000000 !important;
    color: #FFFFFF !important;
  }
  .well {
    background-color: #222222 !important;
    border-color: #444444 !important;
  }
  .selectize-input {
    background-color: #333333 !important;
    color: #FFFFFF !important;
  }
  .selectize-dropdown {
    background-color: #333333 !important;
    color: #FFFFFF !important;
  }
  .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to {
    background: #007BFF !important;
    border-color: #007BFF !important;
  }
"

# UI Definition
ui <- fluidPage(
  tags$head(tags$style(HTML(dark_theme))),
  
  titlePanel(
    h1('Berkeley Earth Temperature Analysis', style = "color: #FFFFFF;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #222222; border-color: #444444;",
      
      selectInput(inputId = 'year_min', 
                  label = 'Starting Year', 
                  choices = 1850:2015,
                  selected = 1850),
      
      selectInput(inputId = 'year_max', 
                  label = 'Ending Year', 
                  choices = 1850:2015,
                  selected = 2015),
      
      selectInput(inputId = "dataset", 
                  label = "Data", 
                  choices = c("Global Average Land Temperature", 
                              "Global Average Land and Ocean Temperature")),
      
      checkboxInput(inputId = 'lin',
                    label = 'Show Best Fit Line',
                    value = FALSE),
      
      # New forecasting controls
      selectInput(inputId = "model_type",
                  label = "Forecast Model",
                  choices = c("None", "ARIMA", "SARIMA")),
      
      conditionalPanel(
        condition = "input.model_type != 'None'",
        sliderInput(inputId = "forecast_years",
                    label = "Years to Forecast",
                    min = 1, max = 30, value = 10),
        actionButton(inputId = "run_forecast",
                     label = "Run Forecast",
                     style = "background-color: #007BFF; color: #FFFFFF;")
      )
    ),
    
    mainPanel(
      style = "background-color: #111111;",
      tabsetPanel(
        tabPanel("Temperature Plot",
                 plotlyOutput("plot", height = "500px")),
        tabPanel("Forecast",
                 plotlyOutput("forecast_plot", height = "500px"),
                 verbatimTextOutput("model_summary"),
                 plotOutput("diagnostics_plot", height = "500px"))
      )
    )
  )
)