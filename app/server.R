library(shiny)
library(tidyverse)
library(forecast)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(plotly)

# Define server logic
shinyServer(function(input, output) {
  
  # Load data and convert to tsibble
  landtemperature <- read_csv('landtemperature.csv') %>% 
    as_tsibble(index = Year)
  
  landandoceantemperature <- read_csv('landandoceantemperature.csv') %>% 
    as_tsibble(index = Year)
  
  # Reactive data filter
  filtered_data <- reactive({
    req(input$year_min, input$year_max)
    
    if(input$dataset == "Global Average Land Temperature") {
      landtemperature %>% 
        filter(Year >= input$year_min, Year <= input$year_max)
    } else {
      landandoceantemperature %>% 
        filter(Year >= input$year_min, Year <= input$year_max)
    }
  })
  
  # Main temperature plot
  output$plot <- renderPlotly({
    data <- filtered_data()
    y_var <- ifelse(input$dataset == "Global Average Land Temperature", 
                    "LandAverageTemp", "LandAndOceanAverageTemp")
    
    p <- ggplot(data, aes_string(x = "Year", y = y_var)) +
      geom_point(color = "#1E90FF") +
      geom_line(color = "#1E90FF") +
      ylab('Temperature (°C)') +
      ggtitle(input$dataset) +
      theme(
        plot.background = element_rect(fill = "#111111"),
        panel.background = element_rect(fill = "#111111"),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_line(color = "#222222"),
        text = element_text(color = "#FFFFFF"),
        axis.text = element_text(color = "#CCCCCC"),
        title = element_text(color = "#FFFFFF")
      )
    
    if(input$lin) {
      p <- p + geom_smooth(color = 'blue', method = 'lm', se = FALSE)
    }
    
    ggplotly(p) %>% 
      layout(plot_bgcolor = "#111111",
             paper_bgcolor = "#111111",
             font = list(color = "#FFFFFF"))
  })
  
  # Forecast plot
  # Forecast plot
  output$forecast_plot <- renderPlotly({
    req(input$model_type != "None", input$run_forecast > 0)
    
    isolate({
      data <- filtered_data()
      y_var <- ifelse(input$dataset == "Global Average Land Temperature", 
                      "LandAverageTemp", "LandAndOceanAverageTemp")
      
      ts_data <- ts(data[[y_var]], start = min(data$Year), frequency = 1)
      
      # Fit model
      if(input$model_type == "ARIMA") {
        fit <- auto.arima(ts_data, seasonal = FALSE)
      } else {
        fit <- auto.arima(ts_data, seasonal = TRUE)
      }
      
      # Generate forecast
      fc <- forecast(fit, h = input$forecast_years)
      
      # Create a custom forecast plot
      p <- ggplot() +
        geom_line(aes(x = time(ts_data), y = ts_data), color = "#1E90FF") + # original data
        geom_line(aes(x = time(fc$mean), y = fc$mean), color = "#1E90FF", size = 1) + # forecast
        geom_ribbon(aes(x = time(fc$mean),
                        ymin = fc$lower[,2],
                        ymax = fc$upper[,2]),
                    fill = "#1E90FF", alpha = 0.2) +
        labs(title = paste(input$model_type, "Forecast"),
             x = "Year",
             y = "Temperature (°C)") +
        theme(
          plot.background = element_rect(fill = "#111111"),
          panel.background = element_rect(fill = "#111111"),
          panel.grid.major = element_line(color = "#333333"),
          panel.grid.minor = element_line(color = "#222222"),
          text = element_text(color = "#FFFFFF"),
          axis.text = element_text(color = "#CCCCCC"),
          title = element_text(color = "#FFFFFF")
        )
      
      ggplotly(p) %>% 
        layout(plot_bgcolor = "#111111",
               paper_bgcolor = "#111111",
               font = list(color = "#FFFFFF"))
    })
  })
  
  
  # Model summary
  output$model_summary <- renderPrint({
    req(input$model_type != "None", input$run_forecast > 0)
    
    isolate({
      data <- filtered_data()
      y_var <- ifelse(input$dataset == "Global Average Land Temperature", 
                      "LandAverageTemp", "LandAndOceanAverageTemp")
      ts_data <- ts(data[[y_var]], start = min(data$Year), frequency = 1)
      
      if(input$model_type == "ARIMA") {
        fit <- auto.arima(ts_data, seasonal = FALSE)
      } else {
        fit <- auto.arima(ts_data, seasonal = TRUE)
      }
      
      summary(fit)
    })
  })
  
  # Residual diagnostics
  output$diagnostics_plot <- renderPlot({
    req(input$model_type != "None", input$run_forecast > 0)
    
    isolate({
      data <- filtered_data()
      y_var <- ifelse(input$dataset == "Global Average Land Temperature", 
                      "LandAverageTemp", "LandAndOceanAverageTemp")
      ts_data <- ts(data[[y_var]], start = min(data$Year), frequency = 1)
      
      if(input$model_type == "ARIMA") {
        fit <- auto.arima(ts_data, seasonal = FALSE)
      } else {
        fit <- auto.arima(ts_data, seasonal = TRUE)
      }
      
      checkresiduals(fit)
    })
  })
})