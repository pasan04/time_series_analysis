library(tidyverse)
library(readr)
library(lubridate)
library(plotly)

# Read and process data
process_temperature_data <- function() {
  # Read raw data
  data <- read_csv("GlobalTemperatures.csv")
  
  # Filter data since 1850
  since_1850 <- function(df) {
    df %>% 
      mutate(Year = year(dt)) %>% 
      select(Year, everything()) %>% 
      filter(Year >= 1850)
  }
  
  # Calculate land average temperature by year
  land_average_temp_by_year <- function(df) {
    df %>% 
      group_by(Year) %>% 
      summarize(
        LandAverageTemp = mean(LandAverageTemperature, na.rm = TRUE),
        LandAverageTempUncertainty = mean(LandAverageTemperatureUncertainty, na.rm = TRUE)
      )
  }
  
  # Calculate land and ocean average temperature by year
  land_and_ocean_average_temp_by_year <- function(df) {
    df %>% 
      group_by(Year) %>% 
      summarize(
        LandAndOceanAverageTemp = mean(as.numeric(LandAndOceanAverageTemperature), na.rm = TRUE),
        LandAndOceanAverageTempUncertainty = mean(as.numeric(LandAndOceanAverageTemperatureUncertainty), na.rm = TRUE)
  }
  
  # Process data
  land_data <- data %>% since_1850() %>% land_average_temp_by_year()
  land_ocean_data <- data %>% since_1850() %>% land_and_ocean_average_temp_by_year()
  
  # Write to CSV
  write_csv(land_data, 'landtemperature.csv')
  write_csv(land_ocean_data, 'landandoceantemperature.csv')
  
  return(list(land = land_data, land_ocean = land_ocean_data))
}

# Execute the processing
processed_data <- process_temperature_data()