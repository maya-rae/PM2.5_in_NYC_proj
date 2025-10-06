## Heatmap of PM 2.5 concentrations acros neighborhoods
## Brooklyn, Manhattan, & The Bronx
## Maya Arnott

# importing libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# listing all csv files
files <- list.files(
  "data/", pattern = "*.csv", full.names = TRUE)

# combine the files
all_data <- files |> 
  map_dfr(read_csv) |> 
  janitor::clean_names()

# ensuring date time is in proper format
all_data <- all_data |> 
  mutate(datetime = as.POSIXct(datetime_local, tz = "America/New_York"),
         date = as.Date(datetime),
         hour = hour(datetime))

#selecting key columns
all_data<- all_data |> 
  select(datetime_local, location_id, location_name, parameter, value)

# filter for pm2.5 data
nyc_pm25_data <- all_data |> 
  filter(parameter == "pm25") |> 
  mutate(pm25 = value) |>     # creates column called pm25
  select(-parameter, -value) |>  # removes the old columns
  rename(
    id = location_id,
    name = location_name)

