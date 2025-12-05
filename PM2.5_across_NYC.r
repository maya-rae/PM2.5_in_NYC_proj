## Heatmap of PM 2.5 concentrations across neighborhoods
## Brooklyn, Manhattan, & The Bronx
## Maya Arnott

# account for traffic counts + bad weather days
# importing libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras2)

# listing all csv files
files <- list.files(
  "data/", pattern = "*.csv", full.names = TRUE)

# combine the files
all_data <- files |> 
  map_dfr(read_csv) |> 
  janitor::clean_names()

# selecting key columns
all_data <- all_data |> 
  select(datetime_local, location_id, latitude, longitude, location_name, parameter, value)

# ensuring date time is in proper format
all_data <- all_data |> 
  mutate(datetime = as.POSIXct(datetime_local, tz = "America/New_York"),
         date = as.Date(datetime),
         hour = hour(datetime))

# filter for pm2.5 data
nyc_pm25_data <- all_data |> 
  filter(parameter == "pm25") |> 
  mutate(pm25 = value) |>     # creates column called pm25
  select(-parameter, -value) |>  # removes the old columns
  rename(
    id = location_id,
    name = location_name)

# aggregate PM2.5 by hour and neighborhood
pm25_hourly <- nyc_pm25_data |>
  mutate(
    datetime = as.POSIXct(datetime_local, tz = "America/New_York"),
    date = as.Date(datetime),
    hour = as.numeric(format(datetime, "%H"))
  ) |> 
  group_by(latitude, longitude, name, date, hour) |>
  summarize(pm25_value = mean(pm25, na.rm = TRUE), .groups = "drop") |>
  mutate(name = recode(name,
                       "Morrisania" = "Morrisania, Bronx",
                       "Manhattan/IS143" = "Washington Heights, Man", 
                       "E Houston St between Clinton St & Attorney St" = "Lower East Side, Man",
                       "CCNY" = "West Harlem, Man",
                       "Bronx - IS74" = "Hunts Point, Bronx", 
                       "Bronx - IS52" =  "Longwood, Bronx",
                       "Bklyn - PS274" = "Bushwick, Bklyn",
                       "Bklyn - PS 314" = "Bay Ridge, Bklyn",
                       "7th Ave and W 16th St" =  "Chelsea, Man",
                       "DropHome" = "Lincoln Square, Man"))
            
# creating a heatmap of pm2.5 for each day
heatmap_by_date <- 
  ggplot(pm25_hourly, aes(
  x = hour, 
  y = fct_reorder(name, pm25_value, .fun = max), 
    fill = pm25_value
  )) +
  geom_tile(color = "white") + 
    scale_fill_viridis_c(option = "inferno", name = "PM2.5 (µg/m³)") + 
    labs(
      title = "Average Hourly PM2.5 Across NYC Neighborhoods",
      x = "Hour of Day",
      y = "Neighborhood"
    ) +
    facet_wrap(~date) + 
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8) 
    ) + 
    annotate("rect", 
             xmin = 7, xmax = 9, 
             ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "blue") +
    annotate("rect", 
             xmin = 17, xmax = 19, 
             ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "blue")

ggsave("Heatmap_by_date.pdf", plot = heatmap_by_date,
       width = 11, 
       height = 12)

# looking more at pm25 by location
pm25_hourly |> 
  ggplot(aes(
    x = latitude, y = longitude, color = pm25_value)) +
  geom_point()


# creating a heatmap on average hourly pm2.5
heatmap_hourly <- 
  ggplot(pm25_hourly, aes(x = hour, y = name, fill = pm25_value)) + 
  geom_tile(color = "white") + 
  scale_fill_viridis_c(option = "inferno", name = "PM2.5 (µg/m³)") + 
  labs(
    title = "Average Hourly PM2.5 Across NYC Neighborhoods",
    x = "Hour of Day",
    y = "Neighborhood"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

ggsave("Heatmap_by_hour.pdf", plot = heatmap_hourly,
       width = 8,
       height = 6)
