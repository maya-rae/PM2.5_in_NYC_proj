## Animated Map of PM 2.5 concentrations across neighborhoods
## Brooklyn, Manhattan, & The Bronx
## Maya Arnott

#importing libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(shiny)
library(leaflet)
library(dplyr)
library(scales)
library(rsconnect)

# listing all csv files
files <- list.files(
  "data/", pattern = "*.csv", full.names = TRUE)

# combine the files
all_data <- files |> 
  map_dfr(read_csv) |> 
  janitor::clean_names()

#selecting key columns
all_data<- all_data |> 
  select(datetime_local, location_id, 
         latitude, longitude, 
         location_name, parameter, value)

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

# Creating a spatiotemporal visualization

# ensure that hour is numeric
pm25_hourly <- pm25_hourly |> 
  mutate(hour = as.integer(hour))

# color palette
pal <- colorNumeric(
  palette = "viridis",
  domain = pm25_hourly$pm25_value,
  na.color = "transparent"
)

# Defining UI

ui <- fluidPage(
  titlePanel("PM 2.5 Hourly Animation"),
  sliderInput(
              inputId = "hour",
              label = "Hour of Day", 
              min = min(pm25_hourly$hour),
              max = max(pm25_hourly$hour),
              value = min(pm25_hourly$hour),
              step = 1,
              animate = animationOptions(interval = 1000,
                                         loop = TRUE)
  ),
  leafletOutput("pm25_map", height = 600)
)


# Defining server

server <- function(input, output, session) {
  
  # initialize map with all the hour layers
  output$pm25_map <- renderLeaflet({
    leaflet(pm25_hourly) |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~rescale(pm25_value, to = c(4, 12)),
        color = ~pal(pm25_value),
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~paste0(name, "<br>PM2.5: ", 
                       round(pm25_value, 1),
                       " µg/m³<br>Hour: ", hour),
        group = ~paste0("hour_", hour),
      ) |>
    addLegend(
      "bottomright",
      pal = pal,
      values = ~pm25_value,
      title = "PM2.5 (µg/m³)",
      opacity = 1
    ) |> 
      # hide all layers initially
      hideGroup(paste0("hour_", unique(pm25_hourly$hour)))
})

# observe slider and show the selected hour only
observe({
  selected_hour <- input$hour
  # hide all layers first
  leafletProxy("pm25_map") |> 
    hideGroup(paste0("hour_", unique(pm25_hourly$hour))) |> 
  # show only selected hour
    showGroup(paste0("hour_", selected_hour))
  })
}

# Running shiny

shinyApp(ui, server)


