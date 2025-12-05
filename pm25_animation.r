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
library(sf)

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

# rename 'hour' column for readability in code

pm25_hourly <- pm25_hourly |> 
  rename(hour_of_day = hour)


# Adding traffic counts

traffic_sf <- nyc_traffic |> 
  mutate(geometry = st_as_sfc(wkt_geom)) |> 
  st_as_sf(crs = 2263) |>             # assign NY State Plane (ft)
  st_transform(4326)                  # convert to lat/long

# extract coordinates cleanly
coords <- st_coordinates(traffic_sf)
traffic_sf$longitude <- coords[, 1]
traffic_sf$latitude  <- coords[, 2]

# Creating a spatiotemporal visualization

# ensure that hour is numeric
pm25_hourly <- pm25_hourly |> 
  mutate(hour_of_day = as.integer(hour_of_day))

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
              min = min(pm25_hourly$hour_of_day),
              max = max(pm25_hourly$hour_of_day),
              value = min(pm25_hourly$hour_of_day),
              step = 1,
              animate = animationOptions(interval = 1000,
                                         loop = TRUE)
  ),
  leafletOutput("pm25_map", height = 600)
)

# Defining server

server <- function(input, output, session) {
  
  # make sure traffic_counts exists and has lat/lon
  if (!exists("traffic_sf")) {
    stop("traffic_sf data frame not found in the global environment")
  }
  
  output$pm25_map <- renderLeaflet({
    leaflet(pm25_hourly) |> 
      addProviderTiles(providers$CartoDB.Positron) |>
      
      # PM2.5 markers by hour (one group per hour)
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~rescale(pm25_value, to = c(4, 12)),
        color = ~pal(pm25_value),
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~paste0(name, "<br>PM2.5: ", round(pm25_value, 1),
                        " µg/m³<br>Hour: ", hour_of_day),
        group = ~paste0("hour_", hour_of_day)
      ) |>
      
      # Traffic count points as a separate layer
      addCircleMarkers(
        data = traffic_sf,
        lng = ~longitude, lat = ~latitude,
        radius = 4,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.7,
        label = ~paste0("Traffic Site<br>Count: ", vol),
        group = "Traffic Counts"
      ) |>
      
      # Legend for PM2.5
      addLegend(
        "bottomright",
        pal = pal,
        values = pm25_hourly$pm25_value,
        title = "PM2.5 (µg/m³)",
        opacity = 1
      ) |>
      
      # Layers control so users can toggle traffic + hour groups
      addLayersControl(
        overlayGroups = c(paste0("hour_", sort(unique(pm25_hourly$hour_of_day))),
                          "Traffic Counts"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      
      # hide all PM2.5 hour groups initially
      hideGroup(paste0("hour_", sort(unique(pm25_hourly$hour_of_day))))
  })
  
  # observe slider and show the selected hour only
  observe({
    selected_hour <- input$hour
    leafletProxy("pm25_map") |> 
      hideGroup(paste0("hour_", sort(unique(pm25_hourly$hour_of_day)))) |> 
      showGroup(paste0("hour_", selected_hour))
  })
}


# Running shiny

shinyApp(ui, server)



