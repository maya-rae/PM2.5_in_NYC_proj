## Bayesian inferences from PM 2.5 data taken from
## Lower East Side, Manhattan
## Maya Arnott 09/24/2025

## importing libraries
library(ggplot2)
library(dplyr)
library(brms)
library(tidyverse)
library(tidyr)


## importing PM 2.5 dataset
les_pm25_df = 
  read_csv("data/openaq_LES_PM2.5.csv")
les_pm25_df = 
  janitor::clean_names(les_pm25_df)

## importing RH dataset
les_rh_df = 
  read_csv("data/openaq_LES_RH.csv")
les_rh_df = 
  janitor::clean_names(les_rh_df)

## importing Temp dataset
les_temp_df = 
  read_csv("data/openaq_LES_Temp.csv")
les_temp_df = 
  janitor::clean_names(les_temp_df)

## Keeping only necessary columns
## select func allows me to select the columns i want to show

pm25_data <- les_pm25_df |> 
  select(datetime_local, location_id, location_name, parameter, value)

rh_data <- les_rh_df |> 
  select(datetime_local, location_id, location_name, parameter, value)

temp_data <- les_temp_df |> 
  select(datetime_local, location_id, location_name, parameter, value)

## Bind the rows in the 'parameter' column

all_data <- bind_rows(pm25_data, rh_data, temp_data)

## Identifying duplicates

duplicates <- all_data |> 
  group_by(datetime_local, location_id, location_name, parameter) |> 
  summarize(n = n(), .groups = "drop") |> 
  filter( n > 1 )

## Pivot wider so the vars are in separate columns
## Collapsed duplicates by averaging the 2 values

combined_data <- all_data |>
  group_by(datetime_local, location_id, location_name, parameter) |> 
  summarise(value = 
        mean(value, na.rm = TRUE), 
        .groups = "drop") |> 
  pivot_wider(names_from = parameter, values_from = value)

## Renaming columns

combined_data <- combined_data |> 
  rename(
    id = location_id,
    name = location_name,
    pm25 = pm25, 
    rh = relativehumidity,
    temp = temperature
  )

## Scale my data

combined_data <- combined_data |> 
  mutate(
    temp_s = scale(temp)[,1],
    rh_s = scale(rh)[,1]
  )

## Specifying the Bayesian regression
## PM25

fit <- brm(
  formula = pm25 ~ temp_s + rh_s, 
  data = combined_data, 
  family = gaussian(), 
  prior = c(
    prior(normal(0,5), class = "b"),
    prior(normal(0,10), class = "Intercept"),
    prior(exponential(1), class = "sigma")
  )
  
)
