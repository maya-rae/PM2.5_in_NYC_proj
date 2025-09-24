## Bayesian inferences from PM 2.5 data taken from
## Lower East Side, Manhattan
## Maya Arnott 09/24/2025

## importing libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(tidyverse)

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

## Mutate dataset PM2.5

selected_pm25 <- les_pm25_df |> 
  select(location_id, parameter, value, datetime_local)

