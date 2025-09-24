## Bayesian inferences from PM 2.5 data taken from
## Lower East Side, Manhattan
## Maya Arnott 09/24/2025

## importing libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(tidyverse)

## importing dataset
LES_PM25_df = 
  read_csv("/data/openaq_LES_PM2.5.csv")
