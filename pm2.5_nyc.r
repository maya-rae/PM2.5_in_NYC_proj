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

## keeping only necessary columns
## select func allows me to select the columns i want to show

pm25_data <- les_pm25_df |> 
  select(datetime_local, location_id, location_name, parameter, value)

rh_data <- les_rh_df |> 
  select(datetime_local, location_id, location_name, parameter, value)

temp_data <- les_temp_df |> 
  select(datetime_local, location_id, location_name, parameter, value)

## bind the rows in the 'parameter' column

all_data <- bind_rows(pm25_data, rh_data, temp_data)

## identifying duplicates

duplicates <- all_data |> 
  group_by(datetime_local, location_id, location_name, parameter) |> 
  summarize(n = n(), .groups = "drop") |> 
  filter( n > 1 )

## pivot wider so the vars are in separate columns
## collapsed duplicates by averaging the 2 values

combined_data <- all_data |>
  group_by(datetime_local, location_id, location_name, parameter) |> 
  summarise(value = 
        mean(value, na.rm = TRUE), 
        .groups = "drop") |> 
  pivot_wider(names_from = parameter, values_from = value) |> 
  
  # extracting time variables
  mutate(
    datetime = as.POSIXct(datetime_local, tz = "America/New_York"),
    date = as.Date(datetime),
    hour = as.numeric(format(datetime, "%H")),
    month = format(datetime, "%m"),
    day = as.numeric(format(datetime, "%d"))
  ) |> 
  # rename columns for consistency
  rename(
    id = location_id,
    name = location_name,
    pm25 = pm25,
    rh = relativehumidity,
    temp = temperature
  ) |> 
  # scale continuous predictors
  mutate(
    temp_s = scale(temp)[,1],
    rh_s = scale(rh)[,1]
  )


## renaming columns

combined_data <- combined_data |> 
  rename(
    id = location_id,
    name = location_name,
    pm25 = pm25, 
    rh = relativehumidity,
    temp = temperature
  )

## scale my data

combined_data <- combined_data |> 
  mutate(
    temp_s = scale(temp)[,1],
    rh_s = scale(rh)[,1], 
    # transform hour to a cyclic variable
    hour = 2 * pi * hour / 24
  )

# defining knots 
knots <- list(hour = c(0, 2 * pi))

## specifying the Bayesian regression

fit <- brm(
  formula = 
    bf(pm25 ~ temp_s + rh_s + s(hour, bs = "cc", k = 10),
              # preventing divergent transitions
              autocor = cor_ar( ~1 | date, p = 1)),
  data = combined_data, 
  knots = knots,
  family = gaussian(),
  # to model within-day serial dependence
  prior = c(
    prior(normal(0,5), class = "b"),    # for slope
    prior(normal(0,10), class = "Intercept"), # for intercept
    prior(exponential(1), class = "sigma") # for residual SD
  ), 
  chains = 4,
  cores = 4, 
  iter = 4000,
  control = list(adapt_delta = 0.99)
)

summary(fit)

## visualizing the posterior plots (basic level)

plot(fit)

pp_check(fit) # this compares obs vs. prediction of PM2.5

## pivoting my plot longer

posterior_long <- posterior_samples(fit) |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Parameter", 
    values_to = "Value")

## computing 95% credible intervals

ci_df <- posterior_long |> 
  group_by(Parameter) |> 
  summarise(
    lower = quantile(Value, 0.025), 
    upper = quantile(Value, 0.975), 
    .groups = "drop"
  )

## plotting the posterior samples (manual level)

posterior_plot <- ggplot(posterior_long, aes(x = Value , fill = Parameter)) + 
  geom_density(alpha = 0.6) + 
  geom_vline(data = ci_df, aes(xintercept = lower), 
             linetype = "dashed", 
             color = "red") + 
  geom_vline(data = ci_df, aes(xintercept = upper), 
             linetype = "dashed",
             color = "red") +
  facet_wrap(~Parameter, scales = "free") + 
  theme_minimal() +
  labs(title = "Posterior Distributions of PM 2.5 with 95% Credible Intervals", 
       x = "Parameter Value", y = "Density")

