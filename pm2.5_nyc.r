## Bayesian inferences from PM 2.5 data taken from
## Lower East Side, Manhattan
## Maya Arnott

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
  ungroup () |> 
  mutate(
    date = as.factor(date),
    # transform hour to a cyclic variable
    hour = 2 * pi * hour / 24,
    temp_s = scale(temp)[,1],
    rh_s = scale(rh)[,1], 
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

## pivoting my plot longer for fixed effects

posterior_long <- as_draws_df(fit) |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Parameter", 
    values_to = "Value"
  )

## filter out spline basis parameters

posterior_fixed <- posterior_long |> 
  filter(!grepl("^s\\(", Parameter))

## computing 95% credible intervals

ci_df <- posterior_long |> 
  group_by(Parameter) |> 
  summarise(
    lower = quantile(Value, 0.025), 
    upper = quantile(Value, 0.975), 
    .groups = "drop"
  )

## plotting the posterior samples (manual level)

posterior_plot <- ggplot(posterior_fixed, aes(x = Value , fill = Parameter)) + 
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

## visualizing the hourly/cyclic spline effect

# creating a fine grid of hours for prediction
hour_grid <- tibble(
  hour = seq(0, 24, length.out = 100),
  temp_s = 0,
  rh_s = 0,
  date = factor(1)  # add a dummy date to satisfy validate_data
) |> 
  mutate(hour = 2 * pi * hour / 24)

# Compute posterior predicted values for the spline (population-level only)
hour_effect <- posterior_linpred(fit, newdata = hour_grid, re_formula = NA)

# pivot long
hour_long <- as_tibble(hour_effect) |> 
  pivot_longer(
    cols = everything(),
    names_to = "hour_index",
    values_to = "pred"
  ) |> 
  mutate(
    hour_index = as.integer(hour_index),          # convert to integer
    hour = hour_grid$hour[hour_index]            # assign correct hour
  )

# Summarize mean and 95% credible intervals
hour_summary <- as_tibble(hour_effect) |> 
  mutate(hour = pull(hour_grid, hour)) |>   # adding hour column
  summarise(
    hour = hour, 
    mean = apply(as.matrix(.), 2, mean),
    lower = apply(as.matrix(.), 2, quantile, 0.025),
    upper = apply(as.matrix(.), 2, quantile, 0.975)
  )

# Plot diurnal effect
hour_plot <- ggplot(hour_summary, aes(x = hour, y = mean)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  theme_minimal() +
  labs(title = "Estimated Diurnal (Hourly) Effect on PM2.5",
       x = "Hour of Day", 
       y = "PM2.5 effect")

hour_plot
