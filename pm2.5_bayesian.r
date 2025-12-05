## Bayesian inferences from PM 2.5 data taken from
## Lower East Side, Manhattan
## Maya Arnott

## importing libraries
library(ggplot2)
library(dplyr)
library(brms)
library(tidyverse)
library(tidyr)

# create a function to read clean and select each file
load_openaq <- function(path) {
  read_csv(path) |>
    janitor::clean_names() |> 
    select(datetime_local, location_id, location_name, parameter, value)
}

# calling the function for each file
pm25_data = load_openaq("data/openaq_LES_PM2.5.csv")

rh_data  = load_openaq("data/openaq_LES_RH.csv")

temp_data = load_openaq("data/openaq_LES_Temp.csv")

# bind the rows in the 'parameter' column

all_data <- bind_rows(pm25_data, rh_data, temp_data)

# identifying duplicates

all_data |> 
  group_by(datetime_local, location_id, location_name, parameter) |> 
  summarize(n = n(), .groups = "drop") |> 
  filter( n > 1 )

# pivot wider so the vars are in separate columns
# collapsed duplicates by averaging the 2 values

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
    month = factor(format(datetime, "%m"), 
                  levels = sprintf("%02d", 1:12),
                  labels = month.abb),
    day = as.integer(format(datetime, "%d"))
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

## scale my data

combined_data <- combined_data |> 
ungroup () |> 
mutate(
    date = as.factor(date),
    # transform hour to a cyclic variable
    hour = 2 * pi * hour / 24,
  )

# defining knots 
knots <- list(hour = c(0, 2 * pi))

## specifying the Bayesian regression

fit <- brm(
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
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

summary(fit)

## visualizing the posterior plots (basic level)

plot(fit)

pp_check(fit) # this compares obs vs. prediction of PM2.5

# Posterior distributions of parameters

# selecting the parameter columns we want
posterior_clean <- as_draws_df(fit) |> 
  select(-c(.chain, .iteration, .draw)) |>  # remove MCMC bookkeeping
  select(-matches("^sds_shour_"), 
         -matches("^s_shour_"),
         -matches("^lprior"),
         -matches("^lp_")
)

## extract posterior draws and pivot longer
posterior_long <- posterior_clean |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Parameter", 
    values_to = "Value"
  )

# computing 95% credible intervals
ci_df <- posterior_long |> 
  group_by(Parameter) |> 
  summarise(
    lower = quantile(Value, 0.025), 
    upper = quantile(Value, 0.975), 
    .groups = "drop"
  )

# Plot posterior distributions
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

ggsave("Posterior_dist.pdf", plot = posterior_plot,
       width = 8,
       height = 6)


# Visualizing the hourly/cyclic spline effect

# creating a fine grid of hours for prediction
hour_grid <- tibble(
  hour = seq(0, 24, length.out = 100),
  temp_s = 0,
  rh_s = 0,
  date = NA
  ) |>
  mutate(hour = 2 * pi * hour / 24)

# compute posterior predicted values for the spline (population-level only)
hour_effect <- posterior_linpred(fit, newdata = hour_grid, re_formula = NA)

# summarize mean and 95% credible intervals
hour_summary <- as_tibble(hour_effect) |> 
  summarise(across(everything(), list(
    mean = ~mean(.),
    lower = ~quantile(., 0.025),
    upper = ~quantile(., 0.975)
  ))) |> 
  pivot_longer(
    everything(), 
    names_to = c("hour_idx", ".value"),
    names_pattern = "(\\d+)_(.*)"
  ) |> 
  mutate(hour = hour_grid |> pull(hour))

# plot diurnal effect
hour_plot <- 
  ggplot(hour_summary, aes(x = hour * 24 / (2*pi), y = mean)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  theme_minimal() +
  labs(title = "Estimated Diurnal (Hourly) Effect on PM2.5",
       x = "Hour of Day", 
       y = "PM2.5 effect")

ggsave("Diurnal_effects.pdf", plot = hour_plot,
       width = 8,
       height = 6)
