library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(slider) 

# 1. Read and clean the data
hvac_data <- read_csv(here("data", "Turin", "HVAC_NE_EC_19-21.csv")) %>%
  clean_names()

# 2. Clean the timestamp and extract a subset
hvac_clean <- hvac_data %>%
  mutate(
    timestamp = ymd_hms(timestamp) 
  )
hvac_features <- hvac_clean %>%
  # 0. Catch any stray NAs in the timestamp column before sequencing
  filter(!is.na(timestamp)) %>%
  # 1. Ensure the data is perfectly continuous in 15-minute increments
  complete(
    timestamp = seq(
      min(timestamp, na.rm = TRUE), 
      max(timestamp, na.rm = TRUE), 
      by = "15 mins"
    )
  ) %>%
  arrange(timestamp) %>%
  mutate(
    # 2. Context & Control
    is_weekend = if_else(wday(timestamp, week_start = 1) %in% c(6, 7), 1, 0),
    temp_error = t_return - sp_return,
    
    # 3. Common-Mode Rejection (Physics)
    delta_t = t_return - t_supply,
    delta_rh = rh_return - rh_supply
  ) %>%
  mutate(
    # 4. Cyclical Lags
    t_outdoor_lag_24h = t_outdoor - lag(t_outdoor, n = 96),
    rh_outdoor_lag_24h = rh_outdoor - lag(rh_outdoor, n = 96),
    energy_lag_24h = energy - lag(energy, n = 96),
    
    # 5. Instantaneous Derivatives
    energy_deriv_15m = energy - lag(energy, n = 1),
    delta_t_deriv_15m = delta_t - lag(delta_t, n = 1),
    delta_rh_deriv_15m = delta_rh - lag(delta_rh, n = 1),
    t_return_deriv_15m = t_return - lag(t_return, n = 1),
    t_saturation_deriv_15m = t_saturation - lag(t_saturation, n = 1),
    
    # 6. Volatility 
    t_supply_roll_sd_1h = slider::slide_dbl(t_supply, sd, .before = 3, .complete = TRUE)
  ) %>%
  # 7. Purge all artifacts (including the summer gap NAs)
  drop_na()
