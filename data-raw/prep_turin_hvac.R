# data-raw/prep_turin_hvac.R
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(slider)

# 1. Load raw CSV (this file is gitignored)
raw_csv_path <- "data-raw/HVAC_NE_EC_19-21.csv"
raw_data <- read_csv(raw_csv_path, show_col_types = FALSE) %>% clean_names()

# 2. Apply cleaning and engineering pipeline
hvac_features <- raw_data %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  filter(!is.na(timestamp)) %>%
  complete(timestamp = seq(min(timestamp, na.rm = TRUE), 
                           max(timestamp, na.rm = TRUE), 
                           by = "15 mins")) %>%
  arrange(timestamp) %>%
  mutate(
    # Original Context
    is_weekend = if_else(wday(timestamp, week_start = 1) %in% c(6, 7), 1, 0),
    
    # New Occupancy Context (Mon-Fri, 08:00 to 17:59)
    is_occupied = if_else(
      wday(timestamp, week_start = 1) %in% 1:5 & hour(timestamp) >= 8 & hour(timestamp) < 18, 
      1, 
      0
    ),
    
    # Static & Relational Features
    temp_error = t_return - sp_return,
    delta_t = t_return - t_supply,
    delta_rh = rh_return - rh_supply,
    
    # 15-Minute Instantaneous Derivatives
    energy_deriv_15m = energy - lag(energy, n = 1),
    delta_t_deriv_15m = delta_t - lag(delta_t, n = 1),
    delta_rh_deriv_15m = delta_rh - lag(delta_rh, n = 1),
    t_return_deriv_15m = t_return - lag(t_return, n = 1),
    t_saturation_deriv_15m = t_saturation - lag(t_saturation, n = 1),
    
    # 1-Hour Rolling Volatility (Requires 3 prior 15-min rows)
    t_supply_roll_sd_1h = slider::slide_dbl(t_supply, sd, .before = 3, .complete = TRUE)
  ) %>%
  drop_na()

# 3. Export to the data/ directory as an .rda file
usethis::use_data(hvac_features, overwrite = TRUE)