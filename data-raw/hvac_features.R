library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(janitor)

# =============================================================================
# Helper Functions for Thermodynamic Momentum (RLE & Displacement)
# =============================================================================
# Calculates consecutive intervals moving in the same direction (15m increments)
get_run_length <- function(diff_vec, threshold = 0) {
  dir <- dplyr::case_when(
    is.na(diff_vec) ~ 0,
    diff_vec > threshold ~ 1,
    diff_vec < -threshold ~ -1,
    TRUE ~ 0
  )
  tibble::tibble(dir = dir, grp = dplyr::consecutive_id(dir)) |>
    dplyr::group_by(grp) |>
    dplyr::mutate(rl = dplyr::if_else(dir == 0, 0, as.numeric(dplyr::row_number()) * dir)) |>
    dplyr::pull(rl)
}

# Calculates the cumulative sum of differences during a continuous run
get_displacement <- function(diff_vec, threshold = 0) {
  dir <- dplyr::case_when(
    is.na(diff_vec) ~ 0,
    diff_vec > threshold ~ 1,
    diff_vec < -threshold ~ -1,
    TRUE ~ 0
  )
  tibble::tibble(diff = diff_vec, dir = dir, grp = dplyr::consecutive_id(dir)) |>
    dplyr::group_by(grp) |>
    dplyr::mutate(disp = dplyr::if_else(dir == 0, 0, cumsum(dplyr::coalesce(diff, 0)))) |>
    dplyr::pull(disp)
}

# Calculates consecutive 15m intervals spent in a specific operating mode (Low, Medium, High)
# Missing rows (NA) break the contiguous run before drop_na() removes them
get_mode_run_length <- function(val_vec, low_cut, high_cut, target_mode) {
  mode_chr <- dplyr::case_when(
    is.na(val_vec)      ~ "Missing",
    val_vec < low_cut   ~ "Low",
    val_vec <= high_cut ~ "Medium",
    TRUE                ~ "High"
  )
  tibble::tibble(m = mode_chr, grp = dplyr::consecutive_id(mode_chr)) |>
    dplyr::group_by(grp) |>
    dplyr::mutate(rl = dplyr::if_else(m == target_mode, as.numeric(dplyr::row_number()), 0)) |>
    dplyr::pull(rl)
}

# =============================================================================
# Data Loading & Feature Engineering Pipeline
# =============================================================================

# 1. Read and clean the data names
hvac_data <- read_csv(here("data-raw", "HVAC_NE_EC_19-21.csv")) |>
  clean_names()

# 2. Process timestamps and engineer features
hvac_features <- hvac_data |>
  mutate(timestamp = ymd_hms(timestamp)) |>
  # Catch stray NAs in timestamp before sequencing
  filter(!is.na(timestamp)) |>
  # Ensure continuous 15-minute sequence (exposes missing data as NAs)
  complete(
    timestamp = seq(
      min(timestamp, na.rm = TRUE), 
      max(timestamp, na.rm = TRUE), 
      by = "15 mins"
    )
  ) |>
  arrange(timestamp) |>
  
  mutate(
    # --- Power & Energy Weighted Derivatives ---
    dw_power = power * (power - lag(power)),
    dw_energy = energy * (energy - lag(energy)),
    
    # --- Power & Energy Mode Dwell Times (15m increments) ---
    power_low_run_len   = get_mode_run_length(power,  low_cut = 1.0, high_cut = 4.0, target_mode = "Low"),
    power_med_run_len   = get_mode_run_length(power,  low_cut = 1.0, high_cut = 4.0, target_mode = "Medium"),
    power_high_run_len  = get_mode_run_length(power,  low_cut = 1.0, high_cut = 4.0, target_mode = "High"),
    
    energy_low_run_len  = get_mode_run_length(energy, low_cut = 2.5, high_cut = 7.5, target_mode = "Low"),
    energy_med_run_len  = get_mode_run_length(energy, low_cut = 2.5, high_cut = 7.5, target_mode = "Medium"),
    energy_high_run_len = get_mode_run_length(energy, low_cut = 2.5, high_cut = 7.5, target_mode = "High"),
    
    # --- Outdoor Environmental Variables ---
    d_rh_outdoor = rh_outdoor - lag(rh_outdoor),
    d_t_outdoor = t_outdoor - lag(t_outdoor),
    
    # --- Base 15m Derivatives for Indoor/Supply ---
    d_t_return_15m = t_return - lag(t_return),
    d_t_supply_15m = t_supply - lag(t_supply),
    
    # --- System Error & Thermal Effort ---
    control_error = t_return - sp_return,
    supply_delta = t_supply - t_return,
    
    # --- Moisture Gaps ---
    humidity_delta = rh_supply - rh_return,
    supply_margin = t_supply - t_saturation,
    return_margin = t_return - t_saturation,
    
    # --- Moisture Kinematics Base (15m Derivatives) ---
    d_humidity_delta_15m = humidity_delta - lag(humidity_delta),
    d_supply_margin_15m = supply_margin - lag(supply_margin),
    d_return_margin_15m = return_margin - lag(return_margin)
  ) |>
  
  # Compute Run Length Encoding (RLE) and Displacements
  # Applying noise thresholds (0.05 C for temperatures, 0.1 % for humidity)
  mutate(
    # Indoor Return Temp Momentum
    t_ret_run_length = get_run_length(d_t_return_15m, threshold = 0.05),
    t_ret_displacement = get_displacement(d_t_return_15m, threshold = 0.05),
    
    # Supply Temp Momentum
    t_sup_run_length = get_run_length(d_t_supply_15m, threshold = 0.05),
    t_sup_displacement = get_displacement(d_t_supply_15m, threshold = 0.05),
    
    # Moisture Gaps Momentum
    run_length_humidity_delta = get_run_length(d_humidity_delta_15m, threshold = 0.1),
    displacement_humidity_delta = get_displacement(d_humidity_delta_15m, threshold = 0.1),
    
    run_length_supply_margin = get_run_length(d_supply_margin_15m, threshold = 0.05),
    displacement_supply_margin = get_displacement(d_supply_margin_15m, threshold = 0.05),
    
    run_length_return_margin = get_run_length(d_return_margin_15m, threshold = 0.05),
    displacement_return_margin = get_displacement(d_return_margin_15m, threshold = 0.05)
  ) |>
  
  # Purge artifacts and NAs resulting from lags or the summer gap
  drop_na() |>
  
  # Select final 41 variables (timestamp + 10 raw + 30 engineered)
  select(
    timestamp,
    t_supply, t_return, sp_return, t_saturation, t_outdoor, 
    rh_supply, rh_return, rh_outdoor, energy, power,
    
    dw_power, dw_energy,
    power_low_run_len, power_med_run_len, power_high_run_len,
    energy_low_run_len, energy_med_run_len, energy_high_run_len,
    
    d_rh_outdoor, d_t_outdoor,
    
    d_t_return_15m, t_ret_run_length, t_ret_displacement,
    
    control_error, supply_delta, 
    d_t_supply_15m, t_sup_run_length, t_sup_displacement,
    
    humidity_delta, supply_margin, return_margin,
    
    d_humidity_delta_15m, run_length_humidity_delta, displacement_humidity_delta,
    d_supply_margin_15m, run_length_supply_margin, displacement_supply_margin,
    d_return_margin_15m, run_length_return_margin, displacement_return_margin
  )

# =============================================================================
# Package Output
# =============================================================================
# Overwrite the bundled package data
usethis::use_data(hvac_features, overwrite = TRUE)