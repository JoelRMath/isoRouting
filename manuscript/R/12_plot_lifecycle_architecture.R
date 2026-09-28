# manuscript/R/12_plot_lifecycle_architecture.R
library(dynaRchetypes)
library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)
library(lubridate)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. LIFECYCLE FEATURE ENGINEERING & CONTIGUOUS FILTERING
# -----------------------------------------------------------------------------
message("Loading and isolating contiguous 6-month Winter/Spring block...")
data("hvac_raw")

hvac_lifecycle <- hvac_raw %>%
  arrange(timestamp) %>%
  # Isolate the exact contiguous block you identified
  filter(timestamp >= ymd_hms("2020-10-15 00:00:00", tz = "UTC")) %>%
  mutate(
    temp_error = t_supply - sp_return,
    delta_t = t_return - t_supply,
    
    # Engine B: Kinematics
    delta_t_deriv_15m = delta_t - lag(delta_t, n = 1, default = NA),
    energy_deriv_15m = energy - lag(energy, n = 1, default = NA),
    
    # Engine C: Volatility (24-hour rolling standard deviation)
    temp_error_sd_24h = zoo::rollapply(temp_error, width = 96, FUN = sd, fill = NA, align = "right")
  ) %>%
  tidyr::drop_na()

# -----------------------------------------------------------------------------
# 2. CHRONOLOGICAL SPLIT (AUTUMN TRAINING -> WINTER/SPRING TESTING)
# -----------------------------------------------------------------------------
# Train on the first 30 days, test on the remaining ~5 months
train_end_time <- min(hvac_lifecycle$timestamp) + days(30)

train_idx <- which(hvac_lifecycle$timestamp <= train_end_time)
test_idx  <- which(hvac_lifecycle$timestamp > train_end_time)

df_train <- hvac_lifecycle[train_idx, ]
df_test  <- hvac_lifecycle[test_idx, ]

message(sprintf("Training on %d days (Autumn). Testing on %d days (Winter/Spring)...", 
                round(nrow(df_train)/96), round(nrow(df_test)/96)))

# -----------------------------------------------------------------------------
# 3. ENGINE A: NAIVE FROZEN BASELINE
# -----------------------------------------------------------------------------
message("Executing Engine A: Naive Raw Temps (The Seasonal Alert Storm)...")
set.seed(42)
iso_naive <- suppressWarnings(isotree::isolation.forest(
  df_train[, c("t_supply", "t_return", "t_outdoor")], ntrees = 500
))
df_storm <- data.frame(
  timestamp = df_test$timestamp,
  Anomaly_Score = stats::predict(iso_naive, df_test[, c("t_supply", "t_return", "t_outdoor")]),
  Engine = "A) Naive Frozen Baseline (Raw Temps)"
)

# -----------------------------------------------------------------------------
# 4. ENGINE B: KINEMATIC ENGINE
# -----------------------------------------------------------------------------
message("Executing Engine B: Kinematics (Stationary Baseline)...")
set.seed(42)
iso_kinematic <- suppressWarnings(isotree::isolation.forest(
  df_train[, c("delta_t_deriv_15m", "energy_deriv_15m")], ntrees = 500
))
df_stable <- data.frame(
  timestamp = df_test$timestamp,
  Anomaly_Score = stats::predict(iso_kinematic, df_test[, c("delta_t_deriv_15m", "energy_deriv_15m")]),
  Engine = "B) Kinematic Engine (Derivatives)"
)

# -----------------------------------------------------------------------------
# 5. ENGINE C: VOLATILITY ENGINE
# -----------------------------------------------------------------------------
message("Executing Engine C: Volatility (Degradation Tracking)...")
set.seed(42)
iso_volatility <- suppressWarnings(isotree::isolation.forest(
  df_train[, "temp_error_sd_24h", drop = FALSE], ntrees = 500
))
df_drift <- data.frame(
  timestamp = df_test$timestamp,
  Anomaly_Score = stats::predict(iso_volatility, df_test[, "temp_error_sd_24h", drop = FALSE]),
  Engine = "C) Volatility Engine (24h Variance)"
)

# -----------------------------------------------------------------------------
# 6. VISUALIZATION
# -----------------------------------------------------------------------------
message("Generating Figure 12: Contiguous Lifecycle Architecture...")

df_plot <- bind_rows(df_storm, df_stable, df_drift)

p_lifecycle <- ggplot(df_plot, aes(x = timestamp, y = Anomaly_Score, color = Engine)) +
  geom_line(alpha = 0.4) +
  # LOESS is now mathematically valid because the time-series is contiguous
  geom_smooth(method = "loess", span = 0.15, color = "black", linewidth = 1.2, se = FALSE) +
  geom_hline(yintercept = 0.65, linetype = "dashed", color = "red", linewidth = 0.8) +
  facet_wrap(~Engine, ncol = 1, scales = "free_y") +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(
    title = "Prognostic Architecture: Seasonal Concept Drift & Stationary Baselines",
    subtitle = "Trained on 30 Days (Autumn). Tested on 5 Months (Winter to Spring).",
    x = "Chronological Time",
    y = "Isolation Forest Anomaly Score"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank()
  )

ggsave("manuscript/figures/fig12_lifecycle_architecture.pdf", plot = p_lifecycle, width = 10, height = 9)
ggsave("manuscript/figures/fig12_lifecycle_architecture.png", plot = p_lifecycle, width = 10, height = 9)

message("Contiguous validation complete. Check fig12_lifecycle_architecture.")