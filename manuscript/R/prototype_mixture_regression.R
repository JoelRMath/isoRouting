# prototype_mixture_regression.R
library(dplyr)
library(ggplot2)
library(flexmix) # For finite mixture models

# 1. Isolate the target variables
data("hvac_raw")
df_proto <- hvac_raw %>%
  select(t_supply, t_saturation) %>%
  tidyr::drop_na()

# Subsample to make the iterative model training fast for prototyping
set.seed(42)
df_sample <- df_proto %>% sample_n(min(5000, nrow(.)))

# -----------------------------------------------------------------------------
# FIT THE FINITE MIXTURE OF REGRESSIONS
# -----------------------------------------------------------------------------
message("Fitting a Finite Mixture Model (k=3 expected physical states)...")

# We visually observed roughly 3 or 4 bands. We tell flexmix to find k=3 
# distinct linear relationships (t_saturation as a function of t_supply).
set.seed(42)
mix_model <- flexmix::flexmix(t_saturation ~ t_supply, data = df_sample, k = 3)

# Extract the assigned "gear" (latent state) for each point
df_sample$Assigned_State <- as.factor(flexmix::clusters(mix_model))

# -----------------------------------------------------------------------------
# EXTRACT THE EQUATIONS
# -----------------------------------------------------------------------------
# Let's pull the actual slopes and intercepts for the 3 distinct bands
model_params <- flexmix::parameters(mix_model)

intercepts <- model_params[1, ]
slopes     <- model_params[2, ]

message("\n--- Extracted Transfer Functions ---")
for (i in 1:3) {
  message(sprintf("State %d: t_saturation = %.3f * t_supply + %.3f", i, slopes[i], intercepts[i]))
}

# -----------------------------------------------------------------------------
# VISUALIZATION
# -----------------------------------------------------------------------------
p <- ggplot(df_sample, aes(x = t_supply, y = t_saturation, color = Assigned_State)) +
  geom_point(alpha = 0.4, size = 1) +
  # Plot the extracted regression lines for each state
  geom_abline(intercept = intercepts[1], slope = slopes[1], color = "black", linewidth = 1) +
  geom_abline(intercept = intercepts[2], slope = slopes[2], color = "black", linewidth = 1) +
  geom_abline(intercept = intercepts[3], slope = slopes[3], color = "black", linewidth = 1) +
  scale_color_viridis_d(option = "turbo", name = "Latent Physical State") +
  theme_bw(base_size = 14) +
  labs(
    title = "Finite Mixture of Regressions (t_supply vs t_saturation)",
    subtitle = "Deconstructing the nonlinear transfer function into distinct physical gears",
    x = "Supply Temperature (t_supply)",
    y = "Saturation Temperature (t_saturation)"
  ) +
  theme(legend.position = "bottom")
print(p)