library(ggplot2)
library(dplyr)
library(purrr)

# ---------------------------------------------------------
# 1. VISUALIZATION FUNCTION
# ---------------------------------------------------------
plot_k_profile <- function(df, title = "Opt_k Profile") {
  ggplot(df, aes(x = threshold, y = opt_k)) +
    geom_line(linewidth = 1, color = "#0072B2") +
    geom_point(size = 2.5, color = "#0072B2") +
    scale_y_continuous(limits = c(2, 10), breaks = seq(2, 10, by = 1)) +
    labs(
      title = title,
      x = "Anomaly Threshold",
      y = "Optimal Clusters (k)"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# ---------------------------------------------------------
# 2. METRICS CALCULATION FUNCTION
# ---------------------------------------------------------
calculate_stability_metrics <- function(df, k_min = 2, k_max = 10) {
  
  # Ensure chronological order
  df <- df[order(df$threshold), ]
  
  n_points <- nrow(df)
  t_range <- max(df$threshold) - min(df$threshold)
  
  # 1. Normalized Domain Entropy
  # Calculate proportion of domain occupied by each unique k
  p_k <- table(df$opt_k) / n_points
  entropy <- -sum(p_k * log(p_k))
  max_entropy <- log(k_max - k_min + 1)
  norm_entropy <- entropy / max_entropy
  
  # 2. Maximum Domain Dominance
  # Find the longest contiguous sequence of the same k
  runs <- rle(df$opt_k)
  max_plateau_length <- max(runs$lengths)
  dominance <- max_plateau_length / n_points
  
  # 3. Topological Jitter (Total Variation Density)
  # Sum of absolute differences divided by continuous threshold range
  jumps <- abs(diff(df$opt_k))
  max_possible_jumps <- (n_points - 1) * (k_max - k_min)
  jitter <- sum(jumps) / max_possible_jumps
  
  
  return(list(
    Normalized_Entropy = norm_entropy,
    Domain_Dominance = dominance,
    Topological_Jitter = jitter
  ))
}

# ---------------------------------------------------------
# 3. SIMULATION & EVALUATION
# ---------------------------------------------------------

# Define the continuous threshold grid
thresholds <- seq(0.55, 0.75, by = 0.005)
n <- length(thresholds)

# Profile A: Perfect Stability (Strong archetype)
prof_stable <- data.frame(
  threshold = thresholds,
  opt_k = rep(4, n)
)

# Profile B: Single State Transition (Two distinct archetypal regimes)
prof_transition <- data.frame(
  threshold = thresholds,
  opt_k = c(rep(3, floor(n/2)), rep(6, ceiling(n/2)))
)

# Profile C: High Volatility (Similar to the naive Turin configuration)
set.seed(123)
prof_volatile <- data.frame(
  threshold = thresholds,
  opt_k = round(runif(n, min = 2, max = 10))
)
# Inject a minor plateau to mimic the data
prof_volatile$opt_k[10:14] <- 4 

# Profile D: Pure Oscillation (Algorithm bouncing between two adjacent states)
prof_oscillating <- data.frame(
  threshold = thresholds,
  opt_k = rep(c(3, 6), length.out = n)
)

# Bundle for iteration
profiles <- list(
  "Perfect Stability" = prof_stable,
  "Single Transition" = prof_transition,
  "High Volatility" = prof_volatile,
  "Pure Oscillation" = prof_oscillating
)

# Calculate metrics and print results
results <- imap_dfr(profiles, function(df, name) {
  metrics <- calculate_stability_metrics(df)
  data.frame(
    Profile = name,
    Entropy = round(metrics$Normalized_Entropy, 3),
    Dominance = round(metrics$Domain_Dominance, 3),
    Jitter = round(metrics$Topological_Jitter, 1)
  )
})

print(results)

# Optional: Plot them all to visually align with the metrics
library(patchwork)
p1 <- plot_k_profile(prof_stable, "Perfect Stability")
p2 <- plot_k_profile(prof_transition, "Single Transition")
p3 <- plot_k_profile(prof_volatile, "High Volatility")
p4 <- plot_k_profile(prof_oscillating, "Pure Oscillation")
print((p1 | p2) / (p3 | p4))