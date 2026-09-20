# manuscript/R/04_plot_prognostic_drift.R
library(dynaRchetypes)
library(ggplot2)
library(dplyr)
library(patchwork)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. PIPELINE EXECUTION
# -----------------------------------------------------------------------------
message("Initializing Engine and evaluating prognostic drift...")
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

# Run Prognostic Drift Evaluation
df_drift <- evaluate_prognostic_drift(
  engine, 
  train_fraction = 0.6, 
  ntrees = 500, 
  sample_size = 256
)

# -----------------------------------------------------------------------------
# 2. DATA PROCESSING (Ranks)
# -----------------------------------------------------------------------------
# Convert raw scores to normalized percentiles (ECDF equivalent)
df_drift <- df_drift %>%
  mutate(
    base_percentile = percent_rank(baseline_score),
    oos_percentile = percent_rank(oos_score)
  )

# Calculate fit metrics
r_sq_raw <- cor(df_drift$baseline_score, df_drift$oos_score)^2
rho_rank <- cor(df_drift$base_percentile, df_drift$oos_percentile, method = "spearman")

# -----------------------------------------------------------------------------
# 3. VISUALIZATION
# -----------------------------------------------------------------------------
message("Generating two-panel prognostic validation plot...")

# Panel A: Raw Scores
p1 <- ggplot(df_drift, aes(x = baseline_score, y = oos_score)) +
  geom_point(alpha = 0.1, color = "#0072B2", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "#D55E00", se = FALSE, linewidth = 1.2) +
  annotate(
    "text", x = min(df_drift$baseline_score) + 0.02, y = max(df_drift$oos_score) - 0.02, 
    label = sprintf("R^2 == %.3f", r_sq_raw), parse = TRUE, size = 5, fontface = "bold", hjust = 0
  ) +
  labs(
    title = "A) Raw Anomaly Scores",
    x = "Ground Truth Score (Full Model)",
    y = "Out-of-Sample Score"
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# Panel B: Normalized Ranks (Percentiles)
p2 <- ggplot(df_drift, aes(x = base_percentile, y = oos_percentile)) +
  geom_point(alpha = 0.1, color = "#009E73", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  annotate(
    "text", x = 0.05, y = 0.95, 
    label = sprintf("rho == %.3f", rho_rank), parse = TRUE, size = 5, fontface = "bold", hjust = 0
  ) +
  labs(
    title = "B) Normalized Triage Rank (Percentile)",
    x = "Ground Truth Percentile",
    y = "Out-of-Sample Percentile"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# -----------------------------------------------------------------------------
# 4. STITCH AND EXPORT
# -----------------------------------------------------------------------------
fig4 <- p1 | p2

output_path <- "manuscript/figures/fig4_prognostic_drift.pdf"
ggsave(filename = output_path, plot = fig4, width = 12, height = 6, device = "pdf")
output_path <- "manuscript/figures/fig4_prognostic_drift.png"
ggsave(filename = output_path, plot = fig4, width = 12, height = 6, device = "png")

message(sprintf("Figure saved to %s", output_path))

# 1. Determine how many anomalies the 0.68 threshold yields in the full model
threshold_val <- 0.65
n_anomalies <- sum(df_drift$baseline_score >= threshold_val)

# 2. Extract the exact timestamps of the top N anomalies for both models
base_anomalies <- df_drift %>% 
  arrange(desc(baseline_score)) %>% 
  slice(1:n_anomalies) %>% 
  pull(timestamp)

oos_anomalies <- df_drift %>% 
  arrange(desc(oos_score)) %>% 
  slice(1:n_anomalies) %>% 
  pull(timestamp)

# 3. Calculate the exact intersection
overlap <- length(intersect(base_anomalies, oos_anomalies))
overlap_pct <- (overlap / n_anomalies) * 100

message(sprintf("Top %d Anomaly Overlap: %.1f%%", n_anomalies, overlap_pct))