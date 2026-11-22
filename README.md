---
title: "README"
output: html_document
---

# isoRouting

`isoRouting` is an R package providing an S4 diagnostic pipeline for unsupervised multivariate time-series anomaly detection, fault archetype extraction, and online stream routing. 

`isoRouting` uses a fitted **Isolation Forest** (`isotree`) as both the anomaly detector and the similarity engine—extracting terminal leaf memberships directly from the trees to keep threshold calibration, clustering, and live routing fast and scale-free.

## Core Capabilities

- **Fast Scoring & Feature Auditing:** Builds Isolation Forests on fixed subsamples ($\psi$) for linear-time scoring, enabling rapid permutation and Leave-One-Covariate-Out (LOCO) feature attribution (`ablate_features()`).
- **Objective Threshold Calibration:** Sweeps candidate anomaly score cutoffs using **Hartigan's Dip Statistic ($D$)** and **Local Intrinsic Dimensionality ($\text{LID}$)**, paired with Monte Carlo null controls (`simulate_dip_control()`, `simulate_lid_control()`) to identify where anomalous tails separate into low-dimensional, multimodal regimes.
- **Scale-Free Clustering & Medoid Archetypes:** Computes pairwise **Hamming distances** across terminal leaf memberships (`compute_distances()`), partitions anomalies via divisive hierarchical clustering (DIANA), validates cluster count stability (`evaluate_k_stability()`), and extracts representative historical trajectories as cluster medoids (`extract_medoids()`) called archetypes.
- **Constant-Time Streaming Triage & Novelty Detection:** Routes incoming telemetry (`predict_stream()`) by comparing leaf memberships solely against cached cluster medoids. Observations within calibrated radii are assigned to known archetypes; out-of-distribution events are flagged as novel anomalies and can be branched into a secondary engine for sub-clustering (`isolate_novel_anomalies()`).

## Computational Complexity

By decoupling the full time-series length $N$ from the extracted anomaly tail $N_a \ll N$, every stage of the pipeline remains tractable on long telemetry streams:

| Pipeline Stage | S4 Method / Engine Step | Time Complexity |
| :--- | :--- | :--- |
| **Forest Fit & Scoring** | `ingest_time_course()` / `isotree` | $\mathcal{O}(n_t \psi \log \psi + N n_t \log \psi)$ |
| **Feature Ranking (LOO)** | `ablate_features()` | $\mathcal{O}(p \cdot N n_t \log \psi)$ |
| **Threshold Controls ($D$, $\text{LID}$)** | `simulate_dip_control()`, `simulate_lid_control()` | $\mathcal{O}(M \cdot N_a \log N_a)$ |
| **Leaf Hamming Distances** | `compute_distances()` | $\mathcal{O}(N_a^2 n_t)$ |
| **Divisive Clustering & Stability** | `evaluate_k_stability()` (DIANA) | $\mathcal{O}(B \cdot k N_a^2)$ |
| **Streaming Triage** | `predict_stream()` | $\mathcal{O}(N_{\text{new}} n_t \log \psi + N_{\text{new}} k n_t)$ |

*$N$: series length; $N_a \ll N$: extracted tail anomalies; $n_t$: number of trees (`isotree`); $\psi$: subsample size (`isotree`); $p$: features; $k$: archetype clusters; $M, B$: Monte Carlo/bootstrap iterations; $N_{\text{new}}$: streaming observations.*

## Installation

Install the development version directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("joelRMath/isoRouting", build_vignettes = TRUE)
```

## Case Study & Documentation

`isoRouting` includes a bundled multivariate telemetry dataset (`hvac_features`, adapted from the Turin HVAC benchmark) and a full end-to-end vignette covering data continuity checks, feature dilution auditing, threshold calibration, medoid extraction, and post-gap stream routing.

After installation, open the vignette from R:

```r
vignette("isoRouting")
```

## License

This package is released under the MIT License. See the `LICENSE` file for details.