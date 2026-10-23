# ============================================================================
# 04_clustering.R — K-Means Clustering Experiment
#
# Tests whether the three archetypes emerge naturally from the data via
# unsupervised k-means clustering, rather than relying on hand-tuned
# thresholds.
#
# Clustering features: layup_rate, three_rate, ft_rate, fg_pct
#
# Two experiments:
#   1. K-means (k=3) on normalized features — do clusters align with archetypes?
#   2. Elbow plot (k=2..10) — is k=3 actually the natural number of clusters?
#
# Validates cluster assignments against WNBA outcomes to check if the
# data-derived groupings separate pro success as well as the hand-tuned ones.
# ============================================================================

library(tidyverse)

# ── Load data ──

college <- read_csv("data/college_classified.csv", show_col_types = FALSE)
wnba    <- read_csv("data/wnba_classified.csv", show_col_types = FALSE)

cat(sprintf("College players: %d, WNBA players: %d\n\n", nrow(college), nrow(wnba)))

# ── Prepare features ──

features <- c("layup_rate", "three_rate", "ft_rate", "fg_pct")

# Scale features to zero mean, unit variance
college_scaled <- college %>%
  select(all_of(features)) %>%
  scale()

# Store scaling parameters for applying to WNBA data
scale_center <- attr(college_scaled, "scaled:center")
scale_sd     <- attr(college_scaled, "scaled:scale")

# ============================================================================
# EXPERIMENT 1: K-Means (k=3)
# ============================================================================

cat("=== EXPERIMENT 1: K-MEANS (k=3) ===\n\n")

set.seed(42)  # Reproducibility
km3 <- kmeans(college_scaled, centers = 3, nstart = 25, iter.max = 100)

college$cluster <- factor(km3$cluster)

# Cluster centroids (denormalized)
centroids <- as_tibble(km3$centers) %>%
  mutate(across(everything(), ~ .x * scale_sd[cur_column()] + scale_center[cur_column()])) %>%
  mutate(cluster = factor(1:3), n = km3$size)

cat("Cluster centroids (denormalized):\n")
centroids %>%
  select(cluster, n, everything()) %>%
  mutate(across(where(is.numeric) & !c(n), ~ round(.x, 3))) %>%
  print()

# Label clusters by their dominant characteristic
label_cluster <- function(layup, three, ft) {
  case_when(
    layup >= 0.20 & three >= 0.15 & ft >= 0.18 ~ "Scoring Threat",
    layup >= 0.25 & three < 0.15               ~ "Interior Presence",
    three >= 0.15 & layup < 0.25               ~ "Specialist",
    layup > three                               ~ "Interior Presence",
    TRUE                                        ~ "Specialist"
  )
}

centroids <- centroids %>%
  mutate(cluster_label = label_cluster(layup_rate, three_rate, ft_rate))

cat("\nCluster labels:\n")
centroids %>% select(cluster, cluster_label, n) %>% print()

# Map cluster labels to college data
college <- college %>%
  left_join(centroids %>% select(cluster, cluster_label), by = "cluster")

# ── Agreement with hand-tuned classification ──

cat("\n=== AGREEMENT: K-Means vs Hand-Tuned ===\n\n")

agreement <- college %>%
  mutate(agree = archetype == cluster_label) %>%
  summarise(
    total = n(),
    agree = sum(agree),
    disagree = total - agree,
    pct_agree = scales::percent(agree / total, accuracy = 0.1)
  )
print(agreement)

# Confusion matrix
cat("\nConfusion matrix (rows = hand-tuned, cols = k-means):\n\n")
table(HandTuned = college$archetype, KMeans = college$cluster_label) %>%
  print()

# ── WNBA validation of clusters ──

cat("\n=== WNBA VALIDATION OF CLUSTERS ===\n\n")

# Assign WNBA players to clusters using the fitted model
wnba_scaled <- wnba %>%
  select(all_of(features)) %>%
  scale(center = scale_center, scale = scale_sd)

# Find nearest centroid for each WNBA player
wnba_clusters <- apply(wnba_scaled, 1, function(row) {
  dists <- apply(km3$centers, 1, function(cent) sum((row - cent)^2))
  which.min(dists)
})

wnba$cluster <- factor(wnba_clusters)
wnba <- wnba %>%
  left_join(centroids %>% select(cluster, cluster_label), by = "cluster")

# Compare WNBA contribution by cluster vs hand-tuned archetype
cat("By k-means cluster:\n")
wnba %>%
  group_by(cluster_label) %>%
  summarise(n = n(), avg_contrib = round(mean(wnba_total_contrib), 1), .groups = "drop") %>%
  arrange(desc(avg_contrib)) %>%
  print()

cat("\nBy hand-tuned archetype:\n")
wnba %>%
  group_by(archetype) %>%
  summarise(n = n(), avg_contrib = round(mean(wnba_total_contrib), 1), .groups = "drop") %>%
  arrange(desc(avg_contrib)) %>%
  print()

# ============================================================================
# EXPERIMENT 2: Elbow Plot (k = 2..10)
# ============================================================================

cat("\n=== EXPERIMENT 2: OPTIMAL K ===\n\n")

set.seed(42)
wss <- map_dbl(2:10, function(k) {
  km <- kmeans(college_scaled, centers = k, nstart = 20, iter.max = 100)
  km$tot.withinss
})

elbow_data <- tibble(k = 2:10, wss = wss)
cat("Within-cluster sum of squares by k:\n")
elbow_data %>% mutate(wss = round(wss, 1)) %>% print()

# ── Visualizations ──

# Cluster scatter (same axes as 01_classify)
p1 <- ggplot(college, aes(x = three_rate, y = layup_rate, color = cluster_label)) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_vline(xintercept = 0.15, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c(
    "Scoring Threat"    = "#2c5f7c",
    "Interior Presence" = "#c44536",
    "Specialist"        = "#e8a838"
  )) +
  labs(
    title = "K-Means Clusters (k=3) vs Archetype Thresholds",
    subtitle = "Dashed lines = hand-tuned thresholds (15% layup, 15% three-rate)",
    x = "Three-Point Attempt Rate",
    y = "Layup Rate",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("output/04_kmeans_scatter.png", p1, width = 8, height = 6, dpi = 150)
cat("\nSaved: output/04_kmeans_scatter.png\n")

# Elbow plot
p2 <- ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line(color = "#2c5f7c", linewidth = 1) +
  geom_point(color = "#2c5f7c", size = 3) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "#c44536") +
  annotate("text", x = 3.3, y = max(wss) * 0.95, label = "k = 3", color = "#c44536") +
  labs(
    title = "Elbow Plot: Optimal Number of Clusters",
    x = "Number of Clusters (k)",
    y = "Within-Cluster Sum of Squares"
  ) +
  scale_x_continuous(breaks = 2:10) +
  theme_minimal(base_size = 12)

ggsave("output/04_elbow_plot.png", p2, width = 7, height = 5, dpi = 150)
cat("Saved: output/04_elbow_plot.png\n")

# Side-by-side comparison: hand-tuned vs k-means
comparison <- college %>%
  select(player_name, archetype, cluster_label) %>%
  mutate(agree = archetype == cluster_label)

cat(sprintf("\nOverall agreement: %s\n", scales::percent(mean(comparison$agree), accuracy = 0.1)))
