# ============================================================================
# 05_comps.R — WNBA Player Comparisons
#
# For each college prospect, finds the most similar WNBA player(s) by
# Euclidean distance across 10 standardized shooting metrics.
#
# Metrics used for comparison:
#   fga_per_game, fg_pct, two_pct, three_pct, three_rate,
#   layup_rate, ft_rate, ft_pct, self_created, pts_per_att
#
# All metrics are z-scored against the WNBA population before computing
# distance, so no single metric dominates the comparison.
# ============================================================================

library(tidyverse)

# ── Load data ──

college <- read_csv("data/college_benchmarked.csv", show_col_types = FALSE)
wnba    <- read_csv("data/wnba_classified.csv", show_col_types = FALSE)

cat(sprintf("College prospects: %d, WNBA comp pool: %d\n\n", nrow(college), nrow(wnba)))

# ── Standardization ──

comp_metrics <- c("fga_per_game", "fg_pct", "two_pct", "three_pct", "three_rate",
                  "layup_rate", "ft_rate", "ft_pct", "self_created", "pts_per_att")

# Z-score against WNBA population (college prospects measured against WNBA baseline)
wnba_means <- map_dbl(comp_metrics, ~ mean(wnba[[.x]], na.rm = TRUE))
wnba_sds   <- map_dbl(comp_metrics, ~ sd(wnba[[.x]], na.rm = TRUE))
names(wnba_means) <- comp_metrics
names(wnba_sds)   <- comp_metrics

z_score <- function(df, metric) {
  (df[[metric]] - wnba_means[metric]) / wnba_sds[metric]
}

# Build z-scored matrices
wnba_z <- map_dfc(comp_metrics, ~ z_score(wnba, .x)) %>%
  set_names(comp_metrics)

college_z <- map_dfc(comp_metrics, ~ z_score(college, .x)) %>%
  set_names(comp_metrics)

# ── Euclidean distance function ──

find_comps <- function(prospect_idx, n_comps = 3) {
  prospect_vec <- as.numeric(college_z[prospect_idx, ])

  dists <- apply(wnba_z, 1, function(wnba_vec) {
    sqrt(sum((prospect_vec - as.numeric(wnba_vec))^2))
  })

  top_idx <- order(dists)[1:n_comps]

  tibble(
    comp_name    = wnba$player_name[top_idx],
    comp_team    = wnba$wnba_team[top_idx],
    comp_contrib = wnba$wnba_total_contrib[top_idx],
    distance     = round(dists[top_idx], 2)
  )
}

# ── Compute comps for top prospects per archetype ──

cat("=== WNBA COMPARISONS: Top Prospects by Archetype ===\n\n")

top_per_archetype <- college %>%
  group_by(archetype) %>%
  arrange(desc(match_count), desc(fga_per_game)) %>%
  slice_head(n = 10) %>%
  ungroup()

comp_results <- list()

for (i in seq_len(nrow(top_per_archetype))) {
  p <- top_per_archetype[i, ]
  # Find this player's index in the full college data
  idx <- which(college$player_id == p$player_id)[1]
  if (is.na(idx)) next

  comps <- find_comps(idx, n_comps = 3)

  comp_results[[i]] <- tibble(
    player_name = p$player_name,
    team        = p$team,
    archetype   = p$archetype,
    match_count = p$match_count,
    match_total = p$match_total
  ) %>%
    bind_cols(
      comps %>%
        summarise(
          comp_1 = sprintf("%s (%s, %.1f)", comp_name[1], comp_team[1], comp_contrib[1]),
          comp_2 = sprintf("%s (%s, %.1f)", comp_name[2], comp_team[2], comp_contrib[2]),
          comp_3 = sprintf("%s (%s, %.1f)", comp_name[3], comp_team[3], comp_contrib[3]),
          avg_comp_contrib = round(mean(comp_contrib), 1)
        )
    )
}

comp_table <- bind_rows(comp_results)

for (arch in c("Scoring Threat", "Interior Presence", "Specialist")) {
  cat(sprintf("\n--- %s ---\n\n", toupper(arch)))

  arch_comps <- comp_table %>% filter(archetype == arch)

  for (j in seq_len(nrow(arch_comps))) {
    row <- arch_comps[j, ]
    cat(sprintf("  %s (%s) — %d/%d flags\n",
                row$player_name, row$team, row$match_count, row$match_total))
    cat(sprintf("    1: %s\n", row$comp_1))
    cat(sprintf("    2: %s\n", row$comp_2))
    cat(sprintf("    3: %s\n", row$comp_3))
    cat(sprintf("    Avg comp contrib: %.1f\n\n", row$avg_comp_contrib))
  }
}

# ── Comp quality analysis ──
#
# Do prospects with more benchmark flags have higher-producing WNBA comps?

cat("\n=== COMP QUALITY BY BENCHMARK FLAGS ===\n\n")

# Compute comps for all prospects (takes a moment)
all_avg_comp <- map_dbl(seq_len(nrow(college)), function(i) {
  comps <- find_comps(i, n_comps = 3)
  mean(comps$comp_contrib)
})

college$avg_comp_contrib <- all_avg_comp

college %>%
  group_by(archetype, match_count) %>%
  summarise(
    n = n(),
    avg_comp_quality = round(mean(avg_comp_contrib), 1),
    .groups = "drop"
  ) %>%
  arrange(archetype, match_count) %>%
  print(n = Inf)

# ── Visualizations ──

# Comp quality vs benchmark flags
p1 <- college %>%
  ggplot(aes(x = factor(match_count), y = avg_comp_contrib, fill = archetype)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ archetype, scales = "free_x") +
  scale_fill_manual(values = c(
    "Scoring Threat"    = "#2c5f7c",
    "Interior Presence" = "#c44536",
    "Specialist"        = "#e8a838"
  )) +
  labs(
    title = "WNBA Comp Quality by Benchmarks Cleared",
    subtitle = "Do prospects with more WNBA-matching benchmarks have better WNBA comps?",
    x = "WNBA Benchmarks Cleared",
    y = "Average WNBA Comp Contribution"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("output/05_comp_quality.png", p1, width = 10, height = 5, dpi = 150)
cat("\nSaved: output/05_comp_quality.png\n")

# Radar-style profile comparison (top prospect vs best comp)
# Using a faceted bar chart as a more readable alternative to radar

top_prospect <- college %>%
  filter(archetype == "Scoring Threat") %>%
  arrange(desc(match_count), desc(fga_per_game)) %>%
  slice(1)

# Find their best comp
idx <- which(college$player_id == top_prospect$player_id)[1]
best_comp_data <- find_comps(idx, n_comps = 1)
best_comp_name <- best_comp_data$comp_name[1]
best_comp_row <- wnba %>% filter(player_name == best_comp_name) %>% slice(1)

# Build comparison data
if (nrow(best_comp_row) > 0) {
  profile_comparison <- tibble(
    metric = comp_metrics,
    prospect = map_dbl(comp_metrics, ~ top_prospect[[.x]]),
    wnba_comp = map_dbl(comp_metrics, ~ best_comp_row[[.x]])
  ) %>%
    pivot_longer(cols = c(prospect, wnba_comp), names_to = "player", values_to = "value") %>%
    mutate(
      player = if_else(player == "prospect", top_prospect$player_name, best_comp_name),
      metric = str_replace_all(metric, "_", " ") %>% str_to_title()
    )

  p2 <- ggplot(profile_comparison, aes(x = metric, y = value, fill = player)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c("#2c5f7c", "#c44536")) +
    labs(
      title = sprintf("Profile Comparison: %s vs %s", top_prospect$player_name, best_comp_name),
      x = NULL,
      y = "Value",
      fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  ggsave("output/05_profile_comparison.png", p2, width = 10, height = 6, dpi = 150)
  cat("Saved: output/05_profile_comparison.png\n")
}

# Save final dataset
write_csv(college %>% select(-starts_with("flag_")), "data/college_with_comps.csv")
cat("Saved: data/college_with_comps.csv\n")
