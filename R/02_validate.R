# ============================================================================
# 02_validate.R — WNBA Validation
#
# Tests whether the three college archetypes predict WNBA success.
# Uses 83 current WNBA players whose college shooting profiles are available
# (4 seasons of ESPN play-by-play data).
#
# Key question: do Scoring Threats in college become more productive WNBA
# players than Interior Presence or Specialists?
# ============================================================================

library(tidyverse)

# ── Load classified data ──

wnba <- read_csv("data/wnba_classified.csv", show_col_types = FALSE)

cat(sprintf("WNBA validation set: %d players\n\n", nrow(wnba)))

# ── WNBA contribution by archetype ──

cat("=== WNBA CONTRIBUTION BY COLLEGE ARCHETYPE ===\n\n")

archetype_summary <- wnba %>%
  group_by(archetype) %>%
  summarise(
    n             = n(),
    avg_contrib   = mean(wnba_total_contrib),
    median_contrib = median(wnba_total_contrib),
    sd_contrib    = sd(wnba_total_contrib),
    min_contrib   = min(wnba_total_contrib),
    max_contrib   = max(wnba_total_contrib),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_contrib))

print(archetype_summary)

# ── Statistical test: ANOVA ──

cat("\n=== ONE-WAY ANOVA: Contribution ~ Archetype ===\n\n")

aov_result <- aov(wnba_total_contrib ~ archetype, data = wnba)
print(summary(aov_result))

# Pairwise comparisons
cat("\n=== PAIRWISE T-TESTS (Bonferroni adjusted) ===\n\n")
pairwise <- pairwise.t.test(wnba$wnba_total_contrib, wnba$archetype, p.adjust.method = "bonferroni")
print(pairwise)

# Effect size (eta-squared)
ss <- summary(aov_result)[[1]]
eta_sq <- ss$`Sum Sq`[1] / sum(ss$`Sum Sq`)
cat(sprintf("\nEta-squared: %.3f (%.1f%% of variance explained by archetype)\n", eta_sq, eta_sq * 100))

# ── Tier the WNBA players by contribution ──

cat("\n=== WNBA SUCCESS TIERS ===\n\n")

wnba <- wnba %>%
  mutate(
    wnba_tier = case_when(
      wnba_total_contrib >= quantile(wnba_total_contrib, 0.80) ~ "Elite",
      wnba_total_contrib >= quantile(wnba_total_contrib, 0.50) ~ "Solid",
      TRUE ~ "Replacement"
    )
  )

tier_by_archetype <- wnba %>%
  count(archetype, wnba_tier) %>%
  pivot_wider(names_from = wnba_tier, values_from = n, values_fill = 0) %>%
  select(archetype, Elite, Solid, Replacement)

print(tier_by_archetype)

# ── College metrics that predict WNBA success ──

cat("\n=== CORRELATIONS: College Metrics vs WNBA Contribution ===\n\n")

metric_cols <- c("fga_per_game", "fg_pct", "two_pct", "three_pct",
                 "three_rate", "layup_rate", "ft_rate", "ft_pct",
                 "self_created", "pts_per_att")

correlations <- tibble(
  metric = metric_cols,
  r = map_dbl(metric_cols, ~ cor(wnba[[.x]], wnba$wnba_total_contrib, use = "complete.obs")),
  r_sq = r^2
) %>%
  arrange(desc(abs(r)))

correlations %>%
  mutate(
    direction = case_when(
      r > 0.1  ~ "more -> better WNBA",
      r < -0.1 ~ "more -> worse WNBA",
      TRUE     ~ "weak"
    ),
    r   = round(r, 3),
    r_sq = round(r_sq, 3)
  ) %>%
  print(n = Inf)

# ── Per-archetype college profile comparison ──

cat("\n=== COLLEGE PROFILE COMPARISON BY WNBA TIER ===\n\n")

tier_profiles <- wnba %>%
  group_by(wnba_tier) %>%
  summarise(
    n          = n(),
    fga_pg     = mean(fga_per_game),
    fg_pct     = mean(fg_pct),
    three_pct  = mean(three_pct),
    three_rate = mean(three_rate),
    layup_rate = mean(layup_rate),
    ft_rate    = mean(ft_rate),
    ft_pct     = mean(ft_pct),
    self_cr    = mean(self_created),
    pts_att    = mean(pts_per_att),
    .groups = "drop"
  ) %>%
  arrange(factor(wnba_tier, levels = c("Elite", "Solid", "Replacement")))

print(tier_profiles)

# ── Visualizations ──

# Box plot: WNBA contribution by college archetype
p1 <- ggplot(wnba, aes(x = reorder(archetype, -wnba_total_contrib), y = wnba_total_contrib, fill = archetype)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c(
    "Scoring Threat"    = "#2c5f7c",
    "Interior Presence" = "#c44536",
    "Specialist"        = "#e8a838"
  )) +
  labs(
    title = "WNBA Contribution by College Scoring Archetype",
    subtitle = sprintf("%d WNBA players classified by their college shooting profiles", nrow(wnba)),
    x = NULL,
    y = "WNBA Total Contribution"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("output/02_wnba_contribution_by_archetype.png", p1, width = 8, height = 6, dpi = 150)
cat("\nSaved: output/02_wnba_contribution_by_archetype.png\n")

# Scatter: college FGA/game vs WNBA contribution, colored by archetype
p2 <- ggplot(wnba, aes(x = fga_per_game, y = wnba_total_contrib, color = archetype)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  scale_color_manual(values = c(
    "Scoring Threat"    = "#2c5f7c",
    "Interior Presence" = "#c44536",
    "Specialist"        = "#e8a838"
  )) +
  labs(
    title = "College Volume vs WNBA Production",
    subtitle = "FGA/game in college vs total contribution in WNBA",
    x = "College FGA per Game",
    y = "WNBA Total Contribution",
    color = "Archetype"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("output/02_volume_vs_production.png", p2, width = 8, height = 6, dpi = 150)
cat("Saved: output/02_volume_vs_production.png\n")

# Top WNBA players table
cat("\n=== TOP 15 WNBA PLAYERS BY CONTRIBUTION ===\n\n")

wnba %>%
  arrange(desc(wnba_total_contrib)) %>%
  select(player_name, wnba_team, archetype, wnba_total_contrib, fga_per_game, fg_pct, three_rate, layup_rate) %>%
  head(15) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = 15, width = Inf)

# Save for downstream
write_csv(wnba, "data/wnba_validated.csv")
cat("\nSaved: data/wnba_validated.csv\n")
