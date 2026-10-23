# ============================================================================
# 03_benchmarks.R — WNBA Benchmark Matching
#
# Applies WNBA-derived performance benchmarks to college players to identify
# prospects whose shooting profiles match WNBA success patterns.
#
# Each archetype has its own benchmarks based on what separates the top half
# of WNBA players in that archetype from the bottom half:
#
#   Scoring Threat:    FGA/gm >= 10, FG% >= 43%, 3P% >= 32%, FT% >= 75%, FTrt >= 25%
#   Interior Presence: FGA/gm >= 9, FG% >= 50%, FTrt >= 35%, FT% >= 68%
#   Specialist:        3P% >= 34%, FT% >= 78%, 2P% >= 43%, FG% >= 40%
#
# Players are ranked by how many benchmarks they clear ("match flags").
# ============================================================================

library(tidyverse)

# ── Load classified data ──

college <- read_csv("data/college_classified.csv", show_col_types = FALSE)

cat(sprintf("College prospects: %d\n\n", nrow(college)))

# ── Define benchmarks per archetype ──

get_match_flags <- function(df) {
  df %>%
    mutate(
      flag_1 = case_when(
        archetype == "Scoring Threat"    ~ fga_per_game >= 10,
        archetype == "Interior Presence" ~ fga_per_game >= 9,
        archetype == "Specialist"        ~ three_pct >= 0.34
      ),
      flag_2 = case_when(
        archetype == "Scoring Threat"    ~ fg_pct >= 0.43,
        archetype == "Interior Presence" ~ fg_pct >= 0.50,
        archetype == "Specialist"        ~ ft_pct >= 0.78
      ),
      flag_3 = case_when(
        archetype == "Scoring Threat"    ~ three_pct >= 0.32,
        archetype == "Interior Presence" ~ ft_rate >= 0.35,
        archetype == "Specialist"        ~ two_pct >= 0.43
      ),
      flag_4 = case_when(
        archetype == "Scoring Threat"    ~ ft_pct >= 0.75,
        archetype == "Interior Presence" ~ ft_pct >= 0.68,
        archetype == "Specialist"        ~ fg_pct >= 0.40
      ),
      flag_5 = case_when(
        archetype == "Scoring Threat"    ~ ft_rate >= 0.25,
        archetype == "Interior Presence" ~ NA,
        archetype == "Specialist"        ~ NA
      ),
      match_count = rowSums(across(starts_with("flag_"), ~ replace_na(.x, FALSE))),
      match_total = case_when(
        archetype == "Scoring Threat"    ~ 5L,
        archetype == "Interior Presence" ~ 4L,
        archetype == "Specialist"        ~ 4L
      )
    )
}

college <- get_match_flags(college)

# ── Summary: benchmark clearing rates ──

cat("=== BENCHMARK CLEARING RATES ===\n\n")

college %>%
  group_by(archetype) %>%
  summarise(
    n = n(),
    avg_flags = mean(match_count),
    cleared_all = sum(match_count == match_total),
    cleared_all_pct = scales::percent(cleared_all / n, accuracy = 0.1),
    cleared_none = sum(match_count == 0),
    .groups = "drop"
  ) %>%
  print()

# ── Match count distribution ──

cat("\n=== MATCH COUNT DISTRIBUTION ===\n\n")

college %>%
  count(archetype, match_count) %>%
  group_by(archetype) %>%
  mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  ungroup() %>%
  print(n = Inf)

# ── Top prospects per archetype ──

print_top_prospects <- function(df, arch, n = 15) {
  cat(sprintf("\n=== TOP %d %s PROSPECTS (by benchmark flags, then FGA/gm) ===\n\n", n, toupper(arch)))

  top <- df %>%
    filter(archetype == arch) %>%
    arrange(desc(match_count), desc(fga_per_game)) %>%
    head(n)

  top %>%
    mutate(
      level = if_else(is_p5 == 1, "P5", ""),
      flags = sprintf("%d/%d", match_count, match_total)
    ) %>%
    select(player_name, team, conference, level, games, flags,
           fga_per_game, fg_pct, three_pct, ft_pct, ft_rate) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    print(n = n, width = Inf)
}

for (arch in c("Scoring Threat", "Interior Presence", "Specialist")) {
  print_top_prospects(college, arch)
}

# ── P5 vs Non-P5 prospect quality ──

cat("\n=== P5 vs NON-P5 PROSPECT QUALITY ===\n\n")

college %>%
  mutate(level = if_else(is_p5 == 1, "P5", "Non-P5")) %>%
  group_by(level, archetype) %>%
  summarise(
    n = n(),
    avg_flags = round(mean(match_count), 2),
    cleared_majority = sum(match_count >= match_total / 2),
    pct_majority = scales::percent(cleared_majority / n, accuracy = 0.1),
    .groups = "drop"
  ) %>%
  print()

# ── Visualizations ──

# Bar chart: match count distribution by archetype
p1 <- college %>%
  count(archetype, match_count) %>%
  group_by(archetype) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(match_count), y = pct, fill = archetype)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ archetype, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Scoring Threat"    = "#2c5f7c",
    "Interior Presence" = "#c44536",
    "Specialist"        = "#e8a838"
  )) +
  labs(
    title = "WNBA Benchmark Match Distribution",
    subtitle = "How many WNBA-validated benchmarks each college player clears",
    x = "Benchmarks Cleared",
    y = "Share of Players"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("output/03_benchmark_distribution.png", p1, width = 10, height = 5, dpi = 150)
cat("\nSaved: output/03_benchmark_distribution.png\n")

# Scatter: FGA/game vs FG% with benchmark flags as size
p2 <- college %>%
  filter(archetype == "Scoring Threat") %>%
  ggplot(aes(x = fga_per_game, y = fg_pct, color = factor(match_count), size = match_count)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0.43, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "gray50") +
  scale_color_viridis_d(option = "D", direction = -1) +
  scale_size_continuous(range = c(1, 4)) +
  labs(
    title = "Scoring Threat Prospects: Volume vs Efficiency",
    subtitle = "Dashed lines = WNBA benchmarks (FGA/gm >= 10, FG% >= 43%)",
    x = "FGA per Game",
    y = "Field Goal %",
    color = "Flags",
    size = "Flags"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/03_scoring_threat_scatter.png", p2, width = 8, height = 6, dpi = 150)
cat("Saved: output/03_scoring_threat_scatter.png\n")

# Save benchmarked data
write_csv(college, "data/college_benchmarked.csv")
cat("Saved: data/college_benchmarked.csv\n")
