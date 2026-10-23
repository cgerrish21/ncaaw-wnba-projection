# ============================================================================
# 01_classify.R — Archetype Classification
#
# Classifies NCAA women's basketball players into three scoring archetypes
# based on their college shooting profiles:
#   - Scoring Threat:    attacks from all three levels (rim + perimeter + line)
#   - Interior Presence: rim-dominant scorers without a perimeter game
#   - Specialist:        perimeter-only scorers missing at least one level
#
# The classification uses three shot profile thresholds derived from the
# population distribution of ~2,800 qualified D1 players.
# ============================================================================

library(tidyverse)

# ── Load data ──

college <- read_csv("data/college_profiles.csv", show_col_types = FALSE)
wnba    <- read_csv("data/wnba_college_profiles.csv", show_col_types = FALSE)

cat(sprintf("College players: %d\n", nrow(college)))
cat(sprintf("WNBA players with college data: %d\n\n", nrow(wnba)))

# ── Classification function ──
#
# Thresholds:
#   has_rim:   layup_rate >= 15% (takes meaningful attempts at the basket)
#   has_three: three_rate >= 15% (takes meaningful 3PA as share of FGA)
#   has_line:  ft_rate >= 20%    (draws fouls, gets to the free throw line)
#
# A Scoring Threat checks all three boxes, or at minimum has both perimeter
# and free throw line presence. Interior Presence has rim without perimeter.
# Specialist has perimeter without rim.

classify_archetype <- function(layup_rate, three_rate, ft_rate) {
  has_rim   <- layup_rate >= 0.15
  has_three <- three_rate >= 0.15
  has_line  <- ft_rate >= 0.20

  case_when(
    has_rim & has_three & has_line ~ "Scoring Threat",
    has_three & has_line           ~ "Scoring Threat",
    has_rim & !has_three           ~ "Interior Presence",
    has_three & !has_rim           ~ "Specialist",
    has_rim & has_three            ~ "Specialist",
    three_rate > layup_rate        ~ "Specialist",
    TRUE                           ~ "Interior Presence"
  )
}

# ── Apply classification ──

college <- college %>%
  mutate(archetype = classify_archetype(layup_rate, three_rate, ft_rate))

wnba <- wnba %>%
  mutate(archetype = classify_archetype(layup_rate, three_rate, ft_rate))

# ── Distribution ──

cat("=== ARCHETYPE DISTRIBUTION (College) ===\n\n")

college %>%
  count(archetype) %>%
  mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  arrange(desc(n)) %>%
  print()

cat("\n=== ARCHETYPE DISTRIBUTION (WNBA) ===\n\n")

wnba %>%
  count(archetype) %>%
  mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  arrange(desc(n)) %>%
  print()

# ── P5 vs non-P5 breakdown ──

cat("\n=== P5 vs NON-P5 ===\n\n")

college %>%
  mutate(level = if_else(is_p5 == 1, "P5", "Non-P5")) %>%
  count(level, archetype) %>%
  pivot_wider(names_from = level, values_from = n, values_fill = 0) %>%
  print()

# ── Threshold population analysis ──
#
# Where do the 15%/15%/20% thresholds sit in the actual population?

cat("\n=== THRESHOLD POPULATION CONTEXT ===\n\n")

thresholds <- tibble(
  metric    = c("layup_rate", "three_rate", "ft_rate"),
  threshold = c(0.15, 0.15, 0.20),
  pctile    = c(
    mean(college$layup_rate < 0.15) * 100,
    mean(college$three_rate < 0.15) * 100,
    mean(college$ft_rate < 0.20) * 100
  )
)

thresholds %>%
  mutate(
    label = sprintf("%s >= %.0f%% excludes %.1f%% of players",
                    metric, threshold * 100, pctile)
  ) %>%
  select(label) %>%
  pull() %>%
  walk(~ cat(.x, "\n"))

# ── Visualizations ──

# Shot profile scatter: layup_rate vs three_rate, colored by archetype
p1 <- ggplot(college, aes(x = three_rate, y = layup_rate, color = archetype)) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_vline(xintercept = 0.15, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c(
    "Scoring Threat"    = "#2c5f7c",
    "Interior Presence" = "#c44536",
    "Specialist"        = "#e8a838"
  )) +
  labs(
    title = "NCAA Women's Basketball: Scoring Archetypes",
    subtitle = sprintf("%s qualified D1 players (200+ career FGA, 4 seasons)", scales::comma(nrow(college))),
    x = "Three-Point Attempt Rate (3PA / FGA)",
    y = "Layup Rate (Layups / FGA)",
    color = "Archetype"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("output/01_archetype_scatter.png", p1, width = 8, height = 6, dpi = 150)
cat("\nSaved: output/01_archetype_scatter.png\n")

# Distribution of free throw rate by archetype
p2 <- ggplot(college, aes(x = ft_rate, fill = archetype)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0.20, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c(
    "Scoring Threat"    = "#2c5f7c",
    "Interior Presence" = "#c44536",
    "Specialist"        = "#e8a838"
  )) +
  labs(
    title = "Free Throw Rate Distribution by Archetype",
    x = "FT Rate (FTA / FGA)",
    y = "Density",
    fill = "Archetype"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("output/01_ft_rate_density.png", p2, width = 8, height = 5, dpi = 150)
cat("Saved: output/01_ft_rate_density.png\n")

# Save classified data for downstream scripts
write_csv(college, "data/college_classified.csv")
write_csv(wnba, "data/wnba_classified.csv")
cat("\nSaved classified data for downstream scripts.\n")
