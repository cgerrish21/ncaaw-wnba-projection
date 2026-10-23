# NCAAW-to-WNBA Scoring Archetype Projection

Classifying NCAA women's basketball players into scoring archetypes based on college shooting profiles and validating those archetypes against WNBA outcomes.

## What This Does

This project takes 4 seasons of NCAA women's basketball play-by-play shot data (~3.3 million shots), builds shooting profiles for every qualified Division I player, and classifies them into three scoring archetypes:

- **Scoring Threat** — attacks from all three levels (rim, perimeter, and free throw line)
- **Interior Presence** — rim-dominant scorers without a perimeter game
- **Specialist** — perimeter-only scorers missing at least one scoring level

The classification is validated against 83 current WNBA players whose college shooting data is available. Scoring Threats in college produce significantly more in the WNBA (avg contribution 10.1) than Interior Presence (9.8) or Specialists (4.7).

The system then applies WNBA-derived performance benchmarks to identify which current college players most closely match the profiles of successful WNBA players.

## Key Findings

| Archetype | WNBA Players | Avg Contribution | College Players |
|---|---|---|---|
| Scoring Threat | 41 | 10.1 | 1,411 |
| Interior Presence | 28 | 9.8 | 675 |
| Specialist | 14 | 4.7 | 771 |

- Three-level scoring ability in college is the strongest predictor of WNBA production
- College FGA/game (volume) correlates more strongly with WNBA success than shooting efficiency
- K-means clustering (k=3) largely recovers the same groupings as the threshold-based classification, suggesting the archetypes reflect real structure in the data

## Data

- **`data/college_profiles.csv`** — 2,857 qualified D1 players (200+ career FGA, active 2023-25, 75+ FGA in most recent season). Aggregated from 4 seasons of ESPN play-by-play data (2021-22 through 2024-25).
- **`data/wnba_college_profiles.csv`** — 83 current WNBA players with their college shooting profiles and WNBA production values (total contribution, PAR, scoring, assists, rebounds).

Each row contains: FGA/game, FG%, 2P%, 3P%, 3PA rate, layup rate, FT rate, FT%, self-created shot rate, points per attempt, plus team/conference metadata.

## Scripts

Run scripts in order from the project root directory. Each script reads output from the previous step.

| Script | Purpose |
|---|---|
| `R/01_classify.R` | Classify players into three archetypes using shooting profile thresholds |
| `R/02_validate.R` | Validate archetypes against WNBA outcomes (ANOVA, correlations, tier analysis) |
| `R/03_benchmarks.R` | Apply WNBA benchmarks to identify top prospects per archetype |
| `R/04_clustering.R` | K-means experiment: do archetypes emerge from unsupervised clustering? |
| `R/05_comps.R` | Find closest WNBA comparisons for each prospect via Euclidean distance |

## Requirements

```r
install.packages(c("tidyverse", "scales"))
```

## Classification Thresholds

The three scoring levels are defined by shot profile rates:

| Level | Metric | Threshold | Meaning |
|---|---|---|---|
| Rim | `layup_rate` | >= 15% | Takes meaningful attempts at the basket |
| Perimeter | `three_rate` | >= 15% | Takes meaningful 3-point attempts |
| Line | `ft_rate` | >= 20% | Draws fouls, gets to the free throw line |

A **Scoring Threat** checks all three boxes. This means at least 15% of their field goal attempts are layups/dunks, at least 15% are three-pointers, and they generate free throw attempts at 20%+ of their FGA. These players can score from anywhere and force defenses to guard the full court.

## WNBA Benchmarks

Each archetype has performance benchmarks derived from the top half of WNBA players in that archetype:

**Scoring Threat:** FGA/gm >= 10, FG% >= 43%, 3P% >= 32%, FT% >= 75%, FT rate >= 25%

**Interior Presence:** FGA/gm >= 9, FG% >= 50%, FT rate >= 35%, FT% >= 68%

**Specialist:** 3P% >= 34%, FT% >= 78%, 2P% >= 43%, FG% >= 40%

Prospects are ranked by how many benchmarks they clear, then by volume (FGA/game) as a tiebreaker.

## WNBA Comparisons

Player comparisons use Euclidean distance across 10 shooting metrics, all z-scored against the WNBA population:

`fga_per_game, fg_pct, two_pct, three_pct, three_rate, layup_rate, ft_rate, ft_pct, self_created, pts_per_att`

This surfaces the WNBA players whose college shooting profiles most closely resembled a given prospect's current profile.

## Methodology Source

This is the analytical framework behind the shooting archetype articles published on [Draft Chalkboard](https://draftchalkboard.com):

1. [Shooting Archetypes: A Framework for WNBA Draft Evaluation](https://draftchalkboard.com/ncaaw/articles/shooting-archetypes)
2. [College Archetypes: Major Conference Prospects](https://draftchalkboard.com/ncaaw/articles/college-archetypes)
3. [Small School Archetypes: Finding Prospects Outside the Power Conferences](https://draftchalkboard.com/ncaaw/articles/small-school-archetypes)

## Author

Christopher Gerrish
- [Draft Chalkboard](https://draftchalkboard.com) — women's sports prospect intelligence
- [Soccer Dossier](https://soccerdossier.com) — NWSL scouting
