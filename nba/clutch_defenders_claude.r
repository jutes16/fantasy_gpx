library(tidyverse)
library(hoopR)
library(janitor)
library(glue)
library(ggrepel)
library(ggimage)

# ============== CONFIGURATION ==============
season_year <- 2023
include_playoffs <- FALSE
clutch_window <- "Last 2 Minutes"  # Options: "Last 5 Minutes", "Last 4 Minutes", etc.
score_margin <- 5                   # Within +/- N points
per_mode <- "Totals"               # "Totals" or "Per100Possessions"
min_clutch_minutes <- 10           # Minimum clutch minutes to qualify
min_clutch_games <- 10             # Minimum clutch games to qualify
compare_mode <- "delta"            # "absolute" or "delta" (clutch minus regular)
save_plot <- TRUE
# ===========================================

# Helper functions
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

normalize_abbr <- function(x) {
  dplyr::recode(
    x,
    "NY" = "NYK", "NO" = "NOP", "GS" = "GSW",
    "SA" = "SAS", "UTAH" = "UTA", "WSH" = "WAS",
    .default = x
  )
}

# Safely parse numeric columns
parse_numeric_cols <- function(df, cols) {
  df %>%
    mutate(across(
      any_of(cols),
      ~ readr::parse_number(as.character(.x))
    ))
}

# Calculate defensive impact score
calculate_defensive_impact <- function(df) {
  df %>%
    mutate(
      # Defensive impact: lower def_rating is better, so we invert it
      # Also factor in PIE (overall contribution) and net rating
      def_impact_base = 120 - def_rating,  # Higher = better defense
      
      # Weighted defensive impact score
      defensive_impact = (
        def_impact_base * 0.4 +           # Base defensive rating (40%)
        net_rating * 0.3 +                # Net rating contribution (30%)
        pie * 100 * 0.3                   # PIE contribution (30%)
      ),
      
      # Per-minute impact (normalize by playing time)
      defensive_impact_per_min = defensive_impact / pmax(min, 1)
    )
}

# Calculate clutch performance delta vs regular season
calculate_clutch_delta <- function(clutch_df, season_year, per_mode) {
  # Pull regular season stats
  reg_res <- hoopR::nba_leaguedashplayerstats(
    season = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    per_mode = per_mode
  )
  
  regular <- reg_res$LeagueDashPlayerStats %>%
    janitor::clean_names() %>%
    parse_numeric_cols(c("off_rating", "def_rating", "net_rating", "pie")) %>%
    dplyr::select(
      player_id,
      off_rating_reg = off_rating,
      def_rating_reg = def_rating,
      net_rating_reg = net_rating,
      pie_reg = pie
    )
  
  # Join and calculate deltas
  clutch_df %>%
    left_join(regular, by = "player_id") %>%
    mutate(
      # Delta calculations (clutch - regular)
      delta_off = off_rating - off_rating_reg,
      # For defense: regular - clutch (so positive = better defense in clutch)
      delta_def = def_rating_reg - def_rating,
      delta_net = net_rating - net_rating_reg,
      delta_pie = pie - pie_reg,
      
      # Clutch improvement score (higher = more improvement in clutch)
      clutch_improvement = (
        delta_net * 0.4 +                 # Net rating improvement (40%)
        delta_def * 0.3 +                 # Defensive improvement (30%)
        delta_pie * 100 * 0.3             # PIE improvement (30%)
      )
    )
}

# Main analysis
season_types <- if (include_playoffs) {
  c("Regular Season", "Playoffs")
} else {
  "Regular Season"
}

# Team lookup
team_lookup <- load_nba_team_box(season = season_year) %>%
  distinct(team_id, team_abbreviation, team_display_name, team_logo) %>%
  mutate(
    team_name = stringr::str_trim(team_display_name),
    team_abbreviation = normalize_abbr(team_abbreviation)
  )

# Pull clutch stats for each season type
message("Fetching clutch statistics...")
pull_clutch_one <- function(st) {
  res <- hoopR::nba_leaguedashplayerclutch(
    season = season_year,
    season_type = st,
    per_mode = per_mode,
    measure_type = "Advanced",
    clutch_time = clutch_window,
    ahead_behind = "Ahead or Behind",
    point_diff = score_margin,
    pace_adjust = "N",
    plus_minus = "N"
  )
  
  res$LeagueDashPlayerClutch %>%
    janitor::clean_names() %>%
    mutate(season_type = st)
}

clutch <- purrr::map_dfr(season_types, pull_clutch_one)

# Process clutch data
numeric_cols <- c("min", "off_rating", "def_rating", "net_rating", "gp", "pie")

clutch_processed <- clutch %>%
  parse_numeric_cols(numeric_cols) %>%
  mutate(team_abbreviation = normalize_abbr(team_abbreviation)) %>%
  left_join(
    team_lookup %>% select(team_abbreviation, team_logo, team_name),
    by = "team_abbreviation"
  ) %>%
  # Filter for meaningful sample sizes
  filter(
    !is.na(def_rating),
    !is.na(off_rating),
    !is.na(min),
    !is.na(gp),
    min >= min_clutch_minutes,
    gp >= min_clutch_games
  ) %>%
  # Calculate defensive impact
  calculate_defensive_impact()

message(glue(
  "Filtered to ≥{min_clutch_minutes} clutch minutes and ≥{min_clutch_games} games. ",
  "Players remaining: {nrow(clutch_processed)}"
))

# Calculate deltas if in delta mode
if (compare_mode == "delta") {
  message("Calculating clutch vs regular season deltas...")
  scatter_df <- calculate_clutch_delta(clutch_processed, season_year, per_mode)
} else {
  scatter_df <- clutch_processed
}

# Identify top performers
# Top 5 by Net Rating
top5_net_ids <- scatter_df %>%
  arrange(desc(net_rating)) %>%
  slice_head(n = 5) %>%
  pull(player_id)

# Top 5 by Defensive Impact
top5_def_ids <- scatter_df %>%
  arrange(desc(defensive_impact)) %>%
  slice_head(n = 5) %>%
  pull(player_id)

# Top 5 by clutch improvement (if in delta mode)
if (compare_mode == "delta") {
  top5_improvement_ids <- scatter_df %>%
    arrange(desc(clutch_improvement)) %>%
    slice_head(n = 5) %>%
    pull(player_id)
} else {
  top5_improvement_ids <- integer(0)
}

# Categorize players for visualization
scatter_df <- scatter_df %>%
  mutate(
    is_top5_net = player_id %in% top5_net_ids,
    is_top5_def = player_id %in% top5_def_ids,
    is_top5_improvement = player_id %in% top5_improvement_ids,
    category = dplyr::case_when(
      compare_mode == "delta" & is_top5_improvement ~ "Top 5 Clutch Improvement",
      is_top5_net & is_top5_def ~ "Top 5 (Net & Defense)",
      is_top5_net ~ "Top 5 Net Rating",
      is_top5_def ~ "Top 5 Defense",
      TRUE ~ "Others"
    )
  )

# Set up plot variables based on mode
if (compare_mode == "delta") {
  x_var <- "delta_off"
  y_var <- "delta_def"
  
  x_range <- max(abs(scatter_df[[x_var]]), na.rm = TRUE)
  y_range <- max(abs(scatter_df[[y_var]]), na.rm = TRUE)
  x_pad <- ceiling(x_range / 5) * 5
  y_pad <- ceiling(y_range / 5) * 5
  x_lim <- c(-x_pad, x_pad)
  y_lim <- c(-y_pad, y_pad)
  
  x_label <- "Δ Off Rating (Clutch − Regular)"
  y_label <- "Δ Def Rating (Regular − Clutch; + = better D in clutch)"
  
} else {
  x_var <- "off_rating"
  y_var <- "def_rating"
  
  x_min <- floor(min(scatter_df[[x_var]], na.rm = TRUE) / 5) * 5
  x_max <- ceiling(max(scatter_df[[x_var]], na.rm = TRUE) / 5) * 5
  y_min <- floor(min(scatter_df[[y_var]], na.rm = TRUE) / 5) * 5
  y_max <- ceiling(max(scatter_df[[y_var]], na.rm = TRUE) / 5) * 5
  x_lim <- c(x_min, x_max)
  y_lim <- c(y_max, y_min)
  
  x_label <- "Clutch Offensive Rating (per 100 poss)"
  y_label <- "Clutch Defensive Rating (lower is better)"
}

# Create visualization
p <- ggplot(scatter_df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
  geom_point(aes(size = min, alpha = defensive_impact, color = category)) +
  
  # Labels for top performers
  ggrepel::geom_text_repel(
    data = scatter_df %>% filter(category != "Others"),
    aes(label = glue("{player_name} ({team_abbreviation})")),
    size = 3.2,
    fontface = "bold",
    segment.color = "grey60",
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.25
  ) +
  
  scale_size_continuous(
    name = "Clutch Minutes",
    range = c(2, 12)
  ) +
  scale_alpha_continuous(
    name = "Defensive Impact",
    range = c(0.3, 0.95)
  ) +
  scale_color_manual(
    name = "Highlight",
    values = c(
      "Others" = "grey75",
      "Top 5 Net Rating" = "#1F78B4",
      "Top 5 Defense" = "#33A02C",
      "Top 5 (Net & Defense)" = "#E31A1C",
      "Top 5 Clutch Improvement" = "#FF7F00"
    )
  ) +
  
  # Y-axis: reversed for absolute, symmetric for delta
  {
    if (compare_mode == "delta") {
      scale_y_continuous(limits = y_lim)
    } else {
      scale_y_reverse(limits = y_lim)
    }
  } +
  
  coord_cartesian(xlim = x_lim) +
  
  # Reference lines for delta mode
  {
    if (compare_mode == "delta") {
      list(
        geom_hline(yintercept = 0, color = "grey65", linewidth = 0.6),
        geom_vline(xintercept = 0, color = "grey65", linewidth = 0.6)
      )
    }
  } +
  
  labs(
    title = glue("{season_year} Clutch Performance: {clutch_window} (±{score_margin} pts)"),
    subtitle = glue(
      "{if (compare_mode == 'delta') 'Δ vs Regular Season • ' else ''}",
      "≥{min_clutch_minutes} clutch min & ≥{min_clutch_games} games • ",
      "{per_mode} • size ∝ minutes, alpha ∝ defensive impact"
    ),
    x = x_label,
    y = y_label,
    caption = "Defensive Impact = f(defensive rating, net rating, PIE) | Data: hoopR"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(p)

# Display summary statistics
cat("\n=== Top 10 Clutch Defenders (by Defensive Impact) ===\n")
scatter_df %>%
  arrange(desc(defensive_impact)) %>%
  select(player_name, team_abbreviation, gp, min, 
         def_rating, net_rating, pie, defensive_impact) %>%
  head(10) %>%
  print()

if (compare_mode == "delta") {
  cat("\n\n=== Top 10 Clutch Improvers (vs Regular Season) ===\n")
  scatter_df %>%
    arrange(desc(clutch_improvement)) %>%
    select(player_name, team_abbreviation, 
           delta_off, delta_def, delta_net, clutch_improvement) %>%
    head(10) %>%
    print()
}

# Save plot
if (save_plot) {
  safe_window <- gsub("[^A-Za-z0-9]+", "_", clutch_window)
  file_name <- glue(
    "nba/plots/{season_year}_clutch_{safe_window}_pm{score_margin}_{per_mode}_{compare_mode}_improved.png"
  )
  ggsave(
    filename = file_name,
    plot = p,
    width = 12,
    height = 8,
    dpi = 300,
    bg = "white"
  )
  message(glue("Saved plot to {file_name}"))
  
  # Save data
  csv_name <- glue(
    "nba/data/{season_year}_clutch_{safe_window}_pm{score_margin}_{compare_mode}_data.csv"
  )
  write_csv(scatter_df, csv_name)
  message(glue("Saved data to {csv_name}"))
}

