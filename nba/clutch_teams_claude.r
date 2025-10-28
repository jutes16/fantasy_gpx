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

# Calculate team defensive impact score
calculate_team_defensive_impact <- function(df) {
  df %>%
    mutate(
      # Defensive impact: lower def_rating is better, so we invert it
      def_impact_base = 120 - def_rating,
      
      # Weighted defensive impact score
      defensive_impact = (
        def_impact_base * 0.5 +           # Base defensive rating (50%)
        net_rating * 0.5                  # Net rating contribution (50%)
      ),
      
      # Win percentage in clutch situations (if available)
      clutch_win_pct = w / gp
    )
}

# Calculate clutch performance delta vs regular season
calculate_team_clutch_delta <- function(clutch_df, season_year, per_mode) {
  # Pull regular season stats
  reg_res <- hoopR::nba_leaguedashteamstats(
    season = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    per_mode = per_mode
  )
  
  regular <- reg_res$LeagueDashTeamStats %>%
    janitor::clean_names() %>%
    parse_numeric_cols(c("off_rating", "def_rating", "net_rating")) %>%
    dplyr::select(
      team_id,
      off_rating_reg = off_rating,
      def_rating_reg = def_rating,
      net_rating_reg = net_rating
    )
  
  # Join and calculate deltas
  clutch_df %>%
    left_join(regular, by = "team_id") %>%
    mutate(
      # Delta calculations (clutch - regular)
      delta_off = off_rating - off_rating_reg,
      # For defense: regular - clutch (so positive = better defense in clutch)
      delta_def = def_rating_reg - def_rating,
      delta_net = net_rating - net_rating_reg,
      
      # Clutch improvement score (higher = more improvement in clutch)
      clutch_improvement = (
        delta_net * 0.5 +                 # Net rating improvement (50%)
        delta_def * 0.5                   # Defensive improvement (50%)
      )
    )
}

# Main analysis
season_types <- if (include_playoffs) {
  c("Regular Season", "Playoffs")
} else {
  "Regular Season"
}

#
# Team lookup (local mapping to avoid remote failures)
team_ref <- tibble::tribble(
  ~team_abbreviation, ~team_name,                   ~espn_slug,
  "ATL",              "Atlanta Hawks",              "atl",
  "BOS",              "Boston Celtics",             "bos",
  "BKN",              "Brooklyn Nets",              "bkn",
  "CHA",              "Charlotte Hornets",          "cha",
  "CHI",              "Chicago Bulls",              "chi",
  "CLE",              "Cleveland Cavaliers",        "cle",
  "DAL",              "Dallas Mavericks",           "dal",
  "DEN",              "Denver Nuggets",             "den",
  "DET",              "Detroit Pistons",            "det",
  "GSW",              "Golden State Warriors",      "gsw",
  "HOU",              "Houston Rockets",            "hou",
  "IND",              "Indiana Pacers",             "ind",
  "LAC",              "LA Clippers",                "lac",
  "LAL",              "Los Angeles Lakers",         "lal",
  "MEM",              "Memphis Grizzlies",          "mem",
  "MIA",              "Miami Heat",                 "mia",
  "MIL",              "Milwaukee Bucks",            "mil",
  "MIN",              "Minnesota Timberwolves",     "min",
  "NOP",              "New Orleans Pelicans",       "no",
  "NYK",              "New York Knicks",            "ny",
  "OKC",              "Oklahoma City Thunder",      "okc",
  "ORL",              "Orlando Magic",              "orl",
  "PHI",              "Philadelphia 76ers",         "phi",
  "PHX",              "Phoenix Suns",               "phx",
  "POR",              "Portland Trail Blazers",     "por",
  "SAC",              "Sacramento Kings",           "sac",
  "SAS",              "San Antonio Spurs",          "sa",
  "TOR",              "Toronto Raptors",            "tor",
  "UTA",              "Utah Jazz",                  "uta",
  "WAS",              "Washington Wizards",         "wsh"
) %>%
  mutate(
    team_logo = paste0("https://a.espncdn.com/i/teamlogos/nba/500/", espn_slug, ".png")
  ) %>%
  select(team_abbreviation, team_name, team_logo)

# Build team/logo lookup from hoopR (fallback to static map if remote unavailable)
team_lookup <- tryCatch({
  hoopR::load_nba_team_box(season = season_year) %>%
    dplyr::distinct(team_id, team_abbreviation, team_display_name, team_logo) %>%
    dplyr::mutate(
      team_name = stringr::str_trim(team_display_name),
      team_abbreviation = normalize_abbr(team_abbreviation)
    ) %>%
    dplyr::select(team_id, team_abbreviation, team_name, team_logo)
}, error = function(e) {
  NULL
})

if (is.null(team_lookup) || nrow(team_lookup) == 0) {
  team_lookup <- team_ref
}

#
# Team colors
team_colors <- c(
  ATL = "#E03A3E", BOS = "#007A33", BKN = "#000000", CHA = "#1D1160",
  CHI = "#CE1141", CLE = "#860038", DAL = "#00538C", DEN = "#0E2240",
  DET = "#C8102E", GSW = "#1D428A", HOU = "#CE1141", IND = "#002D62",
  LAC = "#C8102E", LAL = "#552583", MEM = "#5D76A9", MIA = "#98002E",
  MIL = "#00471B", MIN = "#0C2340", NOP = "#0C2340", NYK = "#006BB6",
  OKC = "#007AC1", ORL = "#0077C0", PHI = "#006BB6", PHX = "#1D1160",
  POR = "#E03A3E", SAC = "#5A2D81", SAS = "#C4CED4", TOR = "#CE1141",
  UTA = "#002B5C", WAS = "#002B5C"
)

# Pull clutch stats for each season type
message("Fetching team clutch statistics...")
pull_team_clutch_one <- function(st) {
  res <- hoopR::nba_leaguedashteamclutch(
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
  
  res$LeagueDashTeamClutch %>%
    janitor::clean_names() %>%
    mutate(season_type = st)
}

clutch <- purrr::map_dfr(season_types, pull_team_clutch_one)

# Process clutch data
numeric_cols <- c("off_rating", "def_rating", "net_rating", "gp", "w", "l")

clutch_processed <- clutch %>%
  parse_numeric_cols(numeric_cols) %>%
  # Add team_abbreviation & logo by team_name from our local mapping
  left_join(
    team_lookup,
    by = "team_name"
  ) %>%
  # Normalize any 2-letter variants that may appear later
  mutate(team_abbreviation = normalize_abbr(team_abbreviation)) %>%
  # Filter for meaningful sample sizes
  filter(
    !is.na(def_rating),
    !is.na(off_rating),
    !is.na(gp),
    gp >= min_clutch_games
  ) %>%
  # Calculate defensive impact
  calculate_team_defensive_impact()

message(glue(
  "Filtered to ≥{min_clutch_games} clutch games. ",
  "Teams remaining: {nrow(clutch_processed)}"
))

# Calculate deltas if in delta mode
if (compare_mode == "delta") {
  message("Calculating clutch vs regular season deltas...")
  scatter_df <- calculate_team_clutch_delta(clutch_processed, season_year, per_mode)
} else {
  scatter_df <- clutch_processed
}

# Identify top and bottom performers
# Top 5 by Net Rating
top5_net_ids <- scatter_df %>%
  arrange(desc(net_rating)) %>%
  slice_head(n = 5) %>%
  pull(team_id)

# Bottom 5 by Net Rating
bottom5_net_ids <- scatter_df %>%
  arrange(net_rating) %>%
  slice_head(n = 5) %>%
  pull(team_id)

# Top 5 by Defensive Impact
top5_def_ids <- scatter_df %>%
  arrange(desc(defensive_impact)) %>%
  slice_head(n = 5) %>%
  pull(team_id)

# Top 5 by clutch improvement (if in delta mode)
if (compare_mode == "delta") {
  top5_improvement_ids <- scatter_df %>%
    arrange(desc(clutch_improvement)) %>%
    slice_head(n = 5) %>%
    pull(team_id)
  
  bottom5_improvement_ids <- scatter_df %>%
    arrange(clutch_improvement) %>%
    slice_head(n = 5) %>%
    pull(team_id)
} else {
  top5_improvement_ids <- integer(0)
  bottom5_improvement_ids <- integer(0)
}

# Categorize teams for visualization
scatter_df <- scatter_df %>%
  mutate(
    is_top_performer = if (compare_mode == "delta") {
      team_id %in% top5_improvement_ids
    } else {
      team_id %in% top5_net_ids
    },
    is_bottom_performer = if (compare_mode == "delta") {
      team_id %in% bottom5_improvement_ids
    } else {
      team_id %in% bottom5_net_ids
    },
    category = dplyr::case_when(
      is_top_performer ~ "Top Performers",
      is_bottom_performer ~ "Bottom Performers",
      TRUE ~ "Others"
    )
  )

# Set up plot variables based on mode
if (compare_mode == "delta") {
  x_var <- "delta_off"
  y_var <- "delta_def"
  
  x_range <- max(abs(scatter_df[[x_var]]), na.rm = TRUE)
  y_range <- max(abs(scatter_df[[y_var]]), na.rm = TRUE)
  x_pad <- ceiling(x_range / 2) * 2
  y_pad <- ceiling(y_range / 2) * 2
  x_lim <- c(-x_pad, x_pad)
  y_lim <- c(-y_pad, y_pad)
  
  x_label <- "Δ Off Rating (Clutch − Regular)"
  y_label <- "Δ Def Rating (Regular − Clutch; + = better D in clutch)"
  subtitle_metric <- "Top/Bottom 5 by Clutch Improvement"
  
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
  subtitle_metric <- "Top/Bottom 5 by Net Rating"
}

# Create visualization with logos
label_teams <- scatter_df %>%
  filter(category != "Others")

p <- ggplot(scatter_df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
  
  # All teams as points
  geom_point(
    aes(color = category, size = gp),
    alpha = 0.6
  ) +
  
  # Plot team logos using template-style join
  ggimage::geom_image(
    data = scatter_df %>%
      dplyr::left_join(team_lookup %>% dplyr::select(team_name, team_logo, team_abbreviation), by = "team_name") %>%
      dplyr::filter(!is.na(team_logo)),
    aes(x = .data[[x_var]], y = .data[[y_var]], image = team_logo),
    size = 0.08,
    inherit.aes = FALSE
  ) +
  
  # Labels for top/bottom performers
  ggrepel::geom_text_repel(
    data = label_teams,
    aes(label = team_abbreviation, color = category),
    size = 4,
    fontface = "bold",
    segment.color = "grey60",
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3,
    min.segment.length = 0.1
  ) +
  
  scale_size_continuous(
    name = "Clutch Games",
    range = c(3, 10)
  ) +
  
  scale_color_manual(
    name = "Performance",
    values = c(
      "Others" = "grey70",
      "Top Performers" = "#2E7D32",
      "Bottom Performers" = "#C62828"
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
  
  # Reference lines for delta mode (4 quadrants)
  {
    if (compare_mode == "delta") {
      list(
        geom_hline(yintercept = 0, color = "grey50", linewidth = 0.8, linetype = "dashed"),
        geom_vline(xintercept = 0, color = "grey50", linewidth = 0.8, linetype = "dashed"),
        # Quadrant labels
        annotate("text", x = x_lim[2] * 0.85, y = y_lim[2] * 0.85, 
                label = "Better\nOffense & Defense", 
                color = "darkgreen", fontface = "bold", size = 3.5, alpha = 0.6),
        annotate("text", x = x_lim[1] * 0.85, y = y_lim[1] * 0.85, 
                label = "Worse\nOffense & Defense", 
                color = "darkred", fontface = "bold", size = 3.5, alpha = 0.6)
      )
    }
  } +
  
  labs(
    title = glue("{season_year} Team Clutch Performance: {clutch_window} (±{score_margin} pts)"),
    subtitle = glue(
      "{if (compare_mode == 'delta') 'Δ vs Regular Season • ' else ''}",
      "{subtitle_metric} • ≥{min_clutch_games} games • {per_mode}"
    ),
    x = x_label,
    y = y_label,
    caption = "Size ∝ clutch games played | Data: hoopR"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


print(p)

# Display summary statistics
if (compare_mode == "delta") {
  cat("\n=== Top 10 Teams: Most Clutch Improvement ===\n")
  scatter_df %>%
    arrange(desc(clutch_improvement)) %>%
    select(team_abbreviation, gp, w, l, clutch_win_pct,
           delta_off, delta_def, delta_net, clutch_improvement) %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    head(10) %>%
    print()
  
  cat("\n\n=== Bottom 10 Teams: Least Clutch Improvement ===\n")
  scatter_df %>%
    arrange(clutch_improvement) %>%
    select(team_abbreviation, gp, w, l, clutch_win_pct,
           delta_off, delta_def, delta_net, clutch_improvement) %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    head(10) %>%
    print()
  
} else {
  cat("\n=== Top 10 Teams: Best Clutch Net Rating ===\n")
  scatter_df %>%
    arrange(desc(net_rating)) %>%
    select(team_abbreviation, gp, w, l, clutch_win_pct,
           off_rating, def_rating, net_rating, defensive_impact) %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    head(10) %>%
    print()
  
  cat("\n\n=== Bottom 10 Teams: Worst Clutch Net Rating ===\n")
  scatter_df %>%
    arrange(net_rating) %>%
    select(team_abbreviation, gp, w, l, clutch_win_pct,
           off_rating, def_rating, net_rating, defensive_impact) %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    head(10) %>%
    print()
}

# Save plot
if (save_plot) {
  safe_window <- gsub("[^A-Za-z0-9]+", "_", clutch_window)
  file_name <- glue(
    "{season_year}_team_clutch_{safe_window}_pm{score_margin}_{per_mode}_{compare_mode}.png"
  )
  ggsave(
    filename = file_name,
    plot = p,
    width = 12,
    height = 10,
    dpi = 300,
    bg = "white"
  )
  message(glue("Saved plot to {file_name}"))
  
  # Save data
  csv_name <- glue(
    "{season_year}_team_clutch_{safe_window}_pm{score_margin}_{compare_mode}_data.csv"
  )
  write_csv(scatter_df, csv_name)
  message(glue("Saved data to {csv_name}"))
}
