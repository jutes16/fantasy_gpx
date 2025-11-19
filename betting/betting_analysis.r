# ---- Setup ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(scales)
  library(glue)
  library(nflreadr)
})

# ---- Parameters ----
base_dir   <- "/Users/jgilbert/Documents/GitHub/fantasy_gpx/betting"
data_dir   <- file.path(base_dir, "data")
save_plots <- TRUE

# NEW: Toggle between ATS and Straight Up analysis
analyze_ats <- TRUE  # Set to FALSE to analyze straight-up wins/losses instead

# Helper: normalize NFL abbreviations
normalize_nfl_abbr <- function(x) {
  dplyr::recode(
    x,
    "JAC" = "JAX",
    "WSH" = "WAS",
    "LA"  = "LAR",
    "STL" = "LAR",
    "SD"  = "LAC",
    "OAK" = "LV",
    .default = x
  )
}

# Ensure output directories exist
if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_dir, "data"), recursive = TRUE, showWarnings = FALSE)

# ---- Helper: Find latest CSV ----
latest_csv <- function(dir, pattern = NULL) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (!is.null(pattern)) {
    files <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
  }
  if (length(files) == 0) return(NA_character_)
  info <- file.info(files)
  files[which.max(info$mtime)]
}

# ---- Load GAMES/ATS data ----
games_csv_path <- latest_csv(data_dir, "GAMES")

if (is.na(games_csv_path)) {
  stop("No GAMES CSV found in ", data_dir, ". Please export the GAMES sheet from Google Sheets.")
}

message("Reading GAMES CSV: ", normalizePath(games_csv_path, mustWork = FALSE))
games_raw <- readr::read_csv(games_csv_path, show_col_types = FALSE) %>%
  janitor::clean_names()

# ---- Process games data ----
# Normalize team names and calculate winner from scores
games <- games_raw %>%
  dplyr::mutate(
    week = as.integer(readr::parse_number(as.character(week))),
    away = normalize_nfl_abbr(stringr::str_to_upper(away)),
    home = normalize_nfl_abbr(stringr::str_to_upper(home)),
    spread_winner = normalize_nfl_abbr(stringr::str_to_upper(spread_winner)),
    # Calculate actual winner from scores
    winner = dplyr::case_when(
      score_away > score_home ~ away,
      score_home > score_away ~ home,
      TRUE ~ NA_character_  # Tie (rare in NFL)
    ),
    # home_spread is negative when home is favored
    # Convert to a consistent "spread" value
    spread = abs(home_spread)
  ) %>%
  dplyr::filter(!is.na(week), !is.na(away), !is.na(home)) %>%
  dplyr::arrange(week)

# ---- Create long format with team perspective ----
# Each game becomes two rows: one for each team
games_long <- games %>%
  # Away team perspective
  dplyr::transmute(
    week, team = away, opponent = home,
    location = "away",
    # Away team is favorite when home_spread is positive
    is_favorite = home_spread > 0,
    spread_size = abs(home_spread),
    won_game = (winner == team),
    won_ats = (spread_winner == team)
  ) %>%
  dplyr::bind_rows(
    # Home team perspective
    games %>%
      dplyr::transmute(
        week, team = home, opponent = away,
        location = "home",
        # Home team is favorite when home_spread is negative
        is_favorite = home_spread < 0,
        spread_size = abs(home_spread),
        won_game = (winner == team),
        won_ats = (spread_winner == team)
      )
  ) %>%
  dplyr::arrange(team, week) %>%
  dplyr::filter(!is.na(team))

# ---- Calculate previous game outcomes ----
situational <- games_long %>%
  dplyr::group_by(team) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    # Previous week outcomes
    prev_won_game = dplyr::lag(won_game),
    prev_won_ats = dplyr::lag(won_ats),
    prev_spread_size = dplyr::lag(spread_size),
    prev_is_favorite = dplyr::lag(is_favorite),
    # Situational flags - based on analyze_ats toggle
    coming_off_win = if (analyze_ats) prev_won_ats == TRUE else prev_won_game == TRUE,
    coming_off_loss = if (analyze_ats) prev_won_ats == FALSE else prev_won_game == FALSE,
    coming_off_ats_win = prev_won_ats == TRUE,
    coming_off_ats_loss = prev_won_ats == FALSE
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(prev_won_game))  # Remove first week (no previous game)

# Set analysis labels based on toggle
analysis_type <- if (analyze_ats) "ATS" else "Straight Up"
win_col <- if (analyze_ats) "won_ats" else "won_game"

# ---- Analysis 1: Record coming off Win vs Loss ----
ats_after_result <- situational %>%
  dplyr::filter(!is.na(coming_off_win), !is.na(.data[[win_col]])) %>%
  dplyr::mutate(situation = ifelse(coming_off_win, "After Win", "After Loss")) %>%
  dplyr::group_by(situation) %>%
  dplyr::summarise(
    wins = sum(.data[[win_col]], na.rm = TRUE),
    losses = sum(!.data[[win_col]], na.rm = TRUE),
    total = sum(!is.na(.data[[win_col]])),
    win_pct = wins / total,
    net_record = wins - losses,
    .groups = "drop"
  )

# ---- Analysis 2: Record ATS only ----
ats_record_only <- situational %>%
  dplyr::filter(!is.na(won_ats)) %>%
  dplyr::summarise(
    wins = sum(won_ats, na.rm = TRUE),
    losses = sum(!won_ats, na.rm = TRUE),
    total = sum(!is.na(won_ats)),
    win_pct = wins / total,
    net_record = wins - losses
  )

# ---- Analysis 2a: Record by Favorite/Underdog Status ----
ats_by_favorite <- situational %>%
  dplyr::filter(!is.na(won_ats)) %>%
  dplyr::mutate(status = ifelse(is_favorite, "Favorite", "Underdog")) %>%
  dplyr::group_by(status) %>%
  dplyr::summarise(
    wins = sum(won_ats, na.rm = TRUE),
    losses = sum(!won_ats, na.rm = TRUE),
    total = sum(!is.na(won_ats)),
    win_pct = wins / total,
    net_record = wins - losses,
    .groups = "drop"
  )

# ---- Analysis 2b: Record by Home/Away Status ----
ats_by_location <- situational %>%
  dplyr::filter(!is.na(won_ats)) %>%
  dplyr::mutate(location_label = ifelse(location == "home", "Home", "Away")) %>%
  dplyr::group_by(location_label) %>%
  dplyr::summarise(
    wins = sum(won_ats, na.rm = TRUE),
    losses = sum(!won_ats, na.rm = TRUE),
    total = sum(!is.na(won_ats)),
    win_pct = wins / total,
    net_record = wins - losses,
    .groups = "drop"
  )

# ---- Analysis 2b: Record by favorite status after ATS win/loss ----
ats_by_favorite_status <- situational %>%
  dplyr::filter(!is.na(coming_off_ats_win), !is.na(won_ats)) %>%
  dplyr::mutate(
    prev_result = ifelse(coming_off_ats_win, "After ATS Win", "After ATS Loss"),
    curr_status = ifelse(is_favorite, "Favorite", "Underdog")
  ) %>%
  dplyr::group_by(prev_result, curr_status) %>%
  dplyr::summarise(
    wins = sum(won_ats, na.rm = TRUE),
    losses = sum(!won_ats, na.rm = TRUE),
    total = sum(!is.na(won_ats)),
    win_pct = wins / total,
    net_record = wins - losses,
    .groups = "drop"
  )

# ---- Analysis 3: Record ATS after ATS win/loss by spread margin ----
# Identify games where BOTH teams are coming off ATS losses
#games_to_exclude <- situational %>%
#  dplyr::select(week, team, opponent, coming_off_ats_loss) %>%
#  dplyr::inner_join(
#    situational %>% dplyr::select(week, team, opponent, coming_off_ats_loss),
#    by = c("week" = "week", "team" = "opponent", "opponent" = "team")
#  ) %>%
#  dplyr::filter(coming_off_ats_loss.x == TRUE, coming_off_ats_loss.y == TRUE) %>%
#  dplyr::select(week, team) %>%
#  dplyr::distinct()

#if (nrow(games_to_exclude) > 0) {
#  cat("\nExcluding", nrow(games_to_exclude), "team-games where both opponents came off ATS losses\n")
#}

# Categorize previous spread sizes for both wins and losses
ats_by_margin_and_result <- situational %>%
#  dplyr::anti_join(games_to_exclude, by = c("week", "team")) %>%
  dplyr::filter(!is.na(coming_off_ats_win), !is.na(prev_spread_size), !is.na(won_ats)) %>%
  dplyr::mutate(
    prev_result = ifelse(coming_off_ats_win, "After ATS Win", "After ATS Loss"),
    prev_spread_category = dplyr::case_when(
      prev_spread_size <= 3 ~ "0-3 pts",
      prev_spread_size <= 7 ~ "3.5-7 pts",
      prev_spread_size <= 10 ~ "7.5-10 pts",
      TRUE ~ "10+ pts"
    ),
    prev_spread_category = factor(prev_spread_category, 
                                  levels = c("0-3 pts", "3.5-7 pts", "7.5-10 pts", "10+ pts"))
  ) %>%
  dplyr::group_by(prev_result, prev_spread_category) %>%
  dplyr::summarise(
    wins = sum(won_ats, na.rm = TRUE),
    losses = sum(!won_ats, na.rm = TRUE),
    total = sum(!is.na(won_ats)),
    win_pct = wins / total,
    net_record = wins - losses,
    .groups = "drop"
  )

# ---- Analysis 4: Scatter plot of previous spread vs current spread ----
# Create binned categories for spreads (signed, so we know favorite/underdog)
scatter_data <- situational %>%
  dplyr::anti_join(situational, by = c("week", "team")) %>%
  dplyr::filter(!is.na(prev_spread_size), !is.na(spread_size), !is.na(won_ats)) %>%
  dplyr::mutate(
    # Convert spreads to signed values (negative = favorite, positive = underdog)
    prev_spread_signed = ifelse(prev_is_favorite, -prev_spread_size, prev_spread_size),
    curr_spread_signed = ifelse(is_favorite, -spread_size, spread_size),
    # Bin previous spread
    prev_spread_bin = dplyr::case_when(
      prev_spread_signed <= -10 ~ "-10+",
      prev_spread_signed <= -7 ~ "-10 to -7",
      prev_spread_signed <= -3.5 ~ "-7 to -3.5",
      prev_spread_signed < 0 ~ "-3.5 to 0",
      prev_spread_signed <= 3 ~ "0 to 3",
      prev_spread_signed <= 7 ~ "3.5 to 7",
      prev_spread_signed <= 10 ~ "7 to 10",
      TRUE ~ "10+"
    ),
    # Bin current spread
    curr_spread_bin = dplyr::case_when(
      curr_spread_signed <= -10 ~ "-10+",
      curr_spread_signed <= -7 ~ "-10 to -7",
      curr_spread_signed <= -3.5 ~ "-7 to -3.5",
      curr_spread_signed < 0 ~ "-3.5 to 0",
      curr_spread_signed <= 3 ~ "0 to 3",
      curr_spread_signed <= 7 ~ "3.5 to 7",
      curr_spread_signed <= 10 ~ "7 to 10",
      TRUE ~ "10+"
    ),
    # Factor with proper ordering
    prev_spread_bin = factor(prev_spread_bin, 
                            levels = c("-10+", "-10 to -7", "-7 to -3.5", "-3.5 to 0", 
                                     "0 to 3", "3.5 to 7", "7 to 10", "10+")),
    curr_spread_bin = factor(curr_spread_bin, 
                            levels = c("-10+", "-10 to -7", "-7 to -3.5", "-3.5 to 0", 
                                     "0 to 3", "3.5 to 7", "7 to 10", "10+"))
  ) %>%
  dplyr::group_by(prev_spread_bin, curr_spread_bin) %>%
  dplyr::summarise(
    wins = sum(won_ats, na.rm = TRUE),
    losses = sum(!won_ats, na.rm = TRUE),
    total = sum(!is.na(won_ats)),
    net_record = wins - losses,
    win_pct = wins / total,
    .groups = "drop"
  ) %>%
  dplyr::filter(total > 0)

cat("\nScatter plot data rows:", nrow(scatter_data), "\n")

# Plot 6: Scatter plot of previous spread vs current spread
# Create numeric positions for plotting
spread_bin_positions <- data.frame(
  bin = c("-10+", "-10 to -7", "-7 to -3.5", "-3.5 to 0", 
          "0 to 3", "3.5 to 7", "7 to 10", "10+"),
  position = c(-12, -8.5, -5.25, -1.75, 1.5, 5.25, 8.5, 12)
)

scatter_data <- scatter_data %>%
  dplyr::left_join(spread_bin_positions, by = c("prev_spread_bin" = "bin")) %>%
  dplyr::rename(prev_pos = position) %>%
  dplyr::left_join(spread_bin_positions, by = c("curr_spread_bin" = "bin")) %>%
  dplyr::rename(curr_pos = position)

# Combine analyses 1 and 2 for subplot
combined_situations <- ats_after_result %>%
  mutate(analysis = glue("Coming off Win/Loss ({analysis_type})"))

# Calculate y-axis limits with buffer for text
y_max_combined <- max(abs(combined_situations$net_record)) * 1.25
y_max_margin <- max(abs(ats_by_margin_and_result$net_record)) * 1.3
y_max_favorite_status <- max(abs(ats_by_favorite_status$net_record)) * 1.3
y_max_fav <- max(abs(ats_by_favorite$net_record)) * 1.25
y_max_loc <- max(abs(ats_by_location$net_record)) * 1.25

# Plot 1: Record coming off Win vs Loss
p_combined <- ggplot(combined_situations, aes(x = situation, y = net_record, fill = net_record)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+d\n(%d-%d, %.1f%%)", 
                       net_record, wins, losses, win_pct * 100)),
    vjust = ifelse(combined_situations$net_record >= 0, -0.3, 1.3),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
    midpoint = 0, guide = "none"
  ) +
  scale_y_continuous(limits = c(-y_max_combined, y_max_combined)) +
  labs(
    title = glue("NFL Teams {analysis_type} Performance by Prior Game Outcome"),
    x = NULL,
    y = "Net Record (Wins - Losses)",
    caption = glue("Numbers show: Net Record (W-L, Win%) | Overall {analysis_type}: {ats_record_only$wins}-{ats_record_only$losses} ({scales::percent(ats_record_only$win_pct, accuracy=0.1)})")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 11, face = "bold")
  )

# Plot 2: ATS Record by Favorite/Underdog Status
p_favorite <- ggplot(ats_by_favorite, aes(x = status, y = net_record, fill = net_record)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+d\n(%d-%d, %.1f%%)",
                       net_record, wins, losses, win_pct * 100)),
    vjust = ifelse(ats_by_favorite$net_record >= 0, -0.3, 1.3),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
    midpoint = 0, guide = "none"
  ) +
  scale_y_continuous(limits = c(-y_max_fav, y_max_fav)) +
  labs(
    title = "NFL Teams ATS Performance by Favorite/Underdog Status",
    x = NULL,
    y = "Net Record (Wins - Losses)",
    caption = "Numbers show: Net Record (W-L, Win%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 11, face = "bold")
  )

# Plot 3: ATS Record by Home/Away Status
p_location <- ggplot(ats_by_location, aes(x = location_label, y = net_record, fill = net_record)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+d\n(%d-%d, %.1f%%)",
                       net_record, wins, losses, win_pct * 100)),
    vjust = ifelse(ats_by_location$net_record >= 0, -0.3, 1.3),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
    midpoint = 0, guide = "none"
  ) +
  scale_y_continuous(limits = c(-y_max_loc, y_max_loc)) +
  labs(
    title = "NFL Teams ATS Performance by Home/Away Status",
    x = NULL,
    y = "Net Record (Wins - Losses)",
    caption = "Numbers show: Net Record (W-L, Win%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 11, face = "bold")
  )

# Plot 4: ATS Record by favorite status after ATS win/loss
p_favorite_status <- ggplot(ats_by_favorite_status, 
                            aes(x = curr_status, y = net_record, fill = net_record)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+d\n(%d-%d)\n%.1f%%", 
                       net_record, wins, losses, win_pct * 100)),
    vjust = ifelse(ats_by_favorite_status$net_record >= 0, -0.3, 1.3),
    size = 3.2,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
    midpoint = 0, guide = "none"
  ) +
  scale_y_continuous(limits = c(-y_max_favorite_status, y_max_favorite_status)) +
  facet_wrap(~ prev_result, scales = "free_x") +
  labs(
    title = "NFL Teams ATS Performance After ATS Win/Loss by Favorite Status",
    subtitle = "Current game's favorite/underdog status",
    x = "Current Game Status",
    y = "Net Record (Wins - Losses)",
    caption = "Numbers show: Net Record (W-L) Win%"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  )

# Plot 5: ATS Record after ATS Win/Loss by Spread Margin
p_margins <- ggplot(ats_by_margin_and_result, 
                    aes(x = prev_spread_category, y = net_record, fill = net_record)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+d\n(%d-%d)\n%.1f%%", 
                       net_record, wins, losses, win_pct * 100)),
    vjust = ifelse(ats_by_margin_and_result$net_record >= 0, -0.3, 1.3),
    size = 3.2,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
    midpoint = 0, guide = "none"
  ) +
  scale_y_continuous(limits = c(-y_max_margin, y_max_margin)) +
  facet_wrap(~ prev_result, scales = "free_x") +
  labs(
    title = "NFL Teams ATS Performance After ATS Win/Loss by Spread Margin",
    subtitle = "Previous game's spread margin",
    x = "Previous Game Spread",
    y = "Net Record (Wins - Losses)",
    caption = "Numbers show: Net Record (W-L) Win%"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  )

# Plot 4: Scatter plot of previous spread vs current spread
if (nrow(scatter_data) > 0) {
  max_net <- max(abs(scatter_data$net_record), na.rm = TRUE)
  
  p_scatter <- ggplot(scatter_data, aes(x = prev_pos, y = curr_pos, 
                                        size = total, color = net_record)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.5) +
    scale_size_continuous(range = c(2, 15), name = "Games") +
    scale_color_gradient2(
      low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
      midpoint = 0, name = "Net Record",
      limits = c(-max_net, max_net)
    ) +
    scale_x_continuous(
      breaks = spread_bin_positions$position,
      labels = spread_bin_positions$bin
    ) +
    scale_y_continuous(
      breaks = spread_bin_positions$position,
      labels = spread_bin_positions$bin
    ) +
    labs(
      title = "ATS Performance: Previous Game Spread vs Current Game Spread",
      subtitle = "Negative = favorite, Positive = underdog",
      x = "Previous Game Spread",
      y = "Current Game Spread",
      caption = "Point size = number of games; Color = net ATS record"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      legend.position = "right"
    )
} else {
  p_scatter <- NULL
  message("Warning: No data available for scatter plot")
}

# ---- Save plots ----
if (save_plots) {
  suffix <- if (analyze_ats) "ats" else "su"

  ggsave(
    file.path(base_dir, "plots", glue("situational_combined_{suffix}.png")),
    p_combined, width = 10, height = 6, dpi = 300, bg = "white"
  )
  message("Saved: plots/situational_combined_", suffix, ".png")

  ggsave(
    file.path(base_dir, "plots", "ats_by_favorite.png"),
    p_favorite, width = 10, height = 6, dpi = 300, bg = "white"
  )
  message("Saved: plots/ats_by_favorite.png")

  ggsave(
    file.path(base_dir, "plots", "ats_by_location.png"),
    p_location, width = 10, height = 6, dpi = 300, bg = "white"
  )
  message("Saved: plots/ats_by_location.png")

  ggsave(
    file.path(base_dir, "plots", glue("ats_by_favorite_status_{suffix}.png")),
    p_favorite_status, width = 12, height = 6, dpi = 300, bg = "white"
  )
  message("Saved: plots/ats_by_favorite_status_", suffix, ".png")

  ggsave(
    file.path(base_dir, "plots", "ats_by_spread_margin.png"),
    p_margins, width = 12, height = 6, dpi = 300, bg = "white"
  )
  message("Saved: plots/ats_by_spread_margin.png")

  if (!is.null(p_scatter)) {
    ggsave(
      file.path(base_dir, "plots", "ats_spread_transition_scatter.png"),
      p_scatter, width = 12, height = 9, dpi = 300, bg = "white"
    )
    message("Saved: plots/ats_spread_transition_scatter.png")
  }
}

# ---- Print plots ----
print(p_combined)
print(p_favorite)
print(p_location)
print(p_favorite_status)
print(p_margins)
if (!is.null(p_scatter)) {
  print(p_scatter)
}

# ---- Summary tables ----
cat("\n=== SITUATIONAL PERFORMANCE SUMMARY ===\n\n")
cat("Analysis Type:", analysis_type, "\n\n")

cat("Overall Record:\n")
print(ats_record_only)

cat("\n1. Record Coming Off Win vs Loss:\n")
print(ats_after_result)

cat("\n2. ATS Record by Favorite/Underdog Status:\n")
print(ats_by_favorite)

cat("\n3. ATS Record by Home/Away Status:\n")
print(ats_by_location)

cat("\n4. ATS Record by Favorite Status After ATS Win/Loss:\n")
print(ats_by_favorite_status)

cat("\n5. ATS Record After ATS Win/Loss by Spread Margin:\n")
print(ats_by_margin_and_result)

cat("\n=== Additional Insights ===\n")
cat("Total games analyzed:", nrow(situational), "\n")
cat("Weeks covered:", min(situational$week), "to", max(situational$week), "\n")

# ---- Current Week Teams Coming Off ATS Loss ----
cat("\n=== TEAMS COMING OFF ATS LOSS ===\n\n")

# Find the most recent completed week and next week
max_completed_week <- max(games_long$week[!is.na(games_long$won_ats)], na.rm = TRUE)
next_week <- max_completed_week

games_to_include <- situational %>%
  dplyr::inner_join(
    situational %>% dplyr::select(week, team, opponent, coming_off_ats_loss),
    by = c("week" = "week", "team" = "opponent", "opponent" = "team")
  ) %>%
  dplyr::filter(
    (coming_off_ats_loss.x == TRUE & coming_off_ats_loss.y == FALSE) |
    (coming_off_ats_loss.x == FALSE & coming_off_ats_loss.y == TRUE)
  ) %>%
  dplyr::distinct()

current_week_ats_losers <- games_to_include %>%
  dplyr::filter(week == next_week, coming_off_ats_loss.x == TRUE) %>%
  dplyr::arrange(team)

if (nrow(current_week_ats_losers) > 0) {
  cat("Teams playing in Week", next_week, "coming off an ATS loss:\n\n")
  current_week_ats_losers %>%
    dplyr::mutate(
      matchup = ifelse(location == "home",
                      paste0(team, " vs ", opponent),
                      paste0(team, " @ ", opponent)),
      prev_spread_txt = sprintf("%.1f pts", prev_spread_size),
      curr_spread_txt = ifelse(is_favorite,
                              sprintf("-%0.1f", spread_size),
                              sprintf("+%.1f", spread_size)),
      favorite_status = ifelse(is_favorite, "Favorite", "Underdog")
    ) %>%
    dplyr::select(Team = team, Matchup = matchup,
                  `Prev Spread` = prev_spread_txt,
                  `Current Spread` = curr_spread_txt,
                  `This Week` = favorite_status) %>%
    print(row.names = FALSE)

  cat("\nTotal teams:", nrow(current_week_ats_losers), "\n")
  cat("Week", next_week, "games (coming off Week", max_completed_week, ")\n")
} else {
  cat("No Week", next_week, "data available or no teams coming off ATS loss.\n")
  cat("Most recent completed week:", max_completed_week, "\n")
}

# ---- All Current Week Lines ----
cat("\n=== ALL WEEK", next_week, "LINES ===\n\n")

# Get all games for the current week
current_week_games <- games %>%
  dplyr::filter(week == next_week) %>%
  dplyr::arrange(away, home)

if (nrow(current_week_games) > 0) {
  cat("All games scheduled for Week", next_week, ":\n\n")
  current_week_games %>%
    dplyr::mutate(
      matchup = paste0(away, " @ ", home),
      spread_display = ifelse(
        home_spread < 0,
        paste0(home, " -", abs(home_spread)),
        paste0(away, " -", home_spread)
      ),
      favorite = ifelse(home_spread < 0, home, away)
    ) %>%
    dplyr::select(Matchup = matchup,
                  Spread = spread_display,
                  Favorite = favorite) %>%
    print(row.names = FALSE)

  cat("\nTotal games scheduled:", nrow(current_week_games), "\n")

  # Summary statistics
  cat("\n--- Week", next_week, "Spread Summary ---\n")
  spread_summary <- current_week_games %>%
    dplyr::mutate(
      spread_abs = abs(home_spread),
      spread_category = dplyr::case_when(
        spread_abs <= 3 ~ "Field Goal or Less (0-3)",
        spread_abs <= 7 ~ "Touchdown or Less (3.5-7)",
        spread_abs <= 10 ~ "Big Spread (7.5-10)",
        TRUE ~ "Double Digit (10+)"
      ),
      home_favored = home_spread < 0
    )

  # Count by spread category
  cat("\nGames by Spread Size:\n")
  spread_summary %>%
    dplyr::group_by(spread_category) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::arrange(count) %>%
    print(row.names = FALSE)

  # Home vs Away favorites
  cat("\nHome vs Away Favorites:\n")
  cat("  Home teams favored:", sum(spread_summary$home_favored), "\n")
  cat("  Away teams favored:", sum(!spread_summary$home_favored), "\n")

  # Spread statistics
  cat("\nSpread Statistics:\n")
  cat("  Average spread:", round(mean(spread_summary$spread_abs), 2), "points\n")
  cat("  Median spread:", round(median(spread_summary$spread_abs), 2), "points\n")
  cat("  Largest spread:", max(spread_summary$spread_abs), "points\n")
  cat("  Smallest spread:", min(spread_summary$spread_abs), "points\n")

} else {
  cat("No games scheduled for Week", next_week, "\n")
  cat("Available weeks:", paste(unique(games$week), collapse = ", "), "\n")
}
