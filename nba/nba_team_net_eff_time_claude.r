library(tidyverse)
library(hoopR)
library(janitor)
library(slider)
library(glue)
library(ggimage)
library(magick)

# ============== CONFIGURATION FLAGS ==============
highlight_teams <- c("CLE", "BOS", "OKC")  # List of teams to highlight
season_year <- 2025
rolling_window <- 10
create_gif <- TRUE  # Set to FALSE for single static plot
include_playoffs <- FALSE  # Set to TRUE to include playoff games
rating_type <- "off_rating"  # Options: "net_rating", "off_rating", "def_rating"
# ================================================

# Determine season type
season_types <- if (include_playoffs) {
  c("Regular Season", "Playoffs")
} else {
  "Regular Season"
}

# Team lookup table
team_lookup <- load_nba_team_box(season = season_year) %>%
  distinct(team_id, team_abbreviation, team_display_name, team_logo) %>%
  mutate(team_name = stringr::str_trim(team_display_name))

# Team colors
team_colors <- c(
  ATL = "#E03A3E", BOS = "#007A33", BKN = "#000000", CHA = "#1D1160",
  CHI = "#CE1141", CLE = "#860038", DAL = "#00538C", DEN = "#0E2240",
  DET = "#C8102E", GSW = "#1D428A", HOU = "#CE1141", IND = "#002D62",
  LAC = "#C8102E", LAL = "#552583", MEM = "#5D76A9", MIA = "#98002E",
  MIL = "#00471B", MIN = "#0C2340", NOP = "#0C2340", NYK = "#006BB6",
  OKC = "#007AC1", ORL = "#0077C0", PHI = "#006BB6", PHX = "#1D1160",
  POR = "#E03A3E", SAC = "#5A2D81", SAS = "#C4CED4", TOR = "#CE1141",
  UTA = "#002B5C", WSH = "#002B5C"
)

# Calculate team net ratings from box scores
calculate_team_net_ratings <- function(season, season_types) {
  message("Loading team box scores...")
  
  # Load box scores for all specified season types
  box_scores_list <- lapply(season_types, function(st) {
    load_nba_team_box(season = season, season_type = st) %>%
      mutate(season_type = st)
  })
  
  box_scores <- bind_rows(box_scores_list) %>%
    janitor::clean_names() %>%
    mutate(
      game_date = as.Date(game_date),
      team_score = as.numeric(team_score),
      opponent_team_score = as.numeric(opponent_team_score)
    ) %>%
    filter(
      !is.na(game_date), 
      !is.na(team_score), 
      !is.na(opponent_team_score),
      # Filter for regular season only
      season_type == "Regular Season"
    )
  
  # Calculate per-game ratings
  team_ratings <- box_scores %>%
    mutate(
      # Estimate possessions (simplified formula)
      poss = field_goals_attempted - offensive_rebounds + turnovers + 
             0.4 * free_throws_attempted,
      off_rating = (team_score / poss) * 100,
      def_rating = (opponent_team_score / poss) * 100,
      net_rating = off_rating - def_rating
    ) %>%
    select(game_id, game_date, team_id, team_abbreviation, team_display_name,
           team_score, opponent_team_score, poss, off_rating, def_rating, 
           net_rating, season_type) %>%
    arrange(team_abbreviation, game_date)
  
  return(team_ratings)
}

# Get all team ratings
all_team_ratings <- calculate_team_net_ratings(season_year, season_types)

# Calculate rolling averages for each team
team_rolling_ratings <- all_team_ratings %>%
  group_by(team_abbreviation) %>%
  arrange(game_date) %>%
  mutate(
    # Remove duplicate games (if any) by keeping unique game_date + game_id
    games_played = row_number(),
    # Use slider package for more robust rolling means
    rolling_net_rating = slider::slide_dbl(
      net_rating, mean, .before = rolling_window - 1, .complete = TRUE
    ),
    rolling_off_rating = slider::slide_dbl(
      off_rating, mean, .before = rolling_window - 1, .complete = TRUE
    ),
    rolling_def_rating = slider::slide_dbl(
      def_rating, mean, .before = rolling_window - 1, .complete = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(rolling_net_rating)) %>%
  left_join(team_lookup, by = "team_abbreviation")

# Determine which rating to plot and axis labels
rating_column <- paste0("rolling_", rating_type)
rating_labels <- list(
  net_rating = list(
    title = "Net Rating",
    subtitle = "Net Rating = Offensive Rating - Defensive Rating"
  ),
  off_rating = list(
    title = "Offensive Rating",
    subtitle = "Points Scored per 100 Possessions"
  ),
  def_rating = list(
    title = "Defensive Rating",
    subtitle = "Points Allowed per 100 Possessions (Lower is Better)"
  )
)

# Function to create plot for a specific game number
create_rating_plot <- function(data, up_to_game, highlight_teams, rating_column, rating_type) {
  plot_data <- data %>%
    filter(games_played <= up_to_game)
  
  # Get logo positions for highlighted teams
  logo_data <- plot_data %>%
    filter(team_abbreviation %in% highlight_teams) %>%
    group_by(team_abbreviation) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Get the rating label info
  label_info <- rating_labels[[rating_type]]
  
  # Create plot
  p <- ggplot(plot_data, aes(x = games_played, y = .data[[rating_column]], 
                             group = team_abbreviation)) +
    geom_line(data = filter(plot_data, !team_abbreviation %in% highlight_teams),
              color = "grey70", alpha = 0.4, linewidth = 0.6) +
    geom_line(data = filter(plot_data, team_abbreviation %in% highlight_teams),
              aes(color = team_abbreviation), linewidth = 1.5) +
    scale_color_manual(values = team_colors[highlight_teams], 
                      name = "Teams",
                      labels = highlight_teams) +
    labs(
      title = glue("NBA {season_year} Season: {rolling_window}-Game Rolling {label_info$title}"),
      subtitle = glue("{label_info$subtitle} | Through Game {up_to_game}"),
      x = "Games Played",
      y = glue("Rolling {label_info$title}"),
      caption = "Data: hoopR"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  # Add logos if data exists
  if (nrow(logo_data) > 0) {
    logo_data_valid <- logo_data %>%
      filter(!is.na(team_logo))
    
    if (nrow(logo_data_valid) > 0) {
      p <- p + geom_image(
        data = logo_data_valid,
        aes(image = team_logo),
        size = 0.08,
        by = "width",
        asp = 1.5
      )
    }
  }
  
  return(p)
}

# Create output directory
output_dir <- "nba/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

if (create_gif) {
  # Create animation
  message("Creating animation frames...")
  
  # Get unique game numbers (after rolling window is complete)
  game_numbers <- team_rolling_ratings %>%
    filter(games_played >= rolling_window) %>%
    distinct(games_played) %>%
    arrange(games_played) %>%
    pull(games_played)
  
  # Sample every Nth game to reduce frames (adjust 'by' parameter as needed)
  animation_games <- game_numbers[seq(1, length(game_numbers), by = 3)]
  
  # Create frames
  frame_files <- character(length(animation_games))
  
  for (i in seq_along(animation_games)) {
    message(glue("Creating frame {i} of {length(animation_games)}"))
    
    p <- create_rating_plot(team_rolling_ratings, animation_games[i], 
                           highlight_teams, rating_column, rating_type)
    
    frame_file <- glue("{output_dir}/frame_{sprintf('%03d', i)}.png")
    ggsave(
      filename = frame_file,
      plot = p,
      width = 12,
      height = 7,
      dpi = 150
    )
    frame_files[i] <- frame_file
  }
  
  # Create GIF
  message("Creating GIF...")
  img_list <- lapply(frame_files, image_read)
  img_joined <- image_join(img_list)
  img_animated <- image_animate(img_joined, fps = 5)
  
  gif_file <- glue("{output_dir}/rating_animation_{rating_type}_{paste(highlight_teams, collapse='_')}.gif")
  image_write(img_animated, gif_file)
  message(glue("Saved GIF to: {gif_file}"))
  
  # Clean up frame files
  message("Cleaning up frame files...")
  file.remove(frame_files)
  
} else {
  # Create single static plot (latest data)
  message("Creating static plot...")
  latest_game <- max(team_rolling_ratings$games_played)
  final_plot <- create_rating_plot(team_rolling_ratings, latest_game, 
                                   highlight_teams, rating_column, rating_type)
  
  file_name <- glue("{output_dir}/rating_trend_{rating_type}_{paste(highlight_teams, collapse='_')}.png")
  ggsave(
    filename = file_name,
    plot = final_plot,
    width = 12,
    height = 7,
    dpi = 300,
    bg = "white"
  )
  message(glue("Saved plot to: {file_name}"))
}

# Display summary statistics for highlighted teams
cat("\n=== Summary Statistics ===\n")
team_rolling_ratings %>%
  filter(team_abbreviation %in% highlight_teams) %>%
  group_by(team_abbreviation) %>%
  summarise(
    avg_rating = mean(.data[[rating_column]], na.rm = TRUE),
    max_rating = max(.data[[rating_column]], na.rm = TRUE),
    min_rating = min(.data[[rating_column]], na.rm = TRUE),
    final_rating = last(.data[[rating_column]]),
    .groups = "drop"
  ) %>%
  print()
