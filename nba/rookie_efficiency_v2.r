# install.packages("hoopR")  # if needed
library(hoopR)
library(dplyr)
library(purrr)
library(progress)
library(readr)
library(ggplot2)
library(ggrepel)

# ---- Parameters -------------------------------------------------------------
most_recent <- most_recent_nba_season()-1

# Seasons to fetch (if not already cached)
all_available_seasons <- c(2020, 2021, 2022, 2023, 2024, most_recent)

# Seasons to include in the plot
seasons_to_plot <- c(most_recent)

# Control flags
OVERWRITE_EXISTING <- FALSE
HIGHLIGHT_SEASON <- as.character(most_recent)

# Plot filters (applied only to visualization, not to cached data)
MIN_GAMES <- 15
MIN_MPG <- 10
GAMES_CURRENT_SEASON <- 10
ROOKIES_ONLY <- FALSE  # Set to TRUE to only show rookies in the plot
FILTER_TEAM <- NULL    # e.g., "LAL" or c("LAL", "BOS") to filter specific teams
FILTER_MIN_USAGE <- 15  # e.g., 20 to only show players with usage >= 20%
FILTER_MAX_USAGE <- NULL  # e.g., 30 to only show players with usage <= 30%

# ---- Helper Functions -------------------------------------------------------
get_all_players_ratings <- function(season) {
  Sys.sleep(0.1)
  
  # Convert season format if needed (e.g., "2025-26" -> "2025")
  api_season <- if (grepl("-", season)) {
    sub("^(\\d{4})-.*", "\\1", season)
  } else {
    season
  }
  
  tryCatch({
    response <- nba_leaguedashplayerstats(
      season = api_season,
      season_type = "Regular Season",
      per_mode = "PerGame",
      measure_type = "Advanced"
    )
    
    if (is.null(response) || is.null(response$LeagueDashPlayerStats)) {
      return(tibble())
    }
    
    data <- response$LeagueDashPlayerStats
    
    if (nrow(data) == 0) {
      return(tibble())
    }
    
    data %>%
      mutate(
        PLAYER_ID = as.character(PLAYER_ID),
        PLAYER_NAME = as.character(PLAYER_NAME),
        GP = as.numeric(GP),
        MIN = as.numeric(MIN),
        OFF_RATING = as.numeric(OFF_RATING),
        DEF_RATING = as.numeric(DEF_RATING),
        NET_RATING = OFF_RATING - DEF_RATING,
        USG_PCT = as.numeric(USG_PCT) * 100
      ) %>%
      distinct(PLAYER_ID, .keep_all = TRUE) %>%
      mutate(
        SEASON = season,
        MPG = MIN
      ) %>%
      select(SEASON, PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, GP, MPG,
             OFF_RATING, DEF_RATING, NET_RATING, USG_PCT)
    
  }, error = function(e) {
    cat("    Error fetching players for", season, ":", conditionMessage(e), "\n")
    return(tibble())
  })
}

identify_rookies <- function(season) {
  Sys.sleep(0.1)
  
  # Convert season format if needed
  api_season <- if (grepl("-", season)) {
    sub("^(\\d{4})-.*", "\\1", season)
  } else {
    season
  }
  
  tryCatch({
    rookies <- nba_leaguedashplayerstats(
      season = api_season,
      season_type = "Regular Season",
      player_experience = "Rookie",
      per_mode = "Totals",
      measure_type = "Base"
    )$LeagueDashPlayerStats %>%
      mutate(
        PLAYER_ID = as.character(PLAYER_ID)
      ) %>%
      distinct(PLAYER_ID, .keep_all = TRUE) %>%
      pull(PLAYER_ID)

    return(rookies)
    
  }, error = function(e) {
    cat("    Error identifying rookies for", season, ":", conditionMessage(e), "\n")
    return(character())
  })
}

# ---- Main Processing --------------------------------------------------------
if (!dir.exists("season_outputs")) {
  dir.create("season_outputs")
}

# Function to get cached file paths for a season
get_season_files <- function(season) {
  list(
    players = file.path("season_outputs", paste0("players_", season, ".csv")),
    rookies = file.path("season_outputs", paste0("rookies_", season, ".csv"))
  )
}

# Function to load or fetch data for a single season
load_or_fetch_season <- function(season) {
  files <- get_season_files(season)

  # Load or fetch players
  if (file.exists(files$players) && !OVERWRITE_EXISTING) {
    cat("  Loading players from cache:", season, "\n")
    players <- read_csv(files$players, show_col_types = FALSE) %>%
      mutate(PLAYER_ID = as.character(PLAYER_ID))
  } else {
    cat("  Fetching players for season:", season, "\n")
    players <- get_all_players_ratings(season)
    if (nrow(players) > 0) {
      write_csv(players, files$players)
      cat("    Saved to:", files$players, "\n")
    }
  }

  # Load or fetch rookies
  if (file.exists(files$rookies) && !OVERWRITE_EXISTING) {
    cat("  Loading rookies from cache:", season, "\n")
    rookie_ids <- read_csv(files$rookies, show_col_types = FALSE) %>%
      mutate(PLAYER_ID = as.character(PLAYER_ID))
  } else {
    cat("  Identifying rookies for season:", season, "\n")
    ids <- identify_rookies(season)
    if (length(ids) > 0) {
      rookie_ids <- tibble(SEASON = season, PLAYER_ID = ids)
      write_csv(rookie_ids, files$rookies)
      cat("    Saved to:", files$rookies, "\n")
    } else {
      rookie_ids <- tibble()
    }
  }

  list(players = players, rookies = rookie_ids)
}

# Fetch or load data for all available seasons
cat("=== Loading/Fetching Season Data ===\n")
cat("Available seasons:", paste(all_available_seasons, collapse = ", "), "\n\n")

all_season_data <- map(all_available_seasons, load_or_fetch_season)
names(all_season_data) <- as.character(all_available_seasons)

# Combine data for selected seasons
cat("\n=== Preparing Plot Data ===\n")
cat("Seasons to plot:", paste(seasons_to_plot, collapse = ", "), "\n")

all_players <- map_dfr(seasons_to_plot, function(s) {
  all_season_data[[as.character(s)]]$players
})

rookie_ids <- map_dfr(seasons_to_plot, function(s) {
  all_season_data[[as.character(s)]]$rookies
})

# Tag rookies in the all_players dataset (this is the full dataset)
all_players <- all_players %>%
  left_join(
    rookie_ids %>% mutate(is_rookie = TRUE),
    by = c("SEASON", "PLAYER_ID")
  ) %>%
  mutate(is_rookie = ifelse(is.na(is_rookie), FALSE, is_rookie))

# Extract rookie subset for summary
rookies <- all_players %>%
  filter(is_rookie) %>%
  arrange(SEASON, desc(NET_RATING))

# ---- Summary Statistics -----------------------------------------------------
cat("\n=== Summary ===\n")
cat("Total players in dataset:", nrow(all_players), "\n")
cat("Total rookies identified:", nrow(rookies), "\n")
cat("Seasons covered:", paste(unique(all_players$SEASON), collapse = ", "), "\n")
if (ROOKIES_ONLY) {
  cat("Plot filter: ROOKIES ONLY\n")
}

if (nrow(rookies) > 0) {
  cat("\nTop 10 rookies by Net Rating:\n")
  print(rookies %>% 
          select(SEASON, PLAYER_NAME, TEAM_ABBREVIATION, NET_RATING, 
                 OFF_RATING, DEF_RATING) %>%
          slice_head(n = 10), n = 10)
}

# ---- Scatter Plot -----------------------------------------------------------
if (nrow(all_players) > 0) {
  cat("\n=== Creating Scatter Plot ===\n")
  
  # Calculate league averages
  league_avg_off <- mean(all_players$OFF_RATING, na.rm = TRUE)
  league_avg_def <- mean(all_players$DEF_RATING, na.rm = TRUE)
  
  cat("  League averages - Off:", round(league_avg_off, 1), 
      "Def:", round(league_avg_def, 1), "\n")
  
  # Prepare plot data with filters
  cat("  Applying plot filters...\n")
  plot_data <- all_players

  # Apply MIN_GAMES filter
  plot_data <- plot_data %>%
    filter(
      GP >= if_else(SEASON == HIGHLIGHT_SEASON, GAMES_CURRENT_SEASON, MIN_GAMES)
    )

  # Apply MIN_MPG filter
  plot_data <- plot_data %>%
    filter(MPG >= MIN_MPG)

  # Apply ROOKIES_ONLY filter
  if (ROOKIES_ONLY) {
    cat("    - Rookies only\n")
    plot_data <- plot_data %>% filter(is_rookie)
  }

  # Apply TEAM filter
  if (!is.null(FILTER_TEAM)) {
    cat("    - Teams:", paste(FILTER_TEAM, collapse = ", "), "\n")
    plot_data <- plot_data %>% filter(TEAM_ABBREVIATION %in% FILTER_TEAM)
  }

  # Apply USAGE filters
  if (!is.null(FILTER_MIN_USAGE)) {
    cat("    - Min usage:", FILTER_MIN_USAGE, "%\n")
    plot_data <- plot_data %>% filter(USG_PCT >= FILTER_MIN_USAGE)
  }

  if (!is.null(FILTER_MAX_USAGE)) {
    cat("    - Max usage:", FILTER_MAX_USAGE, "%\n")
    plot_data <- plot_data %>% filter(USG_PCT <= FILTER_MAX_USAGE)
  }

  cat("  Players after filtering:", nrow(plot_data), "\n")

  # Add normalized ratings
  plot_data <- plot_data %>%
    mutate(
      OFF_RATING_NORM = OFF_RATING - league_avg_off,
      DEF_RATING_NORM = -(DEF_RATING - league_avg_def),  # Flip for better = positive
      NET_RATING_NORM = OFF_RATING_NORM + DEF_RATING_NORM,
      is_highlight_rookie = is_rookie & SEASON == HIGHLIGHT_SEASON
    )
  
  # Get top 5 overall players in highlight season for labeling
  top_players <- plot_data %>%
    filter(SEASON == HIGHLIGHT_SEASON) %>%
    arrange(desc(NET_RATING_NORM)) %>%
    slice_head(n = 5) %>%
    mutate(is_top = TRUE)
  
  plot_data <- plot_data %>%
    left_join(
      top_players %>% select(PLAYER_ID, is_top),
      by = "PLAYER_ID"
    ) %>%
    mutate(
      is_top = ifelse(is.na(is_top), FALSE, is_top),
      label = case_when(
        is_highlight_rookie ~ PLAYER_NAME,
        is_top ~ PLAYER_NAME,
        TRUE ~ ""
      )
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = OFF_RATING_NORM, y = DEF_RATING_NORM)) +
    # Reference lines
    geom_vline(xintercept = 0, linetype = "dashed", 
               color = "gray50", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", 
               color = "gray50", alpha = 0.5) +
    geom_abline(intercept = 0, slope = -1, linetype = "dotted",
                color = "darkgreen", linewidth = 1, alpha = 0.7) +
    # All non-rookie players
    geom_point(
      data = filter(plot_data, !is_rookie),
      aes(size = USG_PCT, alpha = MPG),
      color = "gray70",
      stroke = 0
    ) +
    # Rookie players (non-highlighted season)
    #geom_point(
    #  data = filter(plot_data, is_rookie & !is_highlight_rookie),
    #  aes(size = USG_PCT, alpha = MPG),
    #  color = "#2A9D8F",
    #  stroke = 0
    #) +
    # Highlighted season rookies
    geom_point(
      data = filter(plot_data, is_highlight_rookie),
      aes(size = USG_PCT, alpha = MPG),
      color = "#E63946",
      stroke = 0
    ) +
    # Labels for highlighted rookies
    geom_text_repel(
      data = filter(plot_data, is_highlight_rookie),
      aes(label = PLAYER_NAME),
      size = 3.5,
      fontface = "bold",
      segment.color = "gray50",
      color = "#E63946",
      force = 2,
      box.padding = 0.5,
      point.padding = 0.3,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    # Labels for top overall players
    geom_text_repel(
      data = filter(plot_data, is_top & !is_highlight_rookie),
      aes(label = PLAYER_NAME),
      size = 3,
      fontface = "plain",
      segment.color = "gray70",
      color = "gray40",
      alpha = 0.8,
      force = 1.5,
      box.padding = 0.4,
      point.padding = 0.2,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    # Scales
    scale_size_continuous(
      name = "Usage Rate %",
      range = c(1, 10),
      guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    scale_alpha_continuous(
      name = "Minutes Per Game",
      range = c(0.3, 0.9),
      guide = guide_legend(override.aes = list(size = 4))
    ) +
    # Labels and theme
    labs(
      title = "NBA Player Efficiency Landscape",
      subtitle = paste(HIGHLIGHT_SEASON, "rookies in red",
                      "| Normalized to league average",
                      if (ROOKIES_ONLY) "| ROOKIES ONLY" else ""),
      x = "Offensive Rating vs League Avg (Higher = Better)",
      y = "Defensive Rating vs League Avg (Higher = Better)",
      caption = paste0(
        "Data: ", paste(unique(plot_data$SEASON), collapse = ", "),
        " | Filters: Min ", MIN_GAMES, " GP (", GAMES_CURRENT_SEASON, " for current), ",
        MIN_MPG, " MPG",
        if (ROOKIES_ONLY) ", Rookies only" else "",
        if (!is.null(FILTER_TEAM)) paste0(", Teams: ", paste(FILTER_TEAM, collapse = ", ")) else "",
        if (!is.null(FILTER_MIN_USAGE)) paste0(", Min usage: ", FILTER_MIN_USAGE, "%") else "",
        if (!is.null(FILTER_MAX_USAGE)) paste0(", Max usage: ", FILTER_MAX_USAGE, "%") else "",
        "\n",
        "League averages: Off: ", round(league_avg_off, 1),
        ", Def: ", round(league_avg_def, 1),
        " | Green diagonal = Net Rating = 0 | Size = Usage%, Alpha = MPG"
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray30")
    ) +
    coord_cartesian(clip = "off")
  
  # Save plot
  plot_file <- file.path("season_outputs", "player_efficiency_landscape.png")
  
  ggsave(
    filename = plot_file,
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  cat("Saved plot to:", plot_file, "\n")
  print(p)
}

cat("\n=== Script Complete ===\n")