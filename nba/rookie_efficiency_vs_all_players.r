# install.packages("hoopR")  # if needed
library(hoopR)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)

# ---- Parameters -------------------------------------------------------------
most_recent <- most_recent_nba_season()-1
current_season <- as.character(most_recent)

# Configurable filters
MIN_GAMES <- 5  # For current season
MIN_MPG <- 10
MIN_USG_PCT <- 0.05  # 10% usage rate (stored as 0-1 in API)

# Control flags
OVERWRITE_EXISTING <- TRUE
HIGHLIGHT_TOP10_PICKS <- TRUE  # Highlight top 10 draft picks with different color

# Debug flags
DEBUG_MODE <- FALSE

# ---- Helpers ---------------------------------------------------------------
get_all_players_leaderboard <- function(season) {
  Sys.sleep(0.1)

  if (DEBUG_MODE) cat("  [DEBUG] Fetching all players from leaderboard for season:", season, "\n")

  tryCatch({
    # Fetch all players with Advanced stats directly from leaderboard
    players_adv <- nba_leaguedashplayerstats(
      season = season,
      season_type = "Regular Season",
      per_mode = "PerGame",
      measure_type = "Advanced"
    )$LeagueDashPlayerStats %>%
      mutate(
        PLAYER_ID = as.character(PLAYER_ID),
        GP = as.numeric(GP),
        MIN = as.numeric(MIN),
        #MPG = MIN / GP,
        OFF_RATING = as.numeric(OFF_RATING),
        DEF_RATING = as.numeric(DEF_RATING),
        NET_RATING = as.numeric(NET_RATING),
        PIE = as.numeric(PIE),
        USG_PCT = as.numeric(USG_PCT)
      ) %>%
      filter(
        GP >= MIN_GAMES,
        MIN >= MIN_MPG
      ) %>%
      distinct(PLAYER_ID, .keep_all = TRUE)

    if (DEBUG_MODE) {
      cat("  [DEBUG] Found", nrow(players_adv), "players after filters\n")
    }

    if (nrow(players_adv) == 0) {
      cat("    No qualifying players found\n")
      return(tibble())
    }

    result <- players_adv %>%
      filter(USG_PCT >= MIN_USG_PCT) %>%
      mutate(SEASON = season) %>%
      select(SEASON, PLAYER_NAME, TEAM_ABBREVIATION, PLAYER_ID,
             GP, MIN, OFF_RATING, DEF_RATING, NET_RATING,
             PIE, USG_PCT)

    cat("    Found", nrow(result), "qualifying players\n")
    return(result)

  }, error = function(e) {
    cat("    Error fetching leaderboard:", conditionMessage(e), "\n")
    return(tibble())
  })
}

get_rookie_list <- function(season) {
  Sys.sleep(0.1)

  if (DEBUG_MODE) cat("  [DEBUG] Fetching rookie list for season:", season, "\n")

  tryCatch({
    # Fetch just the rookie player IDs
    rookies <- nba_leaguedashplayerstats(
      season = season,
      season_type = "Regular Season",
      player_experience = "Rookie",
      per_mode = "Totals",
      measure_type = "Base"
    )$LeagueDashPlayerStats %>%
      mutate(
        PLAYER_ID = as.character(PLAYER_ID),
        GP = as.numeric(GP),
        MIN = as.numeric(MIN),
        #MPG = MIN / GP
      ) %>%
      filter(
        GP >= MIN_GAMES,
        MIN >= MIN_MPG
      ) %>%
      distinct(PLAYER_ID, .keep_all = TRUE) %>%
      pull(PLAYER_ID)

    if (DEBUG_MODE) {
      cat("  [DEBUG] Found", length(rookies), "qualifying rookies\n")
    }

    return(rookies)

  }, error = function(e) {
    cat("    Error fetching rookies:", conditionMessage(e), "\n")
    return(character(0))
  })
}

get_draft_data <- function(season) {
  Sys.sleep(0.1)

  if (DEBUG_MODE) cat("  [DEBUG] Fetching draft data for season:", season, "\n")

  tryCatch({
    # Convert season to draft year (e.g., "2024" -> 2024)
    draft_year <- as.numeric(season)

    # Fetch draft history data which includes draft position
    draft_data <- nba_drafthistory(season = draft_year)$DraftHistory %>%
      mutate(
        PLAYER_ID = as.character(PLAYER_ID),
        OVERALL_PICK = as.numeric(OVERALL_PICK)
      ) %>%
      filter(!is.na(OVERALL_PICK)) %>%
      select(PLAYER_ID, PLAYER_NAME, OVERALL_PICK) %>%
      distinct(PLAYER_ID, .keep_all = TRUE)

    if (DEBUG_MODE) {
      cat("  [DEBUG] Found draft data for", nrow(draft_data), "players\n")
      top10 <- draft_data %>% filter(OVERALL_PICK <= 10)
      cat("  [DEBUG] Top 10 picks:", nrow(top10), "\n")
    }

    return(draft_data)

  }, error = function(e) {
    if (DEBUG_MODE) {
      cat("  [DEBUG] Error fetching draft data:", conditionMessage(e), "\n")
    }
    # Return empty tibble if draft data unavailable
    return(tibble(PLAYER_ID = character(), PLAYER_NAME = character(), OVERALL_PICK = numeric()))
  })
}

# ---- Main ------------------------------------------------------------------
cat("Processing current season:", current_season, "\n")

if (!dir.exists("season_outputs")) {
  dir.create("season_outputs")
}

# File paths for cached data
combined_file <- file.path("season_outputs", paste0("rookie_vs_all_players_efficiency_", current_season, ".csv"))

# Check if we have cached data
if (file.exists(combined_file) && !OVERWRITE_EXISTING) {
  cat("\n=== Loading cached data ===\n")
  cat("  Loading existing data from CSV...\n")
  player_ratings <- read_csv(combined_file, show_col_types = FALSE)
  cat("  Loaded", nrow(player_ratings), "players from cache\n")
  cat("  Rookies:", sum(player_ratings$IS_ROOKIE), "\n")
  cat("  Veterans:", sum(!player_ratings$IS_ROOKIE), "\n")
} else {
  # Fetch fresh data
  cat("\n=== Fetching All Players from Leaderboard ===\n")
  all_players <- get_all_players_leaderboard(current_season)

  cat("\n=== Fetching Rookie List ===\n")
  rookie_ids <- get_rookie_list(current_season)

  # Fetch draft data if highlighting top 10 picks
  if (HIGHLIGHT_TOP10_PICKS) {
    cat("\n=== Fetching Draft Data ===\n")
    draft_data <- get_draft_data(current_season)
    top10_picks <- draft_data %>%
      filter(OVERALL_PICK <= 10) %>%
      pull(PLAYER_ID)
    cat("  Found", length(top10_picks), "top 10 draft picks\n")
  } else {
    top10_picks <- character(0)
  }

  # Combine and label
  player_ratings <- all_players %>%
    mutate(
      IS_ROOKIE = PLAYER_ID %in% rookie_ids,
      IS_TOP10_PICK = PLAYER_ID %in% top10_picks,
      PLAYER_TYPE = case_when(
        IS_TOP10_PICK ~ "Top 10 Pick",
        IS_ROOKIE ~ "Other Rookie",
        TRUE ~ "Veteran"
      )
    )

  # Save combined results
  if (nrow(player_ratings) > 0) {
    write_csv(player_ratings, file = combined_file)
    cat("\n  Saved combined data to:", combined_file, "\n")
  }
}

# Data validation
if (DEBUG_MODE && nrow(player_ratings) > 0) {
  cat("\n[DEBUG] Data Validation:\n")
  cat("  Total players:", nrow(player_ratings), "\n")
  cat("  Rookies:", sum(player_ratings$IS_ROOKIE), "\n")
  cat("  Veterans:", sum(!player_ratings$IS_ROOKIE), "\n")
  cat("  Rating ranges:\n")
  cat("    OffRtg:", round(range(player_ratings$OFF_RATING, na.rm = TRUE), 1), "\n")
  cat("    DefRtg:", round(range(player_ratings$DEF_RATING, na.rm = TRUE), 1), "\n")
  cat("    NetRtg:", round(range(player_ratings$NET_RATING, na.rm = TRUE), 1), "\n")
}

# Summary statistics
cat("\n=== Summary ===\n")
cat("Total players analyzed:", nrow(player_ratings), "\n")
cat("Rookies:", sum(player_ratings$IS_ROOKIE), "\n")
cat("Veterans:", sum(!player_ratings$IS_ROOKIE), "\n")

cat("\nTop 10 rookies by Net Rating:\n")
print(player_ratings %>%
        filter(IS_ROOKIE) %>%
        arrange(desc(NET_RATING)) %>%
        select(PLAYER_NAME, TEAM_ABBREVIATION, NET_RATING, OFF_RATING, DEF_RATING, USG_PCT, MIN) %>%
        slice_head(n = 10), n = 10)

cat("\nTop 10 overall players by Net Rating:\n")
print(player_ratings %>%
        arrange(desc(NET_RATING)) %>%
        select(PLAYER_NAME, TEAM_ABBREVIATION, PLAYER_TYPE, NET_RATING, OFF_RATING, DEF_RATING, MIN) %>%
        slice_head(n = 10), n = 10)

# Display top 10 draft picks summary if highlighting is enabled
if (HIGHLIGHT_TOP10_PICKS && sum(player_ratings$IS_TOP10_PICK, na.rm = TRUE) > 0) {
  cat("\n=== Top 10 Draft Picks Summary ===\n")
  top10_summary <- player_ratings %>%
    filter(IS_TOP10_PICK) %>%
    arrange(desc(NET_RATING)) %>%
    select(PLAYER_NAME, TEAM_ABBREVIATION, GP, MIN, USG_PCT, OFF_RATING, DEF_RATING, NET_RATING)

  cat("Total top 10 picks found:", nrow(top10_summary), "\n\n")
  print(top10_summary, n = Inf)
}

# ---- Scatter Plot ----------------------------------------------------------
if (nrow(player_ratings) > 0) {
  cat("\n=== Creating Scatter Plot ===\n")

  # Calculate league averages from all players
  league_avg_off <- mean(player_ratings$OFF_RATING, na.rm = TRUE)
  league_avg_def <- mean(player_ratings$DEF_RATING, na.rm = TRUE)

  cat("  League averages - Off:", round(league_avg_off, 1), "Def:", round(league_avg_def, 1), "\n")

  # Prepare data for plotting - normalize around league average and reverse defense
  plot_data <- player_ratings %>%
    mutate(
      label = ifelse(IS_ROOKIE, PLAYER_NAME, ""),
      # Convert USG_PCT to percentage if needed (it's already 0-1 scale, multiply by 100)
      USG_PCT_SCALED = USG_PCT * 100,
      # Normalize: 0 = league average, positive = better
      OFF_RATING_NORM = OFF_RATING - league_avg_off,
      # Reverse defensive rating: lower is better, so negate it
      DEF_RATING_NORM = -(DEF_RATING - league_avg_def)
    )

  # Create plot
  p <- ggplot(plot_data, aes(x = OFF_RATING_NORM, y = DEF_RATING_NORM)) +
    # Reference lines at 0 (league average)
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray50", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray50", alpha = 0.5) +
    # Diagonal line for net rating = 0 (OFF_NORM = DEF_NORM, or y = x)
    geom_abline(intercept = 0, slope = 1, linetype = "dotted",
                color = "darkgreen", linewidth = 1, alpha = 0.7) +
    # All non-top-10-pick players (including other rookies and veterans)
    geom_point(
      data = filter(plot_data, !IS_TOP10_PICK),
      aes(size = USG_PCT_SCALED, alpha = MIN),
      color = "gray60",
      stroke = 0
    ) +
    # Top 10 draft picks
    geom_point(
      data = filter(plot_data, IS_TOP10_PICK),
      aes(size = USG_PCT_SCALED, alpha = MIN),
      color = "#FF6B35",  # Orange color for top 10 picks
      stroke = 0.5,
      shape = 21,
      fill = "#FF6B35"
    ) +
    # Labels for top 10 picks only
    geom_text_repel(
      data = filter(plot_data, IS_TOP10_PICK),
      aes(label = PLAYER_NAME),
      size = 3.5,
      fontface = "bold",
      segment.color = "gray50",
      color = "#FF6B35",
      alpha = 1,
      force = 2.5,
      force_pull = 0.8,
      box.padding = 0.5,
      point.padding = 0.3,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    # Scales
    scale_size_continuous(
      name = "Usage Rate %",
      range = c(0.75, 7),  # Middle ground between c(0.5, 5) and c(1, 10)
      guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    scale_alpha_continuous(
      name = "Minutes Per Game",
      range = c(0.3, 0.9),
      guide = guide_legend(override.aes = list(size = 3.5))
    ) +
    # Labels and theme
    labs(
      title = paste("Rookie Efficiency vs All NBA Players:", current_season, "Season"),
      subtitle = if (HIGHLIGHT_TOP10_PICKS) {
        "Top 10 draft picks in orange, all other players in gray | Normalized to league average (0)"
      } else {
        "Rookies in red vs all players (gray) | Normalized to league average (0)"
      },
      x = "Offensive Rating vs League Avg (Higher = Better)",
      y = "Defensive Rating vs League Avg (Higher = Better)",
      caption = paste0(
        "Season: ", current_season,
        " | Min ", MIN_GAMES, " GP, ", MIN_MPG, " MIN, ", MIN_USG_PCT*100, "% Usage\n",
        "League averages: Off: ", round(league_avg_off, 1),
        ", Def: ", round(league_avg_def, 1), " | Normalized to 0 | Size = Usage%, Alpha = MIN\n",
        "Total players: ", nrow(player_ratings),
        if (HIGHLIGHT_TOP10_PICKS) {
          paste0(" (", sum(player_ratings$IS_TOP10_PICK, na.rm = TRUE), " top 10 picks, ",
                 sum(player_ratings$IS_ROOKIE & !player_ratings$IS_TOP10_PICK, na.rm = TRUE), " other rookies)")
        } else {
          paste0(" (", sum(player_ratings$IS_ROOKIE, na.rm = TRUE), " rookies)")
        }
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
  output_dir <- "season_outputs"
  plot_file <- file.path(output_dir, "rookie_vs_all_players_efficiency.png")

  ggsave(
    filename = plot_file,
    plot = p,
    width = 14,
    height = 10,
    dpi = 300
  )

  cat("Saved plot to:", plot_file, "\n")
  print(p)
}

cat("\n=== Script Complete ===\n")