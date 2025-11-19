# install.packages("hoopR")  # if needed
library(hoopR)
library(dplyr)
library(purrr)
library(progress)
library(readr)
library(ggplot2)
library(ggrepel)

# ---- Parameters -------------------------------------------------------------
most_recent <- most_recent_nba_season()
# Fix: Use just the year for current season instead of year_to_season format
seasons <- c(
  purrr::map_chr((most_recent-1):(most_recent-1), year_to_season),
  as.character(most_recent)  # Current season as just "2025"
)

# Configurable filters
MIN_GAMES <- 25
MIN_MPG <- 10
GAMES_CURRENT_SEASON <- 5
N_GAMES_SAMPLE <- 10

# Control flags
OVERWRITE_EXISTING <- FALSE
HIGHLIGHT_SEASON <- "2025-26" #as.character(most_recent)  # Use current season

# Debug flags
DEBUG_MODE <- TRUE
PROBLEMATIC_PIDS <- c("1642879")  # Known problematic player IDs

# ---- Helpers ---------------------------------------------------------------
get_rookie_ratings <- function(season) {
  Sys.sleep(0.1)
  
  if (DEBUG_MODE) cat("  [DEBUG] Processing season:", season, "\n")
  
  tryCatch({
    rookies <- nba_leaguedashplayerstats(
      season = season,
      season_type = "Regular Season",
      player_experience = "Rookie",
      per_mode = "Totals",
      measure_type = "Base"
    )$LeagueDashPlayerStats %>%
      mutate(
        GP = as.numeric(GP),
        MIN = as.numeric(MIN),
        MPG = MIN / GP
      ) %>%
      filter(
        GP >= if (season == as.character(most_recent)) GAMES_CURRENT_SEASON else MIN_GAMES,
        MPG >= MIN_MPG,
        #!PLAYER_ID %in% PROBLEMATIC_PIDS  # Filter out known bad PIDs
      ) %>%
      distinct(PLAYER_ID, .keep_all = TRUE)
    
    if (DEBUG_MODE) {
      cat("  [DEBUG] Found", nrow(rookies), "rookies after initial filters\n")
      if (nrow(rookies) > 0) {
        cat("  [DEBUG] Sample PIDs:", paste(head(rookies$PLAYER_ID, 3), collapse = ", "), "\n")
      }
    }
    
    if (nrow(rookies) == 0) {
      cat("    No qualifying rookies found\n")
      return(tibble())
    }
    
    cat("    Fetching game logs for", nrow(rookies), "rookies...\n")
    
    pb <- progress_bar$new(
      format = "    Progress [:bar] :current/:total (:percent) ETA: :eta",
      total = nrow(rookies), clear = FALSE, width = 60
    )
    
    adv_logs <- map_dfr(rookies$PLAYER_ID, function(pid) {
      res <- tryCatch({
        data <- nba_playergamelogs(
          season = season,
          season_type = "Regular Season",
          player_id = pid,
          measure_type = "Advanced",
          per_mode = "PerGame"
        )$PlayerGameLogs %>%
          mutate(
            PLAYER_ID = as.character(PLAYER_ID),
            OFF_RATING = as.numeric(OFF_RATING),
            DEF_RATING = as.numeric(DEF_RATING),
            NET_RATING = OFF_RATING - DEF_RATING,
            E_OFF_RATING = as.numeric(E_OFF_RATING),
            E_DEF_RATING = as.numeric(E_DEF_RATING),
            E_NET_RATING = E_OFF_RATING - E_DEF_RATING,
            sp_work_OFF_RATING = as.numeric(sp_work_OFF_RATING),
            sp_work_DEF_RATING = as.numeric(sp_work_DEF_RATING),
            sp_work_NET_RATING = sp_work_OFF_RATING - sp_work_DEF_RATING,
            MIN = as.numeric(MIN),
            PIE = as.numeric(PIE),
            USG_PCT = as.numeric(USG_PCT)
          ) %>%
          arrange(GAME_DATE) %>%
          slice_head(n = N_GAMES_SAMPLE)
        
        if (DEBUG_MODE && nrow(data) == 0) {
          cat("\n  [DEBUG] No game logs for PID:", pid, "\n")
        }
        
        data
      }, error = function(e) {
        if (DEBUG_MODE) {
          cat("\n  [DEBUG] Error fetching PID:", pid, "-", conditionMessage(e), "\n")
        }
        tibble()
      })
      
      pb$tick()
      res
    }) %>%
      group_by(PLAYER_ID) %>%
      summarise(
        GAMES = n(),
        MIN_TOTAL = sum(as.numeric(MIN), na.rm = TRUE),
        OFFRTG_AVG = weighted.mean(OFF_RATING, as.numeric(MIN), na.rm = TRUE),
        DEFRTG_AVG = weighted.mean(DEF_RATING, as.numeric(MIN), na.rm = TRUE),
        NETRTG_AVG = OFFRTG_AVG - DEFRTG_AVG,
        E_OFF_RATING = weighted.mean(E_OFF_RATING, as.numeric(MIN), na.rm = TRUE),
        E_DEF_RATING = weighted.mean(E_DEF_RATING, as.numeric(MIN), na.rm = TRUE),
        E_NET_RATING = E_OFF_RATING - E_DEF_RATING,
        sp_work_OFF_RATING = weighted.mean(sp_work_OFF_RATING, as.numeric(MIN), na.rm = TRUE),
        sp_work_DEF_RATING = weighted.mean(sp_work_DEF_RATING, as.numeric(MIN), na.rm = TRUE),
        sp_work_NET_RATING = sp_work_OFF_RATING - sp_work_DEF_RATING,
        PIE_AVG = weighted.mean(PIE, as.numeric(MIN), na.rm = TRUE),
        USG_PCT_AVG = weighted.mean(USG_PCT*100, as.numeric(MIN), na.rm = TRUE),
        FIRST_GAME = min(GAME_DATE, na.rm = TRUE),
        LAST_GAME = max(GAME_DATE, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (DEBUG_MODE) {
      cat("  [DEBUG] Players with game logs:", nrow(adv_logs), "\n")
      # Check for invalid ratings
      invalid_ratings <- adv_logs %>%
        filter(is.na(OFFRTG_AVG) | is.na(DEFRTG_AVG) | 
               is.infinite(OFFRTG_AVG) | is.infinite(DEFRTG_AVG))
      if (nrow(invalid_ratings) > 0) {
        cat("  [DEBUG] Found", nrow(invalid_ratings), "players with invalid ratings\n")
      }
    }
    
    if (nrow(adv_logs) == 0) {
      cat("    No players with sufficient game logs\n")
      return(tibble())
    }
    
    result <- rookies %>%
      inner_join(adv_logs, by = "PLAYER_ID") %>%
      mutate(SEASON = season) %>%
      select(SEASON, PLAYER_NAME, TEAM_ABBREVIATION, PLAYER_ID, GP, MPG,
             GAMES, MIN_TOTAL, FIRST_GAME, LAST_GAME, OFFRTG_AVG, DEFRTG_AVG, 
             NETRTG_AVG, E_OFF_RATING, E_DEF_RATING, E_NET_RATING,
             sp_work_OFF_RATING, sp_work_DEF_RATING, sp_work_NET_RATING, 
             PIE_AVG, USG_PCT_AVG)
    
    cat("    Found", nrow(result), "qualifying rookies\n")
    return(result)
    
  }, error = function(e) {
    cat("    Error processing season:", conditionMessage(e), "\n")
    return(tibble())
  })
}

# ---- Main ------------------------------------------------------------------
cat("Processing", length(seasons), "seasons...\n")
if (DEBUG_MODE) cat("[DEBUG] Seasons:", paste(seasons, collapse = ", "), "\n")

if (!dir.exists("season_outputs")) {
  dir.create("season_outputs")
}

# Load or fetch data for each season
rookie_ratings <- map_dfr(seasons, function(s) {
  cat("\n=== Season:", s, "===\n")
  
  season_file <- file.path("season_outputs", paste0("rookie_efficiency_", s, ".csv"))
  
  if (file.exists(season_file) && !OVERWRITE_EXISTING) {
    cat("  Loading existing data from CSV...\n")
    result <- read_csv(season_file, show_col_types = FALSE)
    cat("  Loaded", nrow(result), "rookies from cache\n")
    return(result)
  }
  
  result <- get_rookie_ratings(s)
  
  if (nrow(result) > 0) {
    write_csv(result, file = season_file)
    cat("  Saved to:", season_file, "\n")
  }
  
  return(result)
})

# Data validation
if (DEBUG_MODE && nrow(rookie_ratings) > 0) {
  cat("\n[DEBUG] Data Validation:\n")
  cat("  Total rows:", nrow(rookie_ratings), "\n")
  cat("  Unique players:", n_distinct(rookie_ratings$PLAYER_ID), "\n")
  cat("  Rating ranges:\n")
  cat("    OffRtg:", round(range(rookie_ratings$OFFRTG_AVG, na.rm = TRUE), 1), "\n")
  cat("    DefRtg:", round(range(rookie_ratings$DEFRTG_AVG, na.rm = TRUE), 1), "\n")
  cat("    NetRtg:", round(range(rookie_ratings$NETRTG_AVG, na.rm = TRUE), 1), "\n")
  cat("    Usage%:", round(range(rookie_ratings$USG_PCT_AVG, na.rm = TRUE), 1), "\n")
}

# Save combined results
if (nrow(rookie_ratings) > 0) {
  write_csv(rookie_ratings, file = file.path("season_outputs", "rookie_efficiency.csv"))
}

# Sort by net rating
rookie_ratings <- rookie_ratings %>%
  arrange(SEASON, desc(NETRTG_AVG))

# Summary statistics
cat("\n=== Summary ===\n")
cat("Total rookies analyzed:", nrow(rookie_ratings), "\n")
cat("Seasons covered:", paste(unique(rookie_ratings$SEASON), collapse = ", "), "\n")
cat("\nTop 10 rookies by Net Rating:\n")
print(rookie_ratings %>% 
        select(SEASON, PLAYER_NAME, TEAM_ABBREVIATION, NETRTG_AVG, OFFRTG_AVG, DEFRTG_AVG) %>%
        slice_head(n = 10), n = 10)

# ---- Scatter Plot ----------------------------------------------------------
if (nrow(rookie_ratings) > 0) {
  cat("\n=== Creating Scatter Plot ===\n")
  
  # Ensure MIN_TOTAL exists
  if (!"MIN_TOTAL" %in% names(rookie_ratings)) {
    rookie_ratings <- rookie_ratings %>%
      mutate(MIN_TOTAL = MPG * GAMES)
  }
  
  # Prepare data with highlight flag
  plot_data <- rookie_ratings %>%
    mutate(
      is_highlight = SEASON == HIGHLIGHT_SEASON,
      label = ifelse(is_highlight, PLAYER_NAME, "")
    )
  
  # Calculate league averages
  league_avg_off <- 115
  league_avg_def <- 115
  
  # Create plot with updated aesthetics
  p <- ggplot(plot_data, aes(x = OFFRTG_AVG, y = DEFRTG_AVG)) +
    # Reference lines
    geom_vline(xintercept = league_avg_off, linetype = "dashed", 
               color = "gray50", alpha = 0.5) +
    geom_hline(yintercept = league_avg_def, linetype = "dashed", 
               color = "gray50", alpha = 0.5) +
    # Diagonal line for net rating = 0
    geom_abline(intercept = 0, slope = -1, linetype = "dotted", 
                color = "darkgreen", linewidth = 1, alpha = 0.7) +
    # Non-highlighted rookies
    geom_point(
      data = filter(plot_data, !is_highlight),
      aes(size = USG_PCT_AVG, alpha = MPG),
      color = "gray60",
      stroke = 0
    ) +
    # Highlighted season rookies
    geom_point(
      data = filter(plot_data, is_highlight),
      aes(size = USG_PCT_AVG, alpha = MPG),
      color = "#E63946",
      stroke = 0
    ) +
    # Labels for highlighted rookies
    geom_text_repel(
      data = filter(plot_data, is_highlight),
      aes(label = PLAYER_NAME),
      size = 3.5,
      fontface = "bold",
      segment.color = "gray50",
      color = "#E63946",
      alpha = 1,
      force = 2,
      force_pull = 0.8,
      box.padding = 0.5,
      point.padding = 0.3,
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
      title = paste("Rookie Efficiency Landscape: First", N_GAMES_SAMPLE, "Games"),
      subtitle = paste("Highlighting", HIGHLIGHT_SEASON, "rookies in red | Green diagonal = Net Rating = 0"),
      x = "Offensive Rating (Higher = Better)",
      y = "Defensive Rating (Lower = Better)",
      caption = paste0(
        "Data: ", paste(unique(rookie_ratings$SEASON), collapse = ", "), 
        " | Min ", MIN_GAMES, " GP (", GAMES_CURRENT_SEASON, " for current), ", MIN_MPG, " MPG\n",
        "Dashed lines = League average (~115) | Size = Usage%, Alpha = MPG"
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
  plot_file <- file.path(output_dir, "rookie_efficiency_landscape.png")
  
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