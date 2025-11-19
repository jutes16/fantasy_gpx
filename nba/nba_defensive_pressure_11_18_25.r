# NBA Defensive Pressure Analysis
# Examines the theory that teams are playing more pressure defense
# install.packages("hoopR")  # if needed

library(hoopR)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(scales)

# ---- Parameters -------------------------------------------------------------
most_recent <- most_recent_nba_season()

# Seasons to analyze (last 5 seasons + current)
seasons <- c(
  purrr::map_chr((most_recent-5):(most_recent-1), year_to_season),
  as.character(most_recent)  # Current season as just year
)

# Control flags
OVERWRITE_EXISTING <- FALSE

# Output directory
OUTPUT_DIR <- "pressure_defense_outputs"
if (!dir.exists(OUTPUT_DIR)) {

  dir.create(OUTPUT_DIR)
}

# ---- Helper Functions -------------------------------------------------------

# Fetch shots data broken down by closest defender distance ranges
get_shots_by_defender_distance <- function(season) {
  # Defender distance categories
  dist_ranges <- c("0-2 Feet - Very Tight", "2-4 Feet - Tight", 
                   "4-6 Feet - Open", "6+ Feet - Wide Open")
  
  results <- map_dfr(dist_ranges, function(dist) {
    tryCatch({
      Sys.sleep(0.3)
      
      data <- nba_leaguedashteamptshot(
        season = season,
        season_type = "Regular Season",
        per_mode = "Totals",
        close_def_dist_range = dist
      )$LeagueDashPTShots
      
      if (is.null(data) || nrow(data) == 0) return(tibble())
      
      data %>%
        mutate(
          SEASON = season,
          DEFENDER_DISTANCE = dist,
          across(c(FGA_FREQUENCY, FGM, FGA, FG_PCT, EFG_PCT, FG2A_FREQUENCY, 
                   FG2M, FG2A, FG2_PCT, FG3A_FREQUENCY, FG3M, FG3A, FG3_PCT),
                 as.numeric)
        )
      
    }, error = function(e) tibble())
  })
  
  return(results)
}

# Fetch team defense tracking data
get_team_defense_tracking <- function(season) {
  # Defense categories to fetch
  categories <- c("Overall", "3 Pointers", "2 Pointers", "Less Than 6Ft", 
                  "Less Than 10Ft", "Greater Than 15Ft")
  
  results <- map_dfr(categories, function(cat) {
    tryCatch({
      Sys.sleep(0.3)
      
      data <- nba_leaguedashptteamdefend(
        season = season,
        season_type = "Regular Season",
        defense_category = cat,
        per_mode = "PerGame"
      )$LeagueDashPtTeamDefend
      
      if (is.null(data) || nrow(data) == 0) return(tibble())
      
      data %>%
        mutate(
          SEASON = season,
          DEFENSE_CATEGORY = cat
        ) %>%
        mutate(across(c(G, FREQ, D_FGM, D_FGA, D_FG_PCT, NORMAL_FG_PCT, PCT_PLUSMINUS),
                      ~as.numeric(as.character(.))))
      
    }, error = function(e) tibble())
  })
  
  return(results)
}

# ---- Main Data Collection ---------------------------------------------------
cat("=== NBA Defensive Pressure Analysis ===\n")
cat("Analyzing seasons:", paste(seasons, collapse = ", "), "\n\n")

# 1. Collect shots by defender distance
cat("1. Collecting shots by defender distance...\n")

defender_dist_file <- file.path(OUTPUT_DIR, "shots_by_defender_distance.csv")

if (file.exists(defender_dist_file) && !OVERWRITE_EXISTING) {
  cat("   Loading from cache...\n")
  shots_by_distance <- read_csv(defender_dist_file, show_col_types = FALSE)
} else {
  shots_by_distance <- map_dfr(seasons, function(s) {
    cat("  Season:", s, "\n")
    get_shots_by_defender_distance(s)
  })
  
  if (nrow(shots_by_distance) > 0) {
    write_csv(shots_by_distance, defender_dist_file)
    cat("   Saved to:", defender_dist_file, "\n")
  }
}

# 2. Collect team defense tracking
cat("\n2. Collecting team defense tracking...\n")

team_defense_file <- file.path(OUTPUT_DIR, "team_defense_tracking.csv")

if (file.exists(team_defense_file) && !OVERWRITE_EXISTING) {
  cat("   Loading from cache...\n")
  team_defense <- read_csv(team_defense_file, show_col_types = FALSE)
} else {
  team_defense <- map_dfr(seasons, function(s) {
    cat("  Season:", s, "\n")
    get_team_defense_tracking(s)
  })
  
  if (nrow(team_defense) > 0) {
    write_csv(team_defense, team_defense_file)
    cat("   Saved to:", team_defense_file, "\n")
  }
}

# ---- Analysis ---------------------------------------------------------------
cat("\n=== Analyzing Defensive Pressure Trends ===\n")

# Aggregate shots by defender distance across league by season
if (nrow(shots_by_distance) > 0) {
  
  # Calculate league-wide totals by season and defender distance
  league_pressure <- shots_by_distance %>%
    group_by(SEASON, DEFENDER_DISTANCE) %>%
    summarise(
      TOTAL_FGA = sum(FGA, na.rm = TRUE),
      TOTAL_FGM = sum(FGM, na.rm = TRUE),
      TOTAL_FG3A = sum(FG3A, na.rm = TRUE),
      TOTAL_FG3M = sum(FG3M, na.rm = TRUE),
      AVG_FG_PCT = TOTAL_FGM / TOTAL_FGA,
      AVG_FG3_PCT = TOTAL_FG3M / TOTAL_FG3A,
      .groups = "drop"
    ) %>%
    group_by(SEASON) %>%
    mutate(
      SEASON_TOTAL_FGA = sum(TOTAL_FGA),
      PCT_OF_SHOTS = TOTAL_FGA / SEASON_TOTAL_FGA
    ) %>%
    ungroup()
  
  # Create tightness categories
  league_pressure <- league_pressure %>%
    mutate(
      PRESSURE_LEVEL = case_when(
        DEFENDER_DISTANCE %in% c("0-2 Feet - Very Tight", "2-4 Feet - Tight") ~ "Contested (0-4 ft)",
        TRUE ~ "Open (4+ ft)"
      )
    )
  
  # Summary by pressure level
  pressure_summary <- league_pressure %>%
    group_by(SEASON, PRESSURE_LEVEL) %>%
    summarise(
      TOTAL_FGA = sum(TOTAL_FGA),
      PCT_OF_SHOTS = sum(PCT_OF_SHOTS),
      .groups = "drop"
    )
  
  cat("\nContested Shot Rate by Season:\n")
  pressure_summary %>%
    filter(PRESSURE_LEVEL == "Contested (0-4 ft)") %>%
    select(SEASON, PCT_OF_SHOTS) %>%
    mutate(PCT_OF_SHOTS = percent(PCT_OF_SHOTS, accuracy = 0.1)) %>%
    print(n = 20)
  
  # ---- Visualization 1: Shot Distribution by Defender Distance ----
  p1 <- ggplot(league_pressure, aes(x = SEASON, y = PCT_OF_SHOTS, fill = DEFENDER_DISTANCE)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      values = c("0-2 Feet - Very Tight" = "#d62728",
                 "2-4 Feet - Tight" = "#ff7f0e",
                 "4-6 Feet - Open" = "#2ca02c",
                 "6+ Feet - Wide Open" = "#1f77b4"),
      name = "Closest Defender"
    ) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "NBA Shot Distribution by Closest Defender Distance",
      subtitle = "Higher red/orange = more defensive pressure",
      x = "Season",
      y = "Percentage of Total Shots",
      caption = "Data: NBA Stats via hoopR"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
  
  ggsave(file.path(OUTPUT_DIR, "shot_distribution_by_defender.png"), p1, 
         width = 12, height = 7, dpi = 300)
  print(p1)
  
  # ---- Visualization 2: Contested Shot Rate Trend ----
  contested_trend <- pressure_summary %>%
    filter(PRESSURE_LEVEL == "Contested (0-4 ft)")
  
  p2 <- ggplot(contested_trend, aes(x = SEASON, y = PCT_OF_SHOTS, group = 1)) +
    geom_line(linewidth = 1.5, color = "#d62728") +
    geom_point(size = 4, color = "#d62728") +
    geom_text(aes(label = percent(PCT_OF_SHOTS, accuracy = 0.1)), 
              vjust = -1, size = 3.5) +
    scale_y_continuous(labels = percent_format(), 
                       limits = c(0, max(contested_trend$PCT_OF_SHOTS) * 1.15)) +
    labs(
      title = "Contested Shot Rate Trend (Defender Within 4 Feet)",
      subtitle = "Are teams playing more pressure defense?",
      x = "Season",
      y = "% of Shots Contested",
      caption = "Data: NBA Stats via hoopR"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(file.path(OUTPUT_DIR, "contested_shot_trend.png"), p2, 
         width = 10, height = 6, dpi = 300)
  print(p2)
  
  # ---- Visualization 3: FG% by Defender Distance Over Time ----
  p3 <- ggplot(league_pressure, aes(x = SEASON, y = AVG_FG_PCT, 
                                     color = DEFENDER_DISTANCE, group = DEFENDER_DISTANCE)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(
      values = c("0-2 Feet - Very Tight" = "#d62728",
                 "2-4 Feet - Tight" = "#ff7f0e",
                 "4-6 Feet - Open" = "#2ca02c",
                 "6+ Feet - Wide Open" = "#1f77b4"),
      name = "Closest Defender"
    ) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Field Goal Percentage by Closest Defender Distance",
      subtitle = "How effectively are teams shooting against different defensive coverages?",
      x = "Season",
      y = "Field Goal %",
      caption = "Data: NBA Stats via hoopR"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
  
  ggsave(file.path(OUTPUT_DIR, "fg_pct_by_defender.png"), p3, 
         width = 12, height = 7, dpi = 300)
  print(p3)
}

# ---- Team Defense Analysis ----
if (nrow(team_defense) > 0) {
  
  # League average defense by category and season
  league_defense <- team_defense %>%
    filter(DEFENSE_CATEGORY == "Overall") %>%
    group_by(SEASON) %>%
    summarise(
      AVG_D_FG_PCT = mean(D_FG_PCT, na.rm = TRUE),
      AVG_NORMAL_FG_PCT = mean(NORMAL_FG_PCT, na.rm = TRUE),
      AVG_PCT_PLUSMINUS = mean(PCT_PLUSMINUS, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nLeague Average Defensive Impact by Season:\n")
  league_defense %>%
    mutate(across(c(AVG_D_FG_PCT, AVG_NORMAL_FG_PCT), ~percent(., accuracy = 0.1)),
           AVG_PCT_PLUSMINUS = round(AVG_PCT_PLUSMINUS, 2)) %>%
    print(n = 20)
  
  # ---- Visualization 4: Defensive FG% Impact ----
  p4 <- ggplot(league_defense, aes(x = SEASON, y = AVG_PCT_PLUSMINUS, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(linewidth = 1.5, color = "#2ca02c") +
    geom_point(size = 4, color = "#2ca02c") +
    geom_text(aes(label = round(AVG_PCT_PLUSMINUS, 2)), 
              vjust = -1, size = 3.5) +
    labs(
      title = "League Average Defensive FG% Impact",
      subtitle = "Negative = defense lowering opponent FG% below normal",
      x = "Season",
      y = "FG% +/- vs Normal",
      caption = "Data: NBA Stats via hoopR"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(file.path(OUTPUT_DIR, "defensive_fg_impact.png"), p4, 
         width = 10, height = 6, dpi = 300)
  print(p4)
}

# ---- Summary Statistics ----
cat("\n=== Summary ===\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("Seasons analyzed:", length(seasons), "\n")

if (nrow(shots_by_distance) > 0) {
  # Calculate year-over-year change in contested rate
  contested_rates <- pressure_summary %>%
    filter(PRESSURE_LEVEL == "Contested (0-4 ft)") %>%
    arrange(SEASON) %>%
    mutate(
      YOY_CHANGE = PCT_OF_SHOTS - lag(PCT_OF_SHOTS),
      YOY_CHANGE_PCT = (PCT_OF_SHOTS - lag(PCT_OF_SHOTS)) / lag(PCT_OF_SHOTS)
    )
  
  latest_season <- tail(contested_rates, 1)
  first_season <- head(contested_rates, 1)
  
  cat("\nKey Findings:\n")
  cat("  First season contested rate:", percent(first_season$PCT_OF_SHOTS, accuracy = 0.1), "\n")
  cat("  Latest season contested rate:", percent(latest_season$PCT_OF_SHOTS, accuracy = 0.1), "\n")
  
  total_change <- latest_season$PCT_OF_SHOTS - first_season$PCT_OF_SHOTS
  cat("  Total change:", ifelse(total_change > 0, "+", ""), 
      percent(total_change, accuracy = 0.1), "\n")
  
  if (!is.na(latest_season$YOY_CHANGE)) {
    cat("  Latest YoY change:", ifelse(latest_season$YOY_CHANGE > 0, "+", ""),
        percent(latest_season$YOY_CHANGE, accuracy = 0.1), "\n")
  }
}

cat("\n=== Script Complete ===\n")