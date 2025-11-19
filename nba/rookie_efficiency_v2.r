# install.packages(c("hoopR","dplyr","purrr","lubridate","readr"))
library(hoopR)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(stringr)

# ---------------- Parameters ----------------
most_recent <- most_recent_nba_season()   # e.g., 2025 for 2024-25
# Last 10 seasons including the most recent season
seasons <- map_chr((most_recent-9):(most_recent-9), year_to_season)

MIN_GAMES <- 25          # prior seasons rookie GP threshold
MIN_MPG   <- 10
CURR_MIN_GAMES <- 5      # most recent season rookie GP threshold
N_GAMES_SAMPLE <- 10     # first N games to average
cache_dir <- "cache_nba_rookies"
if (!dir.exists(cache_dir)) dir.create(cache_dir)

# ---------------- Helpers ----------------
rookies_for_season <- function(season_chr) {
  # Get rookies with GP and MPG filters
  x <- nba_leaguedashplayerstats(
    season = season_chr,
    season_type = "Regular Season",
    player_experience = "Rookie",
    per_mode = "Totals",
    measure_type = "Base"
  )$LeagueDashPlayerStats %>%
    mutate(
      GP  = suppressWarnings(as.numeric(GP)),
      MIN = suppressWarnings(as.numeric(MIN)),
      MPG = MIN / GP
    )

  thr <- if (season_chr == year_to_season(most_recent - 1)) CURR_MIN_GAMES else MIN_GAMES

  x %>%
    filter(GP >= thr, MPG >= MIN_MPG) %>%
    transmute(
      SEASON = season_chr,
      PLAYER_ID = as.character(PLAYER_ID),
      PLAYER_NAME,
      TEAM_ABBREVIATION,
      GP, MPG
    ) %>%
    distinct(PLAYER_ID, .keep_all = TRUE)
}

# League wide advanced player game logs for a season with simple on disk cache
adv_logs_for_season <- function(season_chr) {
  f <- file.path(cache_dir, paste0("adv_logs_", gsub("[^0-9-]", "_", season_chr), ".rds"))
  if (file.exists(f)) return(readRDS(f))

  # Omit player_id to request league wide logs
  df <- nba_playergamelogs(
    season = season_chr,
    season_type = "Regular Season",
    measure_type = "Advanced",
    per_mode = "PerGame"
  )$PlayerGameLogs %>%
    select(
      PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION,
      GAME_DATE, OFF_RATING, DEF_RATING
    )

  saveRDS(df, f)
  df
}

first_n_avgs <- function(glogs, n_first = N_GAMES_SAMPLE) {
  glogs %>%
    mutate(
      PLAYER_ID   = as.character(PLAYER_ID),
      GAME_DATE   = mdy(GAME_DATE),
      OFF_RATING  = suppressWarnings(as.numeric(OFF_RATING)),
      DEF_RATING  = suppressWarnings(as.numeric(DEF_RATING))
    ) %>%
    arrange(PLAYER_ID, GAME_DATE) %>%
    group_by(PLAYER_ID) %>%
    slice_head(n = n_first) %>%              # take up to first 10 by date
    summarise(
      GAMES       = n(),
      OFFRTG_AVG  = mean(OFF_RATING, na.rm = TRUE),
      DEFRTG_AVG  = mean(DEF_RATING, na.rm = TRUE),
      NETRTG_AVG  = OFFRTG_AVG - DEFRTG_AVG,
      FIRST_GAME  = min(GAME_DATE, na.rm = TRUE),
      LAST_GAME   = max(GAME_DATE, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------- Main ----------------
cat("Processing", length(seasons), "seasons...\n")

rookie_ratings <- map_dfr(seasons, function(season_chr) {
  cat("  Season:", season_chr, "\n")

  rooks <- rookies_for_season(season_chr)
  if (nrow(rooks) == 0) {
    cat("    No qualifying rookies\n")
    return(tibble())
  }

  # One league wide pull
  logs <- adv_logs_for_season(season_chr)

  # Keep only rookies of interest, then compute first 10 averages
  out <- logs %>%
    semi_join(rooks, by = join_by(PLAYER_ID)) %>%
    first_n_avgs(N_GAMES_SAMPLE) %>%
    # keep rookies even if GAMES < 10 per your requirement
    left_join(rooks, by = "PLAYER_ID") %>%
    mutate(SEASON = season_chr) %>%
    select(SEASON, PLAYER_NAME, TEAM_ABBREVIATION, PLAYER_ID, GP, MPG,
           GAMES, FIRST_GAME, LAST_GAME, OFFRTG_AVG, DEFRTG_AVG, NETRTG_AVG)

  cat("    Rookies in table:", nrow(out), "\n")
  out
})

rookie_ratings <- rookie_ratings %>%
  arrange(SEASON, desc(NETRTG_AVG))

# Summary
cat("\n=== Summary ===\n")
cat("Total rookies:", nrow(rookie_ratings), "\n")
if (nrow(rookie_ratings) > 0) {
  cat("Seasons covered:", paste(range(rookie_ratings$SEASON), collapse = " to "), "\n")
  cat("\nTop 10 by Net Rating:\n")
  print(
    rookie_ratings %>%
      select(SEASON, PLAYER_NAME, TEAM_ABBREVIATION, GAMES, NETRTG_AVG, OFFRTG_AVG, DEFRTG_AVG) %>%
      slice_head(n = 10),
    n = 10
  )
}

rookie_ratings