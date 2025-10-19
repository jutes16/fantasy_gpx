# Set working directory to the "nba" folder within the project
setwd("nba")

# ---- Robust installation check for required packages ----
required_packages <- c(
  "tidyverse", "hoopR", "janitor", "hablar", "paletteer", "prismatic", "ggpmisc"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# ---- End installation check ----

# for data wrangling, nba data, etc. (libraries loaded above)

# custom ggplot2 theme
theme_f5 <- function (font_size = 9) { 
  theme_minimal(base_size = font_size, base_family = "sans") %+replace% 
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'), 
      plot.subtitle = element_text(color = 'gray65', hjust = 0, margin=margin(2.5,0,10,0), size = 11), 
      plot.caption = element_text(color = 'gray65', margin=margin(-5,0,0,0), hjust = 1, size = 6)
    )
}

# create player lookup 
player_lookup <- nba_playerindex()[[1]] %>%
  clean_names() %>%
  transmute(person_id = as.numeric(person_id),
            first_year = from_year)

# ---- Add caching logic for sl_logs and rs_logs ----
if (!dir.exists("data")) dir.create("data")

sl_logs <- NULL
if (file.exists("data/sl_logs_cache.rds")) {
  sl_logs <- readRDS("data/sl_logs_cache.rds")
  if (is.null(sl_logs) || (is.data.frame(sl_logs) && nrow(sl_logs) == 0) || (is.list(sl_logs) && length(sl_logs) == 0)) {
    file.remove("data/sl_logs_cache.rds")
    sl_logs <- NULL
  }
}
if (is.null(sl_logs)) {
  sl_logs <- map_df(map_chr(2008:2025, year_to_season), function(x) {
    message(sprintf("Fetching summer league logs for season %s...", x))
    league_ids <- c(13, 15, 16)
    league_names <- c("California Classic", "Vegas", "Salt Lake City")
    league_results <- vector("list", length(league_ids))
    for (i in seq_along(league_ids)) {
      lid <- league_ids[i]
      lname <- league_names[i]
      result <- tryCatch({
        logs <- nba_leaguegamelog(season = x, league_id = lid, player_or_team = 'P', season_type = "Regular Season")
        # Check for empty or missing data
        if (is.null(logs) || !"LeagueGameLog" %in% names(logs) || is.null(logs$LeagueGameLog) || nrow(logs$LeagueGameLog) == 0) {
          message(sprintf("[%s] No data found for league_id %s, season %s. Skipping.", lname, lid, x))
          return(NULL)
        }
        df <- logs$LeagueGameLog
        # Only assign plus_minus if column exists
        if ("plus_minus" %in% names(df)) {
          df$plus_minus <- as.numeric(df$plus_minus)
        }
        df$season <- x
        df$league_id <- lid
        message(sprintf("[%s] Successfully fetched %d rows for season %s.", lname, nrow(df), x))
        return(df)
      }, error = function(e) {
        message(sprintf("[%s] Error fetching league_id %s for season %s: %s. Skipping.", lname, lid, x, e$message))
        return(NULL)
      })
      league_results[[i]] <- result
    }
    # Only bind non-null data frames
    valid_dfs <- Filter(Negate(is.null), league_results)
    if (length(valid_dfs) == 0) {
      message(sprintf("No valid summer league data for season %s. Skipping.", x))
      return(NULL)
    }
    df <- bind_rows(valid_dfs)
    return(df)
  })
  sl_logs <- Filter(Negate(is.null), sl_logs)
  if (is.data.frame(sl_logs) && nrow(sl_logs) > 0) {
    saveRDS(sl_logs, "data/sl_logs_cache.rds")
  }
}

rs_logs <- NULL
if (file.exists("data/rs_logs_cache.rds")) {
  rs_logs <- readRDS("data/rs_logs_cache.rds")
  if (is.null(rs_logs) || (is.data.frame(rs_logs) && nrow(rs_logs) == 0) || (is.list(rs_logs) && length(rs_logs) == 0)) {
    file.remove("data/rs_logs_cache.rds")
    rs_logs <- NULL
  }
}
if (is.null(rs_logs)) {
  rs_logs <- map_df(map_chr(2008:2024, year_to_season), function(x) {
    message(sprintf("Fetching NBA regular season logs for season %s...", x))
    result <- tryCatch({
      nba_logs <- nba_leaguegamelog(season = x, league_id = "00", player_or_team = 'P', season_type = "Regular Season")
      # check for valid data structure
      if (is.null(nba_logs) || !"LeagueGameLog" %in% names(nba_logs) || is.null(nba_logs$LeagueGameLog) || nrow(nba_logs$LeagueGameLog) == 0) {
        warning(sprintf("No data found for season %s. Skipping.", x))
        return(NULL)
      }
      nba_logs <- nba_logs$LeagueGameLog
      # Conditionally convert plus_minus column to numeric
      if ("PLUS_MINUS" %in% names(nba_logs)) {
        nba_logs$PLUS_MINUS <- as.numeric(nba_logs$PLUS_MINUS)
      } else if ("plus_minus" %in% names(nba_logs)) {
        nba_logs$plus_minus <- as.numeric(nba_logs$plus_minus)
      } else {
        message(sprintf("plus_minus column not found for season %s.", x))
      }
      nba_logs$season <- x
      message(sprintf("Successfully fetched %d rows for season %s.", nrow(nba_logs), x))
      return(nba_logs)
    }, error = function(e) {
      warning(sprintf("Error fetching data for season %s: %s. Skipping.", x, e$message))
      return(NULL)
    })
    return(result)
  })
  rs_logs <- Filter(Negate(is.null), rs_logs)
  if (is.data.frame(rs_logs) && nrow(rs_logs) > 0) {
    saveRDS(rs_logs, "data/rs_logs_cache.rds")
  }
}

# Move clean_names and retype after bind_rows for sl_logs and after map_df for rs_logs
# dtplyr optimization: use lazy_dt for summarization, then as_tibble
library(dtplyr)

# Guard clause for summer league logs
if (nrow(sl_logs) == 0) stop("No valid summer league logs found.")

# Clean summer league data
sl_logs <- sl_logs %>% janitor::clean_names() %>% hablar::retype()
sl_df <- sl_logs %>%
  mutate(across(c(fg3a, fg3m, fga, ftm, fta, min, pts, ast, tov, blk, stl, oreb, dreb), as.numeric)) %>%
  mutate(season = parse_number(season), gp = 1) %>%
  relocate(gp, .before = min) %>%
  relocate(plus_minus, .before = fantasy_pts) %>%
  select(-ends_with("_pct"), -game_date, -matchup, -game_id, -wl, -video_available) %>%
  lazy_dt() %>%
  group_by(player_id, player_name, season) %>%
  summarise(across(gp:fantasy_pts, ~ sum(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(
    across(fgm:fantasy_pts, ~ (. / min) * 36, .names = "{.col}_per36"),
    mpg = min / gp,
    game_score = (pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) + 0.7 * oreb + 0.3 * dreb + stl + 0.7 * ast + 0.7 * blk - 0.4 * pf - tov) / min * 36,
    dre = (.79 * pts - .72 * (fga - fg3a) - .55 * fg3a - .16 * fta + .13 * oreb + .40 * dreb + .54 * ast + 1.68 * stl + .76 * blk - 1.36 * tov - .11 * pf) / min * 36,
    fg3_pct = if_else(fg3a <= 4, NA_real_, fg3m / fg3a),
    fg2_pct = if_else(fga - fg3a <= 4, NA_real_, (fgm - fg3m) / (fga - fg3a)),
    ft_pct = if_else(fta <= 4, NA_real_, ftm / fta),
    ft_rate = fta / fga,
    fg3a_rate = fg3a / fga,
    ast_to = ast/tov,
    ts_pct = pts / (2 * (fga + 0.44 * fta)),
    efg_pct = ((fgm-fg3m) + 1.5 * fg3m) / fga
  ) %>%
  left_join(player_lookup, by = c("player_id" = "person_id")) %>%
  filter(season == as.numeric(first_year), min >= 50) %>%
  select(
    player_id,
    player_name,
    season,
    mpg,
    ends_with("_per36"),
    game_score,
    dre,
    ast_to,
    ends_with("_pct"),
    ends_with("_rate")
  ) %>%
  as_tibble() %>%
  pivot_longer(-c(player_id, player_name, season), names_to = 'stat', values_to = 'summer')

# Guard clause for regular season logs
if (nrow(rs_logs) == 0) stop("No valid regular season logs found.")

# Clean regular season data
rs_logs <- rs_logs %>% janitor::clean_names() %>% hablar::retype()
rs_df <- rs_logs %>%
  mutate(across(c(fg3a, fg3m, fga, ftm, fta, min, pts, ast, tov, blk, stl, oreb, dreb), as.numeric)) %>%
  mutate(season = parse_number(season), gp = 1) %>%
  relocate(plus_minus, .before = fantasy_pts) %>%
  relocate(gp, .before = min) %>%
  select(-ends_with("_pct"), -game_date, -matchup, -game_id, -wl, -video_available) %>%
  lazy_dt() %>%
  group_by(player_id, player_name, season) %>%
  summarise(across(gp:fantasy_pts, ~ sum(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(
    across(fgm:fantasy_pts, ~ (. / min) * 36, .names = "{.col}_per36"),
    mpg = min / gp,
    game_score = (pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) + 0.7 * oreb + 0.3 * dreb + stl + 0.7 * ast + 0.7 * blk - 0.4 * pf - tov) / min * 36,
    dre = (.79 * pts - .72 * (fga - fg3a) - .55 * fg3a - .16 * fta + .13 * oreb + .40 * dreb + .54 * ast + 1.68 * stl + .76 * blk - 1.36 * tov - .11 * pf) / min * 36,
    fg3_pct = if_else(fg3a <= 4, NA_real_, fg3m / fg3a),
    fg2_pct = if_else(fga - fg3a <= 4, NA_real_, (fgm - fg3m) / (fga - fg3a)),
    ft_pct = if_else(fta <= 4, NA_real_, ftm / fta),
    ft_rate = fta / fga,
    fg3a_rate = fg3a / fga,
    ast_to = ast/tov,
    ts_pct = pts / (2 * (fga + 0.44 * fta)),
    efg_pct = ((fgm-fg3m) + 1.5 * fg3m) / fga
  ) %>%
  left_join(player_lookup, by = c("player_id" = "person_id")) %>%
  filter(season == as.numeric(first_year), min >= 250) %>%
  select(
    player_id,
    player_name,
    season,
    mpg,
    ends_with("_per36"),
    game_score,
    dre,
    ast_to,
    ends_with("_pct"),
    ends_with("_rate")
  ) %>%
  as_tibble() %>%
  pivot_longer(-c(player_id, player_name, season), names_to = 'stat', values_to = 'reg_season')

# which stats i want to show
stats_to_show <- c(
  "fg3a_per36",
  "blk_per36",
  "ast_per36",
  "mpg",
  "reb_per36",
  "oreb_per36",
  "dreb_per36",
  "pf_per36",
  "fga_per36",
  "fta_per36",
  "tov_per36",
  "pts_per36",
  "stl_per36",
  "plus_minus_per36",
  "ts_pct",
  "efg_pct",
  "game_score",
  "ft_pct",
  "dre",
  "fg2_pct",
  "fg3_pct",
  "fg3a_rate",
  "plus_minus_per36",
  "ft_rate",
  "ast_to", 
  "fantasy_pts_per36"
)

# combine summer league and regular season, and simplify mutate chain
df <- left_join(sl_df, rs_df, by = c("player_id", "stat", "season")) %>%
  filter(stat %in% stats_to_show) %>%
  mutate(
    stat = case_match(
      stat,
      "mpg" ~ "MPG",
      "ast_per36" ~ "AST per 36",
      "blk_per36" ~ "BLK per 36", 
      "dreb_per36" ~ "DRB per 36",
      "reb_per36" ~ "TRB per 36",
      "fg3a_per36" ~ "3PA per 36",
      "ft_pct" ~ "FT%",
      "fta_per36" ~ "FTA per 36",
      "fga_per36" ~ "FGA per 36",
      "oreb_per36" ~ "ORB per 36",
      "pf_per36" ~ "PF per 36",
      "pts_per36" ~ "PTS per 36",
      "stl_per36" ~ "STL per 36",
      "tov_per36" ~ "TOV per 36",
      "ts_pct" ~ "TS%",
      "efg_pct" ~ "eFG%",
      "game_score" ~ "Game Score per 36",
      "dre" ~ "DRE per 36",
      "fg2_pct" ~ "2P%",
      "fg3_pct" ~ "3P%",
      "ast_to" ~ "AST/TO",
      "ft_rate" ~ "Free Throw Attempt Rate",
      "fg3a_rate" ~ "3PT Attempt Rate",
      "fantasy_pts_per36" ~ "Fantasy Points per 36",
      "plus_minus_per36" ~ "+/- per 36",
      .default = stat
    )
  )

# find correlation
correlations <- df %>%
  na.omit() %>% 
  group_by(stat) %>%
  summarise(correlation = (cor(summer, reg_season, use = "complete.obs"))^2) %>% 
  arrange(-correlation)

# join in correlations
df <- df %>%
  left_join(., correlations, by = c("stat")) 

# convert stat to factor and order by strength of correlation
df$stat <- as.factor(df$stat)
df$stat <- factor(df$stat, levels = correlations$stat)

# create a dummy dataset for our facet limits
facetlims <- df %>%
  na.omit() %>%
  ungroup() %>%
  group_by(stat) %>%
  summarise(
    min = min(summer, reg_season, na.rm = TRUE),
    max = max(summer, reg_season, na.rm = TRUE)
  ) %>%
  pivot_longer(-stat, names_to = "summer", values_to = "reg_season") %>%
  mutate(summer = reg_season) %>% 
  left_join(., correlations, by = "stat") 

# make plot
p <- df %>%
  # remove NAs
  na.omit() %>%
  # summer stats on x axis, regular season stats on y axis
  ggplot(aes(x = summer, y = reg_season)) +
  # add line of best fit
  geom_smooth(aes(color = correlation), method = 'lm', linewidth = .25) +
  # add geom points
  geom_point(
    aes(fill = correlation, color = after_scale(clr_darken(fill, 0.3))),
    size = .666,
    shape = 21,
    alpha = 0.35
  ) +
  # layer on the dummy datset so that our axis limits on each facet are equal
  geom_blank(data = facetlims) +
  # add r2
  stat_poly_eq(size = 2, label.x = 0.5, label.y = 1) +
  # set fill
  scale_fill_paletteer_c("grDevices::Sunset", direction = -1)  +
  # set color
  scale_color_paletteer_c("grDevices::Sunset", direction = -1)  +
  # turn into a mini multiple
  facet_wrap(~as.factor(stat), scales = 'free', nrow = 5) +
  # call custom theme
  theme_f5() +
  # theme tweaks
  theme(
    legend.position = 'none',
    plot.title = element_text(face = 'bold', size = 14, hjust = .5),
    plot.subtitle = element_text(size = 7, hjust = .5),
    plot.title.position = 'plot',
    axis.text = element_text(size = 4.5),
    strip.text = element_text(size = 6.5, vjust = -1, face = 'bold'),
    strip.clip = "off"
  ) +
  # add axis titles and plot titles
  labs(
    x = "Summer League Stats",
    y = "Rookie Season Stats",
    title = "Summer League Stats vs. Rookie Season Stats",
    subtitle = "Minimum 50 minutes in first summer league & 250 minutes in corresponding rookie season (2008 - 2024)"
  ) 
# save plot
ggsave("r_sticky_summer.png", p, w = 6, h = 6, dpi = 300)
