library(tidyverse)
library(hoopR)
library(janitor)
library(progressr)
library(zoo)
library(glue)
library(ggimage)

highlight_team <- "CLE"
season_year <- 2024
rolling_window <- 15
step_size <- 5


schedule <- hoopR::nba_schedule(season = season_year)
game_days <- unique(as.Date(schedule$game_date))
date_seq <- sort(game_days)
season_end <- max(date_seq)

step_seq <- date_seq[seq(1, length(date_seq), by = step_size)]

team_lookup <- load_nba_team_box(season = season_year) %>%
  distinct(team_id, team_abbreviation, team_display_name, team_logo) %>%
  mutate(team_name = stringr::str_trim(team_display_name))

fetch_team_stats <- function(start_date, end_date) {
  date_from_fmt <- format(start_date, "%m/%d/%Y")
  date_to_fmt <- format(end_date, "%m/%d/%Y")
  stats <- hoopR::nba_leaguedashteamstats(
    season = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    date_from = date_from_fmt,
    date_to = date_to_fmt
  )
  if (is.null(stats) || is.null(stats$LeagueDashTeamStats) || nrow(stats$LeagueDashTeamStats) == 0) return(NULL)
  df <- stats$LeagueDashTeamStats %>%
    select(TEAM_ID, TEAM_NAME, OFF_RATING, DEF_RATING) %>%
    mutate(
      OFF_RATING = suppressWarnings(as.numeric(OFF_RATING)),
      DEF_RATING = suppressWarnings(as.numeric(DEF_RATING)),
      net_rating = OFF_RATING - DEF_RATING,
      game_date = end_date
    )
  return(df)
}

team_stats_list <- list()

handlers(global = TRUE)
with_progress({
  p <- progressor(along = step_seq)
  for (i in seq_along(step_seq)) {
    p(sprintf("Processing window %d of %d", i, length(step_seq)))
    date_from <- step_seq[i]
    date_to <- min(date_from + rolling_window, season_end)
    if (date_from > season_end) break
    stats <- fetch_team_stats(date_from, date_to)
    if (!is.null(stats)) {
      team_stats_list[[length(team_stats_list) + 1]] <- stats
    }
    Sys.sleep(0.25)
    if (date_to == season_end) break
  }
})

effective_k <- ceiling(rolling_window / step_size)
team_daily_net_all <- bind_rows(team_stats_list) %>%
  janitor::clean_names() %>%
  rename_with(~"team_id", matches("team_?id$")) %>%
  mutate(team_id = as.integer(team_id)) %>%  # ensure numeric
  left_join(team_lookup %>% mutate(team_id = as.integer(team_id)), by = "team_id") %>%
  arrange(team_abbreviation, game_date) %>%
  group_by(team_abbreviation) %>%
  mutate(
    rolling_net_rating = zoo::rollmean(net_rating, k = effective_k, fill = NA, align = "right")
  ) %>%
  ungroup()

team_daily_net_list <- team_daily_net_all %>%
  group_split(team_abbreviation) %>%
  set_names(unique(team_daily_net_all$team_abbreviation)) %>%
  map(~ select(.x, game_date, off_rating, def_rating, net_rating, rolling_net_rating))

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

plot_net_rating <- ggplot(team_daily_net_all, aes(x = game_date, y = rolling_net_rating, group = team_abbreviation)) +
  geom_line(data = filter(team_daily_net_all, team_abbreviation != highlight_team), color = "grey70", alpha = 0.4) +
  geom_line(data = filter(team_daily_net_all, team_abbreviation == highlight_team),
            aes(color = team_abbreviation), linewidth = 1.2) +
  scale_color_manual(values = team_colors, guide = "none") +
  labs(
    title = paste("Rolling", rolling_window, "Game Average Net Rating Trend by Team"),
    subtitle = paste("Highlighting:", highlight_team),
    x = "Date",
    y = "Rolling Average Net Rating (Offensive - Defensive)",
    caption = "Data source: hoopR"
  ) +
  theme_minimal(base_size = 14)

logo_data <- team_daily_net_all %>%
  filter(team_abbreviation == highlight_team) %>%
  group_by(team_abbreviation) %>%
  slice_tail(n = 1) %>%
  left_join(team_lookup, by = "team_abbreviation")

plot_net_rating <- plot_net_rating +
  geom_image(
    data = logo_data,
    aes(image = team_logo),
    size = 0.08,
    by = "width",
    asp = 1.0
  )

output_dir <- "nba/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
file_name <- paste0(output_dir, "/net_rating_trend_", highlight_team, ".png")

ggsave(
  filename = file_name,
  plot = plot_net_rating,
  width = 12,
  height = 6,
  dpi = 300
)
message("Saved plot to: ", file_name)
