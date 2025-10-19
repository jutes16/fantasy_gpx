required_packages <- c(
  "tidyverse", "hoopR", "janitor", "ggrepel", "jsonlite", "rvest","tictoc", "progressr", "purrr"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# %% Load NBA game data for the  season
season_year <- 2024

nba_team_box <- hoopR::load_nba_team_box(season_year)
nba_player_box <- hoopR::load_nba_player_box(season_year)

# Filter starters and select relevant columns
message("Fetching starter RPM stats for each unique starter...")
starters <- nba_player_box %>%
  filter(starter == TRUE) %>%
  select(athlete_id, athlete_display_name, team_abbreviation, game_id) %>%
  distinct()

# Define a safe version of espn_nba_player_stats to handle errors gracefully
safe_espn_stats <- purrr::possibly(function(id) {
  espn_nba_player_stats(id, season_year, season_type = "regular", total = FALSE)
}, otherwise = NULL)

adv_data <- nba_leaguedashplayerstats(measure_type = "Advanced",season=2024)
adv_data <- adv_data$LeagueDashPlayerStats %>% janitor::clean_names()

# Convert net_rating to numeric and rename to net_rtg
adv_data <- adv_data %>%
  mutate(net_rtg = as.numeric(net_rating))

# %% Fetch RPM stats for each unique starter
tictoc::tic()
progressr::with_progress({
  p <- progressr::progressor(steps = length(unique(starters$athlete_id)))
  
  starter_rpm_db <- purrr::map_dfr(unique(starters$athlete_id), function(id) {
    p(message = paste0("Fetching stats for athlete_id: ", id))  # increment + display
    stats <- safe_espn_stats(id)
    
    if (is.null(stats)) {
      return(tibble())
    }
    if (!"offensive_orpm" %in% names(stats)) stats$offensive_orpm <- NA_real_
    if (!"defensive_drpm" %in% names(stats)) stats$defensive_drpm <- NA_real_
    
    stats %>%
      mutate(athlete_id = id) %>%
      select(
        athlete_id,
        full_name,
        team_abbreviation,
        offensive_orpm,
        defensive_drpm
      ) %>%
      distinct()
  })
})
tictoc::toc()

# Join net_rtg data to starter_rpm_db by player name and team abbreviation
starter_rpm_db <- starter_rpm_db %>%
  left_join(
    adv_data %>% select(player_name, team_abbreviation, net_rtg),
    by = c("full_name" = "player_name", "team_abbreviation" = "team_abbreviation")
  )

starter_rpm_db <- starter_rpm_db %>%
  mutate(total_rpm = offensive_orpm + defensive_drpm)

# View the result
print(starter_rpm_db)
# %%
# ---- Compute team-level strongest/weakest starter stats and score differentials ----
message("Summarizing strongest/weakest starter RPMs and score differentials per game...")
unique_game_ids <- unique(nba_team_box$game_id)

game_link_summary <- progressr::with_progress({
  p <- progressr::progressor(steps = length(unique_game_ids))
  purrr::map_dfr(unique_game_ids, function(game_id) {
    p(message = paste0("Processing game_id: ", game_id))
    # Get home and away teams for this game
    teams_this_game <- nba_team_box %>% filter(game_id == !!game_id)
    if (nrow(teams_this_game) != 2) return(tibble()) # skip malformed games
    home_team <- teams_this_game %>% filter(team_home_away == "home") %>% as_tibble()
    away_team <- teams_this_game %>% filter(team_home_away == "away") %>% as_tibble()

    home_score <- dplyr::pull(home_team, team_score)[1]
    away_score <- dplyr::pull(away_team, team_score)[1]
    home_abbrev <- dplyr::pull(home_team, team_abbreviation)[1]
    away_abbrev <- dplyr::pull(away_team, team_abbreviation)[1]
    if (nrow(home_team) == 0 | nrow(away_team) == 0) return(tibble())

    # Get starters for each team
    home_starters <- nba_player_box %>%
      filter(game_id == !!game_id, starter == TRUE, team_abbreviation == home_team$team_abbreviation[1])
    away_starters <- nba_player_box %>%
      filter(game_id == !!game_id, starter == TRUE, team_abbreviation == away_team$team_abbreviation[1])

    # Join with RPM
    home_starters_rpm <- home_starters %>%
      left_join(starter_rpm_db, by = "athlete_id")
    away_starters_rpm <- away_starters %>%
      left_join(starter_rpm_db, by = "athlete_id")

    home_strongest <- max(home_starters_rpm$total_rpm, na.rm = TRUE)
    home_weakest <- min(home_starters_rpm$total_rpm, na.rm = TRUE)
    away_strongest <- max(away_starters_rpm$total_rpm, na.rm = TRUE)
    away_weakest <- min(away_starters_rpm$total_rpm, na.rm = TRUE)
    home_net_rtg <- max(home_starters_rpm$net_rtg, na.rm = TRUE)
    away_net_rtg <- max(away_starters_rpm$net_rtg, na.rm = TRUE)
    score_diff <- home_team$team_score[1] - away_team$team_score[1]
    strongest_diff <- home_strongest - away_strongest
    weakest_diff <- home_weakest - away_weakest
    net_rtg_diff <- home_net_rtg - away_net_rtg

    tibble(
      game_id = game_id,
      home_team = home_abbrev,
      away_team = away_abbrev,
      home_score = home_score,
      away_score = away_score,
      home_strongest = home_strongest,
      home_weakest = home_weakest,
      away_strongest = away_strongest,
      away_weakest = away_weakest,
      home_net_rtg = home_net_rtg,
      away_net_rtg = away_net_rtg,
      score_diff = home_score - away_score,
      strongest_diff = strongest_diff,
      weakest_diff = weakest_diff,
      net_rtg_diff = net_rtg_diff
    )
  })
})

print(game_link_summary)

message("Calculating correlations between score differential and link differentials...")
cor_strong <- cor(game_link_summary$strongest_diff, game_link_summary$score_diff, use = "complete.obs")
cor_weak <- cor(game_link_summary$weakest_diff, game_link_summary$score_diff, use = "complete.obs")
cor_netrtg <- cor(game_link_summary$net_rtg_diff, game_link_summary$score_diff, use = "complete.obs")

# Combine correlations into a data frame and sort by absolute correlation
correlations <- tibble(
  metric = c("strongest_diff", "weakest_diff", "net_rtg_diff"),
  correlation = c(cor_strong, cor_weak, cor_netrtg)
) %>%
  mutate(abs_correlation = abs(correlation)) %>%
  arrange(desc(abs_correlation))

cat("Correlations:\n")
print(correlations)

message("Visualizing results...")

# Prepare data for plotting in long format
plot_data <- game_link_summary %>%
  select(score_diff, strongest_diff, weakest_diff, net_rtg_diff) %>%
  pivot_longer(cols = c(strongest_diff, weakest_diff, net_rtg_diff),
               names_to = "metric", values_to = "metric_value") %>%
  mutate(metric = factor(metric, levels = correlations$metric))

# Add correlation labels for annotation
cor_labels <- correlations %>%
  mutate(label = paste0("r = ", round(correlation, 3)))

library(ggplot2)

p <- ggplot(plot_data, aes(x = metric_value, y = score_diff)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  facet_wrap(~ metric, scales = "free_x", labeller = as_labeller(c(
    strongest_diff = "Strongest Link Differential",
    weakest_diff = "Weakest Link Differential",
    net_rtg_diff = "Net Rating Differential"
  ))) +
  labs(
    title = paste0("Score Differential vs Link Differentials\n",
                   "Correlations: Strongest = ", round(cor_strong,3),
                   ", Weakest = ", round(cor_weak,3),
                   ", Net Rating = ", round(cor_netrtg,3)),
    x = "Difference in Metric (Home - Away)",
    y = "Score Differential (Home - Away)"
  ) +
  theme_minimal()

# Add correlation annotations to each facet
p <- p + geom_text(data = cor_labels, aes(x = -Inf, y = Inf, label = label),
                   inherit.aes = FALSE, hjust = -0.1, vjust = 1.1, size = 4, color = "black")

print(p)
