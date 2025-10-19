required_packages <- c(
  "tidyverse", "hoopR", "janitor", "ggrepel", "jsonlite", "rvest"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

rating_source <- "NBA"  # Default rating source; options: "EPM", "DARKO", "NBA"

if (rating_source == "NBA") {
  message("Fetching NBA player-level stats from NBA.com via hoopR::nba_playergamelogs()...")
  # Fetch NBA player game logs for 2023-24 season (season 2024)
  nba_stats <- tryCatch({
    logs <- hoopR::nba_playergamelogs(season = 2024, season_type = "Regular Season") %>%
      clean_names()
    # Calculate per-player averages for off_rating, def_rating, net_rating, ts_pct
    logs %>%
      group_by(player_name, team = team_abbreviation) %>%
      summarise(
        off_rating = mean(off_rating, na.rm = TRUE),
        def_rating = mean(def_rating, na.rm = TRUE),
        net_rating = mean(net_rating, na.rm = TRUE),
        ts_pct = mean(ts_pct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        rating_total = net_rating,
        rating_offense = off_rating,
        rating_defense = def_rating
      ) %>%
      select(player_name, team, rating_total, rating_offense, rating_defense)
  }, error = function(e) {
    message("Error fetching NBA player game logs: ", e$message)
    return(tibble())
  })
  player_ratings <- nba_stats
  message("NBA data fetched successfully.")
} else if (rating_source == "EPM") {
  message("Fetching EPM player-level stats from dunksandthrees.com...")
  epm_url <- "https://dunksandthrees.com/epm/actual"
  epm_data <- read_csv(epm_url) %>%
    clean_names() %>%
    rename(player_name = player,
           team = tm) %>%
    mutate(
      rating_total = epm,
      rating_offense = off_epm,
      rating_defense = def_epm
    ) %>%
    select(player_name, team, rating_total, rating_offense, rating_defense)
  
  player_ratings <- epm_data
  message("EPM data fetched successfully.")
  
} else if (rating_source == "DARKO") {
  message("Fetching DARKO player-level stats from craftednba.com...")
  darko_url <- "https://craftednba.com/stats/darko"
  darko_page <- read_html(darko_url)
  darko_table <- darko_page %>%
    html_node("table") %>%
    html_table(fill = TRUE) %>%
    as_tibble() %>%
    clean_names()
  
  darko_data <- darko_table %>%
    rename(player_name = player,
           team = team_abbreviation) %>%
    mutate(
      rating_total = as.numeric(darko_rating),
      rating_offense = as.numeric(off_rating),
      rating_defense = as.numeric(def_rating)
    ) %>%
    select(player_name, team, rating_total, rating_offense, rating_defense)
  
  player_ratings <- darko_data
  message("DARKO data fetched successfully.")
  
} else {
  stop("Invalid rating_source specified. Choose one of 'NBA', 'EPM', or 'DARKO'.")
}

message(paste0("Computing per-team strongest and weakest players from top 5 by ", rating_source, " total..."))
team_top5 <- player_ratings %>%
  group_by(team) %>%
  arrange(desc(rating_total)) %>%
  slice_head(n = 5) %>%
  summarise(
    strongest_link = max(rating_total, na.rm = TRUE),
    weakest_link = min(rating_total, na.rm = TRUE),
    .groups = "drop"
  )
message("Team strongest and weakest links computed.")

message("Fetching team box scores via hoopR::load_nba_team_box()...")
games <- hoopR::load_nba_team_box(
  seasons = 2023L
) %>%
  clean_names() %>%
  mutate(
    team_id = as.character(team_id),
    pts = team_score
  ) %>%
  select(game_id, season, team_id, team_abbreviation, home_away = team_home_away,
         opponent_team_id, opponent_team_abbreviation, pts)
message("Team box scores fetched successfully.")

# Join team abbreviations with team_top5 data
# Note: team_top5$team uses team abbreviations, hoopR uses team_abbreviation
# So join by team_abbreviation and team
team_ratings <- team_top5 %>%
  rename(team_abbreviation = team)

# Prepare games with strongest and weakest link ratings
games_with_ratings <- games %>%
  left_join(team_ratings, by = "team_abbreviation")

# Pivot wider to get home and away teams on same row
games_joined <- games_with_ratings %>%
  select(game_id, team_abbreviation, home_away, strongest_link, weakest_link, pts) %>%
  pivot_wider(names_from = home_away, values_from = c(strongest_link, weakest_link, pts)) %>%
  mutate(
    diff_score = pts_home - pts_away,
    diff_strongest = strongest_link_home - strongest_link_away,
    diff_weakest = weakest_link_home - weakest_link_away
  )

message("Calculating correlations between score differential and link differentials...")
cor_strong <- cor(games_joined$diff_strongest, games_joined$diff_score, use = "complete.obs")
cor_weak <- cor(games_joined$diff_weakest, games_joined$diff_score, use = "complete.obs")

cat("Correlation (diff_score vs diff_strongest): ", round(cor_strong, 3), "\n")
cat("Correlation (diff_score vs diff_weakest): ", round(cor_weak, 3), "\n")

message("Visualizing results...")
p1 <- ggplot(games_joined, aes(x = diff_strongest, y = diff_score)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  labs(
    title = "Score Differential vs Strongest Link Differential",
    x = "Difference in Strongest Link Rating Total (Home - Away)",
    y = "Score Differential (Home - Away)"
  ) +
  theme_minimal()

p2 <- ggplot(games_joined, aes(x = diff_weakest, y = diff_score)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  labs(
    title = "Score Differential vs Weakest Link Differential",
    x = "Difference in Weakest Link Rating Total (Home - Away)",
    y = "Score Differential (Home - Away)"
  ) +
  theme_minimal()

print(p1)
print(p2)
