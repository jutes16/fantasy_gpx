# %% Package bootstrap
required_packages <- c(
  "tidyverse", "hoopR", "gt", "gtExtras", "janitor", "hablar", "chromote"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))
installed <- rownames(installed.packages())
for (p in required_packages) if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
invisible(lapply(required_packages, library, character.only = TRUE))

# Folders
if (!dir.exists("nba/images/plots")) dir.create("nba/images/plots", recursive = TRUE)

# %%

# Per game basic stats and advanced stats
base_pergame <- nba_leaguedashplayerstats(
  season = "2023-24",  # or "2024-25" once it’s active
  per_mode = "PerGame",
  measure_type = "Base"
)
adv_pergame <- nba_leaguedashplayerstats(
  season = "2023-24",  # or "2024-25" once it’s active
  per_mode = "Totals",
  measure_type = "Advanced"
)
nba_base_stats <- base_pergame$LeagueDashPlayerStats %>%
  janitor::clean_names() %>%
  dplyr::select(player_id, player_name, age, gp, min, pts, reb, ast) %>%
  hablar::retype()

nba_adv_stats <- adv_pergame$LeagueDashPlayerStats %>%
  janitor::clean_names() %>%
  dplyr::select(player_id, player_name, usg_pct, ts_pct) %>%
  hablar::retype()

# Pick your players
players <- c("Jonathan Kuminga","Cam Thomas","Quentin Grimes","Josh Giddey")

nba_stats <- nba_base_stats %>%
  left_join(nba_adv_stats, by = c("player_id","player_name")) %>% 
  filter(player_name %in% players)


df <- nba_stats %>%
  select(-player_id) %>% 
  pivot_longer(-player_name) %>% 
  pivot_wider(names_from = player_name, values_from = value) %>%
  mutate(
    name = dplyr::case_match(
      name,
      "age"     ~ "Age",
      "min"     ~ "Minutes per Game",
      "gp"      ~ "Games Played",
      "pts"     ~ "Points per Game",
      "reb"     ~ "Rebounds per Game",
      "ast"     ~ "Assists per Game",
      "usg_pct" ~ "Usage Rate %",
      "ts_pct"  ~ "True Shooting %"
    )
  )


# Column headers with images
tbl <- df %>% 
  gt() %>% 
  gtExtras::gt_theme_538(quiet = TRUE) %>% 
  tab_header(
    title = "The Big Four Restricted Free Agents", 
    subtitle = "Data from the 2024–25 Regular Season"
  ) %>%
  cols_label(
    name = "",
    `Cam Thomas` = gtExtras::img_header(
      "Cam Thomas",
      "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/4432174.png&w=350&h=254",
      height = 60, font_size = 14
    ),
    `Jonathan Kuminga` = gtExtras::img_header(
      "Jonathan Kuminga",
      "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/4433247.png&w=350&h=254",
      height = 60, font_size = 14
    ),
    `Josh Giddey` = gtExtras::img_header(
      "Josh Giddey",
      "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/4871145.png&w=350&h=254",
      height = 60, font_size = 14
    ),
    `Quentin Grimes` = gtExtras::img_header(
      "Quentin Grimes",
      "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/4397014.png&w=350&h=254",
      height = 60, font_size = 14
    )
  ) %>%
  cols_align(columns = -name, align = "center") %>%
  tab_options(data_row.padding = '5px') %>%
  fmt_number(columns = -name, rows = name %in% c("Age","Minutes per Game","Games Played"), decimals = 0) %>%
  fmt_number(columns = -name, rows = name %in% c("Points per Game","Rebounds per Game","Assists per Game"), decimals = 1) %>%
  fmt_percent(columns = -name, rows = name %in% c("True Shooting %","Usage Rate %"), decimals = 1)

gt::gtsave(tbl, filename = "nba/images/plots/rfa_r.png")
