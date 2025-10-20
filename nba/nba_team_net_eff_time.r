required_packages <- c(
  "tidyverse", "hoopR", "janitor", "ggrepel", "jsonlite", "rvest","tictoc", "progressr", "purrr","ggimage", "patchwork"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# Load NBA game data for the  season
highlight_team <- "BOS"
season_year <- 2024
args <- commandArgs(trailingOnly = TRUE)

# --- New optional parameters to filter data by game segment ---
# Set these to restrict data to a subset of games within the season.
# For example, to analyze the last 15 games, set last_n_games <- 15.
# If left NULL, data for the full season will be loaded.
game_segment_start <- NULL  # e.g., "2024-02-01" or NULL for no start date filter
game_segment_end <- NULL    # e.g., "2024-03-15" or NULL for no end date filter
last_n_games <- NULL       # e.g., 15 for last 15 games, or NULL to ignore

# Load datra
nba_team_box <- hoopR::load_nba_team_box(season_year)
nba_player_box <- hoopR::load_nba_player_box(season_year)

# Load advanced player stats with optional filtering by date range or last n games
if (!is.null(last_n_games)) {
  adv_data <- nba_leaguedashplayerstats(
    measure_type = "Advanced",
    season = season_year,
    last_n_games = last_n_games
  )
} else {
  adv_data <- nba_leaguedashplayerstats(
    measure_type = "Advanced",
    season = season_year,
    date_from = game_segment_start,
    date_to = game_segment_end
  )
}
adv_data <- adv_data$LeagueDashPlayerStats %>% janitor::clean_names()

# Convert key numeric columns
adv_data <- adv_data %>%
  mutate(
    off_rating = as.numeric(off_rating),
    def_rating = as.numeric(def_rating),
    usg_pct = suppressWarnings(as.numeric(usg_pct)) * 100, # rescale to 0-100
    min = suppressWarnings(as.numeric(min)),
    net_rtg = suppressWarnings(as.numeric(net_rating))
  )

# Load advanced team stats with optional filtering by date range or last n games
if (!is.null(last_n_games)) {
  team_dash <- nba_leaguedashteamstats(
    season     = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    last_n_games = last_n_games
  )
} else {
  team_dash <- nba_leaguedashteamstats(
    season     = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    date_from = game_segment_start,
    date_to = game_segment_end
  )
}

# Clean team dashboard and coerce ratings to numeric
team_dash_clean <- team_dash$LeagueDashTeamStats %>%
  janitor::clean_names() %>%
  mutate(
    off_rating = suppressWarnings(as.numeric(off_rating)),
    def_rating = suppressWarnings(as.numeric(def_rating)),
    team_name = stringr::str_trim(team_name)
  )

# --- Efficiency Landscape Plot for all teams together ---

# Create team lookup table from hoopR teams data
team_lookup <- load_nba_team_box(season = season_year) %>%
  distinct(team_id, team_abbreviation, team_display_name, team_logo) %>%
  mutate(team_name = stringr::str_trim(team_display_name))

# Standardize team abbreviations to NBA 3-letter codes
team_lookup <- team_lookup %>%
  mutate(
    team_abbreviation = case_when(
      team_abbreviation == "UTAH" ~ "UTA",
      team_abbreviation == "NY"   ~ "NYK",
      team_abbreviation == "NO"   ~ "NOP",
      team_abbreviation == "GS"   ~ "GSW",
      team_abbreviation == "SA"   ~ "SAS",
      TRUE ~ team_abbreviation
    )
  )

# Ensure unique rows for stable lookup
team_lookup <- team_lookup %>%
  distinct(team_id, team_abbreviation, team_name, team_logo, .keep_all = TRUE)

# Filter player-level data for min >= 15
adv_data_filtered <- adv_data %>%
  filter(min >= 15) %>%
  drop_na(off_rating, def_rating)

# Standardize to league averages
league_off_avg <- mean(team_dash_clean$off_rating, na.rm = TRUE)
league_def_avg <- mean(team_dash_clean$def_rating, na.rm = TRUE)

team_dash_clean <- team_dash_clean %>%
  mutate(
    off_rating_std = off_rating - league_off_avg,
    def_rating_std = def_rating - league_def_avg
  )

adv_data_filtered <- adv_data_filtered %>%
  mutate(
    off_rating_std = off_rating - league_off_avg,
    def_rating_std = def_rating - league_def_avg
  )

# Negate defensive ratings for consistent placement (reverse y-axis)
team_dash_clean <- team_dash_clean %>%
  mutate(def_rating_std = -def_rating_std)

adv_data_filtered <- adv_data_filtered %>%
  mutate(def_rating_std = -def_rating_std)

# Join adv_data_filtered with team_lookup to get team colors and logos
adv_data_filtered <- adv_data_filtered %>%
  left_join(team_lookup %>% select(team_abbreviation, team_name, team_logo), by = "team_abbreviation")

# Define team primary colors (hex codes)
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

# fallback operator
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

library(ggplot2)

# Determine symmetric range for axes (fixed at 10 for comparability)
range_limit <- 10

# Filter player data for min >= 20 for bottom panel
adv_data_filtered_20 <- adv_data_filtered %>% filter(min >= 15)

# Select top 5 players by usage percentage (usg_pct) for labeling
top5_players <- adv_data_filtered_20 %>%
  arrange(desc(usg_pct)) %>%
  slice_head(n = 5)

# --- Base Plot Components (for reuse) ---
base_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.2) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.2) +
  geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  annotate(
    "text",
    x = range_limit * 0.90,
    y = range_limit * -0.05,
    label = "Better Offense \u2192",
    color = "grey40",
    fontface = "italic",
    size = 4,
    hjust = 0.5,
    vjust = 0
  ) +
  annotate(
    "text",
    x = range_limit * 0.03,
    y = range_limit * 0.92,
    label = "\u2191 Better Defense",
    color = "grey40",
    fontface = "italic",
    size = 4,
    hjust = 0,
    vjust = 0
  ) +
  annotate(
    "text",
    x = -range_limit * 0.93 + 0.3,
    y = range_limit * 0.93 + 0.3,
    label = "Net Positive (↑ Off + Def)",
    color = "grey40",
    fontface = "italic",
    size = 4,
    hjust = 0,
    vjust = 0,
    angle = -45
  ) +
  annotate(
    "text",
    x = -range_limit * 0.93 - 0.3,
    y = range_limit * 0.93 - 0.3,
    label = "Net Negative (↓ Off + Def)",
    color = "grey40",
    fontface = "italic",
    size = 4,
    hjust = 0,
    vjust = 1,
    angle = -45
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(-range_limit, range_limit), labels = scales::label_number(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-range_limit, range_limit), labels = scales::label_number(accuracy = 0.1)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# --- Top Panel: Team Logos Only ---
p_top <- base_plot

if (requireNamespace("ggimage", quietly = TRUE)) {
  p_top <- p_top + ggimage::geom_image(
    data = team_dash_clean %>%
      left_join(team_lookup %>% select(team_name, team_logo), by = "team_name") %>%
      filter(!is.na(team_logo)),
    aes(x = off_rating_std, y = def_rating_std, image = team_logo),
    size = 0.08,
    inherit.aes = FALSE
  )
}

p_top <- p_top +
  labs(
    title = "League Efficiency Landscape (Teams)",
    x = "Offensive Rating",
    y = "Defensive Rating",
    caption = "Standardized to league average; diagonal = net neutral. Team logos only."
  )

# --- Bottom Panel: Player Points with Team Highlight ---
# Set the team to highlight


# Prepare data for highlight
highlight_players <- adv_data_filtered_20 %>% filter(team_abbreviation == highlight_team)
other_players <- adv_data_filtered_20 %>% filter(team_abbreviation != highlight_team)

p_bottom <- base_plot +
  scale_x_continuous(limits = c(-15, 15), labels = scales::label_number(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-15, 15), labels = scales::label_number(accuracy = 0.1)) +
  # Plot all players as grey dots with alpha mapped to min * 0.6
  geom_point(
    data = other_players,
    aes(
      x = off_rating_std,
      y = def_rating_std,
      size = usg_pct,
      alpha = min * 0.6
    ),
    color = "grey70",
    fill = NA,
    stroke = 0
  ) +
  # Plot highlighted team players in their team color, alpha mapped to min
  geom_point(
    data = highlight_players,
    aes(
      x = off_rating_std,
      y = def_rating_std,
      size = usg_pct,
      color = team_abbreviation,
      alpha = min
    ),
    fill = NA,
    stroke = 0
  ) +
  # Team color scale for highlighted team
  scale_color_manual(values = team_colors, guide = "none") +
  # Make usage percent size range more pronounced and override legend size
  scale_size_continuous(name = "Usage (%)", range = c(1, 8), guide = guide_legend()) +
  scale_alpha_continuous(name = "Minutes per Game", range = c(0.1, 0.7),guide=guide_legend(override.aes = list(size = 6))) +
  # Add text labels for highlighted team players with ggrepel
  ggrepel::geom_text_repel(
    data = highlight_players,
    aes(x = off_rating_std, y = def_rating_std, label = player_name, color = team_abbreviation),
    size = 4,
    fontface = "bold",
    box.padding = 0.3,
    point.padding = 0.5,
    segment.color = "grey50",
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title = paste0("League Efficiency Landscape (Players, Highlight: ", highlight_team, ")"),
    x = "Offensive Rating",
    y = "Defensive Rating",
    caption = "Standardized to league average; diagonal = net neutral. All players in grey, highlighted team colored and labeled. Players sized by usage %. Only players with min ≥ 15."
  )

# --- Combine Plots Vertically ---
combined_plot <- p_top / p_bottom + plot_layout(ncol = 1, heights = c(1, 1))

# --- Save Combined Plot ---
output_dir <- "nba/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

file_name <- paste0(output_dir, "/league_efficiency_subplots_", highlight_team, ".png")

ggsave(
  filename = file_name,
  plot = combined_plot,
  width = 10,
  height = 16,
  dpi = 300
)
message("Saved plot to: ", file_name)
