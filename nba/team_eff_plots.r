required_packages <- c(
  "tidyverse", "hoopR", "janitor", "ggrepel", "jsonlite", "rvest","tictoc", "progressr", "purrr","ggimage"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# Load NBA game data for the  season
season_year <- 2021
# Choose the team to plot (by NBA 3-letter abbreviation)
team_input <- "UTA"  # NBA 3-letter abbreviation (e.g., "UTA")

# Load datra
nba_team_box <- hoopR::load_nba_team_box(season_year)
nba_player_box <- hoopR::load_nba_player_box(season_year)
adv_data <- nba_leaguedashplayerstats(measure_type = "Advanced",season=2024)
adv_data <- adv_data$LeagueDashPlayerStats %>% janitor::clean_names()

# Convert key numeric columns
adv_data <- adv_data %>%
  mutate(
    off_rating = as.numeric(off_rating),
    def_rating = as.numeric(def_rating),
    usg_pct = suppressWarnings(as.numeric(usg_pct)),
    min = suppressWarnings(as.numeric(min)),
    net_rtg = suppressWarnings(as.numeric(net_rating))
  )

team_dash <- nba_leaguedashteamstats(
  season     = season_year,
  season_type = "Regular Season",
  measure_type = "Advanced"     # or "Base", depending on availability
)

# Clean team dashboard and coerce ratings to numeric
team_dash_clean <- team_dash$LeagueDashTeamStats %>%
  janitor::clean_names() %>%
  mutate(
    off_rating = suppressWarnings(as.numeric(off_rating)),
    def_rating = suppressWarnings(as.numeric(def_rating)),
    team_name = stringr::str_trim(team_name)
  )

# --- Efficiency Landscape Plot for a Single Team ---



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

if (team_input %in% team_lookup$team_abbreviation) {
  team_to_plot <- team_lookup %>%
    filter(team_abbreviation == team_input) %>%
    pull(team_name) %>%
    unique()
} else if (team_input %in% team_lookup$team_name) {
  team_to_plot <- team_input
  team_input <- team_lookup %>%
    filter(team_name == team_input) %>%
    pull(team_abbreviation) %>%
    unique()
} else {
  stop("team_input must be a valid NBA 3-letter team abbreviation or full team name.")
}


# Filter player-level data for the selected team using NBA 3-letter abbr
adv_data_team <- adv_data %>%
  filter(team_abbreviation == team_input)

# Remove redundant numeric coercion, just drop NA
adv_data_team <- adv_data_team %>%
  drop_na(off_rating, def_rating)

# Get team-level advanced stats for this team
team_row <- team_dash_clean %>%
  filter(team_name == team_to_plot)

# Team-level ratings for overlay
team_off_rating <- as.numeric(team_row$off_rating)
team_def_rating <- as.numeric(team_row$def_rating)

# --- Standardize to league averages ---
league_off_avg <- mean(team_dash_clean$off_rating, na.rm = TRUE)
league_def_avg <- mean(team_dash_clean$def_rating, na.rm = TRUE)

team_dash_clean <- team_dash_clean %>%
  mutate(
    off_rating_std = off_rating - league_off_avg,
    def_rating_std = def_rating - league_def_avg
  )

adv_data_team <- adv_data_team %>%
  mutate(
    off_rating_std = off_rating - league_off_avg,
    def_rating_std = def_rating - league_def_avg
  )

team_off_rating_std <- team_off_rating - league_off_avg
team_def_rating_std <- team_def_rating - league_def_avg

# Use player name column as available
player_name_col <- if ("player_name" %in% names(adv_data_team)) "player_name" else if ("full_name" %in% names(adv_data_team)) "full_name" else names(adv_data_team)[1]

# Try to use usg_pct for size, else min
size_col <- if ("usg_pct" %in% names(adv_data_team)) "usg_pct" else "min"

# Get logo path (try to use team logo from hoopR or NBA assets; placeholder here)
# You may need to download logo and use local path, or use a URL if ggimage is installed.
logo_url <- team_lookup %>%
  filter(team_name == team_to_plot) %>%
  pull(team_logo) %>%
  unique()

# Load ggimage if available for logo overlay


# --- Define team primary colors (hex codes) ---
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

team_color <- team_colors[[team_input]] %||% "#002B5C"  # default navy if missing

library(ggrepel)
library(ggplot2)

# Determine symmetric range for axes
max_range <- max(abs(c(adv_data_team$off_rating_std, adv_data_team$def_rating_std, team_off_rating_std, team_def_rating_std)), na.rm = TRUE)
range_limit <- ceiling(max_range * 10) / 10  # round up to nearest 0.1 for nicer axis limits

ggplot(adv_data_team, aes(
    x = off_rating_std,
    y = def_rating_std,
    size = min,
    alpha = scales::rescale(usg_pct, to = c(0.3, 0.7)),
    label = !!rlang::sym(player_name_col)
  )) +
  # Overlay team marker first
  { if (requireNamespace("ggimage", quietly = TRUE) & length(logo_url) & !is.na(logo_url[1])) {
      ggimage::geom_image(
        data = data.frame(x = team_off_rating_std, y = team_def_rating_std, image = logo_url[1]),
        aes(x = x, y = y, image = image),
        inherit.aes = FALSE,
        size = 0.10
      )
    } else {
      list(
        geom_point(
          data = data.frame(x = team_off_rating_std, y = team_def_rating_std),
          aes(x = x, y = y),
          inherit.aes = FALSE,
          shape = 21, stroke = 1.2, size = 5, fill = "white"
        ),
        ggrepel::geom_label_repel(
          data = data.frame(x = team_off_rating_std, y = team_def_rating_std, lab = team_to_plot),
          aes(x = x, y = y, label = lab),
          inherit.aes = FALSE,
          size = 4, fontface = "bold", fill = "white", label.size = 0.3, segment.color = "grey50"
        )
      )
    } } +
  # Then players on top
  geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  geom_point(
    color = rgb(t(col2rgb(team_color))[1]/255, t(col2rgb(team_color))[2]/255, t(col2rgb(team_color))[3]/255, alpha = 0.6),
    fill = NA,
    stroke = 0
  ) +
  ggrepel::geom_text_repel(size = 3, fontface = "bold", segment.color = "grey50") +
  scale_size_continuous(name = "Minutes") +
  scale_alpha_continuous(name = "Usage %", range = c(0.3, 0.7)) +
  labs(
    title = paste("Efficiency Landscape (vs League Avg):", team_to_plot),
    x = "Relative Offensive Rating (Above/Below League Avg)",
    y = "Relative Defensive Rating (Below = Better)",
    caption = "Standardized to league average; diagonal = net neutral. Players sized by minutes, alpha scaled by usage %."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(-range_limit, range_limit), labels = scales::label_number(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-range_limit, range_limit), labels = scales::label_number(accuracy = 0.1))
