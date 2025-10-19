required_packages <- c(
  "tidyverse", "hoopR", "janitor", "ggrepel", "jsonlite", "rvest","tictoc", "progressr", "purrr","ggimage"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# Load NBA game data for the  season
season_year <- 2024
# Choose the team to plot (by NBA 3-letter abbreviation)
team_input <- "OKC"  # NBA 3-letter abbreviation (e.g., "UTA")
team_input_2 <- NULL  # e.g., "BOS" or NULL optional parameter to compare two teams ---

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  team_input <- args[1]
}

# --- New optional parameters to filter data by game segment ---
# Set these to restrict data to a subset of games within the season.
# For example, to analyze the last 15 games, set last_n_games <- 15.
# If left NULL, data for the full season will be loaded.
game_segment_start <- NULL  # e.g., "2024-02-01" or NULL for no start date filter
game_segment_end <- NULL    # e.g., "2024-03-15" or NULL for no end date filter
last_n_games <- 20       # e.g., 15 for last 15 games, or NULL to ignore

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

# --- Efficiency Landscape Plot for a Single Team or Two Teams ---



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

# Function to get team name and abbreviation from input
get_team_info <- function(input) {
  if (input %in% team_lookup$team_abbreviation) {
    team_name <- team_lookup %>%
      filter(team_abbreviation == input) %>%
      pull(team_name) %>%
      unique()
    team_abbr <- input
  } else if (input %in% team_lookup$team_name) {
    team_name <- input
    team_abbr <- team_lookup %>%
      filter(team_name == input) %>%
      pull(team_abbreviation) %>%
      unique()
  } else {
    stop("team_input must be a valid NBA 3-letter team abbreviation or full team name.")
  }
  list(team_name = team_name, team_abbr = team_abbr)
}

team1_info <- get_team_info(team_input)
team_to_plot <- team1_info$team_name
team_input <- team1_info$team_abbr

if (!is.null(team_input_2)) {
  team2_info <- get_team_info(team_input_2)
  team_to_plot_2 <- team2_info$team_name
  team_input_2 <- team2_info$team_abbr
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

if (!is.null(team_input_2)) {
  adv_data_team_2 <- adv_data %>%
    filter(team_abbreviation == team_input_2) %>%
    drop_na(off_rating, def_rating)
  team_row_2 <- team_dash_clean %>%
    filter(team_name == team_to_plot_2)
  team_off_rating_2 <- as.numeric(team_row_2$off_rating)
  team_def_rating_2 <- as.numeric(team_row_2$def_rating)
}

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

if (!is.null(team_input_2)) {
  adv_data_team_2 <- adv_data_team_2 %>%
    mutate(
      off_rating_std = off_rating - league_off_avg,
      def_rating_std = def_rating - league_def_avg
    )
  team_off_rating_std_2 <- team_off_rating_2 - league_off_avg
  team_def_rating_std_2 <- team_def_rating_2 - league_def_avg
}

# Negate defensive ratings for consistent placement (reverse y-axis)
team_dash_clean <- team_dash_clean %>%
  mutate(def_rating_std = -def_rating_std)

adv_data_team <- adv_data_team %>%
  mutate(def_rating_std = -def_rating_std)

team_def_rating_std <- -team_def_rating_std

if (!is.null(team_input_2)) {
  adv_data_team_2 <- adv_data_team_2 %>%
    mutate(def_rating_std = -def_rating_std)
  team_def_rating_std_2 <- -team_def_rating_std_2
}

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

if (!is.null(team_input_2)) {
  logo_url_2 <- team_lookup %>%
    filter(team_name == team_to_plot_2) %>%
    pull(team_logo) %>%
    unique()
}

# Load ggimage if available for logo overlay


# --- Define team primary colors (hex codes) ---
team_colors <- c(
  ATL = "#E03A3E", BOS = "#007A33", BKN = "#000000", CHA = "#1D1160",
  CHI = "#CE1141", CLE = "#860038", DAL = "#00538C", DEN = "#0E2240",
  DET = "#C8102E", GSW = "#1D428A", HOU = "#CE1141", IND = "#002D62",
  LAC = "#C8102E", LAL = "#552583", MEM = "#5D76A9", MIA = "#98002E",
  MIL = "#00471B", MIN = "#0C2340", NOP = "#0C2340", NYK = "#006BB6",
  OKC = "#EF3B24", ORL = "#0077C0", PHI = "#006BB6", PHX = "#1D1160",
  POR = "#E03A3E", SAC = "#5A2D81", SAS = "#C4CED4", TOR = "#CE1141",
  UTA = "#002B5C", WSH = "#002B5C"
)

# fallback operator
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

team_color <- team_colors[[team_input]] %||% "#002B5C"  # default navy if missing

if (!is.null(team_input_2)) {
  # Choose a second color different from the first team color
  team_color_2 <- team_colors[[team_input_2]] %||% "#CE1141"  # default red if missing
}

library(ggrepel)
library(ggplot2)

# Determine symmetric range for axes
max_range <- max(abs(c(adv_data_team$off_rating_std, adv_data_team$def_rating_std, team_off_rating_std, team_def_rating_std)), na.rm = TRUE)
if (!is.null(team_input_2)) {
  max_range_2 <- max(abs(c(adv_data_team_2$off_rating_std, adv_data_team_2$def_rating_std, team_off_rating_std_2, team_def_rating_std_2)), na.rm = TRUE)
  max_range <- max(max_range, max_range_2)
}
range_limit <- ceiling(max_range * 10) / 10  # round up to nearest 0.1 for nicer axis limits
range_limit <- 20 # fixed range for better comparability


#
# --- Base Plot: Grid, Axes, Diagonal, and Reference Labels ---
# --- Create axis obstacle "ghost" points for repel ---
axis_obstacles <- rbind(
  data.frame(x = seq(-range_limit, range_limit, length.out = 200), y = 0),
  data.frame(x = 0, y = seq(-range_limit, range_limit, length.out = 200))
)


# --- Combine "ghost obstacle" points for repel: axes + dense clusters around team logo positions ---
# To strengthen label avoidance near logos, we add dense clusters ("virtual repulsion zones") of ghost points around each logo.
# This creates a stronger repulsion effect for player labels near team logos.
repel_obstacles <- axis_obstacles
logo_repulsion_radius <- 1.0  # radius of repulsion zone around logo
logo_repulsion_points <- 24   # number of ghost points in circle
logo_repulsion_alpha <- 0     # invisible, but could use e.g. 0.05 for debugging
logo_repulsion_size <- 30     # size matches invisible ghost points

# Helper function to generate circle of points around a center (x0, y0)
circle_points <- function(x0, y0, r, n) {
  theta <- seq(0, 2*pi, length.out = n + 1)[-1]
  data.frame(
    x = x0 + r * cos(theta),
    y = y0 + r * sin(theta)
  )
}

# Add main logo point and surrounding repulsion zone for team 1
repel_obstacles <- rbind(
  repel_obstacles,
  # Main logo point (strong repulsion, duplicated for effect)
  data.frame(x = rep(team_off_rating_std, 2), y = rep(team_def_rating_std, 2)),
  # Surrounding circle of ghost points (stronger repulsion)
  circle_points(team_off_rating_std, team_def_rating_std, logo_repulsion_radius, logo_repulsion_points)
)

# Add second team logo and repulsion zone if applicable
if (!is.null(team_input_2)) {
  repel_obstacles <- rbind(
    repel_obstacles,
    # Main logo point (strong repulsion, duplicated)
    data.frame(x = rep(team_off_rating_std_2, 2), y = rep(team_def_rating_std_2, 2)),
    # Surrounding circle of ghost points
    circle_points(team_off_rating_std_2, team_def_rating_std_2, logo_repulsion_radius, logo_repulsion_points)
  )
}

p <- ggplot() +
  # Horizontal and vertical gridlines at league average
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.2) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.2) +
  # Diagonal net neutral line
  geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  # Reference line labels for interpretability (use annotate instead of ggrepel for axes)
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
  )

# --- Add First Team ---
p <- p +
  { if (requireNamespace("ggimage", quietly = TRUE) & length(logo_url) & !is.na(logo_url[1])) {
      list(
        ggimage::geom_image(
          data = data.frame(x = team_off_rating_std, y = team_def_rating_std, image = logo_url[1]),
          aes(x = x, y = y, image = image),
          inherit.aes = FALSE,
          size = 0.08
        )
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
          size = 14, fontface = "bold", fill = "white", label.size = 0.3, segment.color = "grey50"
        )
      )
    } }

# --- Add Second Team (optional) ---
p <- p +
  { if (!is.null(team_input_2)) {
      if (requireNamespace("ggimage", quietly = TRUE) & length(logo_url_2) & !is.na(logo_url_2[1])) {
        list(
          ggimage::geom_image(
            data = data.frame(x = team_off_rating_std_2, y = team_def_rating_std_2, image = logo_url_2[1]),
            aes(x = x, y = y, image = image),
            inherit.aes = FALSE,
            size = 0.06
          )
        )
      } else {
        list(
          geom_point(
            data = data.frame(x = team_off_rating_std_2, y = team_def_rating_std_2),
            aes(x = x, y = y),
            inherit.aes = FALSE,
            shape = 21, stroke = 1.2, size = 5, fill = "white"
          ),
          ggrepel::geom_label_repel(
            data = data.frame(x = team_off_rating_std_2, y = team_def_rating_std_2, lab = team_to_plot_2),
            aes(x = x, y = y, label = lab),
            inherit.aes = FALSE,
            size = 14, fontface = "bold", fill = "white", label.size = 0.3, segment.color = "grey50"
          )
        )
      }
    } else NULL }

# --- Add Player Data ---
p <- p +
  # First team's players
  geom_point(
    data = adv_data_team,
    aes(
      x = off_rating_std,
      y = def_rating_std,
      size = usg_pct,
      alpha = min
    ),
    color = rgb(t(col2rgb(team_color))[1]/255, t(col2rgb(team_color))[2]/255, t(col2rgb(team_color))[3]/255, alpha = 0.6),
    fill = NA,
    stroke = 0
  ) +
  # Second team's players (if any)
  { if (!is.null(team_input_2)) {
      geom_point(
        data = adv_data_team_2,
        aes(
          x = off_rating_std,
          y = def_rating_std,
          size = usg_pct,
          alpha = min
        ),
        color = rgb(t(col2rgb(team_color_2))[1]/255, t(col2rgb(team_color_2))[2]/255, t(col2rgb(team_color_2))[3]/255, alpha = 0.6),
        fill = NA,
        stroke = 0
      )
    } else NULL }

# --- Add invisible "ghost obstacle" points for repel (axes + both team logo positions) ---
p <- p +
  geom_point(
    data = repel_obstacles,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    alpha = 0,
    size = 30
  )

# --- Add Player Labels ---
# First team player labels, repelled from each other and obstacles
p <- p +
  ggrepel::geom_text_repel(
    data = adv_data_team,
    aes(
      x = off_rating_std,
      y = def_rating_std,
      label = !!rlang::sym(player_name_col),
      alpha = min
    ),
    size = 3,
    fontface = "bold",
    segment.color = "grey50",
    color = scales::alpha(team_color, 0.8),
    force = 2,
    force_pull = 0.8,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf,
    # Removed repel.obstacles argument
    show.legend = FALSE
  )
# Second team player labels (if any), repelled from each other and obstacles
if (!is.null(team_input_2)) {
  p <- p +
    ggrepel::geom_text_repel(
      data = adv_data_team_2,
      aes(
        x = off_rating_std,
        y = def_rating_std,
        label = !!rlang::sym(player_name_col),
        alpha = min
      ),
      size = 3,
      fontface = "bold",
      segment.color = "grey50",
      color = team_color_2,
      force = 2,
      force_pull = 0.8,
      box.padding = 0.4,
      point.padding = 0.3,
      max.overlaps = Inf,
      # Removed repel.obstacles argument
      show.legend = FALSE
    )
}

# --- Add Guides, Theme, and Axis Settings ---
p <- p +
  scale_size_continuous(name = "Usage (%)", range = c(1, 8)) +
  scale_alpha_continuous(
    name = "Minutes",
    guide = guide_legend(
      title = "Minutes",
      title.theme = element_text(size = 14),
      label.theme = element_text(size = 12),
      override.aes = list(size = 6)
    )
  ) +
  labs(
    title = paste("Efficiency Landscape (vs League Avg):", team_to_plot, ifelse(!is.null(team_input_2), paste("vs", team_to_plot_2), "")),
    x = "Offensive Rating",
    y = "Defensive Rating",
    caption = "Standardized to league average; diagonal = net neutral. Players sized by usage %, alpha scaled by minutes."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(-range_limit, range_limit), labels = scales::label_number(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-range_limit, range_limit), labels = scales::label_number(accuracy = 0.1))

#print(p)
# --- Save Plot ---
output_dir <- "nba/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Construct filename (e.g., "OKC_efficiency_landscape.png" or "OKC_vs_BOS_efficiency_landscape.png")
file_name <- paste0(
  output_dir, "/",
  team_input,
  if (!is.null(team_input_2)) paste0("_vs_", team_input_2) else "",
  "_efficiency_landscape.png"
)

ggsave(
  filename = file_name,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)
message("Saved plot to: ", file_name)


