required_packages <- c(
  "tidyverse", "hoopR", "janitor", "ggrepel", "jsonlite", "rvest","tictoc", "progressr", "purrr","ggimage", "patchwork"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# ============================================================================
# CONFIGURATION PARAMETERS
# ============================================================================

# Season to analyze
season_year <- 2024

# Team to highlight (set to NULL to plot league without highlight)
highlight_team <- "UTA"

# Control what to plot for highlighted team
# Options: "full_season", "last_n", "both", "none"
highlight_plot_mode <- "both"  

# Number of last games to analyze for highlighted team (only used if highlight_plot_mode includes "last_n" or "both")
highlight_team_last_n_games <- 10

# Minimum minutes filter for players
min_minutes_threshold <- 15

# ============================================================================
# LOAD DATA
# ============================================================================

# Load full season data for all teams
nba_team_box <- hoopR::load_nba_team_box(season_year)
nba_player_box <- hoopR::load_nba_player_box(season_year)

# Load advanced player stats - FULL SEASON for all teams
adv_data <- nba_leaguedashplayerstats(
  measure_type = "Advanced",
  season = season_year
)
adv_data <- adv_data$LeagueDashPlayerStats %>% janitor::clean_names()

# Convert key numeric columns
adv_data <- adv_data %>%
  mutate(
    off_rating = as.numeric(off_rating),
    def_rating = as.numeric(def_rating),
    usg_pct = suppressWarnings(as.numeric(usg_pct)) * 100,
    min = suppressWarnings(as.numeric(min)),
    net_rtg = suppressWarnings(as.numeric(net_rating))
  )

# Load advanced team stats - FULL SEASON for all teams
team_dash <- nba_leaguedashteamstats(
  season     = season_year,
  season_type = "Regular Season",
  measure_type = "Advanced"
)

# Clean team dashboard and coerce ratings to numeric
team_dash_clean <- team_dash$LeagueDashTeamStats %>%
  janitor::clean_names() %>%
  mutate(
    off_rating = suppressWarnings(as.numeric(off_rating)),
    def_rating = suppressWarnings(as.numeric(def_rating)),
    team_name = stringr::str_trim(team_name)
  )

# ============================================================================
# LOAD LAST N GAMES DATA (if applicable)
# ============================================================================

# Initialize to NULL
adv_hi <- NULL
team_hi <- NULL

# Load last N games data only if highlight_team is set and mode requires it
if (!is.null(highlight_team) && highlight_plot_mode %in% c("last_n", "both")) {
  
  # Fetch player-level last-N snapshot for highlighted team
  adv_hi <- nba_leaguedashplayerstats(
    measure_type = "Advanced",
    season = season_year,
    last_n_games = highlight_team_last_n_games
  )$LeagueDashPlayerStats %>%
    janitor::clean_names() %>%
    dplyr::filter(team_abbreviation == highlight_team) %>%
    dplyr::mutate(
      off_rating = suppressWarnings(as.numeric(off_rating)),
      def_rating = suppressWarnings(as.numeric(def_rating)),
      usg_pct = suppressWarnings(as.numeric(usg_pct)) * 100,
      min = suppressWarnings(as.numeric(min)),
      net_rtg = suppressWarnings(as.numeric(net_rating))
    )
  
  # Fetch team-level last-N snapshot for highlighted team
  team_hi_raw <- nba_leaguedashteamstats(
    season     = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    last_n_games = highlight_team_last_n_games
  )$LeagueDashTeamStats %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      off_rating = suppressWarnings(as.numeric(off_rating)),
      def_rating = suppressWarnings(as.numeric(def_rating)),
      team_name = stringr::str_trim(team_name)
    )
}

# ============================================================================
# PREPARE TEAM LOOKUP
# ============================================================================

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

# Join team_hi with lookup if it exists and keep ONLY the highlighted team
if (!is.null(team_hi_raw)) {
  team_hi <- team_hi_raw %>%
    dplyr::left_join(
      team_lookup %>% dplyr::select(team_name, team_abbreviation, team_logo),
      by = "team_name"
    ) %>%
    dplyr::filter(team_abbreviation == highlight_team)
}

# ============================================================================
# STANDARDIZE TO LEAGUE AVERAGES
# ============================================================================

# Filter player-level data for min >= threshold
adv_data_filtered <- adv_data %>%
  filter(min >= min_minutes_threshold) %>%
  drop_na(off_rating, def_rating)

# Calculate league averages (using full season data)
league_off_avg <- mean(team_dash_clean$off_rating, na.rm = TRUE)
league_def_avg <- mean(team_dash_clean$def_rating, na.rm = TRUE)

# Standardize team data
team_dash_clean <- team_dash_clean %>%
  mutate(
    off_rating_std = off_rating - league_off_avg,
    def_rating_std = def_rating - league_def_avg
  )

# Standardize player data
adv_data_filtered <- adv_data_filtered %>%
  mutate(
    off_rating_std = off_rating - league_off_avg,
    def_rating_std = def_rating - league_def_avg
  )

# Standardize highlighted team data (if exists)
if (!is.null(team_hi)) {
  team_hi <- team_hi %>%
    mutate(
      off_rating_std = off_rating - league_off_avg,
      def_rating_std = def_rating - league_def_avg
    )
}

if (!is.null(adv_hi)) {
  adv_hi <- adv_hi %>%
    mutate(
      off_rating_std = off_rating - league_off_avg,
      def_rating_std = def_rating - league_def_avg
    )
}

# Negate defensive ratings for consistent placement (reverse y-axis)
team_dash_clean <- team_dash_clean %>%
  mutate(def_rating_std = -def_rating_std)

adv_data_filtered <- adv_data_filtered %>%
  mutate(def_rating_std = -def_rating_std)

if (!is.null(team_hi)) {
  team_hi <- team_hi %>%
    mutate(def_rating_std = -def_rating_std)
}

if (!is.null(adv_hi)) {
  adv_hi <- adv_hi %>%
    mutate(def_rating_std = -def_rating_std)
}

# Join logos for players
adv_data_filtered <- adv_data_filtered %>%
  left_join(team_lookup %>% select(team_abbreviation, team_name, team_logo), by = "team_abbreviation")

if (!is.null(adv_hi)) {
  adv_hi <- adv_hi %>%
    left_join(team_lookup %>% select(team_abbreviation, team_name, team_logo), by = "team_abbreviation")
}

# ============================================================================
# DEFINE TEAM COLORS
# ============================================================================

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

# ============================================================================
# CALCULATE DYNAMIC RANGE
# ============================================================================

library(ggplot2)

# Gather all data to include in range calculation
all_team_data <- team_dash_clean
all_player_data <- adv_data_filtered

# Calculate range with buffer
x_range <- max(abs(c(all_team_data$off_rating_std, 
                     if(!is.null(team_hi)) team_hi$off_rating_std else NULL)), na.rm = TRUE)
y_range <- max(abs(c(all_team_data$def_rating_std, 
                     if(!is.null(team_hi)) team_hi$def_rating_std else NULL)), na.rm = TRUE)
range_limit <- ceiling(max(x_range, y_range) * 1.1)  # Add 10% buffer

# ============================================================================
# CREATE BASE PLOT TEMPLATE
# ============================================================================

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

# ============================================================================
# TOP PANEL: TEAMS
# ============================================================================

p_top <- base_plot

# Prepare data with team logos for plotting
team_dash_with_logos <- team_dash_clean %>%
  left_join(team_lookup %>% select(team_name, team_logo, team_abbreviation), by = "team_name")
if (!"team_logo" %in% names(team_dash_with_logos)) {
  team_dash_with_logos <- team_dash_with_logos %>%
    dplyr::mutate(team_logo = NA_character_)
}

# Determine which teams to show based on highlight settings
if (is.null(highlight_team) || highlight_plot_mode == "none") {
  # Show all teams in grey
  teams_to_plot <- team_dash_with_logos
  teams_highlighted_full <- NULL
  teams_highlighted_last_n <- NULL
  
} else if (highlight_plot_mode == "full_season") {
  # Show highlighted team in color (full season), others in grey
  teams_to_plot <- team_dash_with_logos %>% filter(team_abbreviation != highlight_team)
  teams_highlighted_full <- team_dash_with_logos %>% filter(team_abbreviation == highlight_team)
  teams_highlighted_last_n <- NULL
  
} else if (highlight_plot_mode == "last_n") {
  # Show highlighted team last N games only, others in grey
  teams_to_plot <- team_dash_with_logos %>% filter(team_abbreviation != highlight_team)
  teams_highlighted_full <- NULL
  if (!is.null(team_hi)) {
    teams_highlighted_last_n <- team_hi %>%
      # keep the logo we already have from the API
      dplyr::rename(team_logo_from_hi = team_logo) %>%
      # try to grab a backup from the lookup in case name-matching works there
      dplyr::left_join(
        team_lookup %>% dplyr::select(team_name, team_logo),
        by = "team_name"
      ) %>%
      # prefer the logo from the API, fall back to lookup
      dplyr::mutate(
        team_logo = dplyr::coalesce(team_logo_from_hi, team_logo)
      ) %>%
      dplyr::select(-team_logo_from_hi)
  } else {
    teams_highlighted_last_n <- NULL
  }
  
} else if (highlight_plot_mode == "both") {
  # Show highlighted team both full season (faded) and last N games (bright)
  teams_to_plot <- team_dash_with_logos %>% filter(team_abbreviation != highlight_team)
  teams_highlighted_full <- team_dash_with_logos %>% filter(team_abbreviation == highlight_team)
  if (!is.null(team_hi)) {
    teams_highlighted_last_n <- team_hi %>%
      # keep the logo we already have from the API
      dplyr::rename(team_logo_from_hi = team_logo) %>%
      # try to grab a backup from the lookup in case name-matching works there
      dplyr::left_join(
        team_lookup %>% dplyr::select(team_name, team_logo),
        by = "team_name"
      ) %>%
      # prefer the logo from the API, fall back to lookup
      dplyr::mutate(
        team_logo = dplyr::coalesce(team_logo_from_hi, team_logo)
      ) %>%
      dplyr::select(-team_logo_from_hi)
  } else {
    teams_highlighted_last_n <- NULL
  }
}

# Build team plot
if (requireNamespace("ggimage", quietly = TRUE)) {
  # All non-highlighted teams in grey
  if (nrow(teams_to_plot) > 0) {
    p_top <- p_top +
      ggimage::geom_image(
        data = teams_to_plot %>% filter(!is.na(team_logo)),
        aes(x = off_rating_std, y = def_rating_std, image = team_logo),
        size = 0.04,
        inherit.aes = FALSE
      )
  }
  
  # Highlighted team full season (if applicable)
  if (!is.null(teams_highlighted_full) && nrow(teams_highlighted_full) > 0) {
    alpha_val <- if (highlight_plot_mode == "both") 0.4 else 1.0
    size_val <- if (highlight_plot_mode == "both") 0.04 else 0.08
    
    p_top <- p_top +
      ggimage::geom_image(
        data = teams_highlighted_full %>% filter(!is.na(team_logo)),
        aes(x = off_rating_std, y = def_rating_std, image = team_logo),
        size = size_val,
        alpha = alpha_val,
        inherit.aes = FALSE
      )
    
    # Add label for full season only mode
    if (highlight_plot_mode == "full_season") {
      p_top <- p_top +
        ggrepel::geom_text_repel(
          data = teams_highlighted_full,
          aes(
            x = off_rating_std,
            y = def_rating_std,
            label = team_abbreviation,
            color = team_abbreviation
          ),
          size = 4,
          fontface = "bold",
          box.padding = 0.3,
          point.padding = 0.5,
          segment.color = "grey50",
          max.overlaps = Inf,
          show.legend = FALSE
        )
    }
  }
  
  # Highlighted team last N games (if applicable)
  if (!is.null(teams_highlighted_last_n) && nrow(teams_highlighted_last_n) > 0) {
    p_top <- p_top +
      ggimage::geom_image(
        data = teams_highlighted_last_n %>% filter(!is.na(team_logo)),
        aes(x = off_rating_std, y = def_rating_std, image = team_logo),
        size = 0.06,
        inherit.aes = FALSE
      ) +
      ggrepel::geom_text_repel(
        data = teams_highlighted_last_n,
        aes(
          x = off_rating_std,
          y = def_rating_std,
          label = paste0(team_abbreviation, " (last ", highlight_team_last_n_games, "g)")
        ),
        color = "grey30",               # move outside aes()
        size = 4,
        fontface = "bold",
        box.padding = 1.0,              # was 0.3
        point.padding = 1.5,            # was 0.5
        min.segment.length = 0,         # always draw a line
        segment.color = "grey50",
        max.overlaps = Inf,
        force = 3,                      # push a little harder
        nudge_y = 0.5,                 # slight vertical nudge away from logo
        show.legend = FALSE
      )
  }
  
  # Add color scale if needed
  if (!is.null(highlight_team) && highlight_plot_mode != "none") {
    p_top <- p_top + scale_color_manual(values = team_colors, guide = "none")
  }
}

# Title and caption
title_text <- if (is.null(highlight_team) || highlight_plot_mode == "none") {
  "League Efficiency Landscape (Teams)"
} else if (highlight_plot_mode == "full_season") {
  paste0("League Efficiency Landscape (Teams) - ", highlight_team, " Highlighted")
} else if (highlight_plot_mode == "last_n") {
  paste0("League Efficiency Landscape (Teams) - ", highlight_team, " Last ", highlight_team_last_n_games, " Games")
} else {
  paste0("League Efficiency Landscape (Teams) - ", highlight_team, " Comparison")
}

caption_text <- if (is.null(highlight_team) || highlight_plot_mode == "none") {
  "Standardized to league average; diagonal = net neutral."
} else if (highlight_plot_mode == "both") {
  "Standardized to league average; diagonal = net neutral. Highlighted team shows both full season (faded) and last 10 games (bright)."
} else {
  "Standardized to league average; diagonal = net neutral."
}

p_top <- p_top +
  labs(
    title = title_text,
    x = "Offensive Rating",
    y = "Defensive Rating",
    caption = caption_text
  )

# ============================================================================
# BOTTOM PANEL: PLAYERS
# ============================================================================

p_bottom <- base_plot

# Filter players by minutes threshold
adv_data_filtered_min <- adv_data_filtered %>% filter(min >= min_minutes_threshold)

# Determine which players to show based on highlight settings
if (is.null(highlight_team) || highlight_plot_mode == "none") {
  # Show all players in grey
  players_other <- adv_data_filtered_min
  players_highlighted_full <- NULL
  players_highlighted_last_n <- NULL
  
} else if (highlight_plot_mode == "full_season") {
  # Show highlighted team in color (full season), others in grey
  players_other <- adv_data_filtered_min %>% filter(team_abbreviation != highlight_team)
  players_highlighted_full <- adv_data_filtered_min %>% filter(team_abbreviation == highlight_team)
  players_highlighted_last_n <- NULL
  
} else if (highlight_plot_mode == "last_n") {
  # Show highlighted team last N games only, others in grey
  players_other <- adv_data_filtered_min %>% filter(team_abbreviation != highlight_team)
  players_highlighted_full <- NULL
  players_highlighted_last_n <- if (!is.null(adv_hi)) adv_hi %>% filter(min >= min_minutes_threshold) else NULL
  
} else if (highlight_plot_mode == "both") {
  # Show highlighted team both full season (faded) and last N games (bright)
  players_other <- adv_data_filtered_min %>% filter(team_abbreviation != highlight_team)
  players_highlighted_full <- adv_data_filtered_min %>% filter(team_abbreviation == highlight_team)
  players_highlighted_last_n <- if (!is.null(adv_hi)) adv_hi %>% filter(min >= min_minutes_threshold) else NULL
}

# Build player plot
# All non-highlighted players in grey
if (!is.null(players_other) && nrow(players_other) > 0) {
  p_bottom <- p_bottom +
    geom_point(
      data = players_other,
      aes(
        x = off_rating_std,
        y = def_rating_std,
        size = usg_pct,
        alpha = min * 0.6
      ),
      color = "grey70",
      fill = NA,
      stroke = 0
    )
}

# Highlighted team players full season (if applicable)
if (!is.null(players_highlighted_full) && nrow(players_highlighted_full) > 0) {
  alpha_val <- if (highlight_plot_mode == "both") 0.3 else NULL
  
  if (highlight_plot_mode == "both") {
    p_bottom <- p_bottom +
      geom_point(
        data = players_highlighted_full,
        aes(
          x = off_rating_std,
          y = def_rating_std,
          size = usg_pct,
          color = team_abbreviation
        ),
        alpha = alpha_val,
        fill = NA,
        stroke = 0
      )
  } else {
    p_bottom <- p_bottom +
      geom_point(
        data = players_highlighted_full,
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
      ggrepel::geom_text_repel(
        data = players_highlighted_full,
        aes(
          x = off_rating_std,
          y = def_rating_std,
          label = player_name,
          color = team_abbreviation
        ),
        size = 3.8,
        fontface = "bold",
        box.padding = 0.3,
        point.padding = 0.5,
        segment.color = "grey50",
        max.overlaps = Inf,
        show.legend = FALSE
      )
  }
}

# Highlighted team players last N games (if applicable)
if (!is.null(players_highlighted_last_n) && nrow(players_highlighted_last_n) > 0) {
  p_bottom <- p_bottom +
    geom_point(
      data = players_highlighted_last_n,
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
    ggrepel::geom_text_repel(
      data = players_highlighted_last_n,
      aes(
        x = off_rating_std,
        y = def_rating_std,
        label = paste0(player_name, " (last ", highlight_team_last_n_games, "g)"),
        color = team_abbreviation
      ),
      size = 3.8,
      fontface = "bold",
      box.padding = 0.3,
      point.padding = 0.5,
      segment.color = "grey50",
      max.overlaps = Inf,
      show.legend = FALSE
    )
}

# Add scales
if (!is.null(highlight_team) && highlight_plot_mode != "none") {
  p_bottom <- p_bottom + scale_color_manual(values = team_colors, guide = "none")
}

p_bottom <- p_bottom +
  scale_size_continuous(name = "Usage (%)", range = c(1, 8), guide = guide_legend()) +
  scale_alpha_continuous(name = "Minutes per Game", range = c(0.1, 0.7), guide = guide_legend(override.aes = list(size = 6)))

# Title and caption for player panel
player_title_text <- if (is.null(highlight_team) || highlight_plot_mode == "none") {
  "League Efficiency Landscape (Players)"
} else if (highlight_plot_mode == "full_season") {
  paste0("League Efficiency Landscape (Players) - ", highlight_team, " Highlighted")
} else if (highlight_plot_mode == "last_n") {
  paste0("League Efficiency Landscape (Players) - ", highlight_team, " Last ", highlight_team_last_n_games, " Games")
} else {
  paste0("League Efficiency Landscape (Players) - ", highlight_team, " Comparison")
}

player_caption_text <- if (is.null(highlight_team) || highlight_plot_mode == "none") {
  paste0("Standardized to league average; diagonal = net neutral. All players shown. Players sized by usage %. Only players with min ≥ ", min_minutes_threshold, ".")
} else if (highlight_plot_mode == "both") {
  paste0("Standardized to league average; diagonal = net neutral. ", highlight_team, " shows both full season (faded) and last ", highlight_team_last_n_games, " games (bright). All other teams show full season. Players sized by usage %. Only players with min ≥ ", min_minutes_threshold, ".")
} else {
  paste0("Standardized to league average; diagonal = net neutral. Players sized by usage %. Only players with min ≥ ", min_minutes_threshold, ".")
}

p_bottom <- p_bottom +
  labs(
    title = player_title_text,
    x = "Offensive Rating",
    y = "Defensive Rating",
    caption = player_caption_text
  )

# ============================================================================
# COMBINE AND SAVE
# ============================================================================

combined_plot <- p_top / p_bottom + plot_layout(ncol = 1, heights = c(1, 1))

# Save plot
output_dir <- "nba/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Build filename
if (is.null(highlight_team) || highlight_plot_mode == "none") {
  file_name <- paste0(output_dir, "/league_efficiency_", season_year, ".png")
} else {
  mode_tag <- switch(highlight_plot_mode,
                     "full_season" = "_full",
                     "last_n" = paste0("_last", highlight_team_last_n_games, "g"),
                     "both" = paste0("_both_last", highlight_team_last_n_games, "g"),
                     "")
  file_name <- paste0(output_dir, "/league_efficiency_", season_year, "_", highlight_team, mode_tag, ".png")
}

ggsave(
  filename = file_name,
  plot = combined_plot,
  width = 10,
  height = 16,
  dpi = 300
)
message("Saved plot to: ", file_name)