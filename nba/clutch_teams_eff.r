required_packages <- c(
  "tidyverse", "hoopR", "janitor", "ggrepel", "jsonlite", "rvest", 
  "tictoc", "progressr", "purrr", "ggimage", "patchwork"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# Configuration
highlight_team <- "POR"
season_year <- 2023
clutch_definition <- "Last5Minutes"  # Options: "Last2Minutes", "Last5Minutes", "Last1Minute"
date_from <- "2023-03-24"  # e.g., "2023-02-01" or NULL for full season
date_to <- NULL    # e.g., "2023-04-15" or NULL for full season
show_efficiency_change <- TRUE  # TRUE to plot change from regular to clutch efficiency

# Map clutch definition to API parameter
clutch_time_param <- switch(clutch_definition,
  "Last1Minute" = "Last 1 Minute",
  "Last2Minutes" = "Last 2 Minutes",
  "Last5Minutes" = "Last 5 Minutes",
  "Last 5 Minutes"  # default
)

message("Loading clutch team stats...")
# Load clutch team statistics
clutch_team_data <- nba_leaguedashteamclutch(
  season = season_year,
  season_type = "Regular Season",
  measure_type = "Advanced",
  clutch_time = clutch_time_param,
  ahead_behind = "Ahead or Behind",  # Games within 5 points
  point_diff = 5,
  date_from = date_from,
  date_to = date_to
)

# Clean team clutch data
team_clutch_ratings <- clutch_team_data$LeagueDashTeamClutch %>%
  janitor::clean_names() %>%
  mutate(
    off_rating = as.numeric(off_rating),
    def_rating = as.numeric(def_rating),
    net_rating = as.numeric(net_rating),
    team_name = stringr::str_trim(team_name),
    gp = as.numeric(gp)  # games played in clutch situations
  ) %>%
  filter(gp >= 5)  # Minimum 5 clutch games

message("Loading clutch player stats...")
# Load clutch player statistics
clutch_player_data <- nba_leaguedashplayerclutch(
  season = season_year,
  season_type = "Regular Season",
  measure_type = "Advanced",
  clutch_time = clutch_time_param,
  ahead_behind = "Ahead or Behind",
  point_diff = 5,
  date_from = date_from,
  date_to = date_to
)

# Clean player clutch data
clutch_player_stats <- clutch_player_data$LeagueDashPlayerClutch %>%
  janitor::clean_names() %>%
  mutate(
    off_rating = as.numeric(off_rating),
    def_rating = as.numeric(def_rating),
    net_rating = as.numeric(net_rating),
    usg_pct = as.numeric(usg_pct) * 100,  # Convert to percentage
    min = as.numeric(min),
    gp = as.numeric(gp)
  ) %>%
  filter(min >= 5)  # Minimum 5 minutes in clutch time

# Load regular season stats for comparison if flag is set
if (show_efficiency_change) {
  message("Loading regular season stats for comparison...")
  
  regular_team_data <- nba_leaguedashteamstats(
    season = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    date_from = date_from,
    date_to = date_to
  )
  
  regular_team_ratings <- regular_team_data$LeagueDashTeamStats %>%
    janitor::clean_names() %>%
    mutate(
      reg_off_rating = as.numeric(off_rating),
      reg_def_rating = as.numeric(def_rating),
      team_name = stringr::str_trim(team_name)
    ) %>%
    select(team_name, reg_off_rating, reg_def_rating)
  
  regular_player_data <- nba_leaguedashplayerstats(
    season = season_year,
    season_type = "Regular Season",
    measure_type = "Advanced",
    date_from = date_from,
    date_to = date_to
  )
  
  regular_player_ratings <- regular_player_data$LeagueDashPlayerStats %>%
    janitor::clean_names() %>%
    mutate(
      reg_off_rating = as.numeric(off_rating),
      reg_def_rating = as.numeric(def_rating),
      player_name = stringr::str_trim(player_name)
    ) %>%
    select(player_name, reg_off_rating, reg_def_rating)
}

# Load team lookup for logos and colors
team_lookup <- load_nba_team_box(season = season_year) %>%
  distinct(team_id, team_abbreviation, team_display_name, team_logo) %>%
  mutate(
    team_name = stringr::str_trim(team_display_name),
    team_abbreviation = case_when(
      team_abbreviation == "UTAH" ~ "UTA",
      team_abbreviation == "NY"   ~ "NYK",
      team_abbreviation == "NO"   ~ "NOP",
      team_abbreviation == "GS"   ~ "GSW",
      team_abbreviation == "SA"   ~ "SAS",
      TRUE ~ team_abbreviation
    )
  ) %>%
  distinct(team_name, .keep_all = TRUE)

# Join team data with lookup by team_name (both datasets have this field)
team_clutch_ratings <- team_clutch_ratings %>%
  left_join(team_lookup %>% select(team_name, team_abbreviation, team_logo), 
            by = "team_name")

# If comparing to regular season, join and calculate changes
if (show_efficiency_change) {
  team_clutch_ratings <- team_clutch_ratings %>%
    left_join(regular_team_ratings, by = "team_name") %>%
    mutate(
      off_rating_change = off_rating - reg_off_rating,
      def_rating_change = def_rating - reg_def_rating
    )
}

# Join player data with team lookup by team_abbreviation (available in player clutch data)
clutch_player_stats <- clutch_player_stats %>%
  left_join(team_lookup %>% select(team_abbreviation, team_logo), 
            by = "team_abbreviation")

# If comparing to regular season, join and calculate changes
if (show_efficiency_change) {
  clutch_player_stats <- clutch_player_stats %>%
    left_join(regular_player_ratings, by = "player_name") %>%
    mutate(
      off_rating_change = off_rating - reg_off_rating,
      def_rating_change = def_rating - reg_def_rating
    )
}

# Standardize to league averages
league_off_avg <- mean(team_clutch_ratings$off_rating, na.rm = TRUE)
league_def_avg <- mean(team_clutch_ratings$def_rating, na.rm = TRUE)

# Choose what to plot based on flag
if (show_efficiency_change) {
  # Plot the CHANGE from regular to clutch efficiency
  team_clutch_ratings <- team_clutch_ratings %>%
    mutate(
      off_rating_std = off_rating_change,
      def_rating_std = -def_rating_change  # Negate for consistent plotting
    )
  
  clutch_player_stats <- clutch_player_stats %>%
    mutate(
      off_rating_std = off_rating_change,
      def_rating_std = -def_rating_change
    )
  
  plot_subtitle <- "Change from Regular Season Efficiency"
  x_label <- "Offensive Rating Change (Clutch - Regular)"
  y_label <- "Defensive Rating Change (Clutch - Regular)"
} else {
  # Plot absolute clutch efficiency standardized to league average
  team_clutch_ratings <- team_clutch_ratings %>%
    mutate(
      off_rating_std = off_rating - league_off_avg,
      def_rating_std = -(def_rating - league_def_avg)  # Negate for consistent plotting
    )
  
  clutch_player_stats <- clutch_player_stats %>%
    mutate(
      off_rating_std = off_rating - league_off_avg,
      def_rating_std = -(def_rating - league_def_avg)
    )
  
  plot_subtitle <- "Standardized to League Average"
  x_label <- "Offensive Rating (Clutch)"
  y_label <- "Defensive Rating (Clutch)"
}

# Define team colors
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

# Set range limit
range_limit <- 15

# Base plot components
base_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.2) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.2) +
  geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = range_limit * 0.85, y = range_limit * -0.05,
           label = "Better Offense \u2192", color = "grey40", fontface = "italic",
           size = 4, hjust = 0.5, vjust = 0) +
  annotate("text", x = range_limit * 0.03, y = range_limit * 0.90,
           label = "\u2191 Better Defense", color = "grey40", fontface = "italic",
           size = 4, hjust = 0, vjust = 0) +
  annotate("text", x = -range_limit * 0.85, y = range_limit * 0.85,
           label = "Net Positive", color = "grey40", fontface = "italic",
           size = 4, hjust = 0, vjust = 0, angle = -45) +
  coord_equal() +
  scale_x_continuous(limits = c(-range_limit, range_limit)) +
  scale_y_continuous(limits = c(-range_limit, range_limit)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")

# Top panel: Team logos
p_top <- base_plot
if (requireNamespace("ggimage", quietly = TRUE)) {
  p_top <- p_top +
    ggimage::geom_image(
      data = team_clutch_ratings %>% 
        filter(team_abbreviation != highlight_team & !is.na(team_logo)),
      aes(x = off_rating_std, y = def_rating_std, image = team_logo),
      size = 0.08
    ) +
    ggimage::geom_image(
      data = team_clutch_ratings %>% 
        filter(team_abbreviation == highlight_team & !is.na(team_logo)),
      aes(x = off_rating_std, y = def_rating_std, image = team_logo),
      size = 0.09
    )
}

p_top <- p_top +
  labs(
    title = paste0("Clutch Time Efficiency Landscape (", clutch_definition, ") - Teams"),
    subtitle = plot_subtitle,
    x = x_label,
    y = y_label,
    caption = paste0("Clutch = ", clutch_time_param, ", score within 5 pts.", 
                     if(!is.null(date_from) || !is.null(date_to)) paste0(" | Date range: ", date_from %||% "start", " to ", date_to %||% "end") else "")
  )

# Debug: Check if we have any data
message(paste("Total players with clutch data:", nrow(clutch_player_stats)))
message(paste("Players for", highlight_team, ":", nrow(clutch_player_stats %>% filter(team_abbreviation == highlight_team))))
message(paste("Range of off_rating_std:", min(clutch_player_stats$off_rating_std, na.rm=TRUE), "to", max(clutch_player_stats$off_rating_std, na.rm=TRUE)))
message(paste("Range of def_rating_std:", min(clutch_player_stats$def_rating_std, na.rm=TRUE), "to", max(clutch_player_stats$def_rating_std, na.rm=TRUE)))

# Bottom panel: Players
highlight_players <- clutch_player_stats %>% filter(team_abbreviation == highlight_team)
other_players <- clutch_player_stats %>% filter(team_abbreviation != highlight_team)

# Increase range_limit for player plot to accommodate wider spread
player_range_limit <- 25

p_bottom <- ggplot() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.2) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.2) +
  geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = player_range_limit * 0.85, y = player_range_limit * -0.05,
           label = "Better Offense \u2192", color = "grey40", fontface = "italic",
           size = 4, hjust = 0.5, vjust = 0) +
  annotate("text", x = player_range_limit * 0.03, y = player_range_limit * 0.90,
           label = "\u2191 Better Defense", color = "grey40", fontface = "italic",
           size = 4, hjust = 0, vjust = 0) +
  annotate("text", x = -player_range_limit * 0.85, y = player_range_limit * 0.85,
           label = "Net Positive", color = "grey40", fontface = "italic",
           size = 4, hjust = 0, vjust = 0, angle = -45) +
  coord_equal() +
  scale_x_continuous(limits = c(-player_range_limit, player_range_limit)) +
  scale_y_continuous(limits = c(-player_range_limit, player_range_limit)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  geom_point(data = other_players,
             aes(x = off_rating_std, y = def_rating_std, 
                 size = usg_pct, alpha = min),
             color = "grey70", stroke = 0) +
  geom_point(data = highlight_players,
             aes(x = off_rating_std, y = def_rating_std, 
                 size = usg_pct, color = team_abbreviation, alpha = min),
             stroke = 0) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_size_continuous(name = "Clutch Usage (%)", range = c(1, 10)) +
  scale_alpha_continuous(name = "Clutch Minutes", range = c(0.2, 0.8),
                         guide = guide_legend(override.aes = list(size = 6))) +
  ggrepel::geom_text_repel(
    data = highlight_players,
    aes(x = off_rating_std, y = def_rating_std, 
        label = player_name, color = team_abbreviation),
    size = 4, fontface = "bold", box.padding = 0.3,
    segment.color = "grey50", max.overlaps = Inf, show.legend = FALSE
  ) +
  labs(
    title = paste0("Clutch Time Player Performance (Highlight: ", highlight_team, ")"),
    subtitle = plot_subtitle,
    x = x_label,
    y = y_label,
    caption = paste0("Player performance in clutch situations (", clutch_time_param, "). Size = usage %, opacity = minutes played.",
                     if(!is.null(date_from) || !is.null(date_to)) paste0("\nDate range: ", date_from %||% "start", " to ", date_to %||% "end") else "")
  )

# Fallback operator for NULL handling
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Combine plots
combined_plot <- p_top / p_bottom + plot_layout(ncol = 1, heights = c(1, 1))

# Create summary table for highlighted team
team_summary <- team_clutch_ratings %>%
  filter(team_abbreviation == highlight_team) %>%
  select(team_name, gp, w, l, w_pct, off_rating, def_rating, net_rating,
         if(show_efficiency_change) c(reg_off_rating, reg_def_rating, off_rating_change, def_rating_change) else NULL) %>%
  mutate(across(where(is.character), as.numeric))

player_summary <- clutch_player_stats %>%
  filter(team_abbreviation == highlight_team) %>%
  select(player_name, gp, min, off_rating, def_rating, net_rating, usg_pct,
         if(show_efficiency_change) c(reg_off_rating, reg_def_rating, off_rating_change, def_rating_change) else NULL) %>%
  arrange(desc(min))

# Print tables to console
message("\n=== TEAM CLUTCH RATINGS ===")
if (show_efficiency_change) {
  print(team_clutch_ratings %>% 
          select(team_abbreviation, team_name, gp, 
                 reg_off_rating, off_rating, off_rating_change,
                 reg_def_rating, def_rating, def_rating_change, net_rating) %>%
          arrange(desc(off_rating_change)))
} else {
  print(team_clutch_ratings %>% 
          select(team_abbreviation, team_name, gp, off_rating, def_rating, net_rating, off_rating_std, def_rating_std) %>%
          arrange(desc(net_rating)))
}

message(paste0("\n=== ", highlight_team, " CLUTCH STATS ==="))
if(nrow(team_summary) > 0) {
  print(team_summary)
} else {
  message("No team data found for ", highlight_team)
}

message(paste0("\n=== ", highlight_team, " PLAYER CLUTCH STATS ==="))
if(nrow(player_summary) > 0) {
  if (show_efficiency_change) {
    print(player_summary %>% select(player_name, gp, min, reg_off_rating, off_rating, off_rating_change, 
                                     reg_def_rating, def_rating, def_rating_change, usg_pct))
  } else {
    print(player_summary)
  }
} else {
  message("No player data found for ", highlight_team)
}

# Save plot
output_dir <- "nba/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

file_suffix <- if(show_efficiency_change) "_change" else "_absolute"
file_name <- paste0(output_dir, "/clutch_efficiency_", clutch_definition, "_", highlight_team, file_suffix, ".png")
ggsave(filename = file_name, plot = combined_plot, width = 10, height = 16, dpi = 300)
message("Saved clutch time efficiency plot to: ", file_name)
