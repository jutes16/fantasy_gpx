library(tidyverse)
library(hoopR)
library(janitor)
library(glue)
library(ggrepel)
library(ggimage)

# --------- params ----------
season_year <- 2023
include_playoffs <- FALSE          # TRUE to include playoffs
clutch_window   <- "Last 2 Minutes" # other options on NBA Stats: "Last 5 Minutes", etc.
score_margin    <- 5               # within +/- 5
per_mode        <- "Totals"       # per-100 possessions
min_clutch_minutes <- 0       # filter small samples
min_clutch_games <- 10  # filter small samples by games played (e.g., at least 5)
#highlight_team <- "UTA"            # 3-letter code to color; set NULL to disable
compare_mode <- "delta"  # "absolute" (plot Off vs Def) or "delta" (Clutch minus Regular)
save_plot    <- TRUE        # set FALSE to skip saving the PNG
highlight_defense <- TRUE   # TRUE: highlight top-5 defense; FALSE: highlight top-5 net rating
# ---------------------------

# helpers ---------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

normalize_abbr <- function(x) dplyr::recode(
  x,
  "NY"   = "NYK",
  "NO"   = "NOP",
  "GS"   = "GSW",
  "SA"   = "SAS",
  "UTAH" = "UTA",
  "WSH"  = "WAS",
  .default = x
)

season_types <- if (include_playoffs) c("Regular Season","Playoffs") else "Regular Season"

# team lookup (logo + colors you already used earlier)
team_lookup <- load_nba_team_box(season = season_year) %>%
  distinct(team_id, team_abbreviation, team_display_name, team_logo) %>%
  mutate(
    team_name = stringr::str_trim(team_display_name),
    team_abbreviation = normalize_abbr(team_abbreviation)
  )

# ---- 1) Pull clutch once per season type and bind --------------------------
pull_clutch_one <- function(st) {
  res <- hoopR::nba_leaguedashplayerclutch(
    season        = season_year,
    season_type   = st,                 # call once per season type
    per_mode      = per_mode,
    measure_type  = "Advanced",         # OFF/DEF/NET ratings
    clutch_time   = clutch_window,      # e.g., "Last 2 Minutes"
    ahead_behind  = "Ahead or Behind",
    point_diff    = score_margin,       # within +/- score_margin
    pace_adjust   = "N",
    plus_minus    = "N"
  )
  out <- res$LeagueDashPlayerClutch %>%
    janitor::clean_names() %>%
    mutate(season_type = st)            # tag rows with season_type
  out
}

clutch <- purrr::map_dfr(season_types, pull_clutch_one)

# ---- 2) Parse numerics & normalize abbr, then join logos -------------------
num_cols <- c("min","off_rating","def_rating","net_rating","gp","pie")
clutch <- clutch %>%
  # Coerce possible mixed types (char/numeric/factor) to character before parse_number
  mutate(
    across(
      any_of(num_cols),
      ~ readr::parse_number(as.character(.x))
    )
  ) %>%
  mutate(team_abbreviation = normalize_abbr(team_abbreviation)) %>%
  left_join(
    team_lookup %>%
      dplyr::select(
        team_abbreviation,
        team_logo_lkp = team_logo,
        team_name_lkp = team_name
      ),
    by = "team_abbreviation"
  ) %>%
  # take logos/names from lookup as authoritative
  mutate(
    team_logo = team_logo_lkp,
    team_name = team_name_lkp
  ) %>%
  dplyr::select(-team_logo_lkp, -team_name_lkp)

# ---- 3) Keep real samples: min clutch minutes threshold ----
clutch_filtered <- clutch %>%
  # Make absolutely sure these are numeric before filtering
  mutate(
    gp  = as.numeric(gp),
    min = as.numeric(min),
    def_rating = as.numeric(def_rating),
    off_rating = as.numeric(off_rating),
    net_rating = as.numeric(net_rating),
    pie = as.numeric(pie),
  ) %>%
  filter(
    !is.na(def_rating),
    !is.na(min),
    !is.na(gp),
    min >= min_clutch_minutes,
    gp  >= min_clutch_games
  )

# Helpful debug print: show filter result
message(glue::glue(
  "Filtered to ≥{min_clutch_minutes} clutch minutes and ≥{min_clutch_games} clutch games. ",
  "Remaining rows: {nrow(clutch_filtered)}"
))

# ---- 4) Scatter: Off vs Def clutch ratings; highlight Top 5 by Net ----
scatter_df <- clutch_filtered %>%
  mutate(
    net_rating = as.numeric(net_rating),
    off_rating = as.numeric(off_rating),
    def_rating = as.numeric(def_rating),
    pie = as.numeric(pie),
  ) %>%
  filter(!is.na(net_rating), !is.na(off_rating), !is.na(def_rating))

# ---- 4b) Optional: compare clutch to regular season (delta) ----
if (compare_mode == "delta") {
  reg_res <- hoopR::nba_leaguedashplayerstats(
    season       = season_year,
    season_type  = "Regular Season",
    measure_type = "Advanced",
    per_mode     = per_mode
  )
  regular <- reg_res$LeagueDashPlayerStats %>%
    janitor::clean_names() %>%
    mutate(
      off_rating = readr::parse_number(as.character(off_rating)),
      def_rating = readr::parse_number(as.character(def_rating)),
      net_rating = readr::parse_number(as.character(net_rating))
    ) %>%
    dplyr::select(
      player_id,
      off_rating_reg = off_rating,
      def_rating_reg = def_rating,
      net_rating_reg = net_rating
    )

  # Join baseline and compute deltas
  scatter_df <- scatter_df %>%
    left_join(regular, by = "player_id") %>%
    mutate(
      delta_off = off_rating - off_rating_reg,
      # positive = better defense in clutch because baseline minus clutch (lower is better)
      delta_def = def_rating_reg - def_rating,
      delta_net = net_rating - net_rating_reg
    )
}

# ----- Choose primary highlight set (defense or net) + Top 5 PIE -----
# Top 5 by PIE (highest = best)
top5_pie_ids <- scatter_df %>%
  arrange(desc(pie)) %>%
  slice_head(n = 5) %>%
  pull(player_id)

if (isTRUE(highlight_defense)) {
  # Primary = Defense: absolute -> lowest DEF is best; delta -> highest ΔDEF is best
  if (compare_mode == "delta") {
    top5_primary_ids <- scatter_df %>%
      arrange(desc(delta_def)) %>%
      slice_head(n = 5) %>%
      pull(player_id)
    primary_label <- "Top 5 ΔDefense"
    both_label    <- "Top 5 (ΔDefense & PIE)"
  } else {
    top5_primary_ids <- scatter_df %>%
      arrange(def_rating) %>%   # lower = better defense
      slice_head(n = 5) %>%
      pull(player_id)
    primary_label <- "Top 5 Defense"
    both_label    <- "Top 5 (Defense & PIE)"
  }
  primary_text <- if (compare_mode == "delta") "ΔDefense" else "Defense"
} else {
  # Primary = Net Rating (existing behavior)
  top5_primary_ids <- scatter_df %>%
    arrange(desc(net_rating)) %>%
    slice_head(n = 5) %>%
    pull(player_id)
  primary_label <- "Top 5 Net Rating"
  both_label    <- "Top 5 (Net & PIE)"
  primary_text  <- "Net"
}

# Flags + combined category for color mapping
scatter_df <- scatter_df %>%
  mutate(
    is_primary  = player_id %in% top5_primary_ids,
    is_top5_pie = player_id %in% top5_pie_ids,
    category = dplyr::case_when(
      is_primary & is_top5_pie ~ both_label,
      is_primary              ~ primary_label,
      is_top5_pie             ~ "Top 5 PIE",
      TRUE                    ~ "Others"
    )
  )

# Axis ranges and plot-variable mapping depend on compare_mode
if (compare_mode == "delta") {
  plot_df <- scatter_df
  x_var <- "delta_off"
  y_var <- "delta_def"

  x_range <- max(abs(plot_df[[x_var]]), na.rm = TRUE)
  y_range <- max(abs(plot_df[[y_var]]), na.rm = TRUE)
  x_pad <- ceiling(x_range / 5) * 5
  y_pad <- ceiling(y_range / 5) * 5
  x_lim <- c(-x_pad, x_pad)
  y_lim <- c(-y_pad, y_pad)
} else {
  plot_df <- scatter_df
  x_var <- "off_rating"
  y_var <- "def_rating"

  x_min <- floor(min(plot_df[[x_var]], na.rm = TRUE) / 5) * 5
  x_max <- ceiling(max(plot_df[[x_var]], na.rm = TRUE) / 5) * 5
  y_min <- floor(min(plot_df[[y_var]], na.rm = TRUE) / 5) * 5
  y_max <- ceiling(max(plot_df[[y_var]], na.rm = TRUE) / 5) * 5
  x_lim <- c(x_min, x_max)
  y_lim <- c(y_max, y_min)  # used with scale_y_reverse below
}

p <- ggplot(plot_df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
  # All points with mapped color by category
  geom_point(
    aes(size = min, alpha = pie, color = category)
  ) +
  # Labels for primary top-5 (including overlap "Both")
  ggrepel::geom_text_repel(
    data = subset(scatter_df, category %in% c(primary_label, both_label)),
    aes(label = glue::glue("{player_name} ({team_abbreviation})")),
    size = 3.4, fontface = "bold", segment.color = "grey60",
    max.overlaps = Inf, box.padding = 0.35, point.padding = 0.25
  ) +
  ggrepel::geom_text_repel(
    data = subset(scatter_df, category %in% c("Top 5 PIE")),
    aes(label = glue::glue("{player_name} ({team_abbreviation})")),
    color = "#D95F02",
    size = 3.4,
    fontface = "bold",
    segment.color = "grey60",
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.25,
    show.legend = FALSE
  ) +
  scale_size_continuous(name = "Clutch Minutes", range = c(1.5, 10)) +
  scale_alpha_continuous(name = "PIE", range = c(0.25, 0.95)) +
  scale_color_manual(
    name = "Highlight",
    values = setNames(
      c("grey75", "#2C7FB8", "#D95F02", "#6A3D9A"),
      c("Others", primary_label, "Top 5 PIE", both_label)
    )
  ) +
  # Scales: absolute uses reversed y; delta uses symmetric limits about 0
  {
    if (compare_mode == "delta") {
      scale_y_continuous(limits = y_lim)
    } else {
      scale_y_reverse(limits = y_lim)
    }
  } +
  coord_cartesian(xlim = c(x_lim[1], x_lim[2])) +
  # Zero reference lines for delta mode
  {
    if (compare_mode == "delta") {
      list(
        geom_hline(yintercept = 0, color = "grey65", linewidth = 0.6),
        geom_vline(xintercept = 0, color = "grey65", linewidth = 0.6)
      )
    } else {
      NULL
    }
  } +
  labs(
    title = glue("{season_year} Clutch Off vs Def — {clutch_window} (±{score_margin})"),
    subtitle = glue(
      "{if (compare_mode == 'delta') 'Δ vs Regular Season • ' else ''}",
      "Top 5 {primary_text} (blue) & Top 5 PIE (orange) • ≥{min_clutch_minutes} clutch min & ≥{min_clutch_games} games • {per_mode} • size ∝ clutch minutes, alpha ∝ PIE"
    ),
    x = if (compare_mode == "delta") "Δ Off Rating (Clutch − Regular)" else "Clutch Offensive Rating (per 100 poss)",
    y = if (compare_mode == "delta") "Δ Def Rating (Regular − Clutch; + = better D in clutch)" else "Clutch Defensive Rating (lower is better)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

print(p)

if (isTRUE(save_plot)) {
  safe_window <- gsub("[^A-Za-z0-9]+", "_", clutch_window)
  file_name <- glue::glue("nba/plots/{season_year}_clutch_{safe_window}_pm{score_margin}_{per_mode}_{compare_mode}.png")
  ggplot2::ggsave(filename = file_name, plot = p, width = 10, height = 7, dpi = 300)
  message(glue::glue("Saved plot to {file_name}"))
}
