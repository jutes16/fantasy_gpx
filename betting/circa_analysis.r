# ---- Setup ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(scales)
  library(glue)
  library(nflreadr)
})

# ---- Parameters (edit as needed) ----
# You can also set CIRCA_XLSX env var to override `path`
base_dir        <- "/Users/jgilbert/Documents/GitHub/fantasy_gpx/betting"
path            <- file.path(base_dir, "CircaContest.xlsx")
sheet           <- 1
picks_per_week  <- 5
metric          <- "units"         # "pct" (deviation from 50%) or "units"
push_points     <- 0.5           # used only if computing weekly points from pick columns
save_plot       <- TRUE
plot_file       <- if (metric == "pct") file.path(base_dir, "plots", "circa_tornado_pct.png") else file.path(base_dir, "plots", "circa_tornado_units.png")

# New: scatter & tables parameters
scatter_player <- NULL   # e.g., "Alex Izsak" to filter; NULL for all players
save_scatter   <- TRUE
scatter_file   <- file.path(
  base_dir, "plots",
  if (!is.null(scatter_player) && nzchar(scatter_player))
    glue("nfl_team_scatter_{gsub('\\u0020','_',tolower(scatter_player))}.png")
  else
    "nfl_team_scatter.png"
)
save_tables    <- FALSE

tables_prefix  <- file.path(base_dir, "circa_team")

# Helper: normalize NFL abbreviations coming from different sources
normalize_nfl_abbr <- function(x) {
  dplyr::recode(
    x,
    "JAC" = "JAX",
    "WSH" = "WAS",
    "LA"  = "LAR",
    "STL" = "LAR",
    "SD"  = "LAC",
    "OAK" = "LV",
    .default = x
  )
}

# Ensure output directories exist under base_dir
if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_dir, "plots"), recursive = TRUE, showWarnings = FALSE)

message("Reading: ", normalizePath(path, mustWork = FALSE))

# ---- Read & normalize ----
raw <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal") %>%
  janitor::clean_names()

# Guess key columns (robust to slight name differences)
week_col <- names(raw)[str_detect(names(raw), "^week$|week", negate = FALSE)]
if (length(week_col) == 0) stop("Couldn't find a 'week' column.")
week_col <- week_col[1]

name_col <- names(raw)[str_detect(names(raw), "^name$|player|entry")]
if (length(name_col) == 0) stop("Couldn't find a player/name column.")
name_col <- name_col[1]

date_col <- names(raw)[str_detect(names(raw), "date")]
date_col <- date_col[1] %||% NA_character_

# Prefer an explicit weekly total column named `total_points` if present,
# else fall back to a wider search for a weekly total
pts_candidates <- names(raw)[str_detect(
  names(raw),
  "total.*point|week.*point|points_from|total_pts|week_pts|^points$|^pts$",
  negate = FALSE
)]
weekly_pts_col <- if ("total_points" %in% names(raw)) "total_points" else (pts_candidates[1] %||% NA_character_)

df <- raw %>%
  mutate(
    week = .data[[week_col]],
    name = .data[[name_col]],
    pick_date = if (!is.na(date_col)) .data[[date_col]] else NA
  ) %>%
  # Fill down week (handles merged/blank week cells)
  tidyr::fill(week, .direction = "down") %>%
  mutate(
    week = readr::parse_number(as.character(week)),
    name = as.character(name)
  )

# ---- Weekly points ----
if (!is.na(weekly_pts_col) && weekly_pts_col %in% names(df)) {
  # Be robust to numeric or character storage
  df <- df %>%
    mutate(
      weekly_points = if (is.numeric(.data[[weekly_pts_col]])) {
        .data[[weekly_pts_col]]
      } else {
        readr::parse_number(as.character(.data[[weekly_pts_col]]))
      }
    )
  message("Using weekly points column: ", weekly_pts_col)
} else {
  # Fallback: compute from pick columns if they exist (W=1, L=0, P=push_points)
  pick_cols <- names(df)[str_detect(names(df), "pick|result|outcome")]
  if (length(pick_cols) == 0) {
    stop("Couldn't find a weekly points column or pick outcome columns.")
  }
  message("Computing weekly points from pick columns: ", paste(pick_cols, collapse = ", "))
  df <- df %>%
    mutate(
      weekly_points = rowSums(
        across(all_of(pick_cols), ~ {
          v <- as.character(.x)
          case_when(
            str_to_upper(v) %in% c("W", "WIN", "1")     ~ 1,
            str_to_upper(v) %in% c("P", "PUSH", "0.5")  ~ push_points,
            str_to_upper(v) %in% c("L", "LOSS", "0")    ~ 0,
            TRUE ~ suppressWarnings(as.numeric(v))      # numeric fallback
          )
        }),
        na.rm = TRUE
      )
    )
}

# ---- Coerce date if available
df <- df %>%
  mutate(
    pick_date = suppressWarnings(as.Date(pick_date))
  )

# ---- Derived metrics ----
# Weekly units: wins - losses = (2*wins - picks_per_week)
df <- df %>%
  mutate(units_week = 2 * weekly_points - picks_per_week)

# Cumulative by player (ordered by date/week if available)
df_ordered <- df %>%
  arrange(name, dplyr::coalesce(pick_date, as.Date("1900-01-01")), week) %>%
  group_by(name) %>%
  mutate(
    cum_points = cumsum(replace_na(weekly_points, 0)),
    cum_units  = cumsum(replace_na(units_week, 0))
  ) %>%
  ungroup()

# Season totals by player
summary_tbl <- df %>%
  group_by(name) %>%
  summarise(
    total_pts   = sum(weekly_points, na.rm = TRUE),
    total_weeks = sum(!is.na(weekly_points)),
    correct_pct = total_pts / (total_weeks * picks_per_week),
    total_units = sum(units_week, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(correct_pct))

# ---- Tornado plot ----
if (metric == "pct") {
  plot_df <- summary_tbl %>%
    mutate(
      dev_from_50 = correct_pct - 0.50
    )
  units_range <- max(abs(plot_df$total_units), na.rm = TRUE)
  avg_dev <- mean(plot_df$dev_from_50, na.rm = TRUE)

  p <- ggplot(plot_df, aes(x = dev_from_50, y = reorder(name, dev_from_50), fill = total_units)) +
    geom_col(alpha = 0.95) +
    geom_vline(xintercept = avg_dev, linetype = "dotted", linewidth = 0.9, color = "grey30") +
    annotate(
      "text",
      x = avg_dev, y = Inf,
      label = glue("Avg: {scales::percent(avg_dev, accuracy = 0.1)}"),
      vjust = -0.5, hjust = -0.1, color = "grey30"
    ) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_fill_gradient2(
    low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
    midpoint = 0, name = "Units", limits = c(-units_range, units_range)
    ) +
    labs(
      title = "Correct % vs 50% baseline",
      subtitle = "Deviation from 50% (tornado)",
      x = "Deviation (percentage points)",
      y = NULL,
      caption = "Dotted line = average across players"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      panel.grid   = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5.5, 30, 5.5, 5.5)
    )
} else {
  plot_df <- summary_tbl
  units_range <- max(abs(plot_df$total_units), na.rm = TRUE)

  avg_units <- mean(plot_df$total_units, na.rm = TRUE)

  # Signed labels (+/-) and positions near the zero baseline
  plot_df <- plot_df %>%
    mutate(
      units_label = ifelse(total_units >= 0, paste0("+", round(total_units, 1)), as.character(round(total_units, 1))),
      label_x = ifelse(total_units >= 0,  units_range * 0.04, -units_range * 0.04)
    )

  p <- ggplot(plot_df, aes(x = total_units, y = reorder(name, total_units), fill = total_units)) +
    geom_col(alpha = 0.95) +
    geom_vline(xintercept = avg_units, linetype = "dotted", linewidth = 0.9, color = "grey30") +
    {
      avg_units_lbl <- ifelse(avg_units >= 0, paste0("+", round(avg_units, 1)), as.character(round(avg_units, 1)))
      annotate(
        "text",
        x = avg_units, y = Inf,
        label = glue("Avg: {avg_units_lbl} units"),
        vjust = -0.5, hjust = -0.1, color = "grey30"
      )
    } +
    # Add signed unit labels near the bar base (just off zero)
    geom_text(
      data = plot_df,
      aes(x = label_x, y = reorder(name, total_units), label = units_label),
      inherit.aes = FALSE,
      size = 3, color = "grey95", fontface = "bold", vjust = 0.5
    ) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limits = c(-units_range * 1, units_range * 1)) +
    scale_fill_gradient2(
      low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
      midpoint = 0, name = "Units", limits = c(-units_range, units_range)
    ) +
    labs(
      title = "Total Units by Player",
      subtitle = glue("{picks_per_week} picks/week; Units"),
      x = "Units",
      y = NULL,
      caption = "Dotted line = average across players"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      panel.grid   = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5.5, 30, 5.5, 5.5)
    )
}

# ---- Save & print ----
if (save_plot) {
  ggsave(plot_file, p, width = 9, height = 6, dpi = 300, bg = "white")
  message("Saved plot to: ", normalizePath(plot_file, mustWork = FALSE))
}

print(p)
cat("\n=== SUMMARY (top 10 by correct % ) ===\n")
print(head(summary_tbl, 10))

ats_path <- file.path(base_dir, "nfl_picks.csv")
ats_records <- NULL
message("Looking for ATS CSV at: ", normalizePath(ats_path, mustWork = FALSE))
if (file.exists(ats_path)) {
  ats_raw <- readr::read_csv(ats_path, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  # Calculate ATS record for each team
  ats_records <- ats_raw %>%
    filter(!is.na(spread_winner)) %>%
    tidyr::pivot_longer(cols = c(away, home), names_to = "location", values_to = "team") %>%
    mutate(
      ats_win = (location == "away" & spread_winner == team) |
                (location == "home" & spread_winner == team)
    ) %>%
    group_by(team) %>%
    summarise(
      ats_wins = sum(ats_win),
      ats_losses = n() - sum(ats_win),
      ats_games = n(),
      ats_pct = ats_wins / ats_games,
      .groups = "drop"
    ) %>%
    rename(team_code = team)
  
  message("Loaded ATS records from: ", ats_path)
} else {
  message("ATS records file not found: ", ats_path)
}

# ---- Team pick tables & scatter (NFL) ----
# Build long per-pick data by pairing the 'all_picks' string with pick1..pickN results.
picks_cols <- intersect(paste0("pick", 1:picks_per_week), names(df))
if (length(picks_cols) > 0 && "all_picks" %in% names(df)) {
  # Parse all_picks and align with pick columns
  df_teams <- df %>%
    mutate(
      id = row_number(),
      teams_list = stringr::str_split(all_picks, "-"),
      teams_list = purrr::map(teams_list, ~ stringr::str_to_upper(stringr::str_trim(.x)))
    )
  
  # Create long format for teams
  teams_long <- df_teams %>%
    tidyr::unnest_longer(teams_list, indices_to = "pick_idx") %>%
    dplyr::rename(team_code = teams_list) %>%
    dplyr::filter(!is.na(team_code) & nzchar(team_code))

  # Create long format for results
  df_results <- df %>%
    mutate(id = row_number()) %>%
    tidyr::pivot_longer(
      cols = all_of(picks_cols),
      names_pattern = "pick(\\d+)",
      names_to = "pick_idx",
      values_to = "res"
    ) %>%
    mutate(pick_idx = as.integer(pick_idx))

  # Join teams with results and compute units
  picks_long <- teams_long %>%
    dplyr::left_join(
      df_results %>% dplyr::select(id, pick_idx, res),
      by = c("id", "pick_idx")
    ) %>%
    dplyr::mutate(
      res = suppressWarnings(as.numeric(res)),
      unit = dplyr::case_when(
        is.na(res)         ~ 0,
        res == 1           ~ 1,
        res == push_points ~ 0,
        TRUE               ~ -1
      )
    ) %>%
    dplyr::filter(!is.na(team_code) & nzchar(team_code)) %>%
    dplyr::select(name, week, team_code, res, unit)

  # Table 1: Count of picks per player per team
  counts_table <- picks_long %>%
    count(name, team_code, name = "picks") %>%
    tidyr::pivot_wider(names_from = team_code, values_from = picks, values_fill = 0) %>%
    arrange(name)

  # Table 2: Units won/lost per player per team
  units_table <- picks_long %>%
    group_by(name, team_code) %>%
    summarise(units = sum(unit, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = team_code, values_from = units, values_fill = 0) %>%
    arrange(name)

  # Save tables if requested
  if (isTRUE(save_tables)) {
    readr::write_csv(counts_table, paste0(tables_prefix, "_picks_counts.csv"))
    readr::write_csv(units_table,  paste0(tables_prefix, "_picks_units.csv"))
    message("Saved team pick tables: ",
            paste0(tables_prefix, "_picks_counts.csv"), " & ",
            paste0(tables_prefix, "_picks_units.csv"))
  }

  # Aggregate for scatter plot
  scatter_df <- picks_long
  if (!is.null(scatter_player) && nzchar(scatter_player)) {
    scatter_df <- scatter_df %>% filter(name == scatter_player)
  }

  team_agg <- scatter_df %>%
    group_by(team_code) %>%
    summarise(
      picks = n(),
      units = sum(unit, na.rm = TRUE),
      .groups = "drop"
    )

  # Try to add NFL logos if nflreadr is available
  has_logos <- FALSE
  if (requireNamespace("nflreadr", quietly = TRUE)) {
    nfl_raw <- nflreadr::load_teams()
    if ("team_logo_espn" %in% names(nfl_raw)) {
      nfl_lookup <- nfl_raw %>%
        dplyr::mutate(team_logo_final = dplyr::coalesce(.data$team_logo_wikipedia, .data$team_logo_espn)) %>%
        dplyr::transmute(team_code = normalize_nfl_abbr(.data$team_abbr),
                         team_logo = .data$team_logo_final) %>%
        dplyr::distinct()
    } else {
      nfl_lookup <- nfl_raw %>%
        dplyr::transmute(team_code = normalize_nfl_abbr(.data$team_abbr),
                         team_logo = .data$team_logo_wikipedia) %>%
        dplyr::distinct()
    }
    # Normalize codes before joining
    team_agg <- team_agg %>% dplyr::mutate(team_code = normalize_nfl_abbr(team_code)) %>%
      dplyr::left_join(nfl_lookup, by = "team_code")
    has_logos <- "team_logo" %in% names(team_agg) && any(!is.na(team_agg$team_logo))
    

    # Add ATS records if available (normalize first).
    # NOTE: do NOT join here; we’ll join once later when computing OE.
    if (!is.null(ats_records)) {
    ats_records <- ats_records %>%
        dplyr::mutate(team_code = normalize_nfl_abbr(team_code))
    }
    # Report any missing logos so the blank one is easy to identify
    missing_logos <- team_agg %>% dplyr::filter(is.na(team_logo) | !nzchar(team_logo)) %>% dplyr::distinct(team_code) %>% dplyr::arrange(team_code)
    if (nrow(missing_logos) > 0) {
      message("Missing NFL logos for: ", paste(missing_logos$team_code, collapse = ", "))
    }
  }

  # ---- Units over Expectation (Team tornado) ----
  p_team_oe <- NULL
  p_team_units <- NULL
  p_team_panels <- NULL
  if (!is.null(ats_records)) {
    # Join ATS once here (avoid prior joins that create suffixes)
    team_oe_df <- team_agg %>%
      dplyr::left_join(
        ats_records %>% dplyr::mutate(team_code = normalize_nfl_abbr(team_code)),
        by = "team_code"
      ) %>%
      dplyr::mutate(
        #net_record     = .data$ats_wins - .data$ats_losses,          # e.g., 5-2 -> +3
        units_expected = (.data$picks * .data$ats_wins/.data$ats_games) - (.data$picks*.data$ats_losses/.data$ats_games),  # expectation = times bet * net ATS
        units_oe       = .data$units - .data$units_expected,
        units_label    = dplyr::if_else(.data$units_oe >= 0,
                                        paste0("+", round(.data$units_oe, 1)),
                                        as.character(round(.data$units_oe, 1)))
      )

    # ----- Subplot A: Units tornado -----
    units_range2 <- max(abs(team_agg$units), na.rm = TRUE)
    team_units_df <- team_agg %>%
      dplyr::mutate(
        units_label = dplyr::if_else(.data$units >= 0,
                                     paste0("+", round(.data$units, 1)),
                                     as.character(round(.data$units, 1))),
        logo_x    = dplyr::if_else(.data$units >= 0, -units_range2 * 0.08, units_range2 * 0.08),
        label_x   = dplyr::if_else(.data$units >= 0,  units_range2 * 0.05, -units_range2 * 0.05),
        y_key     = reorder(.data$team_code, .data$units),
        label_col = dplyr::if_else(abs(.data$units) < 4, "black", "grey90")
      )

    p_team_units <- ggplot(team_units_df, aes(x = units, y = y_key, fill = units)) +
      geom_col(alpha = 0.95) +
      {
        if (has_logos && requireNamespace("ggimage", quietly = TRUE)) {
          ggimage::geom_image(
            data = team_units_df %>% dplyr::filter(!is.na(team_logo) & nzchar(team_logo)),
            aes(x = logo_x, y = y_key, image = team_logo),
            inherit.aes = FALSE,
            size = 0.03
          )
        }
      } +
      geom_text(
        data = team_units_df,
        aes(
          x = label_x,
          y = y_key,
          label = units_label,
          hjust = ifelse(units >= 0, 0, 1),
          colour = label_col
        ),
        inherit.aes = FALSE,
        size = 3,
        fontface = "bold"
      ) +
      scale_colour_identity(guide = "none") +
      geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(limits = c(-units_range2 * 1.15, units_range2 * 1.15)) +
      scale_fill_gradient2(
        low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
        midpoint = 0, name = "+/- Units", limits = c(-units_range2, units_range2)
      ) +
      labs(
        title = "Units by NFL Team",
        subtitle = if (!is.null(scatter_player) && nzchar(scatter_player)) glue("Based on {scatter_player}'s picks") else "Based on all players' picks",
        x = "+/- Units",
        y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),   # team logos act as y labels
        plot.margin = margin(5.5, 30, 5.5, 5.5)
      )

    # ----- Subplot B: Units over expectation tornado -----
    units_oe_range <- max(abs(team_oe_df$units_oe), na.rm = TRUE)
    team_oe_df <- team_oe_df %>%
      dplyr::mutate(
        # place logo just off zero; labels slightly further out
        logo_x  = dplyr::if_else(units_oe >= 0,  -units_oe_range * 0.08, units_oe_range * 0.08),
        label_x = dplyr::if_else(units_oe >= 0,  units_oe_range * 0.08, -units_oe_range * 0.08),
        y_key   = reorder(team_code, units_oe),
        label_col = dplyr::if_else(abs(.data$units) < 4, "black", "grey90")
      )

    p_team_oe <- ggplot(team_oe_df, aes(x = units_oe, y = y_key, fill = units_oe)) +
      geom_col(alpha = 0.95) +
      {
        if (has_logos && requireNamespace("ggimage", quietly = TRUE)) {
          ggimage::geom_image(
            data = team_oe_df %>% dplyr::filter(!is.na(team_logo) & nzchar(team_logo)),
            aes(x = logo_x, y = y_key, image = team_logo),
            inherit.aes = FALSE,
            size = 0.03
          )
        }
      } +
      geom_text(
        data = team_oe_df,
        aes(
          x = label_x,
          y = y_key,
          label = units_label,
          hjust = ifelse(units_oe >= 0, 0, 1)
        ),
        inherit.aes = FALSE,
        size = 3,
        color = "grey90",
        fontface = "bold",
        vjust = 0.5
      ) +
      geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(limits = c(-units_oe_range * 1.15, units_oe_range * 1.15)) +
      scale_fill_gradient2(
        low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
        midpoint = 0, name = "Units over exp.", limits = c(-units_oe_range, units_oe_range)
      ) +
      labs(
        title = "Units over Expectation by NFL Team",
        subtitle = if (!is.null(scatter_player) && nzchar(scatter_player)) glue("Based on {scatter_player}'s picks") else "Based on all players' picks",
        x = "Units over expectation",
        y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),   # team logos act as y labels
        plot.margin = margin(5.5, 30, 5.5, 5.5)
      )

    # ---- Combine and save the two panels ----
    team_oe_file <- file.path(
      base_dir, "plots",
      if (!is.null(scatter_player) && nzchar(scatter_player))
        glue("team_units_and_over_exp_{gsub('\\u0020','_',tolower(scatter_player))}.png")
      else
        "team_units_and_over_exp_all.png"
    )

    if (requireNamespace("patchwork", quietly = TRUE)) {
      p_team_panels <- p_team_units + p_team_oe + patchwork::plot_layout(widths = c(1, 1))
      ggsave(team_oe_file, p_team_panels, width = 12, height = 7, dpi = 300, bg = "white")
      message("Saved team Units (left) + Units over Expectation (right) to: ", normalizePath(team_oe_file, mustWork = FALSE))
    } else {
      # Save separately if patchwork not available
      ggsave(gsub("\\.png$", "_units.png", team_oe_file), p_team_units, width = 8, height = 7, dpi = 300, bg = "white")
      ggsave(gsub("\\.png$", "_over_exp.png", team_oe_file), p_team_oe, width = 8, height = 7, dpi = 300, bg = "white")
      message("Saved separate team Units & Units over Expectation plots (install 'patchwork' to combine).")
    }
  }

  # Add jitter to prevent overlapping points
  set.seed(42)
  team_agg <- team_agg %>%
    mutate(
      # Tighten jitter and clamp to >= 0 so we can start x-axis at 0 cleanly
      picks_jitter = pmax(picks + runif(n(), -0.15, 0.15), 0),
      units_jitter = units + runif(n(), -0.25, 0.25)
    )
  x_max <- max(team_agg$picks_jitter, na.rm = TRUE)
  # Dynamic x-axis tick step: for all players use 10, for a single player use 1
  break_step <- if (is.null(scatter_player) || !nzchar(scatter_player)) 10 else 1

  # --- Helper calculations for scatter plot annotations ---
  y_min <- min(team_agg$units_jitter, na.rm = TRUE)
  y_max <- max(team_agg$units_jitter, na.rm = TRUE)
  y_span <- y_max - y_min
  # positions for diagonal-line annotations (stay inside panel)
  diag_x  <- min(x_max * 0.85, max(0, y_max) * 0.85)
  diag2_x <- min(x_max * 0.85, max(0, -y_min) * 0.85)

  # Build scatter plot with jittered positions
  p_scatter <- ggplot(team_agg, aes(x = picks_jitter, y = units_jitter)) +
    {
      if (has_logos && requireNamespace("ggimage", quietly = TRUE)) {
        ggimage::geom_image(aes(image = team_logo), size = 0.07, alpha = 0.95)
      } else {
        list(
          geom_point(size = 5, alpha = 0.8, color = "#1E88E5"),
          ggrepel::geom_text_repel(
            aes(label = team_code), 
            size = 2, 
            max.overlaps = Inf,
            box.padding = 0.5,
            point.padding = 0.3
          )
        )
      }
    } +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", alpha = 0.7) +
    geom_abline(slope = 1,  intercept = 0, linetype = "dashed", color = "grey60", alpha = 0.6) +
    geom_abline(slope = -1, intercept = 0, linetype = "dashed", color = "grey60", alpha = 0.6) +
    # axis-direction annotations
    annotate("text",
             x = x_max * 0.98,
             y = 1,
             label = "more bets ->",
             color = "grey35", size = 3.5, hjust = 1) +
    annotate("text",
             x = 0 + x_max * 0.02,
             y = y_max - 0.04 * y_span,
             label = "more units ->",
             color = "grey35", size = 3.5, angle = 90, vjust = 1) +
    # diagonal-line annotations
    annotate("text",
             x = diag_x, y = diag_x+1,
             label = "100% win",
             color = "grey35", size = 3.2, angle = 30, fontface = "italic") +
    annotate("text",
             x = diag2_x, y = -diag2_x-1,
             label = "0% win",
             color = "grey35", size = 3.2, angle = -30, fontface = "italic") +
    scale_x_continuous(
        limits = c(0, x_max * 1.05),
        expand = expansion(mult = c(0, 0.02)),
        breaks = function(x) {
            hi <- max(x, na.rm = TRUE)
            seq(0, ceiling(hi), by = break_step)
        }
    ) + 
    labs(
      title = if (!is.null(scatter_player) && nzchar(scatter_player))
        glue("NFL Teams Bet by {scatter_player}") else "NFL Teams Bet (All Players)",
      subtitle = "Team performance across all picks",
      x = "Number of times bet",
      y = "Units won / lost"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40")
    )

  # Create ATS record plot if data available
  p_ats <- NULL
  if (!is.null(ats_records)) {
    # Merge full ATS data with logos and create positioning
    ats_plot_data <- ats_records %>%
      left_join(nfl_lookup, by = "team_code") %>%
      mutate(record_label = glue("{ats_wins}-{ats_losses}")) %>%
      group_by(ats_wins) %>%
      mutate(
        n_at_wins = n(),
        x_pos = (row_number() - 1) * 0.08
      ) %>%
      ungroup() %>%
      arrange(desc(ats_wins), x_pos)
    # Diagnostic: report any teams missing logos for ATS plot
    missing_logos_ats <- ats_plot_data %>% dplyr::filter(is.na(team_logo) | !nzchar(team_logo)) %>% dplyr::distinct(team_code)
    if (nrow(missing_logos_ats) > 0) {
      message("ATS plot is missing logos for: ", paste(missing_logos_ats$team_code, collapse = ", "))
    }
    message("ATS rows available for plotting: ", nrow(ats_plot_data))
    max_games <- max(ats_plot_data$ats_games, na.rm = TRUE)
    
    # Create custom y-axis labels showing record format
    y_labels <- ats_plot_data %>%
      group_by(ats_wins) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(desc(ats_wins)) %>%
      pull(record_label)
    
    y_breaks <- sort(unique(ats_plot_data$ats_wins), decreasing = TRUE)
    
    p_ats <- ggplot(ats_plot_data, aes(x = x_pos, y = ats_wins)) +
      {
        if (has_logos && requireNamespace("ggimage", quietly = TRUE)) {
          ggimage::geom_image(aes(image = team_logo), size = 0.070, alpha = 0.95)
        } else {
          list(
            geom_point(aes(color = ats_pct), size = 6, alpha = 0.85),
            ggrepel::geom_text_repel(
              aes(label = team_code),
              size = 2,
              max.overlaps = Inf,
              box.padding = 0.05
            )
          )
        }
      } +
      scale_y_continuous(
        breaks = y_breaks,
        labels = y_labels,
        limits = c(min(y_breaks, na.rm = TRUE), max(y_breaks, na.rm = TRUE))
      ) +
      # add fixed padding so end logos don't get clipped
      scale_x_continuous(expand = expansion(mult = c(0, 0), add = c(0.12, 0.12))) +
      scale_color_gradient2(
        low = "#C62828", mid = "#757575", high = "#2E7D32",
        midpoint = 0.5, guide = "none"
      ) +
      labs(
        title = "NFL Teams: Against the Spread (ATS) Records",
        subtitle = glue("Through Week {max(df$week, na.rm = TRUE)}"),
        x = NULL,
        y = "ATS Record"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey40"),
        plot.margin = margin(10, 10, 15, 10)
      )
  }

  # Save plots
  if (isTRUE(save_scatter)) {
    # Combine plots if ATS plot exists
    if (!is.null(p_ats)) {
      if (requireNamespace("patchwork", quietly = TRUE)) {
        p_combined <- p_ats + p_scatter +
          patchwork::plot_layout(widths = c(1, 2))
        
        ggsave(scatter_file, p_combined, width = 16, height = 6, dpi = 300, bg = "white")
        message("Saved combined plot to: ", normalizePath(scatter_file, mustWork = FALSE))
      } else {
        # Fallback: save separately if patchwork not available
        ggsave(scatter_file, p_scatter, width = 10, height = 7, dpi = 300, bg = "white")
        message("Saved scatter to: ", normalizePath(scatter_file, mustWork = FALSE))
        
        ats_file <- gsub("\\.png$", "_ats.png", scatter_file)
        ggsave(ats_file, p_ats, width = 8, height = 6, dpi = 300, bg = "white")
        message("Saved ATS plot to: ", normalizePath(ats_file, mustWork = FALSE))
        message("Note: Install 'patchwork' package to combine plots into one figure")
      }
    } else {
      ggsave(scatter_file, p_scatter, width = 10, height = 7, dpi = 300, bg = "white")
      message("Saved scatter to: ", normalizePath(scatter_file, mustWork = FALSE))
    }
  }

  # Print combined plot or individual plots
  if (!is.null(p_ats)) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      print(p_ats + p_scatter + patchwork::plot_layout(widths = c(1, 2)))
    } else {
      print(p_ats)
      print(p_scatter)
    }
  } else {
    print(p_scatter)
  }

  if (!is.null(p_team_oe)) {
    if (exists("p_team_panels") && !is.null(p_team_panels)) {
      print(p_team_panels)
    } else {
      print(p_team_units)
      print(p_team_oe)
    }
  }
  
  # ---- Teams bet AGAINST: dataset + scatter ----
  # Build a dataset of "teams bet against" by mapping each pick to that week's opponent,
  # then aggregate and plot a logos scatter just like the "bet on" version.
  if (exists("ats_raw")) {
    # Ensure week is numeric and non-NA in picks for the join
    picks_long <- picks_long %>%
      dplyr::mutate(week = as.integer(readr::parse_number(as.character(week)))) %>%
      dplyr::filter(!is.na(week))
    # Try to discover the week column in the ATS data
    week_col_ats <- names(ats_raw)[stringr::str_detect(names(ats_raw), "^week$|week")]
    week_col_ats <- week_col_ats[1] %||% NA_character_

    if (is.na(week_col_ats)) {
      message("Can't build 'teams bet against' dataset: no 'week' column in ats_raw.")
    } else {
      # Create (team, opponent) map for each week
      sched_map <- ats_raw %>%
        dplyr::mutate(
          week = as.integer(readr::parse_number(as.character(.data[[week_col_ats]]))),
          away = normalize_nfl_abbr(stringr::str_to_upper(.data$away)),
          home = normalize_nfl_abbr(stringr::str_to_upper(.data$home))
        ) %>%
        dplyr::transmute(week, team_code = away, opponent = home) %>%
        dplyr::bind_rows(
          ats_raw %>%
            dplyr::mutate(
              week = as.integer(readr::parse_number(as.character(.data[[week_col_ats]]))),
              away = normalize_nfl_abbr(stringr::str_to_upper(.data$away)),
              home = normalize_nfl_abbr(stringr::str_to_upper(.data$home))
            ) %>%
            dplyr::transmute(week, team_code = home, opponent = away)
        ) %>%
        dplyr::filter(!is.na(team_code), nzchar(team_code))

      # Diagnostic: how many picks fail to match a schedule row?
      unmatched <- picks_long %>% dplyr::anti_join(sched_map, by = c("week", "team_code"))
      if (nrow(unmatched) > 0) {
        message("Unmatched picks when mapping to opponents: ", nrow(unmatched))
      }

      # Map each pick to that week's opponent and flip unit sign
      picks_against_long <- picks_long %>%
        dplyr::left_join(sched_map, by = c("week", "team_code")) %>%
        dplyr::rename(team_code_against = opponent) %>%
        dplyr::filter(!is.na(team_code_against) & nzchar(team_code_against)) %>%
        dplyr::mutate(
          team_code_against = normalize_nfl_abbr(team_code_against),
          # For teams bet AGAINST, keep the original unit sign: a winning pick against them is +1
          unit_against = unit
        ) %>%
        dplyr::select(name, week, team_code_against, res, unit_against)

      # Aggregate for scatter plot
      scatter_df_against <- picks_against_long
      if (!is.null(scatter_player) && nzchar(scatter_player)) {
        scatter_df_against <- scatter_df_against %>% dplyr::filter(name == scatter_player)
      }

      team_agg_against <- scatter_df_against %>%
        dplyr::group_by(team_code_against) %>%
        dplyr::summarise(
          picks = dplyr::n(),
          units = sum(unit_against, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::rename(team_code = team_code_against)

      # Attach logos (reuse nfl_lookup if already created)
      has_logos_against <- FALSE
      if (requireNamespace("nflreadr", quietly = TRUE)) {
        if (!exists("nfl_lookup")) {
          nfl_raw2 <- nflreadr::load_teams()
          if ("team_logo_espn" %in% names(nfl_raw2)) {
            nfl_lookup <- nfl_raw2 %>%
              dplyr::mutate(team_logo_final = dplyr::coalesce(.data$team_logo_wikipedia, .data$team_logo_espn)) %>%
              dplyr::transmute(team_code = normalize_nfl_abbr(.data$team_abbr),
                               team_logo = .data$team_logo_final) %>%
              dplyr::distinct()
          } else {
            nfl_lookup <- nfl_raw2 %>%
              dplyr::transmute(team_code = normalize_nfl_abbr(.data$team_abbr),
                               team_logo = .data$team_logo_wikipedia) %>%
              dplyr::distinct()
          }
        }
        team_agg_against <- team_agg_against %>% dplyr::left_join(nfl_lookup, by = "team_code")
        has_logos_against <- "team_logo" %in% names(team_agg_against) && any(!is.na(team_agg_against$team_logo))

        # Report missing logos (if any)
        missing_logos_against <- team_agg_against %>%
          dplyr::filter(is.na(team_logo) | !nzchar(team_logo)) %>%
          dplyr::distinct(team_code) %>%
          dplyr::arrange(team_code)
        if (nrow(missing_logos_against) > 0) {
          message("Against-scatter is missing logos for: ", paste(missing_logos_against$team_code, collapse = ", "))
        }
      }

      if (nrow(team_agg_against) > 0) {
        # Build scatter plot (same style as 'bet on' version)
        set.seed(42)
        team_agg_against <- team_agg_against %>%
          dplyr::mutate(
            picks_jitter = pmax(picks + runif(dplyr::n(), -0.15, 0.15), 0),
            units_jitter = units + runif(dplyr::n(), -0.25, 0.25)
          )
        x_max_a <- max(team_agg_against$picks_jitter, na.rm = TRUE)
        y_min_a <- min(team_agg_against$units_jitter, na.rm = TRUE)
        y_max_a <- max(team_agg_against$units_jitter, na.rm = TRUE)
        y_span_a <- y_max_a - y_min_a
        break_step_a <- if (is.null(scatter_player) || !nzchar(scatter_player)) 10 else 1

        diag_x_a  <- min(x_max_a * 0.85, max(0, y_max_a) * 0.85)
        diag2_x_a <- min(x_max_a * 0.85, max(0, -y_min_a) * 0.85)

        p_scatter_against <- ggplot(team_agg_against, aes(x = picks_jitter, y = units_jitter)) +
          {
            if (has_logos_against && requireNamespace("ggimage", quietly = TRUE)) {
              ggimage::geom_image(aes(image = team_logo), size = 0.07, alpha = 0.95)
            } else {
              list(
                geom_point(size = 5, alpha = 0.8, color = "#8E24AA"),
                ggrepel::geom_text_repel(
                  aes(label = team_code),
                  size = 2,
                  max.overlaps = Inf,
                  box.padding = 0.5,
                  point.padding = 0.3
                )
              )
            }
          } +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", alpha = 0.7) +
          geom_abline(slope = 1,  intercept = 0, linetype = "dashed", color = "grey60", alpha = 0.6) +
          geom_abline(slope = -1, intercept = 0, linetype = "dashed", color = "grey60", alpha = 0.6) +
          annotate("text",
                   x = x_max_a * 0.98,
                   y = 1,
                   label = "more bets ->",
                   color = "grey35", size = 3.5, hjust = 1) +
          annotate("text",
                   x = 0 + x_max_a * 0.02,
                   y = y_max_a - 0.04 * y_span_a,
                   label = "more units ->",
                   color = "grey35", size = 3.5, angle = 90, vjust = 1) +
          annotate("text",
                   x = diag_x_a, y = diag_x_a + 1,
                   label = "100% win",
                   color = "grey35", size = 3.2, angle = 30, fontface = "italic") +
          annotate("text",
                   x = diag2_x_a, y = -diag2_x_a - 1,
                   label = "0% win",
                   color = "grey35", size = 3.2, angle = -30, fontface = "italic") +
          scale_x_continuous(
            limits = c(0, x_max_a * 1.05),
            expand = expansion(mult = c(0, 0.02)),
            breaks = function(x) {
              hi <- max(x, na.rm = TRUE)
              seq(0, ceiling(hi), by = break_step_a)
            }
          ) +
          labs(
            title = if (!is.null(scatter_player) && nzchar(scatter_player))
              glue("NFL Teams Bet AGAINST by {scatter_player}") else "NFL Teams Bet AGAINST (All Players)",
            subtitle = "Units from picks mapped to the opponent (i.e., teams bet against)",
            x = "Number of times bet against",
            y = "Units won / lost (vs that team)"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey40")
          )

        # Save 'against' scatter
        scatter_file_against <- file.path(
          base_dir, "plots",
          if (!is.null(scatter_player) && nzchar(scatter_player))
            glue("nfl_team_scatter_against_{gsub('\\u0020','_',tolower(scatter_player))}.png")
          else
            "nfl_team_scatter_against.png"
        )
        if (isTRUE(save_scatter)) {
          ggsave(scatter_file_against, p_scatter_against, width = 10, height = 7, dpi = 300, bg = "white")
          message("Saved 'teams bet AGAINST' scatter to: ", normalizePath(scatter_file_against, mustWork = FALSE))
        }
        print(p_scatter_against)
      } else {
        message("No 'teams bet AGAINST' rows after joining picks to schedule; check week parsing in ATS CSV and contest file.")
      }
    }
  } else {
    message("Skipping 'teams bet against' scatter: need ATS schedule data (ats_raw) available.")
  }
  # ---- Biggest winning/losing bets (matchups across all players) ----
  # We aggregate picks by matchup (e.g., "IND over DEN") and sum net units.
  # Positive total_units => that side of the bet won overall; negative => lost overall.
  if (exists("ats_raw") && exists("picks_long")) {
    # Build schedule map if not already present
    if (!exists("sched_map")) {
      week_col_ats2 <- names(ats_raw)[stringr::str_detect(names(ats_raw), "^week$|week")]
      week_col_ats2 <- week_col_ats2[1] %||% NA_character_
      if (!is.na(week_col_ats2)) {
        sched_map <- ats_raw %>%
          dplyr::mutate(
            week = as.integer(readr::parse_number(as.character(.data[[week_col_ats2]]))),
            away = normalize_nfl_abbr(stringr::str_to_upper(.data$away)),
            home = normalize_nfl_abbr(stringr::str_to_upper(.data$home))
          ) %>%
          dplyr::transmute(week, team_code = away, opponent = home) %>%
          dplyr::bind_rows(
            ats_raw %>%
              dplyr::mutate(
                week = as.integer(readr::parse_number(as.character(.data[[week_col_ats2]]))),
                away = normalize_nfl_abbr(stringr::str_to_upper(.data$away)),
                home = normalize_nfl_abbr(stringr::str_to_upper(.data$home))
              ) %>%
              dplyr::transmute(week, team_code = home, opponent = away)
          ) %>%
          dplyr::filter(!is.na(team_code), nzchar(team_code))
      }
    }

    if (exists("sched_map")) {
      picks_with_opp <- picks_long %>%
        dplyr::left_join(sched_map, by = c("week", "team_code")) %>%
        dplyr::filter(!is.na(opponent) & nzchar(opponent)) %>%
        dplyr::mutate(
          opponent = normalize_nfl_abbr(opponent),
          matchup  = paste0(team_code, " over ", opponent)
        )

      bets_summary <- picks_with_opp %>%
        dplyr::group_by(matchup) %>%
        dplyr::summarise(
          total_units = sum(unit, na.rm = TRUE),
          n_picks     = dplyr::n(),
          .groups     = "drop"
        )

      if (nrow(bets_summary) > 0) {
        units_range_bets <- max(abs(bets_summary$total_units), na.rm = TRUE)

        # Top 3 winning bets (most positive) and bottom 3 losing bets (most negative)
        top_n_bets <- min(3, nrow(bets_summary))
        win_bets_df <- bets_summary %>%
          dplyr::slice_max(total_units, n = top_n_bets, with_ties = FALSE) %>%
          dplyr::mutate(
            units_label = dplyr::if_else(total_units >= 0, paste0("+", round(total_units, 1)), as.character(round(total_units, 1))),
            label_x     = dplyr::if_else(total_units >= 0, units_range_bets * 0.05, -units_range_bets * 0.05),
            y_key       = reorder(matchup, total_units)
          )

        lose_bets_df <- bets_summary %>%
          dplyr::slice_min(total_units, n = top_n_bets, with_ties = FALSE) %>%
          dplyr::mutate(
            units_label = dplyr::if_else(total_units >= 0, paste0("+", round(total_units, 1)), as.character(round(total_units, 1))),
            label_x     = dplyr::if_else(total_units >= 0, units_range_bets * 0.05, -units_range_bets * 0.05),
            y_key       = reorder(matchup, total_units)
          )

        p_big_win_bets <- ggplot(win_bets_df, aes(x = total_units, y = y_key, fill = total_units)) +
          geom_col(alpha = 0.95) +
          geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
          geom_text(
            data = win_bets_df,
            aes(x = label_x, y = y_key, label = units_label, hjust = ifelse(total_units >= 0, 0, 1)),
            inherit.aes = FALSE, size = 3, color = "grey90", fontface = "bold"
          ) +
          coord_cartesian(clip = "off") +
          scale_x_continuous(limits = c(-units_range_bets * 1.1, units_range_bets * 1.1)) +
          scale_fill_gradient2(low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
                               midpoint = 0, limits = c(-units_range_bets, units_range_bets), name = "+/- Units") +
          labs(title = "Biggest Winning Bets", subtitle = "Net units across all entries", x = "+/- Units", y = NULL) +
          theme_minimal(base_size = 11) +
          theme(panel.grid = element_blank())

        p_big_lose_bets <- ggplot(lose_bets_df, aes(x = total_units, y = y_key, fill = total_units)) +
          geom_col(alpha = 0.95) +
          geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
          geom_text(
            data = lose_bets_df,
            aes(x = label_x, y = y_key, label = units_label, hjust = ifelse(total_units >= 0, 0, 1)),
            inherit.aes = FALSE, size = 3, color = "grey90", fontface = "bold"
          ) +
          coord_cartesian(clip = "off") +
          scale_x_continuous(limits = c(-units_range_bets * 1.1, units_range_bets * 1.1)) +
          scale_fill_gradient2(low = "#C62828", mid = "#BDBDBD", high = "#2E7D32",
                               midpoint = 0, limits = c(-units_range_bets, units_range_bets), name = "+/- Units") +
          labs(title = "Biggest Losing Bets", subtitle = "Net units across all entries", x = "+/- Units", y = NULL) +
          theme_minimal(base_size = 11) +
          theme(panel.grid = element_blank())

        big_bets_file <- file.path(base_dir, "plots", "biggest_winning_losing_bets.png")
        if (requireNamespace("patchwork", quietly = TRUE)) {
          p_bets_combo <- p_big_win_bets + p_big_lose_bets + patchwork::plot_layout(ncol = 2, widths = c(1, 1))
          ggsave(big_bets_file, p_bets_combo, width = 12, height = 5, dpi = 300, bg = "white")
          message("Saved Biggest Winning/Losing Bets figure to: ", normalizePath(big_bets_file, mustWork = FALSE))
          print(p_bets_combo)
        } else {
          ggsave(gsub("\\.png$", "_winners.png", big_bets_file), p_big_win_bets, width = 6, height = 5, dpi = 300, bg = "white")
          ggsave(gsub("\\.png$", "_losers.png",  big_bets_file), p_big_lose_bets, width = 6, height = 5, dpi = 300, bg = "white")
          message("Saved biggest winning/losing bets separately (install 'patchwork' to combine).")
          print(p_big_win_bets); print(p_big_lose_bets)
        }
      } else {
        message("No matchup-level bets to summarise — check picks_with_opp join.")
      }
    } else {
      message("Skipping 'biggest bets' section: need ATS schedule and picks_long available.")
    }
  }

  cat("\n=== TEAM PICKS SUMMARY ===\n")
  cat("Counts table dimensions:", nrow(counts_table), "x", ncol(counts_table), "\n")
  cat("Units table dimensions:", nrow(units_table), "x", ncol(units_table), "\n")
  
  # Print top teams by frequency and units
  team_summary <- picks_long %>%
    group_by(team_code) %>%
    summarise(
      picks = n(),
      wins = sum(unit == 1),
      losses = sum(unit == -1),
      pushes = sum(unit == 0),
      units = sum(unit, na.rm = TRUE),
      win_pct = wins / (wins + losses),
      .groups = "drop"
    ) %>%
    arrange(desc(picks))
  
  cat("\nMost frequently bet teams:\n")
  print(head(team_summary, 10))
  
  cat("\nBest performing teams (min 3 picks):\n")
  print(head(team_summary %>% filter(picks >= 3) %>% arrange(desc(units)), 10))

  # Store objects in global environment for interactive use
} else {
  message("Skipping team scatter/tables; could not find both `all_picks` and pick1..pickN columns.")
}
