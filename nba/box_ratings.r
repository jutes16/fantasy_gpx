# Team and player efficiency ratings computed from hoopR box scores (ESPN data,
# downloaded from the sportsdataverse GitHub releases), so nothing depends on
# stats.nba.com, which blocks cloud servers such as GitHub Actions.
#
#   source("box_ratings.r")
#   r <- box_ratings(2026)          # 2026 = the 2025-26 season (hoopR numbering)
#   r$teams    team_name, team_abbreviation, games, poss, off_rating, def_rating, net_rating
#   r$players  player_name, team_abbreviation, gp, min, usg_pct, off_rating, def_rating, net_rating
#
# Team ratings: points scored / allowed per 100 possessions, using
# Basketball-Reference's possession estimate. Checked against their 2025-26
# table: net ratings within 0.3 for every team checked.
#
# Player ratings are Dean Oliver's box-score individual ratings (the method
# Basketball-Reference uses): ORtg = points produced per 100 individual
# possessions, DRtg = points allowed per 100 possessions, estimated from stops.
# They are ESTIMATES: the NBA's official player ratings come from on-court
# possession data, which box scores don't have. Usage % is the standard
# box-score formula. All on a 0-100+ scale like the NBA's.

suppressPackageStartupMessages({
  library(dplyr)
})

ESPN_TO_NBA <- c(UTAH = "UTA", NY = "NYK", NO = "NOP", GS = "GSW", SA = "SAS")
NBA_TEAMS <- c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
               "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
               "OKC","ORL","PHI","PHX","POR","SAC","SAS","TOR","UTA","WSH")

.nba_abbr <- function(x) ifelse(x %in% names(ESPN_TO_NBA), ESPN_TO_NBA[x], x)

.filter_games <- function(df, date_from, date_to, last_n_games) {
  if (!is.null(date_from)) df <- filter(df, game_date >= as.Date(date_from))
  if (!is.null(date_to))   df <- filter(df, game_date <= as.Date(date_to))
  if (!is.null(last_n_games)) {
    df <- df %>% group_by(team_id) %>% slice_max(game_date, n = last_n_games, with_ties = FALSE) %>% ungroup()
  }
  df
}

box_ratings <- function(season, season_type = 2, date_from = NULL, date_to = NULL,
                        last_n_games = NULL) {
  tb <- hoopR::load_nba_team_box(season) %>%
    filter(season_type == !!season_type, !is.na(team_score), !is.na(field_goals_attempted))
  # NBA teams only (ESPN marks the All-Star games as regular season), and only
  # games where both teams' rows exist
  tb <- tb %>%
    filter(.nba_abbr(team_abbreviation) %in% NBA_TEAMS,
           .nba_abbr(opponent_team_abbreviation) %in% NBA_TEAMS) %>%
    group_by(game_id) %>% filter(n() == 2) %>% ungroup()
  tb <- .filter_games(tb, date_from, date_to, last_n_games)
  if (nrow(tb) == 0) stop("no regular-season box scores for season ", season, " in that window")

  # per-game team rows joined to the opponent's row
  g <- tb %>%
    transmute(game_id, team_id, opponent_team_id, game_date,
              team_name = trimws(team_display_name), team_abbreviation = .nba_abbr(team_abbreviation),
              pts = team_score, fgm = field_goals_made, fga = field_goals_attempted,
              fg3m = three_point_field_goals_made, ftm = free_throws_made,
              fta = free_throws_attempted, orb = offensive_rebounds,
              drb = defensive_rebounds, ast = assists, stl = steals, blk = blocks,
              # total = player + team turnovers; one 2025-26 game is missing it
              tov = coalesce(total_turnovers, turnovers + coalesce(team_turnovers, 0)),
              pf = fouls)
  g <- g %>% inner_join(
    g %>% select(game_id, team_id, o_pts = pts, o_fgm = fgm, o_fga = fga, o_ftm = ftm,
                 o_fta = fta, o_orb = orb, o_drb = drb, o_tov = tov),
    by = c("game_id", "opponent_team_id" = "team_id"))

  # season totals per team (and the same team's opponents)
  tt <- g %>% group_by(team_id, team_name, team_abbreviation) %>%
    summarise(games = n(), across(c(pts:pf, o_pts:o_tov), sum), .groups = "drop") %>%
    mutate(
      # Basketball-Reference's possession estimate (misses weighted by the
      # chance they're rebounded by the offense), averaged over both teams
      poss_tm = fga + 0.4 * fta - 1.07 * (orb / (orb + o_drb)) * (fga - fgm) + tov,
      poss_op = o_fga + 0.4 * o_fta - 1.07 * (o_orb / (o_orb + drb)) * (o_fga - o_fgm) + o_tov,
      poss = (poss_tm + poss_op) / 2,
      off_rating = 100 * pts / poss,
      def_rating = 100 * o_pts / poss,
      net_rating = off_rating - def_rating,
      mp = 240 * games            # team minutes (5 x 48); OT is a rounding error here
    )

  # ---- players: season totals per player per team (traded players split by team)
  pb <- hoopR::load_nba_player_box(season) %>%
    filter(game_id %in% tb$game_id, !isTRUE(did_not_play), !is.na(minutes), minutes > 0) %>%
    filter(is.na(did_not_play) | !did_not_play)
  p <- pb %>%
    group_by(athlete_id, player_name = athlete_display_name, team_id) %>%
    summarise(gp = n(), min = sum(minutes), pts = sum(points),
              fgm = sum(field_goals_made), fga = sum(field_goals_attempted),
              fg3m = sum(three_point_field_goals_made),
              ftm = sum(free_throws_made), fta = sum(free_throws_attempted),
              orb = sum(offensive_rebounds), drb = sum(defensive_rebounds),
              ast = sum(assists), stl = sum(steals), blk = sum(blocks),
              tov = sum(turnovers), pf = sum(fouls), .groups = "drop") %>%
    inner_join(tt %>% select(team_id, team_abbreviation,
                             T_pts = pts, T_fgm = fgm, T_fga = fga, T_fg3m = fg3m,
                             T_ftm = ftm, T_fta = fta, T_orb = orb, T_drb = drb,
                             T_ast = ast, T_stl = stl, T_blk = blk, T_tov = tov, T_pf = pf,
                             O_pts = o_pts, O_fgm = o_fgm, O_fga = o_fga, O_ftm = o_ftm,
                             O_fta = o_fta, O_orb = o_orb, O_drb = o_drb, O_tov = o_tov,
                             T_mp = mp, T_poss = poss, T_drtg = def_rating),
               by = "team_id")

  sd <- function(a, b) ifelse(b == 0, 0, a / b)          # safe divide

  p <- p %>% mutate(
    # usage: share of team plays used while on the floor
    usg_pct = 100 * sd((fga + 0.44 * fta + tov) * (T_mp / 5),
                       min * (T_fga + 0.44 * T_fta + T_tov)),

    # ---- Oliver offensive rating
    q5   = sd(min, T_mp / 5),
    qAST = q5 * (1.14 * sd(T_ast - ast, T_fgm)) +
           sd((T_ast / T_mp) * min * 5 - ast, (T_fgm / T_mp) * min * 5 - fgm) * (1 - q5),
    fg_part  = fgm * (1 - 0.5 * sd(pts - ftm, 2 * fga) * qAST),
    ast_part = 0.5 * sd((T_pts - T_ftm) - (pts - ftm), 2 * (T_fga - fga)) * ast,
    ft_part  = (1 - (1 - sd(ftm, fta))^2) * 0.4 * fta,
    T_scposs = T_fgm + (1 - (1 - sd(T_ftm, T_fta))^2) * T_fta * 0.4,
    T_orbpct = sd(T_orb, T_orb + O_drb),
    T_play   = sd(T_scposs, T_fga + T_fta * 0.4 + T_tov),
    T_orbw   = sd((1 - T_orbpct) * T_play,
                  (1 - T_orbpct) * T_play + T_orbpct * (1 - T_play)),
    orb_part = orb * T_orbw * T_play,
    sc_poss  = (fg_part + ast_part + ft_part) *
               (1 - sd(T_orb, T_scposs) * T_orbw * T_play) + orb_part,
    fgx_poss = (fga - fgm) * (1 - 1.07 * T_orbpct),
    ftx_poss = (1 - sd(ftm, fta))^2 * 0.4 * fta,
    tot_poss = sc_poss + fgx_poss + ftx_poss + tov,
    pprod_fg  = 2 * (fgm + 0.5 * fg3m) * (1 - 0.5 * sd(pts - ftm, 2 * fga) * qAST),
    pprod_ast = 2 * sd(T_fgm - fgm + 0.5 * (T_fg3m - fg3m), T_fgm - fgm) * 0.5 *
                sd((T_pts - T_ftm) - (pts - ftm), 2 * (T_fga - fga)) * ast,
    pprod_orb = orb * T_orbw * T_play * sd(T_pts, T_scposs),
    pprod     = (pprod_fg + pprod_ast + ftm) *
                (1 - sd(T_orb, T_scposs) * T_orbw * T_play) + pprod_orb,
    off_rating = 100 * sd(pprod, tot_poss),

    # ---- Oliver defensive rating
    dor_pct = sd(O_orb, O_orb + T_drb),
    dfg_pct = sd(O_fgm, O_fga),
    fmwt    = sd(dfg_pct * (1 - dor_pct),
                 dfg_pct * (1 - dor_pct) + (1 - dfg_pct) * dor_pct),
    stops1  = stl + blk * fmwt * (1 - 1.07 * dor_pct) + drb * (1 - fmwt),
    stops2  = (sd(O_fga - O_fgm - T_blk, T_mp) * fmwt * (1 - 1.07 * dor_pct) +
               sd(O_tov - T_stl, T_mp)) * min +
              sd(pf, T_pf) * 0.4 * O_fta * (1 - sd(O_ftm, O_fta))^2,
    stop_pct = sd((stops1 + stops2) * T_mp, T_poss * min),
    d_pts_per_scposs = sd(O_pts, O_fgm + (1 - (1 - sd(O_ftm, O_fta))^2) * O_fta * 0.4),
    def_rating = T_drtg + 0.2 * (100 * d_pts_per_scposs * (1 - stop_pct) - T_drtg),

    net_rating = off_rating - def_rating
  )

  list(
    teams = tt %>% select(team_id, team_name, team_abbreviation, games, poss,
                          off_rating, def_rating, net_rating),
    players = p %>% select(athlete_id, player_name, team_id, team_abbreviation, gp, min,
                           usg_pct, off_rating, def_rating, net_rating)
  )
}
