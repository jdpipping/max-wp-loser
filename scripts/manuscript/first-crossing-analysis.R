#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  script_path <- normalizePath("scripts/manuscript/first-crossing-analysis.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "R", "path-io.R"))

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default = NULL) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[[1]])
}

load_team_states <- function(league) {
  data_dir <- file.path(repo_root, "data", "derived", league)
  path_files <- list_enriched_path_files(data_dir)
  if (length(path_files) == 0) {
    stop("No enriched path files found for league ", league, ".")
  }

  columns <- c(
    "season", "game_id", "game_date", "sequence_number",
    "game_seconds_remaining", "home_wp", "away_wp",
    "home_wp_corrected", "away_wp_corrected", "home_won",
    "home_score", "away_score", "starting_wp_home",
    "home_team_abbr", "away_team_abbr"
  )
  paths <- rbindlist(lapply(path_files, function(path) {
    read_path_data(path, columns)
  }), use.names = TRUE, fill = TRUE)

  paths[, `:=`(
    game_id = as.character(game_id),
    game_date = as.Date(game_date),
    sequence_number = as.numeric(sequence_number),
    game_seconds_remaining = as.numeric(game_seconds_remaining),
    home_wp_used = fcoalesce(
      as.numeric(home_wp_corrected),
      as.numeric(home_wp)
    ),
    away_wp_used = fcoalesce(
      as.numeric(away_wp_corrected),
      as.numeric(away_wp)
    )
  )]

  home <- paths[, .(
    season = as.integer(season),
    game_id,
    game_date,
    home_team_abbr = as.character(home_team_abbr),
    away_team_abbr = as.character(away_team_abbr),
    sequence_number,
    game_seconds_remaining,
    game_minutes_remaining = game_seconds_remaining / 60,
    side = "home",
    team = as.character(home_team_abbr),
    p = home_wp_used,
    y = as.integer(home_won),
    lead = as.numeric(home_score) - as.numeric(away_score),
    start_prob = as.numeric(starting_wp_home)
  )]
  away <- paths[, .(
    season = as.integer(season),
    game_id,
    game_date,
    home_team_abbr = as.character(home_team_abbr),
    away_team_abbr = as.character(away_team_abbr),
    sequence_number,
    game_seconds_remaining,
    game_minutes_remaining = game_seconds_remaining / 60,
    side = "away",
    team = as.character(away_team_abbr),
    p = away_wp_used,
    y = as.integer(!home_won),
    lead = as.numeric(away_score) - as.numeric(home_score),
    start_prob = 1 - as.numeric(starting_wp_home)
  )]

  bind_rows(home, away) |>
    filter(
      is.finite(sequence_number),
      is.finite(game_seconds_remaining),
      is.finite(p),
      !is.na(y),
      is.finite(lead),
      is.finite(start_prob)
    ) |>
    arrange(season, game_id, side, sequence_number)
}

first_crossing_rows <- function(team_states,
                                threshold,
                                preterminal = TRUE,
                                boundary = c("weak", "strict")) {
  boundary <- match.arg(boundary)
  states <- team_states
  if (preterminal) {
    states <- states |>
      filter(game_seconds_remaining > 0)
  }

  eligible <- if (boundary == "weak") {
    states |> filter(p >= threshold)
  } else {
    states |> filter(p > threshold)
  }

  eligible |>
    group_by(season, game_id, side) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(
      threshold = threshold,
      boundary = boundary,
      residual = p - y,
      loss = 1 - y,
      implied_loss = 1 - p,
      favoredness = pmax(start_prob, 1 - start_prob),
      minute_bin = cut(
        game_minutes_remaining,
        breaks = c(-Inf, 1, 2, 5, 10, 24, 48, Inf),
        include.lowest = TRUE,
        right = FALSE
      ),
      favored_bin = cut(
        favoredness,
        breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.000001),
        include.lowest = TRUE,
        right = FALSE
      )
    )
}

summarize_crossings <- function(df, group_var = NULL) {
  grouped <- if (is.null(group_var)) {
    df
  } else {
    df |> group_by(.data[[group_var]])
  }

  grouped |>
    summarise(
      n = n(),
      n_games = n_distinct(game_id),
      mean_p = mean(p),
      win_rate = mean(y),
      loss_rate = mean(loss),
      implied_loss = mean(implied_loss),
      excess_loss = mean(residual),
      loss_ratio = if_else(implied_loss > 0, loss_rate / implied_loss, NA_real_),
      mean_minutes_remaining = mean(game_minutes_remaining),
      .groups = "drop"
    )
}

write_league_outputs <- function(league) {
  team_states <- load_team_states(league)
  out_dir <- file.path(
    repo_root,
    "results", "figures", "manuscript", "figures", league
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  thresholds <- c(0.90, 0.95, 0.99)
  boundaries <- c("weak", "strict")
  preterminal_rows <- bind_rows(lapply(boundaries, function(boundary) {
    bind_rows(lapply(thresholds, function(threshold) {
      first_crossing_rows(
        team_states,
        threshold,
        preterminal = TRUE,
        boundary = boundary
      )
    }))
  }))
  all_rows <- bind_rows(lapply(boundaries, function(boundary) {
    bind_rows(lapply(thresholds, function(threshold) {
      first_crossing_rows(
        team_states,
        threshold,
        preterminal = FALSE,
        boundary = boundary
      )
    }))
  }))
  preterminal_keys <- preterminal_rows |>
    select(season, game_id, side, threshold, boundary) |>
    mutate(preterminal_crossing = TRUE)
  terminal_only_rows <- all_rows |>
    left_join(
      preterminal_keys,
      by = c("season", "game_id", "side", "threshold", "boundary")
    ) |>
    filter(is.na(preterminal_crossing)) |>
    select(-preterminal_crossing)

  overall_tbl <- preterminal_rows |>
    group_by(boundary, threshold) |>
    summarize_crossings()
  all_overall_tbl <- all_rows |>
    group_by(boundary, threshold) |>
    summarize_crossings()
  by_time_095_tbl <- preterminal_rows |>
    filter(boundary == "weak", abs(threshold - 0.95) < 1e-8) |>
    summarize_crossings("minute_bin")
  by_favored_095_tbl <- preterminal_rows |>
    filter(boundary == "weak", abs(threshold - 0.95) < 1e-8) |>
    summarize_crossings("favored_bin")

  write_csv(overall_tbl, file.path(out_dir, "first_crossing_overall.csv"))
  write_csv(all_overall_tbl, file.path(out_dir, "first_crossing_overall_all.csv"))
  write_csv(
    by_time_095_tbl,
    file.path(out_dir, "first_crossing_by_time_095.csv")
  )
  write_csv(
    by_favored_095_tbl,
    file.path(out_dir, "first_crossing_by_favoredness_095.csv")
  )
  write_csv(
    preterminal_rows,
    file.path(out_dir, "first_crossing_rows_preterminal.csv")
  )
  write_csv(all_rows, file.path(out_dir, "first_crossing_rows_all.csv"))
  write_csv(
    terminal_only_rows,
    file.path(out_dir, "first_crossing_terminal_only.csv")
  )

  is_095 <- preterminal_rows$boundary == "weak" &
    abs(preterminal_rows$threshold - 0.95) < 1e-8
  message(sprintf(
    "%s first-crossing analysis: %d preterminal rows at 0.95 across %d games",
    toupper(league),
    sum(is_095),
    n_distinct(preterminal_rows$game_id[is_095])
  ))
}

league_arg <- parse_arg("league", "both")

if (league_arg %in% c("both", "nfl")) {
  write_league_outputs("nfl")
}
if (league_arg %in% c("both", "nba")) {
  write_league_outputs("nba")
}
