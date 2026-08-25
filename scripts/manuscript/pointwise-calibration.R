#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  normalizePath("scripts/manuscript/pointwise-calibration.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "R", "fixed-clock-calibration.R"))
source(file.path(repo_root, "R", "path-io.R"))
args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default = NULL) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0L) default else sub(paste0("^--", name, "="), "", hit[[1]])
}

load_fixed_clock_paths <- function(league) {
  data_dir <- file.path(repo_root, "data", "derived", league)
  files <- list_enriched_path_files(data_dir)
  if (length(files) == 0L) stop("No enriched path files found for ", league, ".")
  columns <- c(
    "season", "game_id", "game_date", "home_team_abbr", "away_team_abbr",
    "home_won", "home_wp", "home_wp_corrected", "starting_wp_home",
    "period_number", "clock_display_value", "sequence_number"
  )
  rbindlist(lapply(files, function(path) {
    read_path_data(path, columns)
  }), use.names = TRUE, fill = TRUE)
}

run_fixed_clock <- function(league) {
  message(toupper(league), ": constructing Yeh-style fixed-clock forecasts.")
  paths <- load_fixed_clock_paths(league)
  forecasts <- build_fixed_clock_forecasts(paths, league)
  binned <- assign_fixed_clock_bins(forecasts, n_bins = 10L)
  cells <- summarize_fixed_clock_cells(binned)
  resolution <- summarize_path_resolution(paths, forecasts, league)

  figure_dir <- file.path(
    repo_root, "results", "figures", "manuscript", "figures", league
  )
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(as_tibble(forecasts), file.path(figure_dir, "fixed_clock_forecasts.csv"))
  write_csv(as_tibble(binned), file.path(figure_dir, "fixed_clock_bins.csv"))
  write_csv(as_tibble(cells), file.path(figure_dir, "fixed_clock_cells_descriptive.csv"))
  write_csv(
    as_tibble(resolution$by_game),
    file.path(figure_dir, "fixed_clock_update_density_by_game.csv")
  )
  write_csv(
    as_tibble(resolution$summary),
    file.path(figure_dir, "fixed_clock_feed_summary.csv")
  )

  expected <- length(fixed_clock_grid())
  complete <- as_tibble(forecasts) |>
    count(method, game_id, name = "n") |>
    summarise(complete_games = sum(n == expected), .by = method)
  message(
    toupper(league), ": fixed-clock complete games: ",
    paste0(complete$method, "=", complete$complete_games, collapse = ", "), "."
  )
  as_tibble(resolution$summary)
}

league_arg <- tolower(parse_arg("league", "both"))
if (!league_arg %in% c("both", "nfl", "nba")) {
  stop("--league must be one of both, nfl, or nba.")
}
leagues <- if (league_arg == "both") c("nfl", "nba") else league_arg
summaries <- bind_rows(lapply(leagues, run_fixed_clock))

table_dir <- file.path(repo_root, "results", "tables", "manuscript")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(summaries, file.path(table_dir, "fixed-clock-feed-summary.csv"))
