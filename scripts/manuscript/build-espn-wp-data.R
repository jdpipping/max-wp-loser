suppressPackageStartupMessages({
  library(dplyr)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath("scripts/manuscript/build-espn-wp-data.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "scripts", "manuscript", "espn_enrich_utils.R"))

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default = NULL) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[[1]])
}

league_arg <- parse_arg("league", "both")
season_arg <- parse_arg("seasons", "2018,2019,2020,2021,2022,2023,2024")
max_games_arg <- parse_arg("max-games", NULL)
overwrite_arg <- parse_arg("overwrite", "false")
workers_arg <- parse_arg("workers", "1")

seasons <- as.integer(strsplit(season_arg, ",")[[1]])
max_games <- if (is.null(max_games_arg)) NULL else as.integer(max_games_arg)
overwrite <- tolower(overwrite_arg) %in% c("true", "1", "yes")
workers <- max(1L, as.integer(workers_arg))

run_nfl <- league_arg %in% c("both", "nfl")
run_nba <- league_arg %in% c("both", "nba")

if (run_nfl) {
  message("Building raw NFL ESPN WP data...")
  nfl_schedule <- build_nfl_schedule(seasons)
  write_wp_outputs(
    schedule_df = nfl_schedule,
    build_fun = build_nfl_wp_game,
    out_dir = file.path(repo_root, "data", "derived", "nfl"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers
  )
}

if (run_nba) {
  message("Building raw NBA ESPN WP data...")
  nba_schedule <- build_nba_schedule(seasons)
  write_wp_outputs(
    schedule_df = nba_schedule,
    build_fun = build_nba_wp_game,
    out_dir = file.path(repo_root, "data", "derived", "nba"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers
  )
}
