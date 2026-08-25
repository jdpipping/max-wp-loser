suppressPackageStartupMessages({
  library(dplyr)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath("scripts/manuscript/enrich-espn-wp-data.R")
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

load_local_enrich_schedule <- function(league, seasons) {
  out_dir <- file.path(repo_root, "data", "derived", league)
  season_tables <- lapply(seasons, function(season) {
    summary_file <- file.path(out_dir, sprintf("%s_games.csv", season))
    if (!file.exists(summary_file)) {
      return(NULL)
    }

    readr::read_csv(summary_file, show_col_types = FALSE) |>
      transmute(
        league = league,
        season = as.integer(season),
        game_id = as.character(game_id),
        game_date = as.character(game_date),
        home_team = home_team,
        away_team = away_team,
        home_team_abbr = home_team_abbr,
        away_team_abbr = away_team_abbr,
        home_record = as.character(home_record),
        away_record = as.character(away_record),
        home_score = as.numeric(home_final),
        away_score = as.numeric(away_final)
      )
  })

  bind_rows(Filter(Negate(is.null), season_tables))
}

if (run_nfl) {
  message("Enriching NFL WP data...")
  nfl_schedule <- build_nfl_schedule(seasons)
  write_enriched_outputs_from_wp(
    schedule_df = nfl_schedule,
    out_dir = file.path(repo_root, "data", "derived", "nfl"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers
  )
}

if (run_nba) {
  message("Enriching NBA WP data...")
  nba_schedule <- load_local_enrich_schedule("nba", seasons)
  missing_seasons <- setdiff(seasons, unique(nba_schedule$season))
  if (length(missing_seasons) > 0) {
    nba_schedule <- bind_rows(nba_schedule, build_nba_schedule(missing_seasons))
  }
  write_enriched_outputs_from_wp(
    schedule_df = nba_schedule,
    out_dir = file.path(repo_root, "data", "derived", "nba"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers
  )
}
