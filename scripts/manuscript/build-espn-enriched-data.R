suppressPackageStartupMessages({
  library(dplyr)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath("scripts/manuscript/build-espn-enriched-data.R")
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
skip_wp_arg <- parse_arg("skip-wp", "false")
skip_enrich_arg <- parse_arg("skip-enrich", "false")
output_root_arg <- parse_arg("output-root", file.path("data", "derived"))
reuse_root_arg <- parse_arg("reuse-root", file.path("data", "derived"))

seasons <- as.integer(strsplit(season_arg, ",")[[1]])
max_games <- if (is.null(max_games_arg)) NULL else as.integer(max_games_arg)
overwrite <- tolower(overwrite_arg) %in% c("true", "1", "yes")
workers <- max(1L, as.integer(workers_arg))
skip_wp <- tolower(skip_wp_arg) %in% c("true", "1", "yes")
skip_enrich <- tolower(skip_enrich_arg) %in% c("true", "1", "yes")

resolve_repo_path <- function(path) {
  if (grepl("^/", path)) {
    return(path)
  }
  file.path(repo_root, path)
}

output_root <- resolve_repo_path(output_root_arg)
reuse_root <- resolve_repo_path(reuse_root_arg)

run_nfl <- league_arg %in% c("both", "nfl")
run_nba <- league_arg %in% c("both", "nba")

load_local_enrich_schedule <- function(league, seasons) {
  out_dir <- file.path(output_root, league)
  season_tables <- lapply(seasons, function(season) {
    summary_file <- file.path(out_dir, sprintf("%s_games.csv", season))
    if (!file.exists(summary_file)) {
      return(NULL)
    }

    schedule <- readr::read_csv(summary_file, show_col_types = FALSE) |>
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
    if (identical(league, "nba")) {
      schedule <- schedule |>
        filter(is_nba_franchise_matchup(home_team_abbr, away_team_abbr))
    }
    schedule
  })

  bind_rows(Filter(Negate(is.null), season_tables))
}

load_reuse_paths <- function(league, enriched = FALSE) {
  source_dir <- file.path(reuse_root, league)
  pattern <- if (enriched) {
    "^[0-9]{4}_paths[.]csv$"
  } else {
    "^[0-9]{4}_wp_paths[.]csv$"
  }
  files <- list.files(source_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    return(NULL)
  }
  message(
    "Loading ", if (enriched) "enriched" else "raw",
    " reuse cache from ", source_dir, "."
  )
  core_columns <- c(
    "game_id", "play_id", "sequence_number", "home_wp", "away_wp",
    "home_score", "away_score", "period_number", "clock_display_value",
    "wallclock", "play_text", "short_description", "play_type",
    "game_seconds_remaining", "scoring_play", "yards_gained"
  )
  dplyr::bind_rows(lapply(files, function(path) {
    available <- names(data.table::fread(path, nrows = 0L, showProgress = FALSE))
    data.table::fread(
      path,
      select = intersect(core_columns, available),
      showProgress = FALSE
    ) |>
      as_tibble() |>
      coerce_path_key_types()
  }))
}

nfl_schedule <- NULL
nfl_raw_reuse <- if (run_nfl) load_reuse_paths("nfl", enriched = FALSE) else NULL
nfl_enriched_reuse <- if (run_nfl) load_reuse_paths("nfl", enriched = TRUE) else NULL

if (!skip_wp && run_nfl) {
  message("Building raw NFL ESPN WP data...")
  nfl_schedule <- build_nfl_schedule(seasons)
  write_wp_outputs(
    schedule_df = nfl_schedule,
    build_fun = build_nfl_wp_game,
    out_dir = file.path(output_root, "nfl"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers,
    reuse_paths = nfl_raw_reuse
  )
}

if (!skip_wp && run_nba) {
  message("Building raw NBA ESPN WP data...")
  nba_schedule <- build_nba_schedule(seasons)
  write_wp_outputs(
    schedule_df = nba_schedule,
    build_fun = build_nba_wp_game,
    out_dir = file.path(output_root, "nba"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers
  )
}

if (!skip_enrich && run_nfl) {
  message("Enriching NFL WP data...")
  if (is.null(nfl_schedule)) {
    nfl_schedule <- build_nfl_schedule(seasons)
  }
  write_enriched_outputs_from_wp(
    schedule_df = nfl_schedule,
    out_dir = file.path(output_root, "nfl"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers,
    reuse_paths = nfl_enriched_reuse
  )
}

if (!skip_enrich && run_nba) {
  message("Enriching NBA WP data...")
  nba_schedule <- load_local_enrich_schedule("nba", seasons)
  missing_seasons <- setdiff(seasons, unique(nba_schedule$season))
  if (length(missing_seasons) > 0) {
    nba_schedule <- bind_rows(nba_schedule, build_nba_schedule(missing_seasons))
  }
  write_enriched_outputs_from_wp(
    schedule_df = nba_schedule,
    out_dir = file.path(output_root, "nba"),
    max_games = max_games,
    overwrite = overwrite,
    n_workers = workers
  )
}
