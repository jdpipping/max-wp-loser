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
  normalizePath("scripts/manuscript/validate-data-contracts.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "R", "path-io.R"))
args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0L) default else sub(paste0("^--", name, "="), "", hit[[1]])
}

resolve_repo_path <- function(path) {
  if (grepl("^/", path)) path else file.path(repo_root, path)
}

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

data_root <- resolve_repo_path(parse_arg("data-root", file.path("data", "derived")))
check_fixed_clock <- tolower(parse_arg("check-fixed-clock", "true")) %in%
  c("true", "1", "yes")
nfl_root <- file.path(data_root, "nfl")

required_files <- file.path(
  nfl_root,
  c(
    "schedule_inventory.csv", "feed_coverage_by_season.csv",
    "all_games.csv", "all_games_enriched.csv", "missing_wp_games.csv"
  )
)
assert_true(all(file.exists(required_files)), paste(
  "NFL data contract is missing:",
  paste(basename(required_files[!file.exists(required_files)]), collapse = ", ")
))

inventory <- read_csv(file.path(nfl_root, "schedule_inventory.csv"), show_col_types = FALSE)
coverage <- read_csv(file.path(nfl_root, "feed_coverage_by_season.csv"), show_col_types = FALSE)
games_raw <- read_csv(file.path(nfl_root, "all_games.csv"), show_col_types = FALSE)
games <- read_csv(file.path(nfl_root, "all_games_enriched.csv"), show_col_types = FALSE)
missing_games <- read_csv(file.path(nfl_root, "missing_wp_games.csv"), show_col_types = FALSE)

expected <- tibble(
  season = 2018:2024,
  scheduled_games_expected = c(256L, 256L, 256L, 272L, 272L, 272L, 272L),
  completed_games_expected = c(256L, 256L, 256L, 272L, 271L, 272L, 272L),
  tied_games_expected = c(2L, 1L, 1L, 1L, 2L, 0L, 0L),
  cancelled_games_expected = c(0L, 0L, 0L, 0L, 1L, 0L, 0L),
  eligible_games_expected = c(254L, 255L, 255L, 271L, 269L, 272L, 272L),
  january_carryover_expected = c(0L, 0L, 16L, 32L, 31L, 17L, 16L)
)

assert_true(identical(sort(unique(as.integer(inventory$season))), 2018:2024),
            "NFL inventory does not contain exactly seasons 2018-2024.")
assert_true(n_distinct(as.character(inventory$game_id)) == nrow(inventory),
            "NFL schedule inventory contains duplicate ESPN game IDs.")
assert_true(n_distinct(as.character(games_raw$game_id)) == nrow(games_raw),
            "Raw NFL game summary contains duplicate game IDs.")
assert_true(n_distinct(as.character(games$game_id)) == nrow(games),
            "Enriched NFL game summary contains duplicate game IDs.")
assert_true(setequal(as.character(games_raw$game_id), as.character(games$game_id)),
            "Raw and enriched NFL game summaries contain different game IDs.")

coverage_check <- coverage |>
  left_join(expected, by = "season")
for (column in c("scheduled_games", "completed_games", "tied_games", "cancelled_games", "eligible_games")) {
  expected_column <- paste0(column, "_expected")
  assert_true(
    all(as.integer(coverage_check[[column]]) == as.integer(coverage_check[[expected_column]])),
    paste("Unexpected NFL", column, "by season.")
  )
}
assert_true(all(coverage$scheduled_games == coverage$completed_games +
                  coverage$cancelled_games + coverage$other_nonfinal_games),
            "NFL scheduled-game coverage does not reconcile.")
assert_true(all(coverage$eligible_games == coverage$tied_games * 0L +
                  coverage$analyzed_games + coverage$missing_games),
            "NFL eligible-game coverage does not reconcile.")
assert_true(all(coverage$raw_available_games + coverage$raw_failures >= coverage$analyzed_games),
            "NFL acquisition coverage is internally inconsistent.")
assert_true(nrow(missing_games) == sum(coverage$missing_games),
            "NFL missing-game detail does not match the coverage table.")

inventory_dates <- as.Date(substr(as.character(inventory$game_date), 1L, 10L))
calendar_year <- as.integer(format(inventory_dates, "%Y"))
calendar_month <- as.integer(format(inventory_dates, "%m"))
inventory_season <- as.integer(inventory$season)
assert_true(all(calendar_year %in% c(inventory_season, inventory_season + 1L)),
            "An NFL game falls outside its season year or carryover year.")
assert_true(all(calendar_year == inventory_season | calendar_month %in% c(1L, 2L)),
            "An NFL carryover game falls outside January-February.")

carryover <- inventory |>
  mutate(
    calendar_year = calendar_year,
    is_january_carryover = calendar_year == as.integer(season) + 1L & calendar_month == 1L
  ) |>
  count(season, wt = as.integer(is_january_carryover), name = "january_carryover") |>
  right_join(expected |> select(season, january_carryover_expected), by = "season") |>
  mutate(january_carryover = coalesce(january_carryover, 0L))
assert_true(all(carryover$january_carryover == carryover$january_carryover_expected),
            "NFL January carryover counts do not match the true season slates.")

eligible_ids <- inventory |>
  filter(is_eligible) |>
  pull(game_id) |>
  as.character()
assert_true(setequal(as.character(games$game_id), eligible_ids),
            "Analyzed NFL games do not equal the eligible schedule games.")
assert_true(!"401437947" %in% as.character(games$game_id),
            "The canceled Buffalo-Cincinnati game entered the analytic sample.")

for (season in 2018:2024) {
  path_file <- resolve_path_file(nfl_root, season)
  raw_path_file <- file.path(nfl_root, paste0(season, "_wp_paths.csv"))

  path_ids <- unique(as.character(read_path_data(path_file, "game_id")$game_id))
  season_ids <- games |>
    filter(as.integer(.data$season) == .env$season) |>
    pull(game_id) |>
    as.character()
  assert_true(setequal(path_ids, season_ids), paste("Enriched path IDs disagree in", season))
  if (file.exists(raw_path_file)) {
    raw_path_ids <- unique(as.character(
      fread(raw_path_file, select = "game_id", showProgress = FALSE)$game_id
    ))
    assert_true(setequal(raw_path_ids, season_ids), paste("Raw path IDs disagree in", season))
  }
}

message("NFL data contracts passed for ", nrow(games), " analyzed regular-season games.")

nba_root <- file.path(data_root, "nba")
nba_required_files <- file.path(
  nba_root,
  c(
    "schedule_inventory.csv", "feed_coverage_by_season.csv",
    "all_games.csv", "all_games_enriched.csv", "missing_wp_games.csv"
  )
)
assert_true(all(file.exists(nba_required_files)), paste(
  "NBA data contract is missing:",
  paste(basename(nba_required_files[!file.exists(nba_required_files)]), collapse = ", ")
))

nba_inventory <- read_csv(
  file.path(nba_root, "schedule_inventory.csv"),
  show_col_types = FALSE
)
nba_coverage <- read_csv(
  file.path(nba_root, "feed_coverage_by_season.csv"),
  show_col_types = FALSE
)
nba_games_raw <- read_csv(
  file.path(nba_root, "all_games.csv"),
  show_col_types = FALSE
)
nba_games <- read_csv(
  file.path(nba_root, "all_games_enriched.csv"),
  show_col_types = FALSE
)

nba_team_abbreviations <- c(
  "ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GS",
  "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NO", "NY",
  "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SA", "TOR", "UTAH", "WSH"
)
assert_true(
  all(nba_inventory$home_team_abbr %in% nba_team_abbreviations) &&
    all(nba_inventory$away_team_abbr %in% nba_team_abbreviations),
  "NBA inventory contains a non-franchise exhibition matchup."
)
assert_true(
  all(nba_games$home_team_abbr %in% nba_team_abbreviations) &&
    all(nba_games$away_team_abbr %in% nba_team_abbreviations),
  "NBA analyzed data contain a non-franchise exhibition matchup."
)
nba_missing_games <- read_csv(
  file.path(nba_root, "missing_wp_games.csv"),
  show_col_types = FALSE
)

nba_expected <- tibble(
  season = 2018:2024,
  scheduled_games_expected = c(1230L, 1230L, 1062L, 1112L, 1241L, 1231L, 1233L),
  completed_games_expected = c(1230L, 1230L, 1059L, 1080L, 1230L, 1230L, 1231L),
  tied_games_expected = rep(0L, 7L),
  cancelled_games_expected = rep(0L, 7L),
  eligible_games_expected = c(1230L, 1230L, 1059L, 1080L, 1230L, 1230L, 1231L),
  analyzed_games_expected = c(1222L, 1230L, 1053L, 1080L, 1230L, 1230L, 1231L),
  missing_games_expected = c(8L, 0L, 6L, 0L, 0L, 0L, 0L)
)

assert_true(identical(sort(unique(as.integer(nba_inventory$season))), 2018:2024),
            "NBA inventory does not contain exactly seasons 2018-2024.")
assert_true(n_distinct(as.character(nba_inventory$game_id)) == nrow(nba_inventory),
            "NBA schedule inventory contains duplicate ESPN game IDs.")
assert_true(n_distinct(as.character(nba_games_raw$game_id)) == nrow(nba_games_raw),
            "Raw NBA game summary contains duplicate game IDs.")
assert_true(n_distinct(as.character(nba_games$game_id)) == nrow(nba_games),
            "Enriched NBA game summary contains duplicate game IDs.")
assert_true(setequal(as.character(nba_games_raw$game_id), as.character(nba_games$game_id)),
            "Raw and enriched NBA game summaries contain different game IDs.")

nba_coverage_check <- nba_coverage |>
  left_join(nba_expected, by = "season")
for (column in c(
  "scheduled_games", "completed_games", "tied_games", "cancelled_games",
  "eligible_games", "analyzed_games", "missing_games"
)) {
  expected_column <- paste0(column, "_expected")
  assert_true(
    all(as.integer(nba_coverage_check[[column]]) ==
          as.integer(nba_coverage_check[[expected_column]])),
    paste("Unexpected NBA", column, "by season.")
  )
}
assert_true(all(nba_coverage$scheduled_games == nba_coverage$completed_games +
                  nba_coverage$cancelled_games + nba_coverage$other_nonfinal_games),
            "NBA scheduled-game coverage does not reconcile.")
assert_true(all(nba_coverage$eligible_games ==
                  nba_coverage$analyzed_games + nba_coverage$missing_games),
            "NBA eligible-game coverage does not reconcile.")
assert_true(nrow(nba_missing_games) == sum(nba_coverage$missing_games),
            "NBA missing-game detail does not match the coverage table.")

nba_dates <- as.Date(substr(as.character(nba_inventory$game_date), 1L, 10L))
nba_calendar_year <- as.integer(format(nba_dates, "%Y"))
nba_season <- as.integer(nba_inventory$season)
assert_true(all(nba_calendar_year == nba_season - 1L |
                  nba_calendar_year == nba_season),
            "An NBA game falls outside its ending-year season label.")

nba_eligible_ids <- nba_inventory |>
  filter(is_eligible) |>
  pull(game_id) |>
  as.character()
nba_analyzed_ids <- nba_inventory |>
  filter(analyzed) |>
  pull(game_id) |>
  as.character()
assert_true(setequal(as.character(nba_games$game_id), nba_analyzed_ids),
            "Analyzed NBA games do not equal the schedule inventory analysis set.")
assert_true(setequal(
  setdiff(nba_eligible_ids, as.character(nba_games$game_id)),
  as.character(nba_missing_games$game_id)
), "NBA missing-feed IDs do not reconcile with eligible regular-season games.")

for (season in 2018:2024) {
  path_file <- resolve_path_file(nba_root, season)
  raw_path_file <- file.path(nba_root, paste0(season, "_wp_paths.csv"))

  path_ids <- unique(as.character(read_path_data(path_file, "game_id")$game_id))
  season_ids <- nba_games |>
    filter(as.integer(.data$season) == .env$season) |>
    pull(game_id) |>
    as.character()
  assert_true(setequal(path_ids, season_ids),
              paste("Enriched NBA path IDs disagree in", season))
  if (file.exists(raw_path_file)) {
    raw_path_ids <- unique(as.character(
      fread(raw_path_file, select = "game_id", showProgress = FALSE)$game_id
    ))
    assert_true(setequal(raw_path_ids, season_ids),
                paste("Raw NBA path IDs disagree in", season))
  }
}

message("NBA data contracts passed for ", nrow(nba_games), " analyzed regular-season games.")

if (check_fixed_clock) {
  fixed_clock_expected <- c(nba = 8265L, nfl = 1848L)
  for (league in names(fixed_clock_expected)) {
    forecast_path <- file.path(
      repo_root, "results", "figures", "manuscript", "figures",
      league, "fixed_clock_forecasts.csv"
    )
    bin_path <- file.path(
      repo_root, "results", "figures", "manuscript", "figures",
      league, "fixed_clock_bins.csv"
    )
    assert_true(file.exists(forecast_path) && file.exists(bin_path),
                paste("Missing fixed-clock outputs for", league))
    forecasts <- fread(forecast_path, showProgress = FALSE)
    bins <- fread(bin_path, showProgress = FALSE)
    assert_true(setequal(unique(forecasts$method), c("linear", "locf")),
                paste("Unexpected fixed-clock methods for", league))
    assert_true(identical(sort(unique(forecasts$regulation_fraction)),
                          seq(0, 0.95, by = 0.05)),
                paste("Unexpected fixed-clock grid for", league))
    per_game_method <- forecasts[, .N, by = .(game_id, method)]
    assert_true(all(per_game_method$N == 20L),
                paste("Incomplete fixed-clock game trajectory for", league))
    assert_true(uniqueN(forecasts$game_id) == fixed_clock_expected[[league]],
                paste("Unexpected fixed-clock game count for", league))
    method_ids <- split(as.character(forecasts$game_id), forecasts$method)
    assert_true(setequal(unique(method_ids$linear), unique(method_ids$locf)),
                paste("Linear and LOCF samples differ for", league))
    tie_check <- bins[, .(n_bins = uniqueN(bin_id)),
                      by = .(method, time_index, p)]
    assert_true(all(tie_check$n_bins == 1L),
                paste("A tied forecast value was split across bins for", league))
  }

  message("Fixed-clock data contracts passed.")
}
