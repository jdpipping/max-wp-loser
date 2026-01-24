#################
### LIBRARIES ###
#################

library(espnscrapeR)
library(tidyverse)

##################
### PARAMETERS ###
##################

data_dir = "paper/data/nfl"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# Parallel settings (set USE_PARALLEL=TRUE to enable)
USE_PARALLEL = as.logical(Sys.getenv("USE_PARALLEL", "FALSE"))
MAX_CORES = as.integer(Sys.getenv("MAX_CORES", "12"))
CORES = max(1, min(MAX_CORES, parallel::detectCores() - 1))
if (.Platform$OS.type == "windows") {
  USE_PARALLEL = FALSE
}

# Seasons (regular season only). Optional CLI arg: e.g. `Rscript nfl-scraper.R 2018,2019`
seasons = 2018:2024
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  seasons = as.integer(unlist(strsplit(args[1], ",")))
}

##################
### FUNCTIONS ###
##################

normalize_wp = function(x) {
  if (all(is.na(x))) return(x)
  if (max(x, na.rm = TRUE) > 1) x / 100 else x
}

fetch_wp = function(game_id, season) {
  wp_data = suppressMessages(suppressWarnings(
    tryCatch(get_espn_win_prob(game_id), error = function(e) NULL)
  ))
  if (!is.null(wp_data) && nrow(wp_data) > 0) {
    wp_data$game_id = game_id
    wp_data$season = season
    return(wp_data)
  }
  NULL
}

process_nfl_data = function(wp_data, scores_data) {
  game_data = wp_data |>
    left_join(scores_data, by = "game_id") |>
    group_by(game_id) |>
    arrange(row_id) |>
    mutate(
      home_wp = normalize_wp(home_win_percentage),
      away_wp = normalize_wp(away_win_percentage),
      starting_wp_home = first(home_wp),
      home_final = first(home_score),
      away_final = first(away_score)
    ) |>
    ungroup() |>
    filter(home_final != away_final, !is.na(home_final), !is.na(away_final)) |>
    mutate(
      home_won = home_final > away_final,
      loser_wp = if_else(home_won, away_wp, home_wp),
      starting_wp_favored = if_else(starting_wp_home >= 0.5, starting_wp_home, 1 - starting_wp_home)
    ) |>
    group_by(game_id) |>
    reframe(
      game_id = game_id,
      season = first(season),
      starting_wp_favored = first(starting_wp_favored),
      home_final = first(home_final),
      away_final = first(away_final),
      max_wp_loser = max(loser_wp, na.rm = TRUE)
    ) |>
    distinct() |>
    filter(!is.na(starting_wp_favored), !is.na(max_wp_loser),
           starting_wp_favored >= 0.5, starting_wp_favored <= 1,
           max_wp_loser >= 0, max_wp_loser <= 1)

  return(game_data)
}

##################
### MAIN EXECUTION ###
##################

message("Pulling NFL win probability data from ESPN...")
message("This may take a while depending on data availability...")
if (USE_PARALLEL) {
  message(sprintf("Parallel mode enabled (cores = %d)", CORES))
}

all_games_list = list()

for (season in seasons) {
  message(sprintf("\nProcessing season %d...", season))

  schedule = tryCatch(get_nfl_schedule(season = season), error = function(e) NULL)
  if (is.null(schedule) || nrow(schedule) == 0 || !"game_id" %in% names(schedule)) {
    message(sprintf("  No schedule found for season %d, skipping...", season))
    next
  }

  schedule = schedule |>
    filter(slug == "regular-season")

  if (nrow(schedule) == 0) {
    message(sprintf("  No regular-season games found for season %d, skipping...", season))
    next
  }

  season_scores = schedule |>
    select(game_id, home_score, away_score) |>
    filter(!is.na(home_score), !is.na(away_score))

  game_ids = schedule$game_id
  if (length(game_ids) == 0) {
    message(sprintf("  No game IDs found for season %d, skipping...", season))
    next
  }

  if (USE_PARALLEL) {
    wp_list = parallel::mclapply(game_ids, fetch_wp, season = season, mc.cores = CORES)
  } else {
    pb = utils::txtProgressBar(min = 0, max = length(game_ids), style = 3)
    wp_list = vector("list", length(game_ids))
    for (i in seq_along(game_ids)) {
      wp_list[[i]] = fetch_wp(game_ids[i], season)
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  season_wp_data = bind_rows(purrr::compact(wp_list))
  if (nrow(season_wp_data) == 0) {
    message(sprintf("  No WP data pulled for season %d, skipping...", season))
    next
  }

  season_games = process_nfl_data(season_wp_data, season_scores)
  if (nrow(season_games) == 0) {
    message(sprintf("  No processed games for season %d, skipping...", season))
    next
  }

  write.csv(season_games,
            file.path(data_dir, sprintf("%d_games.csv", season)),
            row.names = FALSE)
  all_games_list[[length(all_games_list) + 1]] = season_games
}

if (length(all_games_list) == 0) {
  stop("No seasons produced game-level data.")
}

all_games = bind_rows(all_games_list)
write.csv(all_games, file.path(data_dir, "all_games.csv"), row.names = FALSE)

message("\nNFL scraping complete!")
