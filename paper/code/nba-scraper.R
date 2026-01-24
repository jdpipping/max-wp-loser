#################
### LIBRARIES ###
#################

library(hoopR)
library(tidyverse)

##################
### PARAMETERS ###
##################

data_dir = "../data/nba"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# Parallel settings (set USE_PARALLEL=TRUE to enable)
USE_PARALLEL = as.logical(Sys.getenv("USE_PARALLEL", "FALSE"))
MAX_CORES = as.integer(Sys.getenv("MAX_CORES", "12"))
CORES = max(1, min(MAX_CORES, parallel::detectCores() - 1))
if (.Platform$OS.type == "windows") {
  USE_PARALLEL = FALSE
}

# Seasons (regular season only). Optional CLI arg: e.g. `Rscript nba-scraper.R 2018,2019`
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

fetch_nba_game = function(game_id, season) {
  wp_result = suppressMessages(suppressWarnings(
    tryCatch(espn_nba_wp(game_id), error = function(e) NULL)
  ))
  if (!is.null(wp_result) && is.list(wp_result) &&
      "winprobability" %in% names(wp_result) &&
      "plays" %in% names(wp_result)) {
    wp_data = wp_result$winprobability
    plays_data = wp_result$plays
    if (!is.null(wp_data) && is.data.frame(wp_data) && nrow(wp_data) > 0 &&
        !is.null(plays_data) && is.data.frame(plays_data) && nrow(plays_data) > 0) {
      wp_data$game_id = game_id
      wp_data$season = season
      return(list(
        wp = wp_data,
        plays = plays_data,
        game_id = game_id,
        season = season
      ))
    }
  }
  NULL
}

process_nba_data = function(wp_data_list) {
  n_games = length(wp_data_list)
  processed_games = vector("list", n_games)
  idx = 0L

  for (i in seq_along(wp_data_list)) {
    game_data = wp_data_list[[i]]
    wp = game_data$wp
    plays = game_data$plays
    game_id = game_data$game_id
    season = game_data$season

    if (nrow(plays) == 0) next
    play_seq = suppressWarnings(as.numeric(plays$sequenceNumber))
    plays_sorted = if (all(is.na(play_seq))) plays else plays[order(play_seq), ]
    home_final = plays_sorted$homeScore[nrow(plays_sorted)]
    away_final = plays_sorted$awayScore[nrow(plays_sorted)]

    if (is.na(home_final) || is.na(away_final) || home_final == away_final) next

    if (nrow(wp) == 0) next
    wp_order = suppressWarnings(as.numeric(stringr::str_extract(wp$playId, "\\d+$")))
    wp_sorted = if (all(is.na(wp_order))) wp else wp[order(wp_order), ]
    home_wp = normalize_wp(wp_sorted$homeWinPercentage)
    starting_wp_home = home_wp[1]

    home_won = home_final > away_final

    tie_wp = if ("tiePercentage" %in% names(wp_sorted)) {
      normalize_wp(wp_sorted$tiePercentage)
    } else {
      rep(0, nrow(wp_sorted))
    }
    tie_wp[is.na(tie_wp)] = 0
    away_wp = if ("awayWinPercentage" %in% names(wp_sorted)) {
      normalize_wp(wp_sorted$awayWinPercentage)
    } else {
      1 - home_wp - tie_wp
    }

    loser_wp = if (home_won) away_wp else home_wp

    starting_wp_favored = if (starting_wp_home >= 0.5) starting_wp_home else (1 - starting_wp_home)

    idx = idx + 1L
    processed_games[[idx]] = data.frame(
      game_id = game_id,
      season = season,
      starting_wp_favored = starting_wp_favored,
      home_final = home_final,
      away_final = away_final,
      max_wp_loser = max(loser_wp, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  processed_games = processed_games[seq_len(idx)]
  game_data = bind_rows(processed_games) |>
    filter(!is.na(starting_wp_favored), !is.na(max_wp_loser),
           starting_wp_favored >= 0.5, starting_wp_favored <= 1,
           max_wp_loser >= 0, max_wp_loser <= 1)

  return(game_data)
}

##################
### MAIN EXECUTION ###
##################

message("Pulling NBA win probability data from ESPN...")
message("This may take a while depending on data availability...")
if (USE_PARALLEL) {
  message(sprintf("Parallel mode enabled (cores = %d)", CORES))
}

all_games_list = list()

for (season in seasons) {
  message(sprintf("\nProcessing season %d...", season))

  schedule = load_nba_schedule(seasons = season)
  if (!is.null(schedule) && nrow(schedule) > 0) {
    if ("season_type" %in% names(schedule)) {
      schedule = schedule |> filter(season_type == 2)
    }
  }

  if (is.null(schedule) || nrow(schedule) == 0) {
    message(sprintf("  No regular-season games found for season %d, skipping...", season))
    next
  }

  game_ids = if ("id" %in% names(schedule)) {
    schedule$id
  } else if ("game_id" %in% names(schedule)) {
    schedule$game_id
  } else if ("gameId" %in% names(schedule)) {
    schedule$gameId
  } else {
    character(0)
  }

  game_ids = unique(game_ids[!is.na(game_ids)])
  if (length(game_ids) == 0) {
    message(sprintf("  No game IDs found for season %d, skipping...", season))
    next
  }

  if (USE_PARALLEL) {
    wp_list = parallel::mclapply(game_ids, fetch_nba_game, season = season, mc.cores = CORES)
  } else {
    pb = utils::txtProgressBar(min = 0, max = length(game_ids), style = 3)
    wp_list = vector("list", length(game_ids))
    for (i in seq_along(game_ids)) {
      wp_list[[i]] = fetch_nba_game(game_ids[i], season)
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  wp_data_list = purrr::compact(wp_list)
  if (length(wp_data_list) == 0) {
    message(sprintf("  No WP data pulled for season %d, skipping...", season))
    next
  }

  season_games = process_nba_data(wp_data_list)
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

message("\nNBA scraping complete!")
