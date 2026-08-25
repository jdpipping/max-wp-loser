suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
  library(httr)
  library(hoopR)
  library(jsonlite)
  library(nflreadr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(espnscrapeR)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

supports_fork_parallel <- function() {
  identical(.Platform$OS.type, "unix") && !identical(Sys.info()[["sysname"]], "Darwin")
}

normalize_worker_count <- function(n_workers) {
  n_workers <- max(1L, as.integer(n_workers))
  if (!supports_fork_parallel()) {
    1L
  } else {
    n_workers
  }
}

median_or_na <- function(x) {
  x_num <- safe_num(x)
  x_num <- x_num[!is.na(x_num)]
  if (length(x_num) == 0) {
    return(NA_real_)
  }
  stats::median(x_num)
}

max_or_na <- function(x) {
  x_num <- safe_num(x)
  x_num <- x_num[!is.na(x_num)]
  if (length(x_num) == 0) {
    return(NA_real_)
  }
  max(x_num)
}

value_or_na <- function(df, col, default = NA) {
  if (!col %in% names(df) || nrow(df) == 0) {
    return(default)
  }
  val <- df[[col]][[1]]
  if (length(val) == 0) {
    return(default)
  }
  val
}

retry_with_backoff <- function(expr_fun,
                               attempts = 5L,
                               sleep_base = 1,
                               sleep_cap = 8,
                               retry_predicate = NULL) {
  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    out <- tryCatch(expr_fun(), error = function(e) e)
    if (!inherits(out, "error")) {
      return(out)
    }

    last_error <- out
    should_retry <- if (is.null(retry_predicate)) {
      TRUE
    } else {
      isTRUE(retry_predicate(out))
    }

    if (!should_retry || attempt == attempts) {
      stop(last_error)
    }

    sleep_for <- min(sleep_cap, sleep_base * 2^(attempt - 1)) + stats::runif(1, 0, 0.5)
    Sys.sleep(sleep_for)
  }

  stop(last_error)
}

is_transient_espn_error <- function(err) {
  msg <- conditionMessage(err)
  patterns <- c(
    "Invalid status line",
    "Malformed encoding found in chunked-encoding",
    "bad HTTP Content or Transfer-Encoding",
    "invalid block type",
    "incorrect data check",
    "invalid code lengths set",
    "invalid stored block lengths",
    "invalid literal/length/distance code",
    "invalid distance too far back",
    "parse error",
    "lexical error",
    "Failure when receiving data from the peer",
    "Could not connect to server",
    "Timeout was reached",
    "Could not resolve host",
    "Connection reset by peer",
    "Empty reply from server",
    "HTTP 429",
    "502 Bad Gateway",
    "503 Service Unavailable",
    "504 Gateway Time-out"
  )
  any(str_detect(msg, fixed(patterns, ignore_case = TRUE)))
}

espn_get_json <- function(url, query = NULL, times = 5L) {
  # Pass query params explicitly because RETRY can drop embedded query strings
  # when a full URL is supplied in `url=`.
  parsed_url <- httr::parse_url(url)
  embedded_query <- parsed_url$query
  parsed_url$query <- NULL
  request_url <- httr::build_url(parsed_url)
  request_query <- if (is.null(query)) {
    embedded_query
  } else {
    utils::modifyList(embedded_query %||% list(), query)
  }

  response <- httr::RETRY(
    verb = "GET",
    url = request_url,
    query = request_query,
    times = times,
    pause_base = 1,
    pause_cap = 8,
    terminate_on = c(400, 401, 403, 404),
    quiet = TRUE,
    httr::add_headers(`Accept-Encoding` = "identity", `User-Agent` = "max-wp-loser/1.0"),
    httr::timeout(30)
  )
  httr::stop_for_status(response)
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(response_text, simplifyVector = FALSE)
}

normalize_wp <- function(x) {
  x_num <- safe_num(x)
  if (all(is.na(x_num))) {
    return(x_num)
  }
  if (max(x_num, na.rm = TRUE) > 1) {
    x_num / 100
  } else {
    x_num
  }
}

parse_clock_to_seconds <- function(clock_text) {
  clock_chr <- as.character(clock_text)
  out <- rep(NA_real_, length(clock_chr))

  minute_mask <- !is.na(clock_chr) & str_detect(clock_chr, "^\\d{1,2}:\\d{2}(?:\\.\\d+)?$")
  if (any(minute_mask)) {
    parts <- str_split_fixed(clock_chr[minute_mask], ":", 2)
    out[minute_mask] <- safe_num(parts[, 1]) * 60 + safe_num(parts[, 2])
  }

  second_mask <- !is.na(clock_chr) & str_detect(clock_chr, "^\\d+(?:\\.\\d+)?$")
  if (any(second_mask)) {
    out[second_mask] <- safe_num(clock_chr[second_mask])
  }

  out
}

extract_nba_overall_record <- function(records_text) {
  if (is.na(records_text) || !nzchar(records_text)) {
    return(NA_character_)
  }

  overall_hit <- str_match(
    records_text,
    "'name':\\s*'overall',\\s*'summary':\\s*'([^']+)'"
  )
  if (!is.na(overall_hit[[1, 2]])) {
    return(overall_hit[[1, 2]])
  }

  total_hit <- str_match(
    records_text,
    "'abbreviation':\\s*'Total'.*?'summary':\\s*'([^']+)'"
  )
  total_hit[[1, 2]] %||% NA_character_
}

pit_cdf_two_team <- function(m, p0) {
  u <- numeric(length(m))
  u[m < (1 - p0)] <- 0
  mid <- m >= (1 - p0) & m < p0
  u[mid] <- 1 - (1 - p0[mid]) / m[mid]
  high <- m >= p0 & m < 1
  u[high] <- 2 - 1 / m[high]
  u[m >= 1] <- 1
  pmin(pmax(u, 0), 1)
}

correct_loser_extreme_runs <- function(path_df,
                                       warn_game_id = NULL) {
  n_rows <- nrow(path_df)
  raw_loser_wp <- safe_num(path_df$loser_wp)
  corrected_loser_wp <- raw_loser_wp
  was_1 <- integer(n_rows)
  was_0 <- integer(n_rows)

  valid_idx <- which(!is.na(raw_loser_wp) & raw_loser_wp > 0 & raw_loser_wp < 1)
  n_bad_1_runs <- 0L
  n_bad_1_rows <- 0L
  n_bad_0_runs <- 0L
  n_bad_0_rows <- 0L

  i <- 1L
  while (i <= n_rows) {
    current_value <- raw_loser_wp[[i]]
    if (is.na(current_value) || current_value != 1) {
      i <- i + 1L
      next
    }

    run_start <- i
    while (i < n_rows && !is.na(raw_loser_wp[[i + 1L]]) && raw_loser_wp[[i + 1L]] == current_value) {
      i <- i + 1L
    }
    run_end <- i

    prior_candidates <- valid_idx[valid_idx < run_start]
    later_candidates <- valid_idx[valid_idx > run_end]
    prior_idx <- if (length(prior_candidates) > 0) prior_candidates[[length(prior_candidates)]] else NA_integer_
    later_idx <- if (length(later_candidates) > 0) later_candidates[[1]] else NA_integer_

    replacement_idx <- if (!is.na(prior_idx)) prior_idx else later_idx
    if (is.na(replacement_idx)) {
      warning(
        sprintf(
          "Unable to correct loser-side exact %.0f run for game %s on rows %d-%d: no valid neighbor found.",
          current_value,
          warn_game_id %||% "unknown",
          run_start,
          run_end
        ),
        call. = FALSE
      )
    } else {
      corrected_loser_wp[run_start:run_end] <- raw_loser_wp[[replacement_idx]]
      was_1[run_start:run_end] <- 1L
      n_bad_1_runs <- n_bad_1_runs + 1L
      n_bad_1_rows <- n_bad_1_rows + (run_end - run_start + 1L)
    }

    i <- i + 1L
  }

  corrected_winner_wp <- ifelse(is.na(corrected_loser_wp), NA_real_, 1 - corrected_loser_wp)
  home_won <- isTRUE(path_df$home_won[[1]])
  if (home_won) {
    home_wp_corrected <- corrected_winner_wp
    away_wp_corrected <- corrected_loser_wp
  } else {
    home_wp_corrected <- corrected_loser_wp
    away_wp_corrected <- corrected_winner_wp
  }

  list(
    paths = path_df |>
      select(-any_of(c(
        "loser_wp_corrected",
        "winner_wp_corrected",
        "home_wp_corrected",
        "away_wp_corrected",
        "was_1",
        "was_0"
      ))) |>
      mutate(
        loser_wp_corrected = corrected_loser_wp,
        winner_wp_corrected = corrected_winner_wp,
        home_wp_corrected = home_wp_corrected,
        away_wp_corrected = away_wp_corrected,
        was_1 = was_1,
        was_0 = was_0
      ),
    audit = list(
      had_bad_1 = n_bad_1_rows > 0L,
      n_bad_1_rows = n_bad_1_rows,
      n_bad_1_runs = n_bad_1_runs,
      had_bad_0 = n_bad_0_rows > 0L,
      n_bad_0_rows = n_bad_0_rows,
      n_bad_0_runs = n_bad_0_runs,
      had_extreme_patch = (n_bad_1_rows + n_bad_0_rows) > 0L
    )
  )
}

compute_nba_game_seconds_remaining <- function(period_number,
                                               clock_display_value,
                                               max_period) {
  clock_seconds <- parse_clock_to_seconds(clock_display_value)
  regular_remaining <- (4 - period_number) * 12 * 60
  overtime_periods_after_reg <- pmax(max_period - 4, 0)
  overtime_remaining_after_current <- pmax(max_period - period_number, 0) * 5 * 60

  ifelse(
    is.na(period_number) | is.na(clock_seconds) | is.na(max_period),
    NA_real_,
    ifelse(
      period_number <= 4,
      regular_remaining + overtime_periods_after_reg * 5 * 60 + clock_seconds,
      overtime_remaining_after_current + clock_seconds
    )
  )
}

compute_nfl_game_seconds_remaining <- function(period_number,
                                               clock_display_value,
                                               max_period) {
  clock_seconds <- parse_clock_to_seconds(clock_display_value)
  regular_remaining <- (4 - period_number) * 15 * 60
  overtime_periods_after_reg <- pmax(max_period - 4, 0)
  overtime_remaining_after_current <- pmax(max_period - period_number, 0) * 10 * 60

  ifelse(
    is.na(period_number) | is.na(clock_seconds) | is.na(max_period),
    NA_real_,
    ifelse(
      period_number <= 4,
      regular_remaining + overtime_periods_after_reg * 10 * 60 + clock_seconds,
      overtime_remaining_after_current + clock_seconds
    )
  )
}

build_nfl_schedule <- function(seasons) {
  schedule_df <- bind_rows(lapply(seasons, function(season) {
    season_dates <- sprintf("%d0801-%d0228", season, season + 1L)
    schedule <- retry_with_backoff(
      expr_fun = function() get_nfl_schedule(season = season_dates),
      attempts = 5L,
      sleep_base = 1,
      sleep_cap = 8,
      retry_predicate = is_transient_espn_error
    )
    schedule |>
      filter(
        slug == "regular-season",
        as.integer(.data$season) == as.integer(.env$season)
      ) |>
      transmute(
        league = "nfl",
        season = as.integer(.data$season),
        game_id = as.character(game_id),
        game_date = as.character(game_date),
        schedule_status = as.character(status_name),
        home_team = home_team_full,
        away_team = away_team_full,
        home_team_abbr = home_team_abb,
        away_team_abbr = away_team_abb,
        home_record = home_record,
        away_record = away_record,
        home_score = safe_num(home_score),
        away_score = safe_num(away_score)
      )
  }))

  # ESPN omits the suspended Buffalo-Cincinnati no-contest from date-range
  # scoreboards but retains it as event 401437947. Keep it in the schedule
  # inventory so the 2022 slate reconciles before completed games are filtered.
  known_nonfinal <- tibble(
    league = "nfl",
    season = 2022L,
    game_id = "401437947",
    game_date = "2023-01-03T01:30Z",
    schedule_status = "STATUS_CANCELED",
    home_team = "Cincinnati Bengals",
    away_team = "Buffalo Bills",
    home_team_abbr = "CIN",
    away_team_abbr = "BUF",
    home_record = NA_character_,
    away_record = NA_character_,
    home_score = NA_real_,
    away_score = NA_real_
  ) |>
    filter(season %in% .env$seasons)
  schedule_df <- bind_rows(schedule_df, known_nonfinal) |>
    distinct(game_id, .keep_all = TRUE)

  nflreadr_seasons <- sort(unique(seasons))
  odds_df <- bind_rows(lapply(nflreadr_seasons, function(season) {
    nflreadr::load_schedules(season) |>
      transmute(
        odds_game_date = as.Date(gameday),
        home_team_abbr = home_team,
        away_team_abbr = away_team,
        pregame_favorite_side_odds = case_when(
          spread_line > 0 ~ "home",
          spread_line < 0 ~ "away",
          TRUE ~ NA_character_
        ),
        pregame_favorite_team_abbr_odds = case_when(
          spread_line > 0 ~ home_team,
          spread_line < 0 ~ away_team,
          TRUE ~ NA_character_
        ),
        pregame_spread_abs = abs(safe_num(spread_line)),
        pregame_total_line = safe_num(total_line),
        pregame_home_moneyline = safe_num(home_moneyline),
        pregame_away_moneyline = safe_num(away_moneyline),
        pregame_home_spread_odds = safe_num(home_spread_odds),
        pregame_away_spread_odds = safe_num(away_spread_odds),
        pregame_odds_n_providers = 1L,
        pregame_odds_source = "nflreadr"
      )
  }))

  schedule_df |>
    mutate(odds_game_date = as.Date(substr(game_date, 1, 10))) |>
    left_join(
      odds_df,
      by = c("odds_game_date", "home_team_abbr", "away_team_abbr")
    ) |>
    select(-odds_game_date)
}

nba_regular_team_abbreviations <- c(
  "ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GS",
  "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NO", "NY",
  "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SA", "TOR", "UTAH", "WSH"
)

is_nba_franchise_matchup <- function(home_abbreviation, away_abbreviation) {
  as.character(home_abbreviation) %in% nba_regular_team_abbreviations &
    as.character(away_abbreviation) %in% nba_regular_team_abbreviations
}

build_nba_schedule <- function(seasons) {
  bind_rows(lapply(seasons, function(season) {
    schedule <- retry_with_backoff(
      expr_fun = function() {
        out <- load_nba_schedule(seasons = season)
        required_cols <- c(
          "season_type",
          "game_id",
          "game_date",
          "home_display_name",
          "away_display_name",
          "home_abbreviation",
          "away_abbreviation",
          "home_score",
          "away_score"
        )
        if (!all(required_cols %in% names(out))) {
          stop("NBA schedule load returned unexpected columns for season ", season)
        }
        out
      },
      attempts = 5L,
      sleep_base = 1,
      sleep_cap = 8
    )
    schedule <- schedule |>
      filter(
        season_type == 2,
        is_nba_franchise_matchup(home_abbreviation, away_abbreviation)
      )

    home_record <- if ("home_records" %in% names(schedule)) {
      vapply(schedule[["home_records"]], extract_nba_overall_record, character(1))
    } else {
      rep(NA_character_, nrow(schedule))
    }
    away_record <- if ("away_records" %in% names(schedule)) {
      vapply(schedule[["away_records"]], extract_nba_overall_record, character(1))
    } else {
      rep(NA_character_, nrow(schedule))
    }

    tibble(
      league = "nba",
      season = .env$season,
      game_id = as.character(schedule$game_id),
      game_date = as.character(schedule$game_date),
      schedule_status = as.character(schedule$status_type_name),
      home_team = schedule$home_display_name,
      away_team = schedule$away_display_name,
      home_team_abbr = schedule$home_abbreviation,
      away_team_abbr = schedule$away_abbreviation,
      home_record = home_record,
      away_record = away_record,
      home_score = safe_num(schedule$home_score),
      away_score = safe_num(schedule$away_score)
    )
  }))
}

summarize_nba_game_odds <- function(game_id, home_team_abbr, away_team_abbr) {
  if (!exists("espn_nba_game_odds", where = asNamespace("hoopR"), inherits = FALSE)) {
    return(tibble(
      pregame_favorite_side_odds = NA_character_,
      pregame_favorite_team_abbr_odds = NA_character_,
      pregame_spread_abs = NA_real_,
      pregame_total_line = NA_real_,
      pregame_home_spread_odds = NA_real_,
      pregame_away_spread_odds = NA_real_,
      pregame_odds_n_providers = 0L,
      pregame_odds_source = "ESPN/hoopR unavailable"
    ))
  }

  odds_df <- tryCatch(
    retry_with_backoff(
      expr_fun = function() {
        odds_out <- NULL
        invisible(capture.output(
          odds_out <- suppressWarnings(
            suppressMessages(
              withCallingHandlers(
                hoopR::espn_nba_game_odds(event_id = as.character(game_id)),
                message = function(m) invokeRestart("muffleMessage"),
                warning = function(w) invokeRestart("muffleWarning")
              )
            )
          )
        ))
        odds_out
      },
      attempts = 4L,
      sleep_base = 1,
      sleep_cap = 8,
      retry_predicate = is_transient_espn_error
    ),
    error = function(e) NULL
  )

  if (is.null(odds_df) || !is.data.frame(odds_df) || nrow(odds_df) == 0) {
    return(tibble(
      pregame_favorite_side_odds = NA_character_,
      pregame_favorite_team_abbr_odds = NA_character_,
      pregame_spread_abs = NA_real_,
      pregame_total_line = NA_real_,
      pregame_home_spread_odds = NA_real_,
      pregame_away_spread_odds = NA_real_,
      pregame_odds_n_providers = 0L,
      pregame_odds_source = "ESPN/hoopR unavailable"
    ))
  }

  detail_abbr <- str_match(odds_df$details %||% NA_character_, "^([A-Z]{2,4})\\s")[, 2]
  detail_abbr <- detail_abbr[!is.na(detail_abbr) & nzchar(detail_abbr)]
  favorite_abbr <- if (length(detail_abbr) == 0) {
    NA_character_
  } else {
    names(sort(table(detail_abbr), decreasing = TRUE))[1]
  }

  favorite_side <- case_when(
    identical(favorite_abbr, home_team_abbr) ~ "home",
    identical(favorite_abbr, away_team_abbr) ~ "away",
    TRUE ~ NA_character_
  )

  tibble(
    pregame_favorite_side_odds = favorite_side,
    pregame_favorite_team_abbr_odds = favorite_abbr,
    pregame_spread_abs = median_or_na(abs(odds_df$spread)),
    pregame_total_line = median_or_na(odds_df$over_under),
    pregame_home_spread_odds = median_or_na(odds_df$home_team_odds_close),
    pregame_away_spread_odds = median_or_na(odds_df$away_team_odds_close),
    pregame_odds_n_providers = nrow(odds_df),
    pregame_odds_source = "ESPN/hoopR consensus"
  )
}

add_missing_context_columns <- function(path_df, league = NA_character_) {
  defaults <- list(
    row_id = seq_len(nrow(path_df)),
    home_score = NA_real_,
    away_score = NA_real_,
    period_number = NA_real_,
    clock_display_value = NA_character_,
    wallclock = NA_character_,
    play_text = NA_character_,
    short_description = NA_character_,
    play_type = NA_character_,
    scoring_play = NA,
    yards_gained = NA_real_,
    game_seconds_remaining = NA_real_
  )

  for (nm in names(defaults)) {
    if (!nm %in% names(path_df)) {
      path_df[[nm]] <- defaults[[nm]]
    }
  }

  if (!"league" %in% names(path_df) && !is.na(league)) {
    path_df$league <- league
  }

  path_df
}

coerce_path_key_types <- function(path_df) {
  character_cols <- intersect(
    c(
      "game_id", "play_id", "sequence_number", "game_date",
      "clock_display_value", "wallclock", "play_text",
      "short_description", "play_type"
    ),
    names(path_df)
  )
  for (column in character_cols) {
    path_df[[column]] <- trimws(as.character(path_df[[column]]))
  }
  path_df
}

fetch_nfl_wp_raw <- function(game_id) {
  raw_url <- glue::glue(
    "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/{game_id}/competitions/{game_id}/probabilities?limit=1000"
  )

  extract_play_text <- function(text_in) {
    text_in |>
      str_remove("\\?lang=en&region=us") |>
      str_remove("http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/[:digit:]+/competitions/[:digit:]+/probabilities/")
  }

  extract_team_text <- function(text_in) {
    text_in |>
      str_remove("http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/[:digit:]+/teams/") |>
      str_remove("\\?lang=en&region=us")
  }

  wp_json <- espn_get_json(raw_url)
  wp_items <- wp_json[["items"]] %||% list()
  if (length(wp_items) == 0) {
    stop("No NFL win probability items returned for game ", game_id)
  }

  wp <- tibble::enframe(wp_items) |>
    tidyr::unnest_wider(value) |>
    rename(row_id = name, play_url_ref = `$ref`) |>
    mutate(
      play_id = extract_play_text(play_url_ref),
      game_id = as.character(game_id)
    ) |>
    tidyr::hoist(homeTeam, home_team_id = "$ref") |>
    tidyr::hoist(awayTeam, away_team_id = "$ref") |>
    select(-play_url_ref, -where(is.list), -any_of(c("lastModified", "secondsLeft"))) |>
    mutate(
      home_team_id = extract_team_text(home_team_id),
      away_team_id = extract_team_text(away_team_id)
    ) |>
    janitor::clean_names() |>
    mutate(
      play_id = as.character(play_id),
      sequence_number = as.character(sequence_number),
      home_wp = normalize_wp(home_win_percentage),
      away_wp = normalize_wp(away_win_percentage)
    ) |>
    select(row_id, game_id, play_id, sequence_number, home_wp, away_wp)

  add_missing_context_columns(wp, "nfl")
}

fetch_nfl_summary_plays <- function(game_id) {
  summary_url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary"
  summary_json <- tryCatch(
    espn_get_json(
      summary_url,
      query = list(event = game_id, enable = "ranks,odds,linescores,logos"),
      times = 8L
    ),
    error = function(e) NULL
  )

  site_plays <- map_dfr(summary_json[["drives"]][["previous"]] %||% list(), function(drive) {
    map_dfr(drive[["plays"]] %||% list(), function(play) {
      tibble(
        play_id = as.character(play$id %||% NA_character_),
        sequence_number = as.character(play$sequenceNumber %||% NA_character_),
        play_text = play$text %||% NA_character_,
        short_description = play$shortText %||% NA_character_,
        play_type = play$type$text %||% NA_character_,
        home_score = safe_num(play$homeScore %||% NA_real_),
        away_score = safe_num(play$awayScore %||% NA_real_),
        period_number = safe_num(play$period$number %||% NA_real_),
        clock_display_value = play$clock$displayValue %||% NA_character_,
        wallclock = play$wallclock %||% NA_character_,
        scoring_play = as.logical(play$scoringPlay %||% NA),
        yards_gained = safe_num(play$statYardage %||% NA_real_)
      )
    })
  })
  if (nrow(site_plays) > 0L) {
    return(site_plays)
  }

  core_url <- glue::glue(
    "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/{game_id}/competitions/{game_id}/plays?limit=1000"
  )
  core_json <- espn_get_json(core_url, times = 8L)
  map_dfr(core_json[["items"]] %||% list(), function(play) {
    tibble(
      play_id = as.character(play$id %||% NA_character_),
      sequence_number = as.character(play$sequenceNumber %||% NA_character_),
      play_text = play$text %||% NA_character_,
      short_description = play$shortText %||% NA_character_,
      play_type = play$type$text %||% NA_character_,
      home_score = safe_num(play$homeScore %||% NA_real_),
      away_score = safe_num(play$awayScore %||% NA_real_),
      period_number = safe_num(play$period$number %||% NA_real_),
      clock_display_value = play$clock$displayValue %||% NA_character_,
      wallclock = play$wallclock %||% NA_character_,
      scoring_play = as.logical(play$scoringPlay %||% NA),
      yards_gained = safe_num(play$statYardage %||% NA_real_)
    )
  })
}

attach_nfl_context <- function(path_df, game_id) {
  context_cols <- c(
    "home_score",
    "away_score",
    "period_number",
    "clock_display_value",
    "wallclock",
    "play_text",
    "short_description",
    "play_type",
    "scoring_play",
    "yards_gained",
    "game_seconds_remaining"
  )

  base_df <- path_df |>
    coerce_path_key_types() |>
    select(-any_of(context_cols))
  plays_df <- fetch_nfl_summary_plays(game_id)
  joined <- base_df |>
    left_join(plays_df, by = c("play_id", "sequence_number")) |>
    arrange(row_id)

  max_period <- max_or_na(joined$period_number)
  joined |>
    mutate(
      game_seconds_remaining = compute_nfl_game_seconds_remaining(
        period_number = period_number,
        clock_display_value = clock_display_value,
        max_period = max_period
      )
    ) |>
    add_missing_context_columns("nfl")
}

fetch_nfl_wp_with_context <- function(game_id) {
  wp <- fetch_nfl_wp_raw(game_id)
  attach_nfl_context(wp, game_id)
}

fetch_nba_wp_raw <- function(game_id) {
  summary_json <- espn_get_json(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/summary",
    query = list(event = game_id, enable = "plays,probability,odds,linescores,logos"),
    times = 8L
  )

  wp_items <- summary_json[["winprobability"]] %||% list()
  if (length(wp_items) == 0) {
    stop("No NBA win probability items returned for game ", game_id)
  }

  wp_df <- purrr::map_dfr(wp_items, function(item) {
    tibble(
      play_id = as.character(item$playId %||% NA_character_),
      home_wp = normalize_wp(item$homeWinPercentage %||% NA_real_),
      tie_percentage = normalize_wp(item$tiePercentage %||% 0)
    )
  }) |>
    mutate(
      away_wp = pmax(0, 1 - home_wp - tie_percentage)
    )

  plays_df <- purrr::map_dfr(summary_json[["plays"]] %||% list(), function(play) {
    tibble(
      play_id = as.character(play$id %||% NA_character_),
      sequence_number = as.character(play$sequenceNumber %||% NA_character_),
      home_score = safe_num(play$homeScore %||% NA_real_),
      away_score = safe_num(play$awayScore %||% NA_real_),
      period_number = safe_num(play$period$number %||% NA_real_),
      clock_display_value = play$clock$displayValue %||% NA_character_,
      wallclock = play$wallclock %||% NA_character_,
      text = play$text %||% NA_character_,
      short_description = play$shortDescription %||% NA_character_,
      type_text = play$type$text %||% NA_character_
    )
  })

  wp <- wp_df |>
    left_join(plays_df, by = "play_id") |>
    mutate(
      game_id = as.character(game_id),
      sequence_number = as.character(sequence_number)
    ) |>
    arrange(safe_num(sequence_number), play_id) |>
    select(
      game_id,
      play_id,
      sequence_number,
      home_wp,
      away_wp,
      home_score,
      away_score,
      period_number,
      clock_display_value,
      wallclock,
      text,
      short_description,
      type_text
    ) |>
    rename(play_text = text, play_type = type_text)

  max_period <- max_or_na(wp$period_number)
  wp |>
    mutate(game_seconds_remaining = compute_nba_game_seconds_remaining(
      period_number = period_number,
      clock_display_value = clock_display_value,
      max_period = max_period
    )) |>
    add_missing_context_columns("nba")
}

fetch_nba_wp_with_context <- fetch_nba_wp_raw

finalize_two_team_game <- function(path_df,
                                   schedule_row,
                                   apply_extreme_correction = FALSE) {
  if (nrow(path_df) == 0) {
    return(NULL)
  }

  path_df <- path_df |>
    arrange(safe_num(sequence_number), play_id)

  home_final <- schedule_row$home_score[[1]]
  away_final <- schedule_row$away_score[[1]]
  if (is.na(home_final)) {
    home_final <- dplyr::last(na.omit(path_df$home_score))
  }
  if (is.na(away_final)) {
    away_final <- dplyr::last(na.omit(path_df$away_score))
  }
  if (is.na(home_final) || is.na(away_final) || home_final == away_final) {
    return(NULL)
  }

  starting_wp_home <- path_df$home_wp[[1]]
  home_won <- home_final > away_final
  loser_side <- if (home_won) "away" else "home"
  winner_side <- if (home_won) "home" else "away"
  favorite_side <- if (starting_wp_home >= 0.5) "home" else "away"

  home_team <- schedule_row$home_team[[1]]
  away_team <- schedule_row$away_team[[1]]
  home_team_abbr <- value_or_na(schedule_row, "home_team_abbr", NA_character_)
  away_team_abbr <- value_or_na(schedule_row, "away_team_abbr", NA_character_)
  home_record <- if ("home_record" %in% names(schedule_row)) schedule_row$home_record[[1]] else NA_character_
  away_record <- if ("away_record" %in% names(schedule_row)) schedule_row$away_record[[1]] else NA_character_
  game_date <- schedule_row$game_date[[1]]
  league <- schedule_row$league[[1]]
  season <- schedule_row$season[[1]]
  game_id <- as.character(schedule_row$game_id[[1]])
  pregame_favorite_side_odds <- value_or_na(schedule_row, "pregame_favorite_side_odds", NA_character_)
  pregame_favorite_team_abbr_odds <- value_or_na(schedule_row, "pregame_favorite_team_abbr_odds", NA_character_)
  pregame_spread_abs <- value_or_na(schedule_row, "pregame_spread_abs", NA_real_)
  pregame_total_line <- value_or_na(schedule_row, "pregame_total_line", NA_real_)
  pregame_home_moneyline <- value_or_na(schedule_row, "pregame_home_moneyline", NA_real_)
  pregame_away_moneyline <- value_or_na(schedule_row, "pregame_away_moneyline", NA_real_)
  pregame_home_spread_odds <- value_or_na(schedule_row, "pregame_home_spread_odds", NA_real_)
  pregame_away_spread_odds <- value_or_na(schedule_row, "pregame_away_spread_odds", NA_real_)
  pregame_odds_n_providers <- value_or_na(schedule_row, "pregame_odds_n_providers", NA_integer_)
  pregame_odds_source <- value_or_na(schedule_row, "pregame_odds_source", NA_character_)

  path_df <- path_df |>
    mutate(
      league = .env$league,
      season = .env$season,
      game_id = .env$game_id,
      game_date = as.character(.env$game_date),
      home_team = .env$home_team,
      away_team = .env$away_team,
      home_team_abbr = .env$home_team_abbr,
      away_team_abbr = .env$away_team_abbr,
      home_record = .env$home_record,
      away_record = .env$away_record,
      home_final = .env$home_final,
      away_final = .env$away_final,
      home_won = .env$home_won,
      starting_wp_home = .env$starting_wp_home,
      starting_wp_favored = max(.env$starting_wp_home, 1 - .env$starting_wp_home),
      favorite_side = .env$favorite_side,
      winner_side = .env$winner_side,
      loser_side = .env$loser_side,
      favorite_team = if_else(.env$favorite_side == "home", .env$home_team, .env$away_team),
      favorite_record = if_else(.env$favorite_side == "home", .env$home_record, .env$away_record),
      winner_team = if_else(.env$winner_side == "home", .env$home_team, .env$away_team),
      winner_record = if_else(.env$winner_side == "home", .env$home_record, .env$away_record),
      loser_team = if_else(.env$loser_side == "home", .env$home_team, .env$away_team),
      loser_record = if_else(.env$loser_side == "home", .env$home_record, .env$away_record),
      pregame_favorite_side_odds = .env$pregame_favorite_side_odds,
      pregame_favorite_team_abbr_odds = .env$pregame_favorite_team_abbr_odds,
      pregame_spread_abs = .env$pregame_spread_abs,
      pregame_total_line = .env$pregame_total_line,
      pregame_home_moneyline = .env$pregame_home_moneyline,
      pregame_away_moneyline = .env$pregame_away_moneyline,
      pregame_home_spread_odds = .env$pregame_home_spread_odds,
      pregame_away_spread_odds = .env$pregame_away_spread_odds,
      pregame_odds_n_providers = .env$pregame_odds_n_providers,
      pregame_odds_source = .env$pregame_odds_source,
      loser_wp = if (.env$home_won) away_wp else home_wp,
      winner_wp = if (.env$home_won) home_wp else away_wp,
      loser_score = if (.env$home_won) away_score else home_score,
      winner_score = if (.env$home_won) home_score else away_score,
      loser_score_margin = loser_score - winner_score
    )

  audit <- list(
    had_bad_1 = FALSE,
    n_bad_1_rows = 0L,
    n_bad_1_runs = 0L,
    had_bad_0 = FALSE,
    n_bad_0_rows = 0L,
    n_bad_0_runs = 0L,
    had_extreme_patch = FALSE
  )

  loser_wp_peak_col <- "loser_wp"

  if (apply_extreme_correction) {
    corrected <- correct_loser_extreme_runs(
      path_df,
      warn_game_id = game_id
    )
    path_df <- corrected$paths
    audit <- corrected$audit
    loser_wp_peak_col <- "loser_wp_corrected"
  }

  peak_series <- path_df[[loser_wp_peak_col]]
  peak_value <- max(peak_series, na.rm = TRUE)
  peak_idx <- which(peak_series == peak_value)
  peak_first <- path_df[peak_idx[[1]], , drop = FALSE]

  summary_df <- tibble(
    league = league,
    season = season,
    game_id = game_id,
    game_date = game_date,
    home_team = home_team,
    away_team = away_team,
    home_team_abbr = home_team_abbr,
    away_team_abbr = away_team_abbr,
    home_record = home_record,
    away_record = away_record,
    home_final = home_final,
    away_final = away_final,
    home_won = home_won,
    favorite_team = path_df$favorite_team[[1]],
    favorite_record = path_df$favorite_record[[1]],
    pregame_favorite_side_odds = pregame_favorite_side_odds,
    pregame_favorite_team_abbr_odds = pregame_favorite_team_abbr_odds,
    pregame_spread_abs = pregame_spread_abs,
    pregame_total_line = pregame_total_line,
    pregame_home_moneyline = pregame_home_moneyline,
    pregame_away_moneyline = pregame_away_moneyline,
    pregame_home_spread_odds = pregame_home_spread_odds,
    pregame_away_spread_odds = pregame_away_spread_odds,
    pregame_odds_n_providers = pregame_odds_n_providers,
    pregame_odds_source = pregame_odds_source,
    winner_team = path_df$winner_team[[1]],
    winner_record = path_df$winner_record[[1]],
    loser_team = path_df$loser_team[[1]],
    loser_record = path_df$loser_record[[1]],
    starting_wp_home = starting_wp_home,
    starting_wp_favored = path_df$starting_wp_favored[[1]],
    max_wp_loser = peak_value,
    pit_u = pit_cdf_two_team(peak_value, path_df$starting_wp_favored[[1]]),
    pit_tail_prob = 1 - pit_cdf_two_team(peak_value, path_df$starting_wp_favored[[1]]),
    n_updates = nrow(path_df),
    n_missing_time = sum(is.na(path_df$game_seconds_remaining)),
    prop_missing_time = mean(is.na(path_df$game_seconds_remaining)),
    peak_rows = length(peak_idx),
    peak_first_update_index = peak_idx[[1]],
    peak_first_play_id = peak_first$play_id[[1]],
    peak_first_sequence_number = peak_first$sequence_number[[1]],
    peak_first_period_number = peak_first$period_number[[1]],
    peak_first_clock_display = peak_first$clock_display_value[[1]],
    peak_first_game_seconds_remaining = peak_first$game_seconds_remaining[[1]],
    peak_first_minutes_remaining = peak_first$game_seconds_remaining[[1]] / 60,
    peak_first_home_score = peak_first$home_score[[1]],
    peak_first_away_score = peak_first$away_score[[1]],
    peak_first_loser_score = peak_first$loser_score[[1]],
    peak_first_winner_score = peak_first$winner_score[[1]],
    peak_first_loser_score_margin = peak_first$loser_score_margin[[1]],
    peak_first_play_text = peak_first$play_text[[1]],
    had_bad_1 = audit$had_bad_1,
    n_bad_1_rows = audit$n_bad_1_rows,
    n_bad_1_runs = audit$n_bad_1_runs,
    had_bad_0 = audit$had_bad_0,
    n_bad_0_rows = audit$n_bad_0_rows,
    n_bad_0_runs = audit$n_bad_0_runs,
    had_extreme_patch = audit$had_extreme_patch
  )

  path_df <- path_df |>
    mutate(
      is_peak_value = .data[[loser_wp_peak_col]] == peak_value,
      is_first_peak = row_number() == peak_idx[[1]]
    )

  list(paths = path_df, summary = summary_df)
}

build_nfl_wp_game <- function(schedule_row) {
  path_df <- fetch_nfl_wp_raw(schedule_row$game_id[[1]]) |>
    add_missing_context_columns("nfl")
  finalize_two_team_game(path_df, schedule_row)
}

build_nba_wp_game <- function(schedule_row) {
  path_df <- fetch_nba_wp_raw(schedule_row$game_id[[1]]) |>
    add_missing_context_columns("nba")
  finalize_two_team_game(path_df, schedule_row)
}

enrich_nfl_game <- function(schedule_row) {
  path_df <- fetch_nfl_wp_with_context(schedule_row$game_id[[1]])
  finalize_two_team_game(path_df, schedule_row, apply_extreme_correction = TRUE)
}

enrich_nba_game <- function(schedule_row) {
  odds_row <- summarize_nba_game_odds(
    game_id = schedule_row$game_id[[1]],
    home_team_abbr = schedule_row$home_team_abbr[[1]],
    away_team_abbr = schedule_row$away_team_abbr[[1]]
  )
  path_df <- fetch_nba_wp_with_context(schedule_row$game_id[[1]])
  finalize_two_team_game(
    path_df,
    bind_cols(schedule_row, odds_row),
    apply_extreme_correction = TRUE
  )
}

enrich_nfl_game_from_raw <- function(raw_path_df, schedule_row) {
  enriched_path <- tryCatch(
    retry_with_backoff(
      expr_fun = function() attach_nfl_context(raw_path_df, schedule_row$game_id[[1]]),
      attempts = 5L,
      sleep_base = 1,
      sleep_cap = 12,
      retry_predicate = is_transient_espn_error
    ),
    error = function(e) {
      message(
        sprintf(
          "NFL context unavailable for %s: %s. Using raw WP path only.",
          schedule_row$game_id[[1]],
          conditionMessage(e)
        )
      )
      add_missing_context_columns(raw_path_df, "nfl")
    }
  )
  finalize_two_team_game(enriched_path, schedule_row, apply_extreme_correction = TRUE)
}

enrich_nba_game_from_raw <- function(raw_path_df, schedule_row) {
  odds_row <- summarize_nba_game_odds(
    game_id = schedule_row$game_id[[1]],
    home_team_abbr = schedule_row$home_team_abbr[[1]],
    away_team_abbr = schedule_row$away_team_abbr[[1]]
  )
  finalize_two_team_game(
    add_missing_context_columns(raw_path_df, "nba"),
    bind_cols(schedule_row, odds_row),
    apply_extreme_correction = TRUE
  )
}

classify_schedule_rows <- function(schedule_df) {
  required <- c(
    "league", "season", "game_id", "game_date",
    "home_team_abbr", "away_team_abbr", "home_score", "away_score"
  )
  missing <- setdiff(required, names(schedule_df))
  if (length(missing) > 0) {
    stop("Schedule is missing columns: ", paste(missing, collapse = ", "))
  }

  if (!"schedule_status" %in% names(schedule_df)) {
    schedule_df$schedule_status <- NA_character_
  }

  schedule_df |>
    mutate(
      season = as.integer(season),
      game_id = as.character(game_id),
      schedule_status = as.character(schedule_status),
      score_available = is.finite(safe_num(home_score)) &
        is.finite(safe_num(away_score)),
      is_cancelled = str_detect(
        coalesce(schedule_status, ""),
        "CANCEL|NO_CONTEST"
      ),
      is_completed = schedule_status == "STATUS_FINAL" |
        (is.na(schedule_status) & score_available),
      is_tie = is_completed & score_available &
        safe_num(home_score) == safe_num(away_score),
      is_eligible = is_completed & score_available & !is_tie,
      schedule_category = case_when(
        is_eligible ~ "eligible",
        is_tie ~ "tie",
        is_completed & !score_available ~ "completed_missing_score",
        is_cancelled ~ "cancelled",
        str_detect(coalesce(schedule_status, ""), "POSTPON|SUSPEND") ~
          "postponed_or_suspended",
        TRUE ~ "nonfinal"
      )
    )
}

empty_failure_table <- function() {
  tibble(
    league = character(),
    season = integer(),
    game_id = character(),
    game_date = character(),
    error = character()
  )
}

write_feed_coverage_audit <- function(schedule_df,
                                      out_dir,
                                      sample_suffix = "") {
  classified <- classify_schedule_rows(schedule_df)
  raw_summary_path <- file.path(out_dir, paste0("all_games", sample_suffix, ".csv"))
  enriched_summary_path <- file.path(
    out_dir,
    paste0("all_games", sample_suffix, "_enriched.csv")
  )
  raw_failures_path <- file.path(
    out_dir,
    paste0("raw_acquisition_failures", sample_suffix, ".csv")
  )
  enrich_failures_path <- file.path(
    out_dir,
    paste0("enrichment_failures", sample_suffix, ".csv")
  )

  read_ids <- function(path) {
    if (!file.exists(path)) {
      return(character())
    }
    readr::read_csv(path, show_col_types = FALSE) |>
      pull(game_id) |>
      as.character()
  }
  read_failures <- function(path, stage) {
    if (!file.exists(path)) {
      return(tibble(game_id = character(), failure_stage = character(), error = character()))
    }
    readr::read_csv(path, show_col_types = FALSE) |>
      transmute(
        game_id = as.character(game_id),
        failure_stage = stage,
        error = as.character(error)
      )
  }

  raw_ids <- read_ids(raw_summary_path)
  analyzed_ids <- read_ids(enriched_summary_path)
  failures <- bind_rows(
    read_failures(raw_failures_path, "raw_acquisition"),
    read_failures(enrich_failures_path, "enrichment")
  ) |>
    group_by(game_id) |>
    summarise(
      failure_stage = paste(unique(failure_stage), collapse = ";"),
      error = paste(unique(error), collapse = " | "),
      .groups = "drop"
    )

  inventory <- classified |>
    mutate(
      raw_available = game_id %in% raw_ids,
      analyzed = game_id %in% analyzed_ids
    ) |>
    left_join(failures, by = "game_id") |>
    mutate(
      audit_status = case_when(
        analyzed ~ "analyzed",
        !is_eligible ~ schedule_category,
        !is.na(failure_stage) ~ failure_stage,
        !raw_available ~ "missing_wp_feed",
        TRUE ~ "missing_enriched_path"
      )
    )

  coverage <- inventory |>
    group_by(league, season) |>
    summarise(
      scheduled_games = n(),
      completed_games = sum(is_completed),
      tied_games = sum(is_tie),
      cancelled_games = sum(is_cancelled),
      other_nonfinal_games = sum(!is_completed & !is_cancelled),
      cancelled_or_nonfinal_games = sum(!is_completed),
      eligible_games = sum(is_eligible),
      raw_available_games = sum(is_eligible & raw_available),
      analyzed_games = sum(is_eligible & analyzed),
      missing_games = sum(is_eligible & !analyzed),
      raw_failures = sum(is_eligible & failure_stage == "raw_acquisition", na.rm = TRUE),
      enrichment_failures = sum(
        is_eligible & str_detect(coalesce(failure_stage, ""), "enrichment"),
        na.rm = TRUE
      ),
      feed_coverage = if_else(
        eligible_games > 0,
        analyzed_games / eligible_games,
        NA_real_
      ),
      .groups = "drop"
    )

  readr::write_csv(
    inventory,
    file.path(out_dir, paste0("schedule_inventory", sample_suffix, ".csv"))
  )
  readr::write_csv(
    coverage,
    file.path(out_dir, paste0("feed_coverage_by_season", sample_suffix, ".csv"))
  )
  readr::write_csv(
    inventory |>
      filter(is_eligible, !analyzed) |>
      select(
        league, season, game_id, game_date,
        home_team, away_team, home_team_abbr, away_team_abbr,
        schedule_status, raw_available, failure_stage, error, audit_status
      ),
    file.path(out_dir, paste0("missing_wp_games", sample_suffix, ".csv"))
  )

  invisible(list(inventory = inventory, coverage = coverage))
}

write_wp_outputs <- function(schedule_df,
                             build_fun,
                             out_dir,
                             max_games = NULL,
                             overwrite = FALSE,
                             n_workers = 1L,
                             reuse_paths = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sample_suffix <- if (is.null(max_games)) "" else "_sample"
  n_workers <- normalize_worker_count(n_workers)
  schedule_df <- classify_schedule_rows(schedule_df)
  reuse_split <- NULL
  if (!is.null(reuse_paths) && nrow(reuse_paths) > 0) {
    reuse_paths <- reuse_paths |>
      mutate(game_id = as.character(game_id))
    reuse_split <- split(reuse_paths, reuse_paths$game_id)
  }

  seasons <- sort(unique(schedule_df$season))
  summary_list <- vector("list", length(seasons))
  failure_list <- list()

  for (i in seq_along(seasons)) {
    season <- seasons[[i]]
    season_schedule <- schedule_df |>
      filter(season == !!season, is_eligible)
    if (!is.null(max_games)) {
      season_schedule <- slice_head(season_schedule, n = max_games)
    }

    paths_file <- file.path(out_dir, sprintf("%s%s_wp_paths.csv", season, sample_suffix))
    summary_file <- file.path(out_dir, sprintf("%s%s_games.csv", season, sample_suffix))
    if (!overwrite && file.exists(paths_file) && file.exists(summary_file)) {
      summary_list[[i]] <- readr::read_csv(summary_file, show_col_types = FALSE)
      next
    }

    message(
      sprintf(
        "Processing %s season %s with %d games using %d worker(s).",
        season_schedule$league[[1]],
        season,
        nrow(season_schedule),
        n_workers
      )
    )

    build_one <- function(j) {
      schedule_row <- season_schedule[j, , drop = FALSE]
      tryCatch(
        {
          game_id <- as.character(schedule_row$game_id[[1]])
          reused <- !is.null(reuse_split) && game_id %in% names(reuse_split)
          result <- if (reused) {
            finalize_two_team_game(
              reuse_split[[game_id]],
              schedule_row,
              apply_extreme_correction = FALSE
            )
          } else {
            retry_with_backoff(
              expr_fun = function() build_fun(schedule_row),
              attempts = 5L,
              sleep_base = 1,
              sleep_cap = 8,
              retry_predicate = is_transient_espn_error
            )
          }
          if (is.null(result) || nrow(result$paths) == 0L) {
            stop("No usable win-probability path returned.")
          }
          list(ok = TRUE, result = result, source = if (reused) "reuse" else "network")
        },
        error = function(e) {
          message(
            sprintf(
              "Skipping %s %s: %s",
              schedule_row$league[[1]],
              schedule_row$game_id[[1]],
              conditionMessage(e)
            )
          )
          list(
            ok = FALSE,
            failure = tibble(
              league = schedule_row$league[[1]],
              season = schedule_row$season[[1]],
              game_id = as.character(schedule_row$game_id[[1]]),
              game_date = as.character(schedule_row$game_date[[1]]),
              error = conditionMessage(e)
            )
          )
        }
      )
    }

    if (n_workers > 1L && nrow(season_schedule) > 1L) {
      season_results <- parallel::mclapply(
        X = seq_len(nrow(season_schedule)),
        FUN = build_one,
        mc.cores = min(n_workers, nrow(season_schedule)),
        mc.preschedule = FALSE,
        mc.set.seed = TRUE
      )
    } else {
      season_results <- lapply(seq_len(nrow(season_schedule)), build_one)
    }

    failures <- keep(season_results, ~ !isTRUE(.x$ok)) |>
      map("failure") |>
      compact()
    if (length(failures) > 0) {
      failure_list <- c(failure_list, failures)
    }
    season_results <- keep(season_results, ~ isTRUE(.x$ok)) |>
      map("result")
    if (length(season_results) == 0) {
      next
    }

    season_paths <- bind_rows(map(season_results, ~ coerce_path_key_types(.x$paths)))
    season_summary <- bind_rows(map(season_results, "summary"))

    readr::write_csv(season_paths, paths_file)
    readr::write_csv(season_summary, summary_file)
    summary_list[[i]] <- season_summary
  }

  summary_list <- compact(summary_list)
  if (length(summary_list) == 0) {
    return(invisible(NULL))
  }

  all_summary <- bind_rows(summary_list)
  if (is.null(max_games)) {
    readr::write_csv(all_summary, file.path(out_dir, "all_games.csv"))
  }
  failures <- bind_rows(failure_list)
  if (ncol(failures) == 0L) {
    failures <- empty_failure_table()
  }
  readr::write_csv(
    failures,
    file.path(out_dir, paste0("raw_acquisition_failures", sample_suffix, ".csv"))
  )
  if (is.null(max_games)) {
    write_feed_coverage_audit(schedule_df, out_dir)
  }

  invisible(all_summary)
}

write_enriched_outputs_from_wp <- function(schedule_df,
                                           out_dir,
                                           max_games = NULL,
                                           overwrite = FALSE,
                                           n_workers = 1L,
                                           reuse_paths = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sample_suffix <- if (is.null(max_games)) "" else "_sample"
  n_workers <- normalize_worker_count(n_workers)
  schedule_df <- classify_schedule_rows(schedule_df)
  reuse_split <- NULL
  if (!is.null(reuse_paths) && nrow(reuse_paths) > 0) {
    reuse_paths <- reuse_paths |>
      mutate(game_id = as.character(game_id))
    reuse_split <- split(reuse_paths, reuse_paths$game_id)
  }

  seasons <- sort(unique(schedule_df$season))
  summary_list <- vector("list", length(seasons))
  failure_list <- list()

  for (i in seq_along(seasons)) {
    season <- seasons[[i]]
    season_schedule <- schedule_df |>
      filter(season == !!season, is_eligible)

    if (!is.null(max_games)) {
      season_schedule <- slice_head(season_schedule, n = max_games)
    }

    if (nrow(season_schedule) == 0) {
      next
    }

    raw_paths_file <- file.path(out_dir, sprintf("%s%s_wp_paths.csv", season, sample_suffix))
    if (!file.exists(raw_paths_file)) {
      message(sprintf("Skipping %s season %s: no raw WP file at %s", season_schedule$league[[1]], season, raw_paths_file))
      next
    }

    paths_file <- file.path(out_dir, sprintf("%s%s_paths.csv", season, sample_suffix))
    summary_file <- file.path(out_dir, sprintf("%s%s_games_enriched.csv", season, sample_suffix))
    if (!overwrite && file.exists(paths_file) && file.exists(summary_file)) {
      summary_list[[i]] <- readr::read_csv(summary_file, show_col_types = FALSE)
      next
    }

    raw_paths <- readr::read_csv(raw_paths_file, show_col_types = FALSE) |>
      coerce_path_key_types()

    league_name <- season_schedule$league[[1]]
    effective_workers <- if (identical(league_name, "nfl")) {
      min(n_workers, 4L)
    } else {
      n_workers
    }

    message(
      sprintf(
        "Enriching %s season %s with %d games using %d worker(s).",
        league_name,
        season,
        nrow(season_schedule),
        effective_workers
      )
    )

    enrich_one <- function(j) {
      schedule_row <- season_schedule[j, , drop = FALSE]
      tryCatch({
        game_id <- as.character(schedule_row$game_id[[1]])
        reused <- !is.null(reuse_split) && game_id %in% names(reuse_split)
        result <- if (reused) {
          finalize_two_team_game(
            reuse_split[[game_id]],
            schedule_row,
            apply_extreme_correction = TRUE
          )
        } else {
          raw_path_df <- raw_paths |>
            filter(.data$game_id == .env$game_id)
          if (nrow(raw_path_df) == 0) {
            stop("Raw win-probability path not found.")
          }
          if (identical(schedule_row$league[[1]], "nfl")) {
            enrich_nfl_game_from_raw(raw_path_df, schedule_row)
          } else {
            enrich_nba_game_from_raw(raw_path_df, schedule_row)
          }
        }
        if (is.null(result) || nrow(result$paths) == 0L) {
          stop("No usable enriched win-probability path returned.")
        }
        list(ok = TRUE, result = result, source = if (reused) "reuse" else "raw")
      }, error = function(e) {
        message(
          sprintf(
            "Skipping enrichment for %s %s: %s",
            schedule_row$league[[1]],
            schedule_row$game_id[[1]],
            conditionMessage(e)
          )
        )
        list(
          ok = FALSE,
          failure = tibble(
            league = schedule_row$league[[1]],
            season = schedule_row$season[[1]],
            game_id = as.character(schedule_row$game_id[[1]]),
            game_date = as.character(schedule_row$game_date[[1]]),
            error = conditionMessage(e)
          )
        )
      })
    }

    if (effective_workers > 1L && nrow(season_schedule) > 1L) {
      season_results <- parallel::mclapply(
        X = seq_len(nrow(season_schedule)),
        FUN = enrich_one,
        mc.cores = min(effective_workers, nrow(season_schedule)),
        mc.preschedule = FALSE,
        mc.set.seed = TRUE
      )
    } else {
      season_results <- lapply(seq_len(nrow(season_schedule)), enrich_one)
    }

    failures <- keep(season_results, ~ !isTRUE(.x$ok)) |>
      map("failure") |>
      compact()
    if (length(failures) > 0) {
      failure_list <- c(failure_list, failures)
    }
    season_results <- keep(season_results, ~ isTRUE(.x$ok)) |>
      map("result")
    if (length(season_results) == 0) {
      next
    }

    season_paths <- bind_rows(map(season_results, ~ coerce_path_key_types(.x$paths)))
    season_summary <- bind_rows(map(season_results, "summary"))

    readr::write_csv(season_paths, paths_file)
    readr::write_csv(season_summary, summary_file)
    summary_list[[i]] <- season_summary
  }

  summary_list <- compact(summary_list)
  if (length(summary_list) == 0) {
    return(invisible(NULL))
  }

  all_summary <- bind_rows(summary_list)
  if (is.null(max_games)) {
    readr::write_csv(all_summary, file.path(out_dir, "all_games_enriched.csv"))
  }
  failures <- bind_rows(failure_list)
  if (ncol(failures) == 0L) {
    failures <- empty_failure_table()
  }
  readr::write_csv(
    failures,
    file.path(out_dir, paste0("enrichment_failures", sample_suffix, ".csv"))
  )
  if (is.null(max_games)) {
    write_feed_coverage_audit(schedule_df, out_dir)
  }

  invisible(all_summary)
}

write_enriched_outputs <- function(schedule_df,
                                   enrich_fun,
                                   out_dir,
                                   max_games = NULL,
                                   overwrite = FALSE,
                                   n_workers = 1L) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sample_suffix <- if (is.null(max_games)) "" else "_sample"
  n_workers <- normalize_worker_count(n_workers)

  seasons <- sort(unique(schedule_df$season))
  summary_list <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    season <- seasons[[i]]
    season_schedule <- schedule_df |>
      filter(season == !!season)
    if (!is.null(max_games)) {
      season_schedule <- slice_head(season_schedule, n = max_games)
    }

    paths_file <- file.path(out_dir, sprintf("%s%s_paths.csv", season, sample_suffix))
    summary_file <- file.path(out_dir, sprintf("%s%s_games_enriched.csv", season, sample_suffix))
    if (!overwrite && file.exists(paths_file) && file.exists(summary_file)) {
      summary_list[[i]] <- readr::read_csv(summary_file, show_col_types = FALSE)
      next
    }

    league_name <- season_schedule$league[[1]]
    effective_workers <- if (identical(league_name, "nfl")) {
      min(n_workers, 4L)
    } else {
      n_workers
    }

    enrich_one <- function(j) {
      schedule_row <- season_schedule[j, , drop = FALSE]
      tryCatch(
        retry_with_backoff(
          expr_fun = function() enrich_fun(schedule_row),
          attempts = 5L,
          sleep_base = if (identical(schedule_row$league[[1]], "nfl")) 1 else 0.5,
          sleep_cap = if (identical(schedule_row$league[[1]], "nfl")) 12 else 6,
          retry_predicate = is_transient_espn_error
        ),
        error = function(e) {
          message(
            sprintf(
              "Skipping %s %s: %s",
              schedule_row$league[[1]],
              schedule_row$game_id[[1]],
              conditionMessage(e)
            )
          )
          NULL
        }
      )
    }

    message(
      sprintf(
        "Processing %s season %s with %d games using %d worker(s).",
        league_name,
        season,
        nrow(season_schedule),
        effective_workers
      )
    )

    if (effective_workers > 1L && nrow(season_schedule) > 1L) {
      season_results <- parallel::mclapply(
        X = seq_len(nrow(season_schedule)),
        FUN = enrich_one,
        mc.cores = min(effective_workers, nrow(season_schedule)),
        mc.preschedule = FALSE,
        mc.set.seed = TRUE
      )
    } else {
      season_results <- lapply(seq_len(nrow(season_schedule)), enrich_one)
    }

    season_results <- compact(season_results)
    if (length(season_results) == 0) {
      next
    }

    season_paths <- bind_rows(map(season_results, "paths"))
    season_summary <- bind_rows(map(season_results, "summary"))

    readr::write_csv(season_paths, paths_file)
    readr::write_csv(season_summary, summary_file)
    summary_list[[i]] <- season_summary
  }

  summary_list <- compact(summary_list)
  if (length(summary_list) == 0) {
    return(invisible(NULL))
  }

  all_summary <- bind_rows(summary_list)
  if (is.null(max_games)) {
    readr::write_csv(all_summary, file.path(out_dir, "all_games_enriched.csv"))
    readr::write_csv(
      all_summary |>
        select(game_id, season, starting_wp_favored, home_final, away_final, max_wp_loser),
      file.path(out_dir, "all_games.csv")
    )
  }

  invisible(all_summary)
}
