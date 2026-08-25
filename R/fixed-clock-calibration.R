fixed_clock_grid <- function() {
  seq(0, 0.95, by = 0.05)
}

regulation_clock_spec <- function(league) {
  league <- tolower(as.character(league))
  if (identical(league, "nfl")) {
    return(list(period_seconds = 15 * 60, regulation_seconds = 60 * 60))
  }
  if (identical(league, "nba")) {
    return(list(period_seconds = 12 * 60, regulation_seconds = 48 * 60))
  }
  stop("Unsupported league for fixed-clock calibration: ", league)
}

parse_published_clock <- function(clock) {
  clock <- as.character(clock)
  out <- rep(NA_real_, length(clock))
  for (i in seq_along(clock)) {
    value <- clock[[i]]
    if (is.na(value) || !nzchar(value)) next
    parts <- strsplit(value, ":", fixed = TRUE)[[1]]
    numeric_parts <- suppressWarnings(as.numeric(parts))
    if (anyNA(numeric_parts)) next
    if (length(numeric_parts) == 2L) {
      out[[i]] <- numeric_parts[[1]] * 60 + numeric_parts[[2]]
    } else if (length(numeric_parts) == 3L && numeric_parts[[3]] == 0) {
      # readr serializes NFL M:SS clocks as M:SS:00 in older frozen files.
      out[[i]] <- numeric_parts[[1]] * 60 + numeric_parts[[2]]
    } else if (length(numeric_parts) == 1L) {
      out[[i]] <- numeric_parts[[1]]
    }
  }
  out
}

first_finite <- function(x) {
  keep <- which(is.finite(x))
  if (length(keep) == 0L) NA_real_ else x[[keep[[1]]]]
}

interpolate_fixed_clock_game <- function(path_df,
                                         league,
                                         grid = fixed_clock_grid()) {
  spec <- regulation_clock_spec(league)
  corrected <- if ("home_wp_corrected" %in% names(path_df)) {
    suppressWarnings(as.numeric(path_df$home_wp_corrected))
  } else {
    rep(NA_real_, nrow(path_df))
  }
  raw <- suppressWarnings(as.numeric(path_df$home_wp))
  p <- ifelse(is.finite(corrected), corrected, raw)
  p0_values <- if ("starting_wp_home" %in% names(path_df)) {
    suppressWarnings(as.numeric(path_df$starting_wp_home))
  } else {
    numeric()
  }
  p0 <- first_finite(p0_values)
  if (!is.finite(p0)) p0 <- first_finite(p)
  if (!is.finite(p0)) return(data.frame())

  period <- suppressWarnings(as.integer(path_df$period_number))
  seconds <- parse_published_clock(path_df$clock_display_value)
  sequence_order <- suppressWarnings(as.numeric(path_df$sequence_number))
  missing_order <- !is.finite(sequence_order)
  sequence_order[missing_order] <- seq_len(nrow(path_df))[missing_order]
  elapsed <- (period - 1L) * spec$period_seconds +
    (spec$period_seconds - seconds)
  progress <- elapsed / spec$regulation_seconds
  valid <- is.finite(p) & period %in% 1:4 & is.finite(seconds) &
    seconds >= 0 & seconds <= spec$period_seconds &
    is.finite(progress) & progress > 0 & progress <= 1

  timeline <- data.frame(
    progress = c(0, progress[valid]),
    sequence_order = c(-Inf, sequence_order[valid]),
    p = c(p0, p[valid])
  )
  timeline <- timeline[order(timeline$progress, timeline$sequence_order), , drop = FALSE]
  timeline <- timeline[!duplicated(timeline$progress, fromLast = TRUE), , drop = FALSE]
  timeline <- timeline[order(timeline$progress), , drop = FALSE]

  linear <- if (nrow(timeline) >= 2L) {
    stats::approx(
      x = timeline$progress,
      y = timeline$p,
      xout = grid,
      method = "linear",
      rule = 1,
      ties = "ordered"
    )$y
  } else {
    c(p0, rep(NA_real_, length(grid) - 1L))
  }
  locf_index <- findInterval(grid, timeline$progress)
  locf <- rep(NA_real_, length(grid))
  locf[locf_index > 0L] <- timeline$p[locf_index[locf_index > 0L]]

  data.frame(
    method = rep(c("linear", "locf"), each = length(grid)),
    time_index = rep(seq_along(grid) - 1L, 2L),
    regulation_fraction = rep(grid, 2L),
    p = c(linear, locf)
  )
}

build_fixed_clock_forecasts <- function(paths,
                                        league,
                                        grid = fixed_clock_grid()) {
  paths <- data.table::as.data.table(paths)
  required <- c(
    "season", "game_id", "game_date", "home_team_abbr",
    "away_team_abbr", "home_won", "home_wp", "period_number",
    "clock_display_value", "sequence_number"
  )
  missing <- setdiff(required, names(paths))
  if (length(missing) > 0L) {
    stop("Fixed-clock paths are missing: ", paste(missing, collapse = ", "))
  }
  paths[, `:=`(
    season = as.character(season),
    game_id = as.character(game_id),
    game_date = as.Date(game_date),
    home_team_abbr = as.character(home_team_abbr),
    away_team_abbr = as.character(away_team_abbr),
    y = as.integer(home_won)
  )]

  metadata <- c(
    "season", "game_id", "game_date", "home_team_abbr",
    "away_team_abbr", "y"
  )
  forecasts <- paths[, {
    interpolation <- interpolate_fixed_clock_game(.SD, league, grid)
    if (nrow(interpolation) == 0L) NULL else interpolation
  }, by = metadata]
  forecasts <- forecasts[is.finite(p)]
  complete_ids <- forecasts[
    method == "linear",
    .(n_grid_points = .N),
    by = game_id
  ][n_grid_points == length(grid), game_id]
  forecasts <- forecasts[game_id %in% complete_ids]
  forecasts[, `:=`(
    league = tolower(league),
    gap = y - p
  )]
  data.table::setcolorder(
    forecasts,
    c("league", metadata, "method", "time_index", "regulation_fraction", "p", "gap")
  )
  forecasts[]
}

tie_preserving_bin_ids <- function(p, n_bins = 10L) {
  p <- as.numeric(p)
  if (length(p) == 0L || any(!is.finite(p))) {
    stop("Tie-preserving bins require finite forecasts.")
  }
  # Interpolation can create sub-decimal floating differences between values
  # that are identical at the precision of the published feed.
  p_key <- round(p, 12L)
  n_bins <- max(1L, as.integer(n_bins))
  if (length(unique(p_key)) == 1L || n_bins == 1L) {
    return(rep(1L, length(p)))
  }
  cutpoints <- unique(as.numeric(stats::quantile(
    p_key,
    probs = seq_len(n_bins - 1L) / n_bins,
    type = 1,
    names = FALSE
  )))
  cutpoints <- cutpoints[cutpoints > min(p_key) & cutpoints < max(p_key)]
  raw_bin <- findInterval(p_key, cutpoints) + 1L
  match(raw_bin, sort(unique(raw_bin)))
}

assign_fixed_clock_bins <- function(forecasts, n_bins = 10L) {
  forecasts <- data.table::as.data.table(forecasts)
  forecasts[, bin_id := tie_preserving_bin_ids(p, n_bins),
            by = .(league, method, time_index)]
  forecasts[, `:=`(
    bin_lower = min(p),
    bin_upper = max(p),
    bin_n_games = .N
  ), by = .(league, method, time_index, bin_id)]
  forecasts[]
}

summarize_fixed_clock_cells <- function(binned_forecasts) {
  data.table::as.data.table(binned_forecasts)[, .(
    n_games = .N,
    mean_forecast = mean(p),
    observed_rate = mean(y),
    gap = mean(gap),
    bin_lower = min(bin_lower),
    bin_upper = max(bin_upper)
  ), by = .(
    league, method, time_index, regulation_fraction, bin_id
  )][]
}

summarize_path_resolution <- function(paths, forecasts, league) {
  paths <- data.table::as.data.table(paths)
  spec <- regulation_clock_spec(league)
  seconds <- parse_published_clock(paths$clock_display_value)
  period <- suppressWarnings(as.integer(paths$period_number))
  paths[, timed_regulation := period %in% 1:4 & is.finite(seconds) &
          seconds >= 0 & seconds <= spec$period_seconds]
  by_game <- paths[, .(
    n_published_updates = .N,
    n_regulation_updates = sum(timed_regulation),
    has_regulation_clock = any(timed_regulation)
  ), by = .(
    season = as.character(season),
    game_id = as.character(game_id),
    game_date = as.Date(game_date),
    home_team_abbr = as.character(home_team_abbr),
    away_team_abbr = as.character(away_team_abbr)
  )]
  included_ids <- unique(as.character(forecasts$game_id))
  by_game[, fixed_clock_included := game_id %in% included_ids]

  expected_points <- length(fixed_clock_grid())
  coverage <- data.table::as.data.table(forecasts)[, .(
    fixed_clock_points = .N
  ), by = .(game_id, method)]
  complete <- coverage[, .(
    complete_games = sum(fixed_clock_points == expected_points),
    games_with_any_point = .N
  ), by = method]
  summary <- complete[, `:=`(
    league = tolower(league),
    n_games = nrow(by_game),
    n_published_updates = sum(by_game$n_published_updates),
    median_updates_per_game = stats::median(by_game$n_published_updates),
    q25_updates_per_game = as.numeric(stats::quantile(by_game$n_published_updates, 0.25)),
    q75_updates_per_game = as.numeric(stats::quantile(by_game$n_published_updates, 0.75)),
    games_with_regulation_clock = sum(by_game$has_regulation_clock),
    games_excluded_incomplete_clock = sum(!by_game$fixed_clock_included)
  )]
  data.table::setcolorder(
    summary,
    c(
      "league", "method", "n_games", "complete_games",
      "games_with_any_point", "games_with_regulation_clock",
      "games_excluded_incomplete_clock",
      "n_published_updates", "median_updates_per_game",
      "q25_updates_per_game", "q75_updates_per_game"
    )
  )
  list(
    by_game = by_game,
    summary = summary
  )
}
