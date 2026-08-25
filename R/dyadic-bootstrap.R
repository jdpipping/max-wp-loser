pit_cdf_two_team <- function(m, p0) {
  m <- as.numeric(m)
  p0 <- as.numeric(p0)
  u <- numeric(length(m))

  below <- m < (1 - p0)
  middle <- m >= (1 - p0) & m < p0
  above <- m >= p0 & m < 1

  u[below] <- 0
  u[middle] <- 1 - (1 - p0[middle]) / m[middle]
  u[above] <- 2 - 1 / m[above]
  u[m >= 1] <- 1
  pmin(pmax(u, 0), 1)
}

ks_upper_components <- function(u) {
  u <- sort(as.numeric(u[is.finite(u)]))
  if (length(u) == 0) {
    stop("At least one finite PIT value is required.")
  }

  runs <- rle(u)
  cumulative <- cumsum(runs$lengths)
  f_left <- (cumulative - runs$lengths) / length(u)
  f_right <- cumulative / length(u)

  list(
    n = length(u),
    values = runs$values,
    counts = runs$lengths,
    f_left = f_left,
    f_right = f_right,
    statistic = max(c(0, runs$values - f_left))
  )
}

ks_upper_stat <- function(u) {
  ks_upper_components(u)$statistic
}

pit_signature_data <- function(u) {
  parts <- ks_upper_components(u)
  data.frame(
    t = c(0, rep(parts$values, each = 2), 1),
    fhat = c(
      0,
      as.vector(rbind(parts$f_left, parts$f_right)),
      1
    )
  ) |>
    transform(upper_gap = t - fhat)
}

normalize_game_columns <- function(game_data) {
  required <- c(
    "season",
    "game_id",
    "home_team_abbr",
    "away_team_abbr"
  )
  missing <- setdiff(required, names(game_data))
  if (length(missing) > 0) {
    stop("Missing game columns: ", paste(missing, collapse = ", "))
  }

  game_data |>
    dplyr::mutate(
      season = as.character(season),
      game_id = as.character(game_id),
      home_team_abbr = as.character(home_team_abbr),
      away_team_abbr = as.character(away_team_abbr)
    )
}

canonical_franchise_id <- function(team_abbr) {
  team_abbr <- as.character(team_abbr)
  relocation_map <- c(
    OAK = "LV",
    SD = "LAC",
    STL = "LAR",
    NJ = "BKN",
    NOH = "NOP",
    NOK = "NOP",
    SEA = "OKC"
  )
  mapped <- unname(relocation_map[team_abbr])
  ifelse(is.na(mapped), team_abbr, mapped)
}

dyadic_raw_totals <- function(counts, home_index, away_index) {
  totals <- numeric(nrow(counts))
  for (j in seq_along(home_index)) {
    totals <- totals + counts[, home_index[[j]]] * counts[, away_index[[j]]]
  }
  totals
}

make_dyadic_design <- function(game_data,
                               replicates = 9999L,
                               seed = 20260815L) {
  game_data <- normalize_game_columns(game_data)
  replicates <- as.integer(replicates)
  if (replicates < 1L) {
    stop("replicates must be positive.")
  }

  set.seed(as.integer(seed))
  season_rows <- split(seq_len(nrow(game_data)), game_data$season)
  season_designs <- lapply(names(season_rows), function(season) {
    rows <- season_rows[[season]]
    teams <- sort(unique(c(
      game_data$home_team_abbr[rows],
      game_data$away_team_abbr[rows]
    )))
    if (anyNA(teams) || length(teams) < 2L) {
      stop("Invalid team identifiers in season ", season, ".")
    }

    home_index <- match(game_data$home_team_abbr[rows], teams)
    away_index <- match(game_data$away_team_abbr[rows], teams)
    counts <- t(stats::rmultinom(
      replicates,
      size = length(teams),
      prob = rep(1 / length(teams), length(teams))
    ))

    zero <- which(dyadic_raw_totals(counts, home_index, away_index) == 0)
    while (length(zero) > 0) {
      counts[zero, ] <- t(stats::rmultinom(
        length(zero),
        size = length(teams),
        prob = rep(1 / length(teams), length(teams))
      ))
      zero_totals <- dyadic_raw_totals(
        counts[zero, , drop = FALSE],
        home_index,
        away_index
      )
      zero <- zero[zero_totals == 0]
    }

    colnames(counts) <- teams
    list(season = season, teams = teams, counts = counts)
  })
  names(season_designs) <- names(season_rows)

  structure(
    list(
      type = "dyadic",
      replicates = replicates,
      seed = as.integer(seed),
      seasons = season_designs
    ),
    class = "cluster_bootstrap_design"
  )
}

make_franchise_design <- function(game_data,
                                  replicates = 9999L,
                                  seed = 20260815L) {
  game_data <- normalize_game_columns(game_data)
  replicates <- as.integer(replicates)
  if (replicates < 1L) stop("replicates must be positive.")

  home_franchise <- canonical_franchise_id(game_data$home_team_abbr)
  away_franchise <- canonical_franchise_id(game_data$away_team_abbr)
  franchises <- sort(unique(c(home_franchise, away_franchise)))
  if (anyNA(franchises) || length(franchises) < 2L) {
    stop("Invalid franchise identifiers.")
  }

  set.seed(as.integer(seed))
  counts <- t(stats::rmultinom(
    replicates,
    size = length(franchises),
    prob = rep(1 / length(franchises), length(franchises))
  ))
  home_index <- match(home_franchise, franchises)
  away_index <- match(away_franchise, franchises)
  season_rows <- split(seq_len(nrow(game_data)), game_data$season)
  positive_in_every_season <- function(candidate_counts) {
    valid <- rep(TRUE, nrow(candidate_counts))
    for (rows in season_rows) {
      totals <- rowSums(
        candidate_counts[, home_index[rows], drop = FALSE] *
          candidate_counts[, away_index[rows], drop = FALSE]
      )
      valid <- valid & totals > 0
    }
    valid
  }
  invalid <- which(!positive_in_every_season(counts))
  while (length(invalid) > 0L) {
    counts[invalid, ] <- t(stats::rmultinom(
      length(invalid),
      size = length(franchises),
      prob = rep(1 / length(franchises), length(franchises))
    ))
    invalid <- invalid[!positive_in_every_season(counts[invalid, , drop = FALSE])]
  }
  colnames(counts) <- franchises

  structure(
    list(
      type = "franchise",
      replicates = replicates,
      seed = as.integer(seed),
      franchises = franchises,
      counts = counts
    ),
    class = "cluster_bootstrap_design"
  )
}

make_calendar_design <- function(game_data,
                                 replicates = 4999L,
                                 seed = 20260916L,
                                 block_length = 2L) {
  game_data <- normalize_game_columns(game_data)
  if (!"game_date" %in% names(game_data)) {
    stop("game_date is required for calendar-block resampling.")
  }
  game_data$game_date <- as.Date(game_data$game_date)
  if (anyNA(game_data$game_date)) {
    stop("Calendar-block resampling requires valid game dates.")
  }

  replicates <- as.integer(replicates)
  block_length <- as.integer(block_length)
  if (replicates < 1L || block_length < 1L) {
    stop("replicates and block_length must be positive.")
  }

  set.seed(as.integer(seed))
  season_rows <- split(seq_len(nrow(game_data)), game_data$season)
  season_designs <- lapply(names(season_rows), function(season) {
    rows <- season_rows[[season]]
    dates <- game_data$game_date[rows]
    first_date <- min(dates)
    week <- as.integer(floor(as.numeric(dates - first_date) / 7)) + 1L
    n_weeks <- max(week)
    observed_weeks <- sort(unique(week))
    counts <- matrix(0L, nrow = replicates, ncol = n_weeks)

    for (b in seq_len(replicates)) {
      sampled <- integer()
      while (length(sampled) < n_weeks) {
        start <- sample(observed_weeks, 1L)
        block <- ((start - 1L + seq_len(block_length) - 1L) %% n_weeks) + 1L
        sampled <- c(sampled, block)
      }
      counts[b, ] <- tabulate(sampled[seq_len(n_weeks)], nbins = n_weeks)
    }

    list(
      season = season,
      first_date = first_date,
      weeks = week,
      n_weeks = n_weeks,
      counts = counts
    )
  })
  names(season_designs) <- names(season_rows)

  structure(
    list(
      type = "calendar",
      replicates = replicates,
      seed = as.integer(seed),
      block_length = block_length,
      seasons = season_designs
    ),
    class = "cluster_bootstrap_design"
  )
}

combine_dyadic_calendar_designs <- function(dyadic_design, calendar_design) {
  if (!identical(dyadic_design$type, "dyadic") ||
      !identical(calendar_design$type, "calendar")) {
    stop("Expected one dyadic and one calendar bootstrap design.")
  }
  if (dyadic_design$replicates != calendar_design$replicates) {
    stop("Dyadic and calendar designs must have the same replicate count.")
  }
  if (!identical(names(dyadic_design$seasons), names(calendar_design$seasons))) {
    stop("Dyadic and calendar designs must contain the same seasons.")
  }

  season_designs <- lapply(names(dyadic_design$seasons), function(season) {
    dyadic <- dyadic_design$seasons[[season]]
    calendar <- calendar_design$seasons[[season]]
    list(
      season = season,
      teams = dyadic$teams,
      team_counts = dyadic$counts,
      first_date = calendar$first_date,
      n_weeks = calendar$n_weeks,
      calendar_counts = calendar$counts
    )
  })
  names(season_designs) <- names(dyadic_design$seasons)

  structure(
    list(
      type = "dyadic_calendar",
      replicates = dyadic_design$replicates,
      seed = c(dyadic = dyadic_design$seed, calendar = calendar_design$seed),
      block_length = calendar_design$block_length,
      seasons = season_designs
    ),
    class = "cluster_bootstrap_design"
  )
}

resample_weight_chunk <- function(design,
                                  target_data,
                                  replicate_index,
                                  normalize_by_season = TRUE) {
  target_data <- normalize_game_columns(target_data)
  replicate_index <- as.integer(replicate_index)
  if (any(replicate_index < 1L | replicate_index > design$replicates)) {
    stop("replicate_index is outside the bootstrap design.")
  }

  weights <- matrix(
    0,
    nrow = length(replicate_index),
    ncol = nrow(target_data)
  )
  season_rows <- split(seq_len(nrow(target_data)), target_data$season)

  if (identical(design$type, "franchise")) {
    home_index <- match(
      canonical_franchise_id(target_data$home_team_abbr),
      design$franchises
    )
    away_index <- match(
      canonical_franchise_id(target_data$away_team_abbr),
      design$franchises
    )
    if (anyNA(home_index) || anyNA(away_index)) {
      stop("Target data contain franchises absent from the design.")
    }
    counts <- design$counts[replicate_index, , drop = FALSE]
    weights <- counts[, home_index, drop = FALSE] *
      counts[, away_index, drop = FALSE]
    if (normalize_by_season) {
      for (season in names(season_rows)) {
        rows <- season_rows[[season]]
        totals <- rowSums(weights[, rows, drop = FALSE])
        if (any(totals <= 0)) {
          stop("A franchise bootstrap replicate assigned zero weight to season ", season, ".")
        }
        weights[, rows] <- weights[, rows, drop = FALSE] *
          (length(rows) / totals)
      }
    }
    return(weights)
  }

  for (season in names(season_rows)) {
    rows <- season_rows[[season]]
    season_design <- design$seasons[[season]]
    if (is.null(season_design)) {
      stop("Bootstrap design has no season ", season, ".")
    }

    if (identical(design$type, "dyadic")) {
      home_index <- match(target_data$home_team_abbr[rows], season_design$teams)
      away_index <- match(target_data$away_team_abbr[rows], season_design$teams)
      if (anyNA(home_index) || anyNA(away_index)) {
        stop("Target data contain teams absent from design season ", season, ".")
      }
      counts <- season_design$counts[replicate_index, , drop = FALSE]
      season_weights <- counts[, home_index, drop = FALSE] *
        counts[, away_index, drop = FALSE]
    } else if (identical(design$type, "dyadic_calendar")) {
      if (!"game_date" %in% names(target_data)) {
        stop("Dyad-plus-calendar target data require game_date.")
      }
      home_index <- match(target_data$home_team_abbr[rows], season_design$teams)
      away_index <- match(target_data$away_team_abbr[rows], season_design$teams)
      if (anyNA(home_index) || anyNA(away_index)) {
        stop("Target data contain teams absent from design season ", season, ".")
      }
      team_counts <- season_design$team_counts[
        replicate_index,
        ,
        drop = FALSE
      ]
      dyadic_weights <- team_counts[, home_index, drop = FALSE] *
        team_counts[, away_index, drop = FALSE]

      dates <- as.Date(target_data$game_date[rows])
      week <- as.integer(
        floor(as.numeric(dates - season_design$first_date) / 7)
      ) + 1L
      week <- pmin(pmax(week, 1L), season_design$n_weeks)
      calendar_counts <- season_design$calendar_counts[
        replicate_index,
        ,
        drop = FALSE
      ]
      season_weights <- dyadic_weights *
        calendar_counts[, week, drop = FALSE]
    } else if (identical(design$type, "calendar")) {
      if (!"game_date" %in% names(target_data)) {
        stop("Calendar target data require game_date.")
      }
      dates <- as.Date(target_data$game_date[rows])
      week <- as.integer(
        floor(as.numeric(dates - season_design$first_date) / 7)
      ) + 1L
      week <- pmin(pmax(week, 1L), season_design$n_weeks)
      counts <- season_design$counts[replicate_index, , drop = FALSE]
      season_weights <- counts[, week, drop = FALSE]
    } else {
      stop("Unknown bootstrap design type: ", design$type)
    }

    if (normalize_by_season) {
      totals <- rowSums(season_weights)
      positive <- totals > 0
      season_weights[positive, ] <- season_weights[positive, , drop = FALSE] *
        (length(rows) / totals[positive])
    }
    weights[, rows] <- season_weights
  }

  weights
}

bootstrap_pit_distribution <- function(game_data,
                                       u,
                                       design,
                                       thresholds = c(0.90, 0.95, 0.99),
                                       chunk_size = 100L) {
  game_data <- normalize_game_columns(game_data)
  u <- as.numeric(u)
  keep <- is.finite(u)
  game_data <- game_data[keep, , drop = FALSE]
  u <- u[keep]
  if (length(u) == 0L) {
    stop("No finite PIT values supplied.")
  }

  components <- ks_upper_components(u)
  order_u <- order(u)
  sorted_u <- u[order_u]
  tie_end <- cumsum(rle(sorted_u)$lengths)
  fhat_right <- components$f_right
  observed_tail <- vapply(thresholds, function(q) mean(u >= q), numeric(1))

  dstar <- numeric(design$replicates)
  dstar_uncentered <- numeric(design$replicates)
  tail_draws <- matrix(
    NA_real_,
    nrow = design$replicates,
    ncol = length(thresholds)
  )
  colnames(tail_draws) <- paste0("u", gsub("\\.", "", thresholds))

  chunks <- split(
    seq_len(design$replicates),
    ceiling(seq_len(design$replicates) / as.integer(chunk_size))
  )
  for (replicate_index in chunks) {
    weights <- resample_weight_chunk(design, game_data, replicate_index)
    totals <- rowSums(weights)
    if (any(totals <= 0)) {
      stop("A bootstrap replicate assigned zero total game weight.")
    }

    for (j in seq_along(thresholds)) {
      tail_draws[replicate_index, j] <-
        rowSums(weights[, u >= thresholds[[j]], drop = FALSE]) / totals
    }

    cumulative <- matrixStats::rowCumsums(weights[, order_u, drop = FALSE])
    fstar_right <- cumulative[, tie_end, drop = FALSE] / totals
    centered_gap <- matrix(
      fhat_right,
      nrow = length(replicate_index),
      ncol = length(fhat_right),
      byrow = TRUE
    ) - fstar_right
    dstar[replicate_index] <- pmax(0, matrixStats::rowMaxs(centered_gap))

    fstar_left <- if (ncol(fstar_right) == 1L) {
      matrix(0, nrow = nrow(fstar_right), ncol = 1L)
    } else {
      cbind(0, fstar_right[, -ncol(fstar_right), drop = FALSE])
    }
    null_gap <- matrix(
      components$values,
      nrow = length(replicate_index),
      ncol = length(components$values),
      byrow = TRUE
    ) - fstar_left
    dstar_uncentered[replicate_index] <- pmax(
      0,
      matrixStats::rowMaxs(null_gap)
    )
  }

  p_value <- (1 + sum(dstar >= components$statistic)) /
    (design$replicates + 1)
  critical_value <- as.numeric(stats::quantile(dstar, 0.95, names = FALSE))
  tail_summary <- data.frame(
    threshold = thresholds,
    nominal_tail = 1 - thresholds,
    observed_tail = observed_tail,
    excess = observed_tail - (1 - thresholds),
    ci_lower = NA_real_,
    ci_upper = NA_real_
  )
  for (j in seq_along(thresholds)) {
    interval <- stats::quantile(
      tail_draws[, j] - (1 - thresholds[[j]]),
      c(0.025, 0.975),
      names = FALSE
    )
    tail_summary$ci_lower[[j]] <- interval[[1]]
    tail_summary$ci_upper[[j]] <- interval[[2]]
  }

  list(
    n = length(u),
    statistic = components$statistic,
    bootstrap_p_value = p_value,
    critical_value_95 = critical_value,
    tail = tail_summary,
    signature = pit_signature_data(u),
    draws = data.frame(
      replicate = seq_len(design$replicates),
      dstar = dstar,
      dstar_uncentered = dstar_uncentered,
      tail_draws,
      check.names = FALSE
    )
  )
}

bootstrap_clustered_means <- function(target_data,
                                      value_columns,
                                      design,
                                      chunk_size = 250L) {
  target_data <- normalize_game_columns(target_data)
  missing <- setdiff(value_columns, names(target_data))
  if (length(missing) > 0) {
    stop("Missing value columns: ", paste(missing, collapse = ", "))
  }
  values <- as.matrix(target_data[, value_columns, drop = FALSE])
  storage.mode(values) <- "double"
  if (any(!is.finite(values))) {
    stop("Clustered means require finite value columns.")
  }

  draws <- matrix(
    NA_real_,
    nrow = design$replicates,
    ncol = length(value_columns),
    dimnames = list(NULL, value_columns)
  )
  chunks <- split(
    seq_len(design$replicates),
    ceiling(seq_len(design$replicates) / as.integer(chunk_size))
  )
  for (replicate_index in chunks) {
    weights <- resample_weight_chunk(design, target_data, replicate_index)
    totals <- rowSums(weights)
    valid <- totals > 0
    if (!all(valid)) {
      stop("A bootstrap replicate assigned zero target weight.")
    }
    draws[replicate_index, ] <- (weights %*% values) / totals
  }

  estimates <- colMeans(values)
  intervals <- t(apply(draws, 2, stats::quantile, c(0.025, 0.975)))
  colnames(intervals) <- c("ci_lower", "ci_upper")

  list(estimates = estimates, intervals = intervals, draws = draws)
}

rounding_pit_bounds <- function(max_wp_loser,
                                starting_wp_favored,
                                resolution = 0.001) {
  half <- resolution / 2
  m_lower <- pmax(0, as.numeric(max_wp_loser) - half)
  m_upper <- pmin(1, as.numeric(max_wp_loser) + half)
  p0_lower <- pmax(0.5, as.numeric(starting_wp_favored) - half)
  p0_upper <- pmin(1, as.numeric(starting_wp_favored) + half)

  data.frame(
    u_lower = pit_cdf_two_team(m_lower, p0_lower),
    u_upper = pit_cdf_two_team(m_upper, p0_upper)
  )
}
