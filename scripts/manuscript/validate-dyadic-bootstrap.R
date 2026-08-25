#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(matrixStats)
  library(readr)
  library(tibble)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  script_path <- normalizePath("scripts/manuscript/validate-dyadic-bootstrap.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "R", "dyadic-bootstrap.R"))
source(file.path(repo_root, "R", "fixed-clock-calibration.R"))

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default = NULL) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[[1]])
}

outer_samples <- as.integer(parse_arg("outer", "250"))
inner_replicates <- as.integer(parse_arg("inner", "399"))
seed <- as.integer(parse_arg("seed", "20260815"))
league_arg <- tolower(parse_arg("league", "both"))
detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
default_cores <- if (
  .Platform$OS.type == "unix" &&
  length(detected_cores) == 1L && is.finite(detected_cores)
) {
  min(4L, as.integer(detected_cores))
} else {
  1L
}
cores <- as.integer(parse_arg("cores", as.character(default_cores)))
if (anyNA(c(outer_samples, inner_replicates, seed, cores)) ||
    outer_samples < 1L || inner_replicates < 1L || cores < 1L) {
  stop("Simulation counts, seed, and cores must be positive integers.")
}
if (!league_arg %in% c("both", "nba", "nfl")) {
  stop("--league must be one of both, nba, or nfl.")
}
leagues <- if (league_arg == "both") c("nba", "nfl") else league_arg
rhos <- c(0, 0.25, 0.5)

table_dir <- file.path(repo_root, "results", "tables", "manuscript")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
validation_path <- file.path(table_dir, "dyadic-bootstrap-validation.csv")

run_deterministic_checks <- function() {
  stopifnot(abs(ks_upper_stat(c(0.5, 0.5)) - 0.5) < 1e-12)
  stopifnot(abs(ks_upper_stat(c(0.2, 0.8)) - 0.3) < 1e-12)

  toy <- tibble(
    season = c("1", "1", "1"),
    game_id = c("ab", "ac", "bc"),
    game_date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    home_team_abbr = c("A", "A", "B"),
    away_team_abbr = c("B", "C", "C")
  )
  design_a <- make_dyadic_design(toy, replicates = 7L, seed = 41L)
  design_b <- make_dyadic_design(toy, replicates = 7L, seed = 41L)
  stopifnot(identical(design_a$seasons[["1"]]$counts,
                      design_b$seasons[["1"]]$counts))
  stopifnot(all(rowSums(design_a$seasons[["1"]]$counts) == 3L))

  weights <- resample_weight_chunk(design_a, toy, seq_len(7L))
  stopifnot(max(abs(rowSums(weights) - nrow(toy))) < 1e-10)

  controlled_design <- design_a
  controlled_design$seasons[["1"]]$counts[,] <- matrix(
    rep(c(2L, 1L, 0L), 7L),
    nrow = 7L,
    byrow = TRUE
  )
  controlled_weights <- resample_weight_chunk(
    controlled_design,
    toy,
    seq_len(7L)
  )
  stopifnot(max(abs(controlled_weights[, 1] - 3)) < 1e-12)
  stopifnot(max(abs(controlled_weights[, 2:3])) < 1e-12)

  duplicate_game <- bind_rows(toy[1, ], toy[1, ])
  positive_design <- design_a
  positive_design$seasons[["1"]]$counts[,] <- 1L
  duplicate_weights <- resample_weight_chunk(
    positive_design,
    duplicate_game,
    seq_len(7L)
  )
  stopifnot(max(abs(duplicate_weights[, 1] - duplicate_weights[, 2])) < 1e-12)
  stopifnot(max(abs(rowSums(duplicate_weights) - nrow(duplicate_game))) < 1e-10)

  calendar_design <- make_calendar_design(
    toy,
    replicates = 7L,
    seed = 43L,
    block_length = 2L
  )
  combined_design <- combine_dyadic_calendar_designs(
    design_a,
    calendar_design
  )
  combined_weights <- resample_weight_chunk(
    combined_design,
    toy,
    seq_len(7L)
  )
  stopifnot(max(abs(rowSums(combined_weights) - nrow(toy))) < 1e-10)

  franchise_a <- make_franchise_design(toy, replicates = 7L, seed = 44L)
  franchise_b <- make_franchise_design(toy, replicates = 7L, seed = 44L)
  stopifnot(identical(franchise_a$counts, franchise_b$counts))
  franchise_weights <- resample_weight_chunk(
    franchise_a,
    toy,
    seq_len(7L)
  )
  stopifnot(max(abs(rowSums(franchise_weights) - nrow(toy))) < 1e-10)

  stopifnot(identical(
    parse_published_clock(c("12:00", "4:21", "4:21:00", NA_character_)),
    c(720, 261, 261, NA_real_)
  ))
  interpolation_toy <- tibble(
    home_wp = c(0.6, 0.8),
    home_wp_corrected = c(0.6, 0.8),
    starting_wp_home = c(0.4, 0.4),
    period_number = c(1L, 1L),
    clock_display_value = c("6:00", "0:00"),
    sequence_number = c(1, 2)
  )
  interpolated <- interpolate_fixed_clock_game(
    interpolation_toy,
    "nba",
    grid = c(0, 0.125, 0.1875, 0.25)
  )
  stopifnot(all.equal(
    interpolated$p[interpolated$method == "linear"],
    c(0.4, 0.6, 0.7, 0.8),
    tolerance = 1e-12
  ))
  stopifnot(all.equal(
    interpolated$p[interpolated$method == "locf"],
    c(0.4, 0.6, 0.6, 0.8),
    tolerance = 1e-12
  ))
  tied_p <- c(0.1, 0.1, 0.2, 0.2, 0.2, 0.8, 0.9, 0.9)
  tied_bins <- tie_preserving_bin_ids(tied_p, n_bins = 4L)
  stopifnot(all(vapply(split(tied_bins, tied_p), function(x) {
    length(unique(x)) == 1L
  }, logical(1))))

  singleton_design <- make_dyadic_design(toy[1, ], replicates = 7L, seed = 42L)
  singleton <- bootstrap_pit_distribution(
    toy[1, ],
    0.7,
    singleton_design,
    thresholds = 0.5,
    chunk_size = 7L
  )
  stopifnot(is.finite(singleton$statistic), length(singleton$draws$dstar) == 7L)

  TRUE
}

load_schedule <- function(league) {
  path <- file.path(
    repo_root,
    "data", "derived", league, "all_games_enriched.csv"
  )
  games <- read_csv(path, show_col_types = FALSE) |>
    transmute(
      season = as.character(season),
      game_id = as.character(game_id),
      game_date = as.Date(game_date),
      home_team_abbr = as.character(home_team_abbr),
      away_team_abbr = as.character(away_team_abbr)
    ) |>
    arrange(season, game_date, game_id)
  if (anyDuplicated(games$game_id)) {
    stop("Duplicate game IDs in the ", league, " validation schedule.")
  }
  games
}

prepare_schedule_indices <- function(games) {
  season_rows <- split(seq_len(nrow(games)), games$season)
  lapply(season_rows, function(rows) {
    teams <- sort(unique(c(
      games$home_team_abbr[rows],
      games$away_team_abbr[rows]
    )))
    list(
      rows = rows,
      home = match(games$home_team_abbr[rows], teams),
      away = match(games$away_team_abbr[rows], teams),
      n_teams = length(teams)
    )
  })
}

simulate_u <- function(schedule_indices, n_games, rho) {
  z <- numeric(n_games)
  for (season in schedule_indices) {
    team_effect <- stats::rnorm(season$n_teams)
    game_noise <- stats::rnorm(length(season$rows))
    z[season$rows] <- sqrt(rho) *
      (team_effect[season$home] + team_effect[season$away]) / sqrt(2) +
      sqrt(1 - rho) * game_noise
  }
  stats::pnorm(z)
}

evaluate_global_test <- function(u, bootstrap_weights) {
  n <- length(u)
  ordered <- order(u)
  sorted_u <- u[ordered]
  runs <- rle(sorted_u)
  tie_end <- cumsum(runs$lengths)
  fhat_right <- tie_end / n
  fhat_left <- (tie_end - runs$lengths) / n
  observed <- max(c(0, runs$values - fhat_left))
  totals <- rowSums(bootstrap_weights)
  cumulative <- matrixStats::rowCumsums(
    bootstrap_weights[, ordered, drop = FALSE]
  )
  fstar <- cumulative[, tie_end, drop = FALSE] / totals
  fhat <- matrix(
    fhat_right,
    nrow = nrow(bootstrap_weights),
    ncol = length(fhat_right),
    byrow = TRUE
  )
  dstar <- pmax(0, matrixStats::rowMaxs(fhat - fstar))
  (1 + sum(dstar >= observed)) / (nrow(bootstrap_weights) + 1)
}

validate_league <- function(league, league_index) {
  games <- load_schedule(league)
  schedule_indices <- prepare_schedule_indices(games)
  design <- make_dyadic_design(
    games,
    replicates = inner_replicates,
    seed = seed + 10000L + league_index
  )
  bootstrap_weights <- resample_weight_chunk(
    design,
    games,
    seq_len(inner_replicates)
  )
  stopifnot(max(abs(rowSums(bootstrap_weights) - nrow(games))) < 1e-8)

  bind_rows(lapply(seq_along(rhos), function(rho_index) {
    rho <- rhos[[rho_index]]
    set.seed(seed + 100000L * league_index + 1000L * rho_index)
    simulated <- vapply(
      seq_len(outer_samples),
      function(i) simulate_u(schedule_indices, nrow(games), rho),
      numeric(nrow(games))
    )
    p_values <- unlist(parallel::mclapply(
      seq_len(outer_samples),
      function(i) evaluate_global_test(simulated[, i], bootstrap_weights),
      mc.cores = cores,
      mc.preschedule = TRUE,
      mc.set.seed = FALSE
    ))
    dyadic_rejection_rate <- mean(p_values <= 0.05)
    tibble(
      league = league,
      rho = rho,
      outer_samples = outer_samples,
      inner_replicates = inner_replicates,
      dyadic_rejections = sum(p_values <= 0.05),
      dyadic_rejection_rate = dyadic_rejection_rate,
      dyadic_mcse = sqrt(
        dyadic_rejection_rate * (1 - dyadic_rejection_rate) / outer_samples
      )
    )
  }))
}

message("Running deterministic dyadic-bootstrap checks.")
run_deterministic_checks()
validation <- bind_rows(lapply(seq_along(leagues), function(i) {
  message(
    toupper(leagues[[i]]), ": finite-sample validation with ",
    outer_samples, " outer samples and ", inner_replicates, " inner draws."
  )
  validate_league(leagues[[i]], i)
}))
write_csv(validation, validation_path)

print(validation, n = Inf)
message("Wrote finite-sample validation: ", validation_path)
