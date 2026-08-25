#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  normalizePath("scripts/manuscript/build-manuscript-artifacts.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
table_dir <- file.path(repo_root, "results", "tables", "manuscript")
figure_root <- file.path(repo_root, "results", "figures", "manuscript", "figures")

required <- c(
  dependent = file.path(table_dir, "dependent-inference-results.csv"),
  patches = file.path(table_dir, "patch-audit.csv"),
  loso = file.path(table_dir, "leave-one-season-out.csv"),
  fixed_feed = file.path(table_dir, "fixed-clock-feed-summary.csv"),
  validation = file.path(table_dir, "dyadic-bootstrap-validation.csv"),
  cases = file.path(figure_root, "case_studies", "case_study_games.csv"),
  simulation_coarsening = file.path(
    figure_root, "simulation", "null_tail_summary.csv"
  ),
  simulation_profile = file.path(
    figure_root, "simulation", "null_pit_profile.csv"
  ),
  nba_exhibitions = file.path(
    repo_root, "data", "derived", "nba", "excluded_exhibition_games.csv"
  )
)
if (any(!file.exists(required))) {
  stop(
    "Missing manuscript component files: ",
    paste(names(required)[!file.exists(required)], collapse = ", ")
  )
}

results <- read_csv(required[["dependent"]], show_col_types = FALSE) |>
  mutate(
    league = as.character(league),
    analysis = as.character(analysis),
    sample = as.character(sample),
    estimand = as.character(estimand)
  )
patches <- read_csv(required[["patches"]], show_col_types = FALSE)
loso <- read_csv(required[["loso"]], show_col_types = FALSE)
fixed_feed <- read_csv(required[["fixed_feed"]], show_col_types = FALSE)
validation <- read_csv(required[["validation"]], show_col_types = FALSE)
cases <- read_csv(required[["cases"]], show_col_types = FALSE)
simulation_coarsening <- read_csv(
  required[["simulation_coarsening"]], show_col_types = FALSE
)
simulation_profile <- read_csv(
  required[["simulation_profile"]], show_col_types = FALSE
)
nba_exhibitions <- read_csv(
  required[["nba_exhibitions"]], show_col_types = FALSE
)
coverage <- bind_rows(lapply(c("nba", "nfl"), function(league) {
  read_csv(
    file.path(repo_root, "data", "derived", league, "feed_coverage_by_season.csv"),
    show_col_types = FALSE
  )
}))

format_count <- function(x) format(as.integer(round(x)), big.mark = ",", scientific = FALSE)
format_number <- function(x, digits = 3L) formatC(x, format = "f", digits = digits)
format_percent <- function(x, digits = 1L) {
  paste0(formatC(100 * x, format = "f", digits = digits), "\\%")
}
format_p <- function(x) {
  if (!is.finite(x)) return("NA")
  value <- if (x < 0.001) "<0.001" else if (x > 0.999) ">0.999" else {
    formatC(x, format = "f", digits = 3L)
  }
  paste0("\\ensuremath{", value, "}")
}

format_clock <- function(x) {
  value <- as.character(x)
  value <- sub(":00$", "", value)
  sub("^0", "", value)
}

lookup_result <- function(league, analysis, sample, estimand, threshold = NULL) {
  hit <- results |>
    filter(
      .data$league == .env$league,
      .data$analysis == .env$analysis,
      .data$sample == .env$sample,
      .data$estimand == .env$estimand
    )
  if (!is.null(threshold)) {
    hit <- hit |> filter(abs(.data$threshold - .env$threshold) < 1e-8)
  }
  if (nrow(hit) != 1L) {
    stop(
      "Expected one result for ",
      paste(league, analysis, sample, estimand, threshold, collapse = "/"),
      "; found ", nrow(hit), "."
    )
  }
  hit
}

artifact_rows <- list()
add_macro <- function(name,
                      value,
                      formatted,
                      source,
                      league = NA_character_,
                      analysis = NA_character_,
                      sample = NA_character_,
                      estimand = NA_character_) {
  if (length(formatted) != 1L || is.na(formatted)) {
    stop("Macro ", name, " has no formatted value.")
  }
  artifact_rows[[length(artifact_rows) + 1L]] <<- tibble(
    key = name,
    value = as.character(value),
    formatted = as.character(formatted),
    source = source,
    league = league,
    analysis = analysis,
    sample = sample,
    estimand = estimand
  )
}

replicate_values <- unique(results$replicates)
seed_values <- unique(results$seed)
if (length(replicate_values) != 1L || length(seed_values) != 1L) {
  stop("Dependent inference does not have one replicate count and seed.")
}
add_macro(
  "DyadicBootstrapReplicates", replicate_values,
  format_count(replicate_values), required[["dependent"]]
)
add_macro("DyadicBootstrapSeed", seed_values, as.character(seed_values), required[["dependent"]])

threshold_names <- c(`0.9` = "Ninety", `0.95` = "NinetyFive", `0.99` = "NinetyNine")
sensitivity_names <- c(
  raw = "Raw",
  exclude_patched = "ExcludePatched",
  rounding_lower = "RoundingLower",
  rounding_upper = "RoundingUpper",
  dyadic_calendar = "DyadicCalendar",
  franchise_linked = "FranchiseLinked"
)

for (league in c("nba", "nfl")) {
  prefix <- toupper(league)
  global <- lookup_result(league, "global", "corrected", "D_upper")
  critical <- lookup_result(league, "global", "corrected", "dyadic_critical_value_95")
  add_macro(paste0(prefix, "Games"), global$n, format_count(global$n), required[["dependent"]], league, "global", "corrected", "n")
  add_macro(paste0(prefix, "GlobalD"), global$estimate, format_number(global$estimate), required[["dependent"]], league, "global", "corrected", "D_upper")
  add_macro(paste0(prefix, "GlobalDyadicP"), global$p_value, format_p(global$p_value), required[["dependent"]], league, "global", "corrected", "p_value")
  add_macro(paste0(prefix, "GlobalCritical"), critical$estimate, format_number(critical$estimate), required[["dependent"]], league, "global", "corrected", "critical_value")

  for (threshold in as.numeric(names(threshold_names))) {
    label <- threshold_names[[as.character(threshold)]]
    tail_rate <- lookup_result(league, "tail", "corrected", "tail_rate", threshold)
    tail_excess <- lookup_result(league, "tail", "corrected", "tail_excess", threshold)
    crossing <- lapply(c("observed_loss", "implied_loss", "residual"), function(estimand) {
      lookup_result(league, "first_crossing", "preterminal", estimand, threshold)
    })
    names(crossing) <- c("loss", "implied", "residual")
    add_macro(paste0(prefix, "Tail", label, "Rate"), tail_rate$estimate, format_percent(tail_rate$estimate), required[["dependent"]], league, "tail", "corrected", "tail_rate")
    add_macro(paste0(prefix, "Tail", label, "Excess"), tail_excess$estimate, format_percent(tail_excess$estimate), required[["dependent"]], league, "tail", "corrected", "tail_excess")
    add_macro(paste0(prefix, "Tail", label, "CILower"), tail_excess$ci_lower, format_percent(tail_excess$ci_lower), required[["dependent"]], league, "tail", "corrected", "ci_lower")
    add_macro(paste0(prefix, "Tail", label, "CIUpper"), tail_excess$ci_upper, format_percent(tail_excess$ci_upper), required[["dependent"]], league, "tail", "corrected", "ci_upper")
    add_macro(paste0(prefix, "Crossing", label, "Episodes"), crossing$loss$n, format_count(crossing$loss$n), required[["dependent"]], league, "first_crossing", "preterminal", "episodes")
    add_macro(paste0(prefix, "Crossing", label, "Games"), crossing$loss$n_games, format_count(crossing$loss$n_games), required[["dependent"]], league, "first_crossing", "preterminal", "games")
    add_macro(paste0(prefix, "Crossing", label, "Loss"), crossing$loss$estimate, format_percent(crossing$loss$estimate), required[["dependent"]], league, "first_crossing", "preterminal", "observed_loss")
    add_macro(paste0(prefix, "Crossing", label, "Implied"), crossing$implied$estimate, format_percent(crossing$implied$estimate), required[["dependent"]], league, "first_crossing", "preterminal", "implied_loss")
    add_macro(paste0(prefix, "Crossing", label, "Residual"), crossing$residual$estimate, format_percent(crossing$residual$estimate), required[["dependent"]], league, "first_crossing", "preterminal", "residual")
    add_macro(paste0(prefix, "Crossing", label, "CILower"), crossing$residual$ci_lower, format_percent(crossing$residual$ci_lower), required[["dependent"]], league, "first_crossing", "preterminal", "ci_lower")
    add_macro(paste0(prefix, "Crossing", label, "CIUpper"), crossing$residual$ci_upper, format_percent(crossing$residual$ci_upper), required[["dependent"]], league, "first_crossing", "preterminal", "ci_upper")
  }

  strict_tail <- lookup_result(league, "boundary", "strict", "tail_rate", 0.95)
  atom_tail <- lookup_result(league, "boundary", "atom", "tail_rate", 0.95)
  add_macro(paste0(prefix, "TailNinetyFiveStrictRate"), strict_tail$estimate, format_percent(strict_tail$estimate), required[["dependent"]], league, "boundary", "strict", "tail_rate")
  add_macro(paste0(prefix, "TailNinetyFiveAtomRate"), atom_tail$estimate, format_percent(atom_tail$estimate, 2L), required[["dependent"]], league, "boundary", "atom", "tail_rate")

  crossing_samples <- c(Strict = "preterminal_strict", All = "all_updates")
  for (boundary_label in names(crossing_samples)) {
    sample_name <- crossing_samples[[boundary_label]]
    crossing <- lapply(c("observed_loss", "implied_loss", "residual"), function(estimand) {
      lookup_result(league, "first_crossing", sample_name, estimand, 0.95)
    })
    names(crossing) <- c("loss", "implied", "residual")
    stem <- paste0(prefix, "CrossingNinetyFive", boundary_label)
    add_macro(paste0(stem, "Episodes"), crossing$loss$n, format_count(crossing$loss$n), required[["dependent"]], league, "first_crossing", sample_name, "episodes")
    add_macro(paste0(stem, "Games"), crossing$loss$n_games, format_count(crossing$loss$n_games), required[["dependent"]], league, "first_crossing", sample_name, "games")
    add_macro(paste0(stem, "Loss"), crossing$loss$estimate, format_percent(crossing$loss$estimate), required[["dependent"]], league, "first_crossing", sample_name, "observed_loss")
    add_macro(paste0(stem, "Implied"), crossing$implied$estimate, format_percent(crossing$implied$estimate), required[["dependent"]], league, "first_crossing", sample_name, "implied_loss")
    add_macro(paste0(stem, "Residual"), crossing$residual$estimate, format_percent(crossing$residual$estimate), required[["dependent"]], league, "first_crossing", sample_name, "residual")
    add_macro(paste0(stem, "CILower"), crossing$residual$ci_lower, format_percent(crossing$residual$ci_lower), required[["dependent"]], league, "first_crossing", sample_name, "ci_lower")
    add_macro(paste0(stem, "CIUpper"), crossing$residual$ci_upper, format_percent(crossing$residual$ci_upper), required[["dependent"]], league, "first_crossing", sample_name, "ci_upper")
  }

  for (method in c("linear", "locf")) {
    fixed <- lookup_result(league, "fixed_clock_summary", method, "rms_gap")
    fixed_cells <- results |>
      filter(
        .data$league == .env$league,
        .data$analysis == "fixed_clock",
        .data$sample == .env$method
      )
    if (nrow(fixed_cells) == 0L || anyNA(fixed_cells$ci_lower) || anyNA(fixed_cells$ci_upper)) {
      stop("Fixed-clock cell intervals are incomplete for ", league, "/", method, ".")
    }
    significant_cells <- sum(fixed_cells$ci_lower > 0 | fixed_cells$ci_upper < 0)
    label <- if (method == "linear") "Linear" else "LOCF"
    add_macro(paste0(prefix, "FixedClock", label, "RMS"), fixed$estimate, format_percent(fixed$estimate), required[["dependent"]], league, "fixed_clock_summary", method, "rms_gap")
    add_macro(paste0(prefix, "FixedClock", label, "Cells"), nrow(fixed_cells), format_count(nrow(fixed_cells)), required[["dependent"]], league, "fixed_clock_summary", method, "cells")
    add_macro(paste0(prefix, "FixedClock", label, "SignificantCells"), significant_cells, format_count(significant_cells), required[["dependent"]], league, "fixed_clock_summary", method, "significant_cells")
  }

  for (sample_name in names(sensitivity_names)) {
    sensitivity <- lookup_result(league, "global", sample_name, "D_upper")
    label <- sensitivity_names[[sample_name]]
    add_macro(paste0(prefix, label, "GlobalD"), sensitivity$estimate, format_number(sensitivity$estimate), required[["dependent"]], league, "global", sample_name, "D_upper")
    add_macro(paste0(prefix, label, "GlobalP"), sensitivity$p_value, format_p(sensitivity$p_value), required[["dependent"]], league, "global", sample_name, "p_value")
  }
  for (sample_name in c("rounding_lower", "rounding_upper")) {
    label <- sensitivity_names[[sample_name]]
    rounding_tail <- lookup_result(
      league, "tail", sample_name, "tail_rate", 0.95
    )
    add_macro(
      paste0(prefix, label, "TailNinetyFiveRate"),
      rounding_tail$estimate,
      format_percent(rounding_tail$estimate),
      required[["dependent"]],
      league,
      "tail",
      sample_name,
      "tail_rate"
    )
  }

  league_patches <- patches |> filter(.data$league == .env$league)
  add_macro(paste0(prefix, "PatchedGames"), nrow(league_patches), format_count(nrow(league_patches)), required[["patches"]], league, "artifact_audit", "exact_one", "games")

  league_coverage <- coverage |>
    filter(.data$league == .env$league) |>
    arrange(season)
  add_macro(paste0(prefix, "ScheduledGames"), sum(league_coverage$scheduled_games), format_count(sum(league_coverage$scheduled_games)), "feed_coverage_by_season.csv", league, "coverage", "all", "scheduled_games")
  add_macro(paste0(prefix, "CompletedGames"), sum(league_coverage$completed_games), format_count(sum(league_coverage$completed_games)), "feed_coverage_by_season.csv", league, "coverage", "all", "completed_games")
  add_macro(paste0(prefix, "TiedGames"), sum(league_coverage$tied_games), format_count(sum(league_coverage$tied_games)), "feed_coverage_by_season.csv", league, "coverage", "all", "tied_games")
  add_macro(paste0(prefix, "NonfinalGames"), sum(league_coverage$cancelled_or_nonfinal_games), format_count(sum(league_coverage$cancelled_or_nonfinal_games)), "feed_coverage_by_season.csv", league, "coverage", "all", "cancelled_or_nonfinal_games")
  add_macro(paste0(prefix, "EligibleGames"), sum(league_coverage$eligible_games), format_count(sum(league_coverage$eligible_games)), "feed_coverage_by_season.csv", league, "coverage", "all", "eligible_games")
  add_macro(paste0(prefix, "MissingFeedGames"), sum(league_coverage$missing_games), format_count(sum(league_coverage$missing_games)), "feed_coverage_by_season.csv", league, "coverage", "all", "missing_games")
  add_macro(paste0(prefix, "MinimumFeedCoverage"), min(league_coverage$feed_coverage), format_percent(min(league_coverage$feed_coverage)), "feed_coverage_by_season.csv", league, "coverage", "all", "minimum_coverage")
  coverage_rows <- paste0(
    league_coverage$season, " & ",
    format_count(league_coverage$analyzed_games), " & ",
    format_count(league_coverage$missing_games), " & ",
    format_percent(league_coverage$feed_coverage), " \\\\"
  )
  add_macro(paste0(prefix, "MissingCoverageRows"), paste(coverage_rows, collapse = "\n"), paste0("\n", paste(coverage_rows, collapse = "\n"), "\n"), "feed_coverage_by_season.csv", league, "coverage", "season", "rows")
  full_coverage_rows <- paste0(
    league_coverage$season, " & ",
    format_count(league_coverage$scheduled_games), " & ",
    format_count(league_coverage$eligible_games), " & ",
    format_count(league_coverage$analyzed_games), " & ",
    format_count(league_coverage$missing_games), " \\\\"
  )
  add_macro(paste0(prefix, "CoverageRows"), paste(full_coverage_rows, collapse = "\n"), paste0("\n", paste(full_coverage_rows, collapse = "\n"), "\n"), "feed_coverage_by_season.csv", league, "coverage", "season", "rows")

  league_loso <- loso |> filter(.data$league == .env$league) |> arrange(omitted_season)
  add_macro(paste0(prefix, "LOSODMin"), min(league_loso$D_upper), format_number(min(league_loso$D_upper)), required[["loso"]], league, "leave_one_season_out", "all", "D_min")
  add_macro(paste0(prefix, "LOSODMax"), max(league_loso$D_upper), format_number(max(league_loso$D_upper)), required[["loso"]], league, "leave_one_season_out", "all", "D_max")
  add_macro(paste0(prefix, "LOSOTailMin"), min(league_loso$tail_excess_95), format_percent(min(league_loso$tail_excess_95)), required[["loso"]], league, "leave_one_season_out", "all", "tail_min")
  add_macro(paste0(prefix, "LOSOTailMax"), max(league_loso$tail_excess_95), format_percent(max(league_loso$tail_excess_95)), required[["loso"]], league, "leave_one_season_out", "all", "tail_max")
  loso_rows <- paste0(
    league_loso$omitted_season, " & ", format_count(league_loso$n), " & ",
    format_number(league_loso$D_upper), " & ",
    format_percent(league_loso$tail_excess_95), " \\\\"
  )
  add_macro(paste0(prefix, "LeaveSeasonRows"), paste(loso_rows, collapse = "\n"), paste0("\n", paste(loso_rows, collapse = "\n"), "\n"), required[["loso"]], league, "leave_one_season_out", "season", "rows")

  feed <- fixed_feed |> filter(.data$league == .env$league, method == "linear")
  if (nrow(feed) != 1L) stop("Expected one linear fixed-clock feed row for ", league, ".")
  add_macro(paste0(prefix, "PublishedUpdates"), feed$n_published_updates, format_count(feed$n_published_updates), required[["fixed_feed"]], league, "feed", "all", "updates")
  add_macro(paste0(prefix, "MedianUpdatesPerGame"), feed$median_updates_per_game, format_count(feed$median_updates_per_game), required[["fixed_feed"]], league, "feed", "all", "median_updates")
  add_macro(paste0(prefix, "UpdateQOne"), feed$q25_updates_per_game, format_count(feed$q25_updates_per_game), required[["fixed_feed"]], league, "feed", "all", "q25_updates")
  add_macro(paste0(prefix, "UpdateQThree"), feed$q75_updates_per_game, format_count(feed$q75_updates_per_game), required[["fixed_feed"]], league, "feed", "all", "q75_updates")
  add_macro(paste0(prefix, "FixedClockGames"), feed$complete_games, format_count(feed$complete_games), required[["fixed_feed"]], league, "fixed_clock", "linear", "games")
  add_macro(paste0(prefix, "FixedClockExcluded"), feed$games_excluded_incomplete_clock, format_count(feed$games_excluded_incomplete_clock), required[["fixed_feed"]], league, "fixed_clock", "linear", "excluded_games")
}

outer_values <- unique(validation$outer_samples)
inner_values <- unique(validation$inner_replicates)
if (length(outer_values) != 1L || length(inner_values) != 1L) {
  stop("Validation output has inconsistent simulation counts.")
}
add_macro("ValidationOuterSamples", outer_values, format_count(outer_values), required[["validation"]])
add_macro("ValidationInnerReplicates", inner_values, format_count(inner_values), required[["validation"]])
rho_names <- c(`0` = "Zero", `0.25` = "TwentyFive", `0.5` = "Fifty")
for (i in seq_len(nrow(validation))) {
  prefix <- toupper(validation$league[[i]])
  rho_name <- rho_names[[as.character(validation$rho[[i]])]]
  add_macro(paste0("Validation", prefix, "Rho", rho_name, "Dyadic"), validation$dyadic_rejection_rate[[i]], format_percent(validation$dyadic_rejection_rate[[i]]), required[["validation"]], validation$league[[i]], "validation", as.character(validation$rho[[i]]), "dyadic_rejection_rate")
}

coarsening_n <- unique(simulation_coarsening$n_games)
base_curve <- unique(simulation_profile$curve[simulation_profile$m == 1])
base_k <- as.integer(gsub(",", "", sub(".*K = ([0-9,]+).*", "\\1", base_curve)))
if (
  length(coarsening_n) != 1L ||
  length(base_curve) != 1L || length(base_k) != 1L || is.na(base_k)
) {
  stop("Simulation design values are not uniquely identified by their outputs.")
}
add_macro(
  "SimulationCoarseningN", coarsening_n, format_count(coarsening_n),
  required[["simulation_coarsening"]], analysis = "simulation",
  sample = "coarsening", estimand = "n"
)
add_macro(
  "SimulationBaseGrid", base_k, format_count(base_k),
  required[["simulation_profile"]], analysis = "simulation",
  sample = "coarsening", estimand = "K_base"
)

add_macro(
  "NBAExcludedExhibitionEvents", nrow(nba_exhibitions),
  format_count(nrow(nba_exhibitions)), required[["nba_exhibitions"]],
  "nba", "coverage", "excluded_exhibitions", "events"
)
add_macro(
  "NBAExcludedExhibitionFeeds", sum(nba_exhibitions$analyzed),
  format_count(sum(nba_exhibitions$analyzed)), required[["nba_exhibitions"]],
  "nba", "coverage", "excluded_exhibitions", "analyzed_feeds"
)

chicago <- cases |> filter(case_label == "nba_extreme")
if (nrow(chicago) != 1L) stop("Expected exactly one Chicago case-study row.")
peak_lead <- chicago$peak_first_loser_score - chicago$peak_first_winner_score
add_macro("ChicagoGameID", chicago$game_id, as.character(chicago$game_id), required[["cases"]], "nba", "case_study", "chicago", "game_id")
add_macro("ChicagoSeason", chicago$season, as.character(chicago$season), required[["cases"]], "nba", "case_study", "chicago", "season")
add_macro("ChicagoDate", chicago$game_date, gsub(" +", " ", trimws(format(as.Date(chicago$game_date), "%B %e, %Y"))), required[["cases"]], "nba", "case_study", "chicago", "game_date")
add_macro("ChicagoWinner", chicago$winner_team, as.character(chicago$winner_team), required[["cases"]], "nba", "case_study", "chicago", "winner")
add_macro("ChicagoLoser", chicago$loser_team, as.character(chicago$loser_team), required[["cases"]], "nba", "case_study", "chicago", "loser")
add_macro("ChicagoStartingWP", chicago$starting_wp_favored, format_percent(chicago$starting_wp_favored), required[["cases"]], "nba", "case_study", "chicago", "starting_wp_favored")
add_macro("ChicagoMaxWP", chicago$max_wp_loser, format_percent(chicago$max_wp_loser), required[["cases"]], "nba", "case_study", "chicago", "max_wp_loser")
add_macro("ChicagoPIT", chicago$pit_u, format_number(chicago$pit_u), required[["cases"]], "nba", "case_study", "chicago", "pit_u")
add_macro("ChicagoTailProbability", chicago$pit_tail_prob, format_percent(chicago$pit_tail_prob), required[["cases"]], "nba", "case_study", "chicago", "pit_tail_prob")
add_macro("ChicagoMinutesRemaining", chicago$peak_first_minutes_remaining, formatC(chicago$peak_first_minutes_remaining, format = "f", digits = 1L), required[["cases"]], "nba", "case_study", "chicago", "minutes_remaining")
add_macro("ChicagoClock", chicago$peak_first_clock_display, format_clock(chicago$peak_first_clock_display), required[["cases"]], "nba", "case_study", "chicago", "clock")
add_macro("ChicagoPeriod", chicago$peak_first_period_number, format_count(chicago$peak_first_period_number), required[["cases"]], "nba", "case_study", "chicago", "period")
add_macro("ChicagoPeakLead", peak_lead, format_count(peak_lead), required[["cases"]], "nba", "case_study", "chicago", "lead")
add_macro("ChicagoPeakHomeScore", chicago$peak_first_home_score, format_count(chicago$peak_first_home_score), required[["cases"]], "nba", "case_study", "chicago", "home_score")
add_macro("ChicagoPeakAwayScore", chicago$peak_first_away_score, format_count(chicago$peak_first_away_score), required[["cases"]], "nba", "case_study", "chicago", "away_score")
add_macro("ChicagoFinalHomeScore", chicago$home_final, format_count(chicago$home_final), required[["cases"]], "nba", "case_study", "chicago", "home_final")
add_macro("ChicagoFinalAwayScore", chicago$away_final, format_count(chicago$away_final), required[["cases"]], "nba", "case_study", "chicago", "away_final")

artifacts <- bind_rows(artifact_rows)
if (anyDuplicated(artifacts$key)) {
  stop("Duplicate generated macro keys: ", paste(unique(artifacts$key[duplicated(artifacts$key)]), collapse = ", "))
}
artifacts <- artifacts |> arrange(key)
csv_path <- file.path(table_dir, "manuscript-results.csv")
tex_path <- file.path(table_dir, "inference-results.tex")
write_csv(artifacts, csv_path, na = "")

tex_lines <- c(
  "% Generated by scripts/manuscript/build-manuscript-artifacts.R; do not edit.",
  vapply(seq_len(nrow(artifacts)), function(i) {
    paste0(
      "\\providecommand{\\", artifacts$key[[i]], "}{",
      artifacts$formatted[[i]], "}"
    )
  }, character(1))
)
writeLines(tex_lines, tex_path, useBytes = TRUE)

written <- readLines(tex_path, warn = FALSE)
expected_text <- paste0(paste(tex_lines, collapse = "\n"), "\n")
written_text <- readChar(tex_path, file.info(tex_path)$size, useBytes = TRUE)
macro_lines <- written[startsWith(written, "\\providecommand{")]
if (!identical(written_text, expected_text) || anyDuplicated(macro_lines)) {
  stop("Generated TeX macros do not agree exactly with manuscript-results.csv.")
}
if (length(macro_lines) != nrow(artifacts)) {
  stop("Generated TeX macro count does not match manuscript-results.csv.")
}

message("Wrote consolidated manuscript values: ", csv_path)
message("Wrote consolidated TeX macros: ", tex_path)
