#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(Matrix)
  library(readr)
  library(scales)
  library(tibble)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  script_path <- normalizePath("scripts/manuscript/dependent-inference.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "R", "dyadic-bootstrap.R"))
source(file.path(repo_root, "R", "plot-style.R"))

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default = NULL) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[[1]])
}

league_arg <- tolower(parse_arg("league", "both"))
replicates <- as.integer(parse_arg("replicates", "9999"))
seed <- as.integer(parse_arg("seed", "20260815"))
if (!league_arg %in% c("both", "nba", "nfl")) {
  stop("--league must be one of both, nba, or nfl.")
}
if (is.na(replicates) || replicates < 1L || is.na(seed)) {
  stop("--replicates must be positive and --seed must be an integer.")
}
leagues <- if (league_arg == "both") c("nba", "nfl") else league_arg
thresholds <- c(0.90, 0.95, 0.99)

table_dir <- file.path(repo_root, "results", "tables", "manuscript")
figure_root <- file.path(
  repo_root,
  "results", "figures", "manuscript", "figures"
)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

new_result <- function(league,
                       analysis,
                       sample,
                       estimand,
                       estimate,
                       threshold = NA_real_,
                       ci_lower = NA_real_,
                       ci_upper = NA_real_,
                       p_value = NA_real_,
                       n = NA_integer_,
                       n_games = NA_integer_,
                       note = NA_character_) {
  tibble(
    league = league,
    analysis = analysis,
    sample = sample,
    estimand = estimand,
    threshold = as.numeric(threshold),
    estimate = as.numeric(estimate),
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    p_value = as.numeric(p_value),
    n = as.integer(n),
    n_games = as.integer(n_games),
    replicates = replicates,
    seed = seed,
    note = note
  )
}

load_game_data <- function(league) {
  data_dir <- file.path(repo_root, "data", "derived", league)
  corrected_path <- file.path(data_dir, "all_games_enriched.csv")
  raw_path <- file.path(data_dir, "all_games.csv")
  if (!file.exists(corrected_path) || !file.exists(raw_path)) {
    stop("Missing game-level files for ", league, ".")
  }

  corrected <- fread(corrected_path, showProgress = FALSE)
  raw <- fread(
    raw_path,
    select = c("game_id", "max_wp_loser", "pit_u"),
    showProgress = FALSE
  )
  setnames(raw, c("max_wp_loser", "pit_u"), c("max_wp_loser_raw", "pit_u_raw"))
  corrected[, game_id := as.character(game_id)]
  raw[, game_id := as.character(game_id)]
  games <- merge(corrected, raw, by = "game_id", all.x = TRUE, sort = FALSE)
  games[, `:=`(
    league = league,
    season = as.character(season),
    game_date = as.Date(game_date),
    home_team_abbr = as.character(home_team_abbr),
    away_team_abbr = as.character(away_team_abbr),
    had_extreme_patch = as.logical(had_extreme_patch),
    had_bad_1 = as.logical(had_bad_1),
    had_bad_0 = as.logical(had_bad_0)
  )]
  setorder(games, season, game_date, game_id)

  required <- c(
    "season", "game_id", "game_date", "home_team_abbr", "away_team_abbr",
    "starting_wp_favored", "max_wp_loser", "pit_u", "pit_u_raw"
  )
  missing <- setdiff(required, names(games))
  if (length(missing) > 0) {
    stop("Missing game fields for ", league, ": ", paste(missing, collapse = ", "))
  }
  if (anyDuplicated(games$game_id)) {
    stop("Duplicate game IDs in ", league, " game data.")
  }
  as_tibble(games)
}

plot_pit_signature <- function(fit, league, out_file) {
  plot_df <- fit$signature
  y_limit <- max(
    0.05,
    1.08 * max(abs(plot_df$upper_gap), fit$critical_value_95, na.rm = TRUE)
  )
  plot <- ggplot(plot_df, aes(t, upper_gap)) +
    annotate(
      "rect",
      xmin = 0.90,
      xmax = 1,
      ymin = -Inf,
      ymax = Inf,
      fill = paper_style$shade,
      alpha = 0.08
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = paper_style$ref) +
    geom_hline(
      yintercept = fit$critical_value_95,
      linetype = "dotdash",
      color = paper_style$shade,
      linewidth = 0.7
    ) +
    geom_path(color = paper_style$ink, linewidth = 1) +
    coord_cartesian(xlim = c(0, 1), ylim = c(-y_limit, y_limit), expand = FALSE) +
    labs(
      x = expression(t),
      y = expression(t - hat(F)[U](t)),
      title = paste0(toupper(league), ": PIT signature"),
      subtitle = "Dot-dash line: 95% season-stratified dyadic critical value"
    ) +
    paper_theme(base_size = 11)
  ggsave(out_file, plot, width = 6, height = 4, dpi = 300)
}

pit_fit_rows <- function(league, sample, fit, include_all_tails = FALSE) {
  rows <- list(
    new_result(
      league,
      "global",
      sample,
      "D_upper",
      fit$statistic,
      p_value = fit$bootstrap_p_value,
      n = fit$n,
      note = "Season-stratified dyadic pigeonhole bootstrap"
    ),
    new_result(
      league,
      "global",
      sample,
      "dyadic_critical_value_95",
      fit$critical_value_95,
      n = fit$n
    )
  )

  tail <- fit$tail
  if (!include_all_tails) {
    tail <- tail[abs(tail$threshold - 0.95) < 1e-8, , drop = FALSE]
  }
  for (i in seq_len(nrow(tail))) {
    rows[[length(rows) + 1L]] <- new_result(
      league,
      "tail",
      sample,
      "tail_rate",
      tail$observed_tail[[i]],
      threshold = tail$threshold[[i]],
      n = fit$n
    )
    rows[[length(rows) + 1L]] <- new_result(
      league,
      "tail",
      sample,
      "tail_excess",
      tail$excess[[i]],
      threshold = tail$threshold[[i]],
      ci_lower = tail$ci_lower[[i]],
      ci_upper = tail$ci_upper[[i]],
      n = fit$n,
      note = "Two-sided 95% percentile dyadic-bootstrap interval"
    )
  }
  bind_rows(rows)
}

bootstrap_crossings <- function(league, design) {
  figure_dir <- file.path(figure_root, league)
  specifications <- list(
    preterminal = "first_crossing_rows_preterminal.csv",
    all_updates = "first_crossing_rows_all.csv"
  )
  result_rows <- list()
  summaries <- list()

  for (sample_name in names(specifications)) {
    path <- file.path(figure_dir, specifications[[sample_name]])
    crossings <- read_csv(path, show_col_types = FALSE) |>
      mutate(
        season = as.character(season),
        game_id = as.character(game_id),
        game_date = as.Date(game_date)
      )
    if (!"boundary" %in% names(crossings)) {
      crossings <- crossings |> mutate(boundary = "weak")
    }

    for (boundary_name in sort(unique(crossings$boundary))) {
      boundary_rows <- crossings |>
        filter(.data$boundary == .env$boundary_name)
      result_sample <- if (boundary_name == "weak") {
        sample_name
      } else {
        paste0(sample_name, "_", boundary_name)
      }

      for (threshold in thresholds) {
        target <- boundary_rows |>
          filter(abs(.data$threshold - .env$threshold) < 1e-8)
        if (nrow(target) == 0L) {
          stop(
            "No ", boundary_name, " ", sample_name,
            " crossings at threshold ", threshold, "."
          )
        }
        fit <- bootstrap_clustered_means(
          target,
          c("loss", "implied_loss", "residual"),
          design
        )
        summary <- tibble(
          league = league,
          sample = sample_name,
          boundary = boundary_name,
          threshold = threshold,
          n = nrow(target),
          n_games = n_distinct(target$game_id),
          observed_loss = unname(fit$estimates[["loss"]]),
          implied_loss = unname(fit$estimates[["implied_loss"]]),
          residual = unname(fit$estimates[["residual"]]),
          loss_ci_lower = fit$intervals["loss", "ci_lower"],
          loss_ci_upper = fit$intervals["loss", "ci_upper"],
          implied_ci_lower = fit$intervals["implied_loss", "ci_lower"],
          implied_ci_upper = fit$intervals["implied_loss", "ci_upper"],
          residual_ci_lower = fit$intervals["residual", "ci_lower"],
          residual_ci_upper = fit$intervals["residual", "ci_upper"]
        )
        summaries[[length(summaries) + 1L]] <- summary

        result_rows[[length(result_rows) + 1L]] <- new_result(
          league,
          "first_crossing",
          result_sample,
          "observed_loss",
          summary$observed_loss,
          threshold = threshold,
          ci_lower = summary$loss_ci_lower,
          ci_upper = summary$loss_ci_upper,
          n = summary$n,
          n_games = summary$n_games
        )
        result_rows[[length(result_rows) + 1L]] <- new_result(
          league,
          "first_crossing",
          result_sample,
          "implied_loss",
          summary$implied_loss,
          threshold = threshold,
          ci_lower = summary$implied_ci_lower,
          ci_upper = summary$implied_ci_upper,
          n = summary$n,
          n_games = summary$n_games
        )
        result_rows[[length(result_rows) + 1L]] <- new_result(
          league,
          "first_crossing",
          result_sample,
          "residual",
          summary$residual,
          threshold = threshold,
          ci_lower = summary$residual_ci_lower,
          ci_upper = summary$residual_ci_upper,
          n = summary$n,
          n_games = summary$n_games,
          note = paste(
            "Boundary:", boundary_name,
            "; each row from the same game receives the same game weight"
          )
        )
      }
    }
  }

  list(results = bind_rows(result_rows), summary = bind_rows(summaries))
}

bootstrap_fixed_clock <- function(league, design, games) {
  figure_dir <- file.path(figure_root, league)
  binned <- read_csv(
    file.path(figure_dir, "fixed_clock_bins.csv"),
    show_col_types = FALSE
  ) |>
    mutate(
      season = as.character(season),
      game_id = as.character(game_id),
      game_date = as.Date(game_date),
      method = as.character(method),
      cell_key = paste(method, time_index, bin_id, sep = ":")
    )
  cells <- binned |>
    distinct(
      cell_key, method, time_index, regulation_fraction, bin_id,
      bin_lower, bin_upper
    ) |>
    arrange(method, time_index, bin_id) |>
    mutate(cell_index = row_number())
  binned <- binned |>
    left_join(cells |> select(cell_key, cell_index), by = "cell_key") |>
    mutate(game_index = match(game_id, games$game_id))
  if (anyNA(binned$game_index) || anyNA(binned$cell_index)) {
    stop("Fixed-clock rows could not be matched to the game-level design.")
  }
  if (nrow(binned |> count(method, time_index, game_id) |> filter(n != 1L)) > 0L) {
    stop("Each fixed-clock game must contribute once at each time point.")
  }

  dimensions <- c(nrow(games), nrow(cells))
  membership <- sparseMatrix(
    i = binned$game_index,
    j = binned$cell_index,
    x = 1,
    dims = dimensions
  )
  gap_values <- sparseMatrix(
    i = binned$game_index,
    j = binned$cell_index,
    x = binned$gap,
    dims = dimensions
  )
  gap_draws <- matrix(
    NA_real_,
    nrow = design$replicates,
    ncol = nrow(cells)
  )
  chunks <- split(
    seq_len(design$replicates),
    ceiling(seq_len(design$replicates) / 200L)
  )
  for (replicate_index in chunks) {
    weights <- resample_weight_chunk(design, games, replicate_index)
    denominator <- as.matrix(weights %*% membership)
    if (any(denominator <= 0)) {
      stop("A fixed-clock bootstrap cell received zero game weight.")
    }
    gap_draws[replicate_index, ] <-
      as.matrix(weights %*% gap_values) / denominator
  }

  table <- binned |>
    group_by(cell_index) |>
    summarise(
      n_games = n(),
      mean_forecast = mean(p),
      observed_rate = mean(y),
      gap = mean(gap),
      .groups = "drop"
    ) |>
    left_join(cells, by = "cell_index") |>
    arrange(cell_index)
  table$standard_error <- NA_real_
  table$simultaneous_critical_value <- NA_real_
  table$gap_ci_lower <- NA_real_
  table$gap_ci_upper <- NA_real_

  for (method_name in unique(table$method)) {
    columns <- which(table$method == method_name)
    centered <- sweep(
      gap_draws[, columns, drop = FALSE],
      2,
      table$gap[columns],
      FUN = "-"
    )
    standard_error <- apply(centered, 2, stats::sd)
    usable <- is.finite(standard_error) & standard_error > 0
    if (!any(usable)) stop("No usable fixed-clock standard errors for ", method_name, ".")
    standardized <- sweep(
      centered[, usable, drop = FALSE],
      2,
      standard_error[usable],
      FUN = "/"
    )
    max_t <- apply(abs(standardized), 1, max)
    critical_value <- as.numeric(stats::quantile(max_t, 0.95, names = FALSE))
    table$standard_error[columns] <- standard_error
    table$simultaneous_critical_value[columns] <- critical_value
    table$gap_ci_lower[columns] <- table$gap[columns] - critical_value * standard_error
    table$gap_ci_upper[columns] <- table$gap[columns] + critical_value * standard_error
  }
  table <- table |>
    mutate(
      league = league,
      significant = gap_ci_lower > 0 | gap_ci_upper < 0,
      observed_ci_lower = mean_forecast + gap_ci_lower,
      observed_ci_upper = mean_forecast + gap_ci_upper
    ) |>
    select(
      league, method, time_index, regulation_fraction, bin_id,
      bin_lower, bin_upper, n_games, mean_forecast, observed_rate, gap,
      standard_error, simultaneous_critical_value,
      gap_ci_lower, gap_ci_upper, observed_ci_lower, observed_ci_upper,
      significant
    )

  envelope <- table |>
    group_by(league, method, time_index, regulation_fraction) |>
    summarise(
      n_games = sum(n_games),
      n_bins = n(),
      gap_min = min(gap),
      gap_max = max(gap),
      band_lower = min(gap_ci_lower),
      band_upper = max(gap_ci_upper),
      max_abs_gap = max(abs(gap)),
      significant_cells = sum(significant),
      .groups = "drop"
    )
  profile_summary <- table |>
    group_by(league, method) |>
    summarise(
      rms_gap = sqrt(weighted.mean(gap^2, n_games)),
      maximum_absolute_gap = max(abs(gap)),
      significant_cells = sum(significant),
      n_cells = n(),
      .groups = "drop"
    )

  result_rows <- lapply(seq_len(nrow(table)), function(i) {
    new_result(
      league,
      "fixed_clock",
      table$method[[i]],
      sprintf("gap_t%02d_bin%02d", table$time_index[[i]], table$bin_id[[i]]),
      table$gap[[i]],
      threshold = table$regulation_fraction[[i]],
      ci_lower = table$gap_ci_lower[[i]],
      ci_upper = table$gap_ci_upper[[i]],
      n = table$n_games[[i]],
      n_games = table$n_games[[i]],
      note = "One game per fixed time; simultaneous dyadic-bootstrap band"
    )
  })
  for (i in seq_len(nrow(profile_summary))) {
    result_rows[[length(result_rows) + 1L]] <- new_result(
      league,
      "fixed_clock_summary",
      profile_summary$method[[i]],
      "rms_gap",
      profile_summary$rms_gap[[i]],
      n = profile_summary$n_cells[[i]],
      note = "Descriptive RMS gap across fixed-time adaptive bins"
    )
  }

  for (method_name in unique(table$method)) {
    plot_data <- table |> filter(.data$method == .env$method_name)
    fill_limit <- max(abs(c(plot_data$gap_ci_lower, plot_data$gap_ci_upper)), na.rm = TRUE)
    surface <- ggplot(
      plot_data,
      aes(regulation_fraction, mean_forecast, fill = gap)
    ) +
      geom_point(shape = 22, size = 3.2, color = "white", stroke = 0.25) +
      geom_point(
        data = plot_data |> filter(significant),
        shape = 22,
        size = 3.2,
        fill = NA,
        color = paper_style$ink,
        stroke = 0.65
      ) +
      scale_fill_gradient2(
        low = "#2166AC",
        mid = "#F7F7F7",
        high = "#B2182B",
        midpoint = 0,
        limits = c(-fill_limit, fill_limit),
        labels = percent_format(accuracy = 1),
        name = "Observed - forecast"
      ) +
      scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2)) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(
        title = paste0(toupper(league), ": fixed-clock calibration surface"),
        subtitle = paste0(
          ifelse(method_name == "linear", "Linear interpolation", "LOCF sensitivity"),
          "; outlined cells exclude zero under the simultaneous band"
        ),
        x = "Fraction of regulation elapsed",
        y = "Mean published win probability"
      ) +
      paper_theme(base_size = 10)
    ggsave(
      file.path(figure_dir, paste0("fixed_clock_surface_", method_name, ".png")),
      surface,
      width = 7,
      height = 4.5,
      dpi = 300
    )
  }

  list(
    results = bind_rows(result_rows),
    summary = table,
    envelope = envelope,
    profile_summary = profile_summary
  )
}

audit_missingness <- function(league, games) {
  coverage_path <- file.path(
    repo_root,
    "data", "derived", league, "feed_coverage_by_season.csv"
  )
  if (file.exists(coverage_path)) {
    return(
      read_csv(coverage_path, show_col_types = FALSE) |>
        transmute(
          league = league,
          season = as.character(season),
          scheduled_games = as.integer(scheduled_games),
          eligible_games = as.integer(eligible_games),
          observed_games = as.integer(analyzed_games),
          missing_games = as.integer(missing_games),
          coverage = as.numeric(feed_coverage),
          audit_status = "schedule inventory"
        )
    )
  }
  observed <- games |>
    count(season, name = "observed_games")
  missing_path <- file.path(
    repo_root,
    "data", "derived", league, "missing_wp_games.csv"
  )
  if (file.exists(missing_path)) {
    missing <- read_csv(missing_path, show_col_types = FALSE) |>
      transmute(season = as.character(season), game_id = as.character(game_id)) |>
      distinct() |>
      count(season, name = "missing_games")
    audit <- observed |>
      left_join(missing, by = "season") |>
      mutate(
        missing_games = coalesce(missing_games, 0L),
        coverage = observed_games / (observed_games + missing_games),
        audit_status = "enumerated"
      )
  } else {
    audit <- observed |>
      mutate(
        missing_games = NA_integer_,
        coverage = NA_real_,
        audit_status = "no separate missing-feed inventory"
      )
  }
  audit |> mutate(league = league, .before = 1)
}

audit_patches <- function(league, games) {
  games |>
    filter(had_extreme_patch) |>
    transmute(
      league,
      season,
      game_id,
      game_date,
      home_team_abbr,
      away_team_abbr,
      had_bad_1,
      had_bad_0,
      n_bad_1_rows,
      n_bad_1_runs,
      max_wp_loser_raw,
      max_wp_loser_corrected = max_wp_loser,
      pit_u_raw,
      pit_u_corrected = pit_u
    )
}

format_count <- function(x) {
  format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_number <- function(x, digits = 3) {
  formatC(x, format = "f", digits = digits)
}

format_percent <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "\\%")
}

format_p_tex <- function(p) {
  if (!is.finite(p)) {
    return("NA")
  }
  if (p < 0.001) {
    return("\\ensuremath{<0.001}")
  }
  paste0("\\ensuremath{", formatC(p, format = "f", digits = 3), "}")
}

lookup_result <- function(results,
                          league,
                          analysis,
                          sample,
                          estimand,
                          threshold = NA_real_) {
  hit <- results |>
    filter(
      .data$league == .env$league,
      .data$analysis == .env$analysis,
      .data$sample == .env$sample,
      .data$estimand == .env$estimand
    )
  if (is.finite(threshold)) {
    hit <- hit |> filter(abs(.data$threshold - .env$threshold) < 1e-8)
  } else {
    hit <- hit |> filter(is.na(.data$threshold))
  }
  if (nrow(hit) != 1L) {
    stop(
      "Expected one result for ",
      paste(league, analysis, sample, estimand, threshold, sep = "/"),
      "; found ", nrow(hit), "."
    )
  }
  hit
}

write_macro_file <- function(results, patch_audit, missing_audit, output_path) {
  lines <- c(
    "% Generated by scripts/manuscript/dependent-inference.R; do not edit.",
    paste0(
      "\\providecommand{\\DyadicBootstrapReplicates}{",
      format_count(replicates),
      "}"
    ),
    paste0("\\providecommand{\\DyadicBootstrapSeed}{", seed, "}")
  )
  threshold_names <- c(
    `0.9` = "Ninety",
    `0.95` = "NinetyFive",
    `0.99` = "NinetyNine"
  )

  for (league in leagues) {
    prefix <- toupper(league)
    global <- lookup_result(results, league, "global", "corrected", "D_upper")
    critical <- lookup_result(
      results,
      league,
      "global",
      "corrected",
      "dyadic_critical_value_95"
    )
    lines <- c(
      lines,
      paste0("\\providecommand{\\", prefix, "Games}{", format_count(global$n), "}"),
      paste0(
        "\\providecommand{\\", prefix, "GlobalD}{",
        format_number(global$estimate), "}"
      ),
      paste0(
        "\\providecommand{\\", prefix, "GlobalDyadicP}{",
        format_p_tex(global$p_value), "}"
      ),
      paste0(
        "\\providecommand{\\", prefix, "GlobalCritical}{",
        format_number(critical$estimate), "}"
      )
    )

    for (threshold in thresholds) {
      name <- threshold_names[[as.character(threshold)]]
      rate <- lookup_result(
        results,
        league,
        "tail",
        "corrected",
        "tail_rate",
        threshold
      )
      excess <- lookup_result(
        results,
        league,
        "tail",
        "corrected",
        "tail_excess",
        threshold
      )
      crossing_loss <- lookup_result(
        results,
        league,
        "first_crossing",
        "preterminal",
        "observed_loss",
        threshold
      )
      crossing_implied <- lookup_result(
        results,
        league,
        "first_crossing",
        "preterminal",
        "implied_loss",
        threshold
      )
      crossing_residual <- lookup_result(
        results,
        league,
        "first_crossing",
        "preterminal",
        "residual",
        threshold
      )
      lines <- c(
        lines,
        paste0(
          "\\providecommand{\\", prefix, "Tail", name, "Rate}{",
          format_percent(rate$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Tail", name, "Excess}{",
          format_percent(excess$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Tail", name, "CILower}{",
          format_percent(excess$ci_lower), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Tail", name, "CIUpper}{",
          format_percent(excess$ci_upper), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Crossing", name, "Episodes}{",
          format_count(crossing_loss$n), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Crossing", name, "Games}{",
          format_count(crossing_loss$n_games), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Crossing", name, "Loss}{",
          format_percent(crossing_loss$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Crossing", name, "Implied}{",
          format_percent(crossing_implied$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Crossing", name, "Residual}{",
          format_percent(crossing_residual$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Crossing", name, "CILower}{",
          format_percent(crossing_residual$ci_lower), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "Crossing", name, "CIUpper}{",
          format_percent(crossing_residual$ci_upper), "}"
        )
      )
    }

    strict_tail <- lookup_result(
      results,
      league,
      "boundary",
      "strict",
      "tail_rate",
      0.95
    )
    atom_tail <- lookup_result(
      results,
      league,
      "boundary",
      "atom",
      "tail_rate",
      0.95
    )
    strict_crossing <- lapply(
      c("observed_loss", "implied_loss", "residual"),
      function(estimand) {
        lookup_result(
          results,
          league,
          "first_crossing",
          "preterminal_strict",
          estimand,
          0.95
        )
      }
    )
    names(strict_crossing) <- c("loss", "implied", "residual")
    all_crossing <- lapply(
      c("observed_loss", "implied_loss", "residual"),
      function(estimand) {
        lookup_result(
          results,
          league,
          "first_crossing",
          "all_updates",
          estimand,
          0.95
        )
      }
    )
    names(all_crossing) <- c("loss", "implied", "residual")
    lines <- c(
      lines,
      paste0(
        "\\providecommand{\\", prefix, "TailNinetyFiveStrictRate}{",
        format_percent(strict_tail$estimate), "}"
      ),
      paste0(
        "\\providecommand{\\", prefix, "TailNinetyFiveAtomRate}{",
        format_percent(atom_tail$estimate, digits = 2), "}"
      )
    )
    for (boundary_name in c("Strict", "All")) {
      crossing <- if (boundary_name == "Strict") {
        strict_crossing
      } else {
        all_crossing
      }
      lines <- c(
        lines,
        paste0(
          "\\providecommand{\\", prefix, "CrossingNinetyFive",
          boundary_name, "Episodes}{", format_count(crossing$loss$n), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "CrossingNinetyFive",
          boundary_name, "Games}{", format_count(crossing$loss$n_games), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "CrossingNinetyFive",
          boundary_name, "Loss}{", format_percent(crossing$loss$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "CrossingNinetyFive",
          boundary_name, "Implied}{",
          format_percent(crossing$implied$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "CrossingNinetyFive",
          boundary_name, "Residual}{",
          format_percent(crossing$residual$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "CrossingNinetyFive",
          boundary_name, "CILower}{",
          format_percent(crossing$residual$ci_lower), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, "CrossingNinetyFive",
          boundary_name, "CIUpper}{",
          format_percent(crossing$residual$ci_upper), "}"
        )
      )
    }

    fixed_clock_names <- c(
      linear = "Linear",
      locf = "LOCF"
    )
    for (sample_name in names(fixed_clock_names)) {
      fixed_clock <- lookup_result(
        results,
        league,
        "fixed_clock_summary",
        sample_name,
        "rms_gap"
      )
      lines <- c(
        lines,
        paste0(
          "\\providecommand{\\", prefix, "FixedClock",
          fixed_clock_names[[sample_name]], "RMS}{",
          format_percent(fixed_clock$estimate), "}"
        )
      )
    }

    sensitivity_names <- c(
      raw = "Raw",
      exclude_patched = "ExcludePatched",
      rounding_lower = "RoundingLower",
      rounding_upper = "RoundingUpper",
      dyadic_calendar = "DyadicCalendar",
      franchise_linked = "FranchiseLinked"
    )
    for (sample_name in names(sensitivity_names)) {
      hit <- lookup_result(
        results,
        league,
        "global",
        sample_name,
        "D_upper"
      )
      macro_name <- sensitivity_names[[sample_name]]
      lines <- c(
        lines,
        paste0(
          "\\providecommand{\\", prefix, macro_name, "GlobalD}{",
          format_number(hit$estimate), "}"
        ),
        paste0(
          "\\providecommand{\\", prefix, macro_name, "GlobalP}{",
          format_p_tex(hit$p_value), "}"
        )
      )
    }

    league_patches <- patch_audit |> filter(.data$league == .env$league)
    lines <- c(
      lines,
      paste0(
        "\\providecommand{\\", prefix, "PatchedGames}{",
        format_count(nrow(league_patches)), "}"
      )
    )
    league_missing <- missing_audit |>
      filter(.data$league == .env$league, is.finite(.data$coverage))
    if (nrow(league_missing) > 0) {
      lines <- c(
        lines,
        paste0(
          "\\providecommand{\\", prefix, "MinimumFeedCoverage}{",
          format_percent(min(league_missing$coverage)), "}"
        )
      )
    } else {
      lines <- c(
        lines,
        paste0(
          "\\providecommand{\\", prefix, "MinimumFeedCoverage}{NA}"
        )
      )
    }

    loso_d <- results |>
      filter(
        .data$league == .env$league,
        analysis == "leave_one_season_out",
        estimand == "D_upper"
      ) |>
      arrange(sample)
    loso_tail <- results |>
      filter(
        .data$league == .env$league,
        analysis == "leave_one_season_out",
        estimand == "tail_excess",
        abs(threshold - 0.95) < 1e-8
      ) |>
      arrange(sample)
    if (nrow(loso_d) != nrow(loso_tail) || nrow(loso_d) == 0L) {
      stop("Incomplete leave-one-season-out results for ", league, ".")
    }
    lines <- c(
      lines,
      paste0(
        "\\providecommand{\\", prefix, "LOSODMin}{",
        format_number(min(loso_d$estimate)), "}"
      ),
      paste0(
        "\\providecommand{\\", prefix, "LOSODMax}{",
        format_number(max(loso_d$estimate)), "}"
      ),
      paste0(
        "\\providecommand{\\", prefix, "LOSOTailMin}{",
        format_percent(min(loso_tail$estimate)), "}"
      ),
      paste0(
        "\\providecommand{\\", prefix, "LOSOTailMax}{",
        format_percent(max(loso_tail$estimate)), "}"
      ),
      paste0("\\providecommand{\\", prefix, "LeaveSeasonRows}{")
    )
    for (i in seq_len(nrow(loso_d))) {
      lines <- c(
        lines,
        paste0(
          loso_d$sample[[i]], " & ",
          format_count(loso_d$n[[i]]), " & ",
          format_number(loso_d$estimate[[i]]), " & ",
          format_percent(loso_tail$estimate[[i]]), " \\\\"
        )
      )
    }
    lines <- c(lines, "}")

    league_missing_all <- missing_audit |>
      filter(.data$league == .env$league) |>
      arrange(season)
    lines <- c(
      lines,
      paste0("\\providecommand{\\", prefix, "MissingCoverageRows}{")
    )
    for (i in seq_len(nrow(league_missing_all))) {
      missing_value <- if (is.na(league_missing_all$missing_games[[i]])) {
        "--"
      } else {
        format_count(league_missing_all$missing_games[[i]])
      }
      coverage_value <- if (is.na(league_missing_all$coverage[[i]])) {
        "--"
      } else {
        format_percent(league_missing_all$coverage[[i]])
      }
      lines <- c(
        lines,
        paste0(
          league_missing_all$season[[i]], " & ",
          format_count(league_missing_all$observed_games[[i]]), " & ",
          missing_value, " & ", coverage_value, " \\\\"
        )
      )
    }
    lines <- c(lines, "}")
  }

  stop(
    "Direct TeX macro generation is retired; use ",
    "scripts/manuscript/build-manuscript-artifacts.R."
  )
}

all_results <- list()
all_crossings <- list()
all_fixed_clock <- list()
all_fixed_envelopes <- list()
all_fixed_profiles <- list()
all_patches <- list()
all_missing <- list()
all_loso <- list()
all_specifications <- list()
all_rounding <- list()

for (league in leagues) {
  message(
    sprintf(
      "%s: constructing %s season-stratified dyadic draws (seed %s)",
      toupper(league),
      format_count(replicates),
      seed
    )
  )
  games <- load_game_data(league)
  design <- make_dyadic_design(games, replicates = replicates, seed = seed)

  rounding <- rounding_pit_bounds(
    games$max_wp_loser,
    games$starting_wp_favored
  )
  all_rounding[[league]] <- games |>
    select(
      league, season, game_id, game_date,
      home_team_abbr, away_team_abbr, pit_u
    ) |>
    bind_cols(as_tibble(rounding)) |>
    mutate(
      tail_90_lower = u_lower >= 0.90,
      tail_90_upper = u_upper >= 0.90,
      tail_95_lower = u_lower >= 0.95,
      tail_95_upper = u_upper >= 0.95,
      tail_99_lower = u_lower >= 0.99,
      tail_99_upper = u_upper >= 0.99
    )
  pit_specs <- list(
    corrected = list(data = games, u = games$pit_u),
    raw = list(data = games, u = games$pit_u_raw),
    exclude_patched = list(
      data = games |> filter(!had_extreme_patch),
      u = games$pit_u[!games$had_extreme_patch]
    ),
    rounding_lower = list(data = games, u = rounding$u_lower),
    rounding_upper = list(data = games, u = rounding$u_upper)
  )
  pit_fits <- list()
  for (sample_name in names(pit_specs)) {
    message(toupper(league), ": PIT specification ", sample_name)
    spec <- pit_specs[[sample_name]]
    fit <- bootstrap_pit_distribution(
      spec$data,
      spec$u,
      design,
      thresholds = thresholds
    )
    pit_fits[[sample_name]] <- fit
    all_results[[length(all_results) + 1L]] <- pit_fit_rows(
      league,
      sample_name,
      fit,
      include_all_tails = TRUE
    )
  }

  calendar_design <- make_calendar_design(
    games,
    replicates = replicates,
    seed = seed + 1L,
    block_length = 2L
  )
  combined_design <- combine_dyadic_calendar_designs(design, calendar_design)
  message(toupper(league), ": dyad-plus-calendar sensitivity")
  calendar_fit <- bootstrap_pit_distribution(
    games,
    games$pit_u,
    combined_design,
    thresholds = thresholds
  )
  all_results[[length(all_results) + 1L]] <- pit_fit_rows(
    league,
    "dyadic_calendar",
    calendar_fit,
    include_all_tails = FALSE
  )

  franchise_design <- make_franchise_design(
    games,
    replicates = replicates,
    seed = seed
  )
  message(toupper(league), ": franchise-linked cross-season sensitivity")
  franchise_fit <- bootstrap_pit_distribution(
    games,
    games$pit_u,
    franchise_design,
    thresholds = thresholds
  )
  all_results[[length(all_results) + 1L]] <- pit_fit_rows(
    league,
    "franchise_linked",
    franchise_fit,
    include_all_tails = FALSE
  )

  figure_dir <- file.path(figure_root, league)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  plot_pit_signature(
    pit_fits$corrected,
    league,
    file.path(figure_dir, "pit.png")
  )
  write_csv(
    tibble(
      league = toupper(league),
      n = pit_fits$corrected$n,
      prop_90 = pit_fits$corrected$tail$observed_tail[[1]],
      prop_95 = pit_fits$corrected$tail$observed_tail[[2]],
      prop_99 = pit_fits$corrected$tail$observed_tail[[3]],
      Dn_upper = pit_fits$corrected$statistic,
      dyadic_pvalue = pit_fits$corrected$bootstrap_p_value,
      dyadic_critical_value_95 = pit_fits$corrected$critical_value_95
    ),
    file.path(figure_dir, "pit_summary.csv")
  )
  write_csv(
    pit_fits$corrected$draws,
    file.path(table_dir, paste0("bootstrap_draws_", league, ".csv.gz"))
  )

  for (threshold in thresholds) {
    weak_rate <- mean(games$pit_u >= threshold)
    strict_rate <- mean(games$pit_u > threshold)
    atom_rate <- mean(games$pit_u == threshold)
    all_results[[length(all_results) + 1L]] <- bind_rows(
      new_result(
        league,
        "boundary",
        "weak",
        "tail_rate",
        weak_rate,
        threshold = threshold,
        n = nrow(games)
      ),
      new_result(
        league,
        "boundary",
        "strict",
        "tail_rate",
        strict_rate,
        threshold = threshold,
        n = nrow(games)
      ),
      new_result(
        league,
        "boundary",
        "atom",
        "tail_rate",
        atom_rate,
        threshold = threshold,
        n = nrow(games)
      )
    )
  }

  crossing <- bootstrap_crossings(league, design)
  fixed_clock <- bootstrap_fixed_clock(league, design, games)
  all_results[[length(all_results) + 1L]] <- crossing$results
  all_results[[length(all_results) + 1L]] <- fixed_clock$results
  all_crossings[[league]] <- crossing$summary
  all_fixed_clock[[league]] <- fixed_clock$summary
  all_fixed_envelopes[[league]] <- fixed_clock$envelope
  all_fixed_profiles[[league]] <- fixed_clock$profile_summary

  loso <- bind_rows(lapply(sort(unique(games$season)), function(held_out) {
    target <- games |> filter(season != held_out)
    tibble(
      league = league,
      omitted_season = held_out,
      n = nrow(target),
      D_upper = ks_upper_stat(target$pit_u),
      tail_rate_95 = mean(target$pit_u >= 0.95),
      tail_excess_95 = mean(target$pit_u >= 0.95) - 0.05
    )
  }))
  all_loso[[league]] <- loso
  for (i in seq_len(nrow(loso))) {
    all_results[[length(all_results) + 1L]] <- bind_rows(
      new_result(
        league,
        "leave_one_season_out",
        loso$omitted_season[[i]],
        "D_upper",
        loso$D_upper[[i]],
        n = loso$n[[i]],
        note = "Descriptive; seven seasons do not support season-level inference"
      ),
      new_result(
        league,
        "leave_one_season_out",
        loso$omitted_season[[i]],
        "tail_excess",
        loso$tail_excess_95[[i]],
        threshold = 0.95,
        n = loso$n[[i]],
        note = "Descriptive"
      )
    )
  }

  patches <- audit_patches(league, games)
  missing <- audit_missingness(league, games)
  all_patches[[league]] <- patches
  all_missing[[league]] <- missing
  all_results[[length(all_results) + 1L]] <- new_result(
    league,
    "artifact_audit",
    "exact_one_correction",
    "patched_games",
    nrow(patches),
    n = nrow(games)
  )
  for (i in seq_len(nrow(missing))) {
    all_results[[length(all_results) + 1L]] <- new_result(
      league,
      "missingness",
      missing$season[[i]],
      "feed_coverage",
      missing$coverage[[i]],
      n = missing$observed_games[[i]],
      note = missing$audit_status[[i]]
    )
  }

  specification <- bind_rows(lapply(names(pit_fits), function(sample_name) {
    fit <- pit_fits[[sample_name]]
    tail_95 <- fit$tail |> filter(abs(threshold - 0.95) < 1e-8)
    tibble(
      league = league,
      specification = sample_name,
      D_upper = fit$statistic,
      dyadic_p_value = fit$bootstrap_p_value,
      tail_rate_95 = tail_95$observed_tail,
      tail_excess_95 = tail_95$excess,
      tail_ci_lower_95 = tail_95$ci_lower,
      tail_ci_upper_95 = tail_95$ci_upper
    )
  })) |>
    bind_rows(tibble(
      league = league,
      specification = "dyadic_calendar",
      D_upper = calendar_fit$statistic,
      dyadic_p_value = calendar_fit$bootstrap_p_value,
      tail_rate_95 = calendar_fit$tail$observed_tail[[2]],
      tail_excess_95 = calendar_fit$tail$excess[[2]],
      tail_ci_lower_95 = calendar_fit$tail$ci_lower[[2]],
      tail_ci_upper_95 = calendar_fit$tail$ci_upper[[2]]
    )) |>
    bind_rows(tibble(
      league = league,
      specification = "franchise_linked",
      D_upper = franchise_fit$statistic,
      dyadic_p_value = franchise_fit$bootstrap_p_value,
      tail_rate_95 = franchise_fit$tail$observed_tail[[2]],
      tail_excess_95 = franchise_fit$tail$excess[[2]],
      tail_ci_lower_95 = franchise_fit$tail$ci_lower[[2]],
      tail_ci_upper_95 = franchise_fit$tail$ci_upper[[2]]
    ))
  all_specifications[[league]] <- specification

  message(sprintf(
    "%s primary: D=%.4f dyadic p=%.4g; 0.95 excess=%.4f",
    toupper(league),
    pit_fits$corrected$statistic,
    pit_fits$corrected$bootstrap_p_value,
    pit_fits$corrected$tail$excess[[2]]
  ))
}

results <- bind_rows(all_results)
crossing_summary <- bind_rows(all_crossings)
fixed_clock_summary <- bind_rows(all_fixed_clock)
fixed_clock_envelope <- bind_rows(all_fixed_envelopes)
fixed_clock_profiles <- bind_rows(all_fixed_profiles)
patch_audit <- bind_rows(all_patches)
missing_audit <- bind_rows(all_missing)
loso_summary <- bind_rows(all_loso)
specification_summary <- bind_rows(all_specifications)
rounding_audit <- bind_rows(all_rounding)

results_path <- file.path(table_dir, "dependent-inference-results.csv")
write_csv(results, results_path, na = "")
write_csv(crossing_summary, file.path(table_dir, "first-crossing-inference.csv"))
write_csv(
  fixed_clock_summary,
  file.path(table_dir, "fixed-clock-calibration-bands.csv")
)
write_csv(
  fixed_clock_envelope,
  file.path(table_dir, "fixed-clock-calibration-envelope.csv")
)
write_csv(
  fixed_clock_profiles,
  file.path(table_dir, "fixed-clock-calibration-summary.csv")
)
write_csv(patch_audit, file.path(table_dir, "patch-audit.csv"))
write_csv(missing_audit, file.path(table_dir, "missing-feed-coverage.csv"))
write_csv(loso_summary, file.path(table_dir, "leave-one-season-out.csv"))
write_csv(
  specification_summary,
  file.path(table_dir, "specification-sensitivity.csv")
)
write_csv(rounding_audit, file.path(table_dir, "rounding-pit-bounds.csv"))

main_envelope <- fixed_clock_envelope |>
  filter(method == "linear") |>
  mutate(league_label = toupper(league))
envelope_plot <- ggplot(main_envelope, aes(regulation_fraction)) +
  geom_ribbon(
    aes(ymin = band_lower, ymax = band_upper),
    fill = paper_style$shade,
    alpha = 0.16
  ) +
  geom_ribbon(
    aes(ymin = gap_min, ymax = gap_max),
    fill = paper_style$shade,
    alpha = 0.48
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = paper_style$ref) +
  facet_wrap(~ league_label, nrow = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Fraction of regulation elapsed",
    y = "Observed win rate minus mean forecast",
    subtitle = paste(
      "Dark envelope: adaptive-bin gaps; light envelope:",
      "simultaneous dyadic-bootstrap limits"
    )
  ) +
  paper_theme(base_size = 10)
ggsave(
  file.path(figure_root, "fixed_clock_calibration_envelope.png"),
  envelope_plot,
  width = 7,
  height = 3.25,
  dpi = 300
)

if (all(c("nba", "nfl") %in% leagues)) {
  nba_global <- lookup_result(results, "nba", "global", "corrected", "D_upper")
  nfl_global <- lookup_result(results, "nfl", "global", "corrected", "D_upper")
  nba_tail <- lookup_result(
    results, "nba", "tail", "corrected", "tail_excess", 0.95
  )
  nba_crossing <- lookup_result(
    results, "nba", "first_crossing", "preterminal", "residual", 0.95
  )
  acceptance <- c(
    nba_global$p_value <= 0.05,
    nfl_global$p_value > 0.05,
    nba_tail$ci_lower > 0,
    nba_crossing$ci_lower > 0
  )
  if (!all(acceptance)) {
    message(
      "Acceptance pattern differs from the provisional analysis; ",
      "the disagreement has been retained in generated outputs."
    )
  }
}

message("Wrote canonical results: ", results_path)
