suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath("scripts/manuscript/case-study-paths.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "scripts", "manuscript", "espn_enrich_utils.R"))
source(file.path(repo_root, "R", "plot-style.R"))
source(file.path(repo_root, "R", "path-io.R"))

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default = NULL) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[[1]])
}

out_dir <- file.path(repo_root, "results", "figures", "manuscript", "figures", "case_studies")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

format_clock_display <- function(x) {
  x_chr <- as.character(x)
  x_chr <- sub("^00:", "", x_chr)
  sub("^(\\d{1,2}:\\d{2}):00$", "\\1", x_chr)
}

with_effective_probabilities <- function(path_df) {
  for (nm in c(
    "home_wp_corrected",
    "away_wp_corrected",
    "loser_wp_corrected",
    "winner_wp_corrected"
  )) {
    if (!nm %in% names(path_df)) {
      path_df[[nm]] <- NA_real_
    }
  }

  path_df |>
    mutate(
      home_wp_plot = dplyr::coalesce(as.numeric(home_wp_corrected), as.numeric(home_wp)),
      away_wp_plot = dplyr::coalesce(as.numeric(away_wp_corrected), as.numeric(away_wp)),
      loser_wp_plot = dplyr::coalesce(as.numeric(loser_wp_corrected), as.numeric(loser_wp)),
      winner_wp_plot = dplyr::coalesce(as.numeric(winner_wp_corrected), as.numeric(winner_wp))
    )
}

load_case_study_catalog <- function(league) {
  enriched_path <- file.path(repo_root, "data", "derived", league, "all_games_enriched.csv")
  fallback_path <- file.path(repo_root, "data", "derived", league, "all_games.csv")

  if (file.exists(enriched_path)) {
    catalog <- readr::read_csv(enriched_path, show_col_types = FALSE)
  } else {
    catalog <- readr::read_csv(fallback_path, show_col_types = FALSE) |>
      mutate(
        pit_u = pit_cdf_two_team(max_wp_loser, starting_wp_favored),
        pit_tail_prob = 1 - pit_u
      )
  }

  catalog |>
    mutate(game_id = as.character(game_id)) |>
    filter(max_wp_loser < 0.9995) |>
    arrange(pit_tail_prob, desc(max_wp_loser), desc(starting_wp_favored))
}

load_or_fetch_case_study <- function(league, game_id, season) {
  data_dir <- file.path(repo_root, "data", "derived", league)
  season_paths <- resolve_path_file(data_dir, season, required = FALSE)
  season_summary <- file.path(repo_root, "data", "derived", league, sprintf("%s_games_enriched.csv", season))
  raw_paths_file <- file.path(repo_root, "data", "derived", league, sprintf("%s_wp_paths.csv", season))

  if (!is.null(season_paths) && file.exists(season_summary)) {
    path_df <- read_path_data(season_paths) |>
      tibble::as_tibble() |>
      mutate(
        game_id = as.character(game_id),
        play_id = as.character(play_id),
        sequence_number = as.character(sequence_number)
      ) |>
      filter(game_id == !!game_id)
    summary_df <- readr::read_csv(season_summary, show_col_types = FALSE) |>
      mutate(game_id = as.character(game_id)) |>
      filter(game_id == !!game_id)
    if (nrow(path_df) > 0 && nrow(summary_df) > 0) {
      return(list(paths = path_df, summary = summary_df))
    }
  }

  schedule_df <- if (league == "nfl") build_nfl_schedule(season) else build_nba_schedule(season)
  schedule_row <- schedule_df |>
    filter(game_id == !!game_id) |>
    slice_head(n = 1)
  if (nrow(schedule_row) == 0) {
    stop("Schedule row not found for ", league, " game ", game_id)
  }

  if (file.exists(raw_paths_file)) {
    raw_path_df <- readr::read_csv(raw_paths_file, show_col_types = FALSE) |>
      mutate(
        game_id = as.character(game_id),
        play_id = as.character(play_id),
        sequence_number = as.character(sequence_number)
      ) |>
      filter(game_id == !!game_id)

    if (nrow(raw_path_df) > 0) {
      if (league == "nfl") {
        return(enrich_nfl_game_from_raw(raw_path_df, schedule_row))
      }
      return(enrich_nba_game_from_raw(raw_path_df, schedule_row))
    }
  }

  if (league == "nfl") {
    raw_path_df <- fetch_nfl_wp_raw(game_id)
    enrich_nfl_game_from_raw(raw_path_df, schedule_row)
  } else {
    raw_path_df <- fetch_nba_wp_raw(game_id)
    enrich_nba_game_from_raw(raw_path_df, schedule_row)
  }
}

case_study_observed_share <- function(game_obj) {
  path_df <- with_effective_probabilities(game_obj$paths)
  mean(
    !is.na(path_df$game_seconds_remaining) &
      !is.na(path_df$home_wp_plot) &
      !is.na(path_df$away_wp_plot)
  )
}

ensure_terminal_path_point <- function(path_df, summary_df) {
  path_df <- with_effective_probabilities(path_df)
  home_terminal_wp <- if (isTRUE(summary_df$home_won[[1]])) 1 else 0
  away_terminal_wp <- 1 - home_terminal_wp
  terminal_period <- if (all(is.na(path_df$period_number))) {
    NA_real_
  } else {
    max(path_df$period_number, na.rm = TRUE)
  }

  terminal_exists <- any(
    !is.na(path_df$game_seconds_remaining) &
      abs(path_df$game_seconds_remaining) < 1e-8 &
      abs(path_df$home_wp_plot - home_terminal_wp) < 1e-8 &
      abs(path_df$away_wp_plot - away_terminal_wp) < 1e-8,
    na.rm = TRUE
  )

  if (terminal_exists) {
    return(path_df)
  }

  terminal_row <- path_df |>
    slice_tail(n = 1) |>
    mutate(
      game_seconds_remaining = 0,
      period_number = terminal_period,
      clock_display_value = "0.0",
      home_wp = home_terminal_wp,
      away_wp = away_terminal_wp,
      loser_wp = if_else(summary_df$loser_side[[1]] == "home", home_terminal_wp, away_terminal_wp),
      winner_wp = if_else(summary_df$winner_side[[1]] == "home", home_terminal_wp, away_terminal_wp),
      home_score = summary_df$home_final[[1]],
      away_score = summary_df$away_final[[1]],
      loser_score = if_else(summary_df$loser_side[[1]] == "home", summary_df$home_final[[1]], summary_df$away_final[[1]]),
      winner_score = if_else(summary_df$winner_side[[1]] == "home", summary_df$home_final[[1]], summary_df$away_final[[1]]),
      loser_score_margin = loser_score - winner_score,
      play_text = "Final outcome",
      is_peak_value = FALSE,
      is_first_peak = FALSE
    )

  if ("home_wp_corrected" %in% names(terminal_row)) {
    terminal_row <- terminal_row |>
      mutate(
        home_wp_corrected = home_terminal_wp,
        away_wp_corrected = away_terminal_wp,
        loser_wp_corrected = if_else(summary_df$loser_side[[1]] == "home", home_terminal_wp, away_terminal_wp),
        winner_wp_corrected = if_else(summary_df$winner_side[[1]] == "home", home_terminal_wp, away_terminal_wp)
      )
  }
  if ("was_1" %in% names(terminal_row)) {
    terminal_row$was_1 <- 0L
  }
  if ("was_0" %in% names(terminal_row)) {
    terminal_row$was_0 <- 0L
  }

  bind_rows(path_df, terminal_row) |>
    arrange(game_seconds_remaining, sequence_number) |>
    with_effective_probabilities()
}

prepare_case_study <- function(league, override_game_id = NULL, candidate_n = 8) {
  catalog <- load_case_study_catalog(league)
  if (!is.null(override_game_id)) {
    chosen <- catalog |>
      filter(game_id == override_game_id) |>
      slice_head(n = 1)
    if (nrow(chosen) == 0) {
      stop("No case-study game available for league ", league)
    }
    return(load_or_fetch_case_study(league, chosen$game_id[[1]], chosen$season[[1]]))
  }

  candidates <- slice_head(catalog, n = candidate_n)
  if (nrow(candidates) == 0) {
    stop("No case-study game available for league ", league)
  }

  best_obj <- NULL
  best_share <- -Inf

  for (i in seq_len(nrow(candidates))) {
    game_obj <- load_or_fetch_case_study(
      league,
      candidates$game_id[[i]],
      candidates$season[[i]]
    )
    observed_share <- case_study_observed_share(game_obj)
    if (observed_share > best_share) {
      best_obj <- game_obj
      best_share <- observed_share
    }
    if (observed_share >= 0.95) {
      return(game_obj)
    }
  }

  best_obj
}

format_peak_state <- function(summary_df) {
  margin <- summary_df$peak_first_loser_score_margin[[1]]
  margin_text <- if (is.na(margin)) {
    "score state unavailable"
  } else if (margin > 0) {
    paste("loser led by", margin)
  } else if (margin < 0) {
    paste("loser trailed by", abs(margin))
  } else {
    "game was tied"
  }

  paste0(
    "Peak at period ", summary_df$peak_first_period_number[[1]],
    ", ", format_clock_display(summary_df$peak_first_clock_display[[1]]),
    " (", margin_text, ")"
  )
}

build_peak_annotation <- function(plot_df, summary_df, peak_point) {
  x_vals <- plot_df$game_minutes_remaining
  x_range <- range(x_vals, na.rm = TRUE)
  x_span <- diff(x_range)
  peak_x <- peak_point$game_seconds_remaining[[1]] / 60
  peak_y <- peak_point$loser_wp_plot[[1]]

  label_x <- min(x_range[[2]] * 0.96, peak_x + max(2, 0.12 * x_span))
  label_y <- if (peak_y > 0.92) {
    max(0.78, peak_y - 0.08)
  } else {
    min(0.92, peak_y + 0.08)
  }

  tibble(
    peak_x = peak_x,
    peak_y = peak_y,
    label_x = label_x,
    label_y = label_y,
    label = paste0(
      "Selected max = ", scales::percent(summary_df$max_wp_loser[[1]], accuracy = 0.1),
      "\nBenchmark PIT = ", format(round(summary_df$pit_u[[1]], 3), nsmall = 3)
    )
  )
}

plot_case_study <- function(game_obj, out_file) {
  path_df <- ensure_terminal_path_point(game_obj$paths, game_obj$summary)
  summary_df <- game_obj$summary

  loser_team <- summary_df$loser_team[[1]]
  winner_team <- summary_df$winner_team[[1]]
  home_team <- summary_df$home_team[[1]]
  away_team <- summary_df$away_team[[1]]
  home_final <- summary_df$home_final[[1]]
  away_final <- summary_df$away_final[[1]]
  peak_point <- path_df |>
    filter(is_first_peak) |>
    slice_head(n = 1)
  if (nrow(peak_point) == 0 || is.na(peak_point$game_seconds_remaining[[1]])) {
    peak_point <- path_df |>
      filter(is_peak_value, !is.na(game_seconds_remaining)) |>
      slice_head(n = 1)
  }

  plot_df <- path_df |>
    transmute(
      game_minutes_remaining = game_seconds_remaining / 60,
      home_team = home_team,
      away_team = away_team,
      home_wp = home_wp_plot,
      away_wp = away_wp_plot
    ) |>
    pivot_longer(
      cols = c(home_wp, away_wp),
      names_to = "side",
      values_to = "win_probability"
    ) |>
    mutate(
      team = if_else(side == "home_wp", home_team, away_team),
      role = if_else(team == loser_team, "Eventual loser", "Eventual winner")
    ) |>
    filter(!is.na(game_minutes_remaining), !is.na(win_probability)) |>
    arrange(team, desc(game_minutes_remaining))

  annotation_df <- build_peak_annotation(plot_df, summary_df, peak_point)
  endpoint_df <- plot_df |>
    group_by(team, role) |>
    filter(game_minutes_remaining == min(game_minutes_remaining)) |>
    slice_tail(n = 1) |>
    ungroup()
  x_max <- ceiling(max(plot_df$game_minutes_remaining, na.rm = TRUE) / 5) * 5

  p <- ggplot(plot_df, aes(x = game_minutes_remaining, y = win_probability, color = role)) +
    geom_hline(yintercept = 0.5, color = paper_style$ref, linetype = "dashed", linewidth = 0.5) +
    geom_vline(
      xintercept = annotation_df$peak_x[[1]],
      color = paper_style$shade,
      linetype = "dotted",
      linewidth = 0.55,
      alpha = 0.8
    ) +
    geom_line(aes(group = team), linewidth = 1) +
    geom_point(
      data = endpoint_df,
      inherit.aes = FALSE,
      aes(x = game_minutes_remaining, y = win_probability, color = role),
      size = 1.8
    ) +
    geom_point(
      data = peak_point,
      aes(x = game_seconds_remaining / 60, y = loser_wp_plot),
      inherit.aes = FALSE,
      color = paper_style$shade,
      size = 2.8
    ) +
    geom_segment(
      data = annotation_df,
      aes(x = peak_x, y = peak_y, xend = label_x, yend = label_y),
      inherit.aes = FALSE,
      color = paper_style$shade,
      linewidth = 0.45
    ) +
    geom_label(
      data = annotation_df,
      aes(x = label_x, y = label_y, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 0.5,
      size = 3.1,
      color = paper_style$ink,
      fill = "white",
      linewidth = 0.2
    ) +
    scale_color_manual(
      values = c("Eventual loser" = paper_style$shade, "Eventual winner" = paper_style$ref)
    ) +
    scale_x_reverse(
      limits = c(x_max, -0.5),
      breaks = pretty(c(0, x_max), n = 6)
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = paste0(loser_team, " reached ", scales::percent(summary_df$max_wp_loser[[1]], accuracy = 0.1), " and still lost"),
      subtitle = paste0(home_team, " ", home_final, " - ", away_final, " ", away_team),
      x = "Minutes remaining",
      y = "Win probability",
      caption = paste(
        format_peak_state(summary_df),
        paste0("Starting favorite probability = ", round(summary_df$starting_wp_favored[[1]], 3)),
        sep = " | "
      )
    ) +
    paper_theme(base_size = 11)

  ggsave(out_file, p, width = 7.2, height = 4.6, dpi = 300)
}

run_case_study_paths <- function() {
  nfl_override <- parse_arg("nfl-game", "401435639")
  nba_override <- parse_arg("nba-game", "401584792")
  nba_extreme_override <- parse_arg("nba-extreme-game", "401160742")

  nfl_obj <- prepare_case_study("nfl", nfl_override)
  nba_obj <- prepare_case_study("nba", nba_override)
  nba_extreme_obj <- prepare_case_study("nba", nba_extreme_override)

  plot_case_study(nfl_obj, file.path(out_dir, "nfl_case_study.png"))
  plot_case_study(nba_obj, file.path(out_dir, "nba_case_study.png"))
  plot_case_study(nba_extreme_obj, file.path(out_dir, "nba_extreme_case_study.png"))

  case_summary <- bind_rows(
    nfl_obj$summary |>
      mutate(
        case_label = "nfl_representative",
        peak_first_clock_display = as.character(peak_first_clock_display)
      ),
    nba_obj$summary |>
      mutate(
        case_label = "nba_representative",
        peak_first_clock_display = as.character(peak_first_clock_display)
      ),
    nba_extreme_obj$summary |>
      mutate(
        case_label = "nba_extreme",
        peak_first_clock_display = as.character(peak_first_clock_display)
      )
  )
  readr::write_csv(case_summary, file.path(out_dir, "case_study_games.csv"))
}

if (sys.nframe() == 0) {
  run_case_study_paths()
}
