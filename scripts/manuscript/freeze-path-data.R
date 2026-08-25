#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  normalizePath("scripts/manuscript/freeze-path-data.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))

for (league in c("nfl", "nba")) {
  data_dir <- file.path(repo_root, "data", "derived", league)
  for (season in 2018:2024) {
    source_path <- file.path(data_dir, paste0(season, "_paths.csv"))
    target_path <- file.path(data_dir, paste0(season, "_paths.parquet"))
    if (!file.exists(source_path)) {
      if (file.exists(target_path)) next
      stop("Missing enriched path source: ", source_path)
    }

    message("Freezing ", league, " ", season, " paths.")
    paths <- fread(source_path, showProgress = FALSE)
    temporary_path <- paste0(target_path, ".tmp")
    write_parquet(paths, temporary_path, compression = "zstd")

    frozen <- read_parquet(
      temporary_path,
      col_select = c("game_id", "home_wp", "away_wp", "home_wp_corrected", "away_wp_corrected"),
      as_data_frame = TRUE
    )
    stopifnot(
      nrow(frozen) == nrow(paths),
      identical(sort(unique(as.character(frozen$game_id))),
                sort(unique(as.character(paths$game_id))))
    )
    if (!file.rename(temporary_path, target_path)) {
      stop("Could not install frozen path file: ", target_path)
    }
  }
}

message("Frozen enriched path files are ready for public reproducibility.")
