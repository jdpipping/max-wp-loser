resolve_path_file <- function(data_dir, season, stem = "paths", required = TRUE) {
  candidates <- file.path(
    data_dir,
    sprintf("%s_%s.%s", season, stem, c("csv", "parquet"))
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0L) {
    return(existing[[1L]])
  }
  if (required) {
    stop("Missing path file for season ", season, " in ", data_dir, ".")
  }
  NULL
}

list_enriched_path_files <- function(data_dir) {
  seasons <- sub(
    "_paths[.](csv|parquet)$",
    "",
    list.files(data_dir, pattern = "^[0-9]{4}_paths[.](csv|parquet)$")
  )
  seasons <- sort(unique(seasons))
  vapply(
    seasons,
    function(season) resolve_path_file(data_dir, season),
    character(1)
  )
}

read_path_data <- function(path, columns = NULL) {
  if (grepl("[.]parquet$", path, ignore.case = TRUE)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Reading frozen Parquet paths requires the R package 'arrow'.")
    }
    available <- arrow::open_dataset(path)$schema$names
    selected <- if (is.null(columns)) available else intersect(columns, available)
    return(data.table::as.data.table(arrow::read_parquet(
      path,
      col_select = tidyselect::all_of(selected),
      as_data_frame = TRUE
    )))
  }

  available <- names(data.table::fread(path, nrows = 0L, showProgress = FALSE))
  selected <- if (is.null(columns)) available else intersect(columns, available)
  data.table::fread(path, select = selected, showProgress = FALSE)
}
