suppressPackageStartupMessages({
  library(glue)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath("scripts/manuscript/run-full-analysis.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default = NULL) {
  hit <- args[grepl(paste0("^--", name, "="), args)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[[1]])
}

parse_flag <- function(name, default = FALSE) {
  val <- parse_arg(name, if (default) "true" else "false")
  tolower(val) %in% c("true", "1", "yes")
}

run_rscript <- function(script, extra_args = character()) {
  cmd_args <- c(script, extra_args)
  message(glue("Running: Rscript {paste(cmd_args, collapse = ' ')}"))
  status <- system2("Rscript", args = cmd_args, stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop(glue("Command failed with status {status}: Rscript {paste(cmd_args, collapse = ' ')}"))
  }
}

run_command <- function(command, args = character(), wd = repo_root) {
  message(glue("Running: {command} {paste(args, collapse = ' ')}"))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  status <- system2(command, args = args, stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop(glue("Command failed with status {status}: {command} {paste(args, collapse = ' ')}"))
  }
}

workers <- max(1L, as.integer(parse_arg("workers", "12")))
seasons <- parse_arg("seasons", "2018,2019,2020,2021,2022,2023,2024")
overwrite <- parse_flag("overwrite", FALSE)
skip_build <- parse_flag("skip-build", FALSE)
skip_simulation <- parse_flag("skip-simulation", FALSE)
skip_validation <- parse_flag("skip-validation", FALSE)
compile_pdf <- parse_flag("compile-pdf", FALSE)
inference_replicates <- max(1L, as.integer(parse_arg("replicates", "9999")))
inference_seed <- as.integer(parse_arg("seed", "20260815"))
validation_outer <- max(1L, as.integer(parse_arg("validation-outer", "250")))
validation_inner <- max(1L, as.integer(parse_arg("validation-inner", "399")))

if (!skip_build) {
  run_rscript(
    file.path(repo_root, "scripts", "manuscript", "build-espn-enriched-data.R"),
    c(
      "--league=both",
      glue("--seasons={seasons}"),
      glue("--workers={workers}"),
      glue("--overwrite={tolower(as.character(overwrite))}")
    )
  )
}

run_rscript(file.path(repo_root, "scripts", "manuscript", "freeze-path-data.R"))

run_rscript(
  file.path(repo_root, "scripts", "manuscript", "validate-data-contracts.R"),
  "--check-fixed-clock=false"
)

run_rscript(file.path(repo_root, "scripts", "manuscript", "two-player.R"))
run_rscript(file.path(repo_root, "scripts", "manuscript", "n-player.R"))
run_rscript(file.path(repo_root, "scripts", "manuscript", "nfl-analysis.R"))
run_rscript(file.path(repo_root, "scripts", "manuscript", "nba-analysis.R"))
run_rscript(file.path(repo_root, "scripts", "manuscript", "pointwise-calibration.R"))
run_rscript(file.path(repo_root, "scripts", "manuscript", "validate-data-contracts.R"))
run_rscript(file.path(repo_root, "scripts", "manuscript", "first-crossing-analysis.R"))
run_rscript(
  file.path(repo_root, "scripts", "manuscript", "dependent-inference.R"),
  c(
    "--league=both",
    glue("--replicates={inference_replicates}"),
    glue("--seed={inference_seed}")
  )
)
if (!skip_validation) {
  run_rscript(
    file.path(repo_root, "scripts", "manuscript", "validate-dyadic-bootstrap.R"),
    c(
      "--league=both",
      glue("--outer={validation_outer}"),
      glue("--inner={validation_inner}"),
      glue("--seed={inference_seed}")
    )
  )
}
run_rscript(file.path(repo_root, "scripts", "manuscript", "case-study-paths.R"))

if (!skip_simulation) {
  run_rscript(file.path(repo_root, "scripts", "manuscript", "simulation-study.R"))
}

run_rscript(
  file.path(repo_root, "scripts", "manuscript", "build-manuscript-artifacts.R")
)
run_rscript(
  file.path(repo_root, "scripts", "manuscript", "write-reproducibility-metadata.R")
)
run_rscript(
  file.path(repo_root, "scripts", "manuscript", "build-submission-packages.R")
)

if (compile_pdf) {
  run_command("latexmk", c("-pdf", "main.tex"), wd = file.path(repo_root, "writing", "manuscript"))
  run_command("latexmk", c("-pdf", "supp.tex"), wd = file.path(repo_root, "writing", "manuscript"))
  run_command("latexmk", c("-pdf", "cover-letter.tex"), wd = file.path(repo_root, "writing", "aoas"))
  run_command("latexmk", c("-pdf", "article.tex"), wd = file.path(repo_root, "writing", "aoas"))
  run_command("latexmk", c("-pdf", "supplement.tex"), wd = file.path(repo_root, "writing", "aoas"))
  run_command("latexmk", c("-pdf", "main.tex"), wd = file.path(repo_root, "writing", "arxiv"))
}

message("Full manuscript analysis pipeline complete.")
