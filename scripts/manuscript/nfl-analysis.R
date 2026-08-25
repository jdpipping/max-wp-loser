# Generate NFL diagnostics for the manuscript/AOAS deliverables.
# Run from repo root: Rscript scripts/manuscript/nfl-analysis.R
# Uses canonical data from data/derived/nfl and writes shared figures to
# results/figures/manuscript/figures/nfl.

#################
### LIBRARIES ###
#################

library(tidyverse)

args_full = commandArgs(trailingOnly = FALSE)
file_arg = args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path = normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path = normalizePath("scripts/manuscript/nfl-analysis.R")
}
repo_root = normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "R", "plot-style.R"))
source(file.path(repo_root, "R", "dyadic-bootstrap.R"))

##################
### PARAMETERS ###
##################

out_dir = file.path(repo_root, "results", "figures", "manuscript", "figures", "nfl")
data_dir = file.path(repo_root, "data", "derived", "nfl")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PIT_FILL = paper_style$shade
PIT_LINE = paper_style$ink
PIT_REF = paper_style$ref

##################
### FUNCTIONS ###
##################

#' PIT CDF: U = F_Mlambda(m; p0)
pit_cdf = function(m, p0) {
  u = numeric(length(m))
  u[m < (1 - p0)] = 0
  mid = m >= (1 - p0) & m < p0
  u[mid] = 1 - (1 - p0[mid]) / m[mid]
  high = m >= p0 & m < 1
  u[high] = 2 - 1 / m[high]
  u[m >= 1] = 1
  pmin(pmax(u, 0), 1)
}

#' PIT analysis on U_i: upper-tail frequencies, global discrepancy, signature plot
pit_analysis = function(game_data, out_dir_fig, league_name = "NFL") {
  game_data = game_data |> filter(!is.na(max_wp_loser) & !is.na(starting_wp_favored))
  p0 = game_data$starting_wp_favored
  m  = game_data$max_wp_loser
  u = pit_cdf_two_team(m, p0)
  n = length(u)

  prop_90 = mean(u >= 0.90)
  prop_95 = mean(u >= 0.95)
  prop_99 = mean(u >= 0.99)

  # Its inferential reference distribution is supplied by dependent-inference.R.
  d_upper = ks_upper_stat(u)

  sig_df = as_tibble(pit_signature_data(u))

  y_lim = max(0.05, 1.1 * max(abs(sig_df$upper_gap), na.rm = TRUE))

  u_signature = ggplot(sig_df, aes(x = t, y = upper_gap)) +
    annotate("rect", xmin = 0.90, xmax = 1, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = PIT_FILL) +
    geom_hline(yintercept = 0, linetype = "dashed", color = PIT_REF) +
    geom_step(linewidth = 1.1, color = PIT_LINE, direction = "hv") +
    coord_cartesian(xlim = c(0, 1), ylim = c(-y_lim, y_lim), expand = FALSE) +
    labs(
      x = expression(t),
      y = expression(t - hat(F)[U](t)),
      title = paste0(league_name, ": PIT signature")
    ) +
    paper_theme(base_size = 11)
  ggsave(file.path(out_dir_fig, "pit.png"), u_signature, width = 6, height = 4, dpi = 300)

  list(n = n, prop_90 = prop_90, prop_95 = prop_95, prop_99 = prop_99,
       d_upper = d_upper)
}

##################
### MAIN EXECUTION ###
##################

all_games_path = file.path(data_dir, "all_games_enriched.csv")
if (!file.exists(all_games_path)) {
  stop(
    "Missing corrected enriched game data: ", all_games_path,
    ". Run scripts/manuscript/build-espn-enriched-data.R first."
  )
} else {
  all_games = read.csv(all_games_path)
}

pit_result = pit_analysis(all_games, out_dir)
write_csv(tibble(
  league = "NFL",
  n = pit_result$n,
  prop_90 = pit_result$prop_90,
  prop_95 = pit_result$prop_95,
  prop_99 = pit_result$prop_99,
  Dn_upper = pit_result$d_upper
), file.path(out_dir, "pit_summary.csv"))
writeLines(sprintf(
  "n=%d prop_90=%.3f prop_95=%.3f prop_99=%.3f Dn_upper=%.4f",
  pit_result$n, pit_result$prop_90, pit_result$prop_95, pit_result$prop_99,
  pit_result$d_upper
), file.path(data_dir, "pit_summary.txt"))

message("\nNFL PIT diagnostics:")
message(sprintf("  n=%d  P(U>=0.90)=%.3f  P(U>=0.95)=%.3f  P(U>=0.99)=%.3f  D_upper=%.4f",
  pit_result$n, pit_result$prop_90, pit_result$prop_95, pit_result$prop_99,
  pit_result$d_upper))
message("\nNFL analysis complete! Output in ", out_dir)
