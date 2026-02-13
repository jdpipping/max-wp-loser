# Generate figures for paper/
# Run from repo root: Rscript paper/code/nba-analysis.R
# Uses data from paper/data/nba, outputs to paper/nba

#################
### LIBRARIES ###
#################

library(tidyverse)
library(viridisLite)

##################
### PARAMETERS ###
##################

out_dir = "paper/nba"
data_dir = "paper/data/nba"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Colors: magma palette to match CDF figures (colorblind-friendly)
PIT_FILL = viridis(5, option = "magma")[3]
PIT_LINE = "gray25"

##################
### FUNCTIONS ###
##################

#' Right-tail PIT p-value: P(M_lambda >= m | p0)
pit_pvalue = function(m, p0) {
  p = numeric(length(m))
  p[m < (1 - p0)] = 1
  mid = m >= (1 - p0) & m < p0
  p[mid] = (1 - p0[mid]) / m[mid]
  high = m >= p0 & m < 1
  p[high] = (1 - m[high]) / m[high]
  p[m >= 1] = 0
  pmin(pmax(p, 0), 1)
}

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

#' PIT analysis: U_i, P_i, KS test, tail fractions
pit_analysis = function(game_data, out_dir_fig, league_name = "NBA") {
  game_data = game_data |> filter(!is.na(max_wp_loser) & !is.na(starting_wp_favored))
  p0 = game_data$starting_wp_favored
  m  = game_data$max_wp_loser
  u = pit_cdf(m, p0)
  pit_p = pit_pvalue(m, p0)
  n = length(u)

  prop_10 = mean(pit_p <= 0.10)
  prop_05 = mean(pit_p <= 0.05)
  prop_01 = mean(pit_p <= 0.01)

  # One-sided KS: D_n^- = sup(F_hat_P(t) - t), alternative="greater"
  ks_result = tryCatch(
    stats::ks.test(pit_p, "punif", 0, 1, alternative = "greater", exact = FALSE),
    error = function(e) list(statistic = NA_real_, p.value = NA_real_)
  )
  ks_stat = if (!is.na(ks_result$statistic)) as.numeric(ks_result$statistic) else NA_real_
  ks_pval = if (!is.na(ks_result$p.value)) as.numeric(ks_result$p.value) else NA_real_

  p_hist = ggplot(data.frame(u = u), aes(x = u)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.05, boundary = 0,
                   fill = PIT_FILL, color = "white") +
    geom_hline(yintercept = 1, color = PIT_LINE, linewidth = 1) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
    labs(x = expression("PIT" ~ U[i]), y = "Density",
         title = paste0(league_name, ": PIT values vs Uniform(0,1)")) +
    theme_minimal()
  ggsave(file.path(out_dir_fig, "pit.png"), p_hist, width = 6, height = 4, dpi = 300)

  list(n = n, prop_10 = prop_10, prop_05 = prop_05, prop_01 = prop_01,
       ks_stat = ks_stat, ks_pval = ks_pval)
}

##################
### MAIN EXECUTION ###
##################

all_games_path = file.path(data_dir, "all_games.csv")
if (!file.exists(all_games_path)) {
  season_files = list.files(data_dir, pattern = "_games\\.csv$", full.names = TRUE)
  season_files = season_files[!grepl("all_games\\.csv$", season_files)]
  if (length(season_files) == 0) {
    stop("No game files found under paper/data/nba.")
  }
  all_games = bind_rows(lapply(season_files, read.csv))
  write.csv(all_games, all_games_path, row.names = FALSE)
} else {
  all_games = read.csv(all_games_path)
}

pit_result = pit_analysis(all_games, out_dir)
writeLines(sprintf(
  "n=%d prop_10=%.3f prop_05=%.3f prop_01=%.3f ks_stat=%.4f ks_pval=%.4f",
  pit_result$n, pit_result$prop_10, pit_result$prop_05, pit_result$prop_01,
  pit_result$ks_stat, pit_result$ks_pval
), file.path(data_dir, "pit_summary.txt"))

message("\nNBA PIT diagnostics:")
message(sprintf("  n=%d  P(P<=0.10)=%.3f  P(P<=0.05)=%.3f  P(P<=0.01)=%.3f  KS=%.4f  p=%.4f",
  pit_result$n, pit_result$prop_10, pit_result$prop_05, pit_result$prop_01,
  pit_result$ks_stat, pit_result$ks_pval))
message("\nNBA analysis complete! Output in ", out_dir)
