#!/usr/bin/env Rscript

# Demonstrate how observing a calibrated path on a coarser reporting grid
# shifts the PIT signature below its continuous-path benchmark.

suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(20260318)
n_null <- 50000L
K_base <- 1000L

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]))
} else {
  normalizePath("scripts/manuscript/simulation-study.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
out_dir <- file.path(
  repo_root,
  "results", "figures", "manuscript", "figures", "simulation"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(repo_root, "R", "plot-style.R"))

logit_safe <- function(p) qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))

pit_cdf_two_team <- function(m, p0_favored) {
  u <- numeric(length(m))
  below <- m < (1 - p0_favored)
  middle <- m >= (1 - p0_favored) & m < p0_favored
  upper <- m >= p0_favored & m < 1

  u[below] <- 0
  u[middle] <- 1 - (1 - p0_favored[middle]) / m[middle]
  u[upper] <- 2 - 1 / m[upper]
  u[m >= 1] <- 1
  pmin(pmax(u, 0), 1)
}

simulate_bridge_paths <- function(n,
                                  K = K_base,
                                  sigma = 0.5,
                                  p0_low = 0.25,
                                  p0_high = 0.75) {
  p0 <- runif(n, p0_low, p0_high)
  y <- rbinom(n, size = 1, prob = p0)
  t_grid <- seq(0, 1, length.out = K + 1L)

  bridge <- matrix(0, nrow = n, ncol = K + 1L)
  if (K > 1L) {
    for (k in seq_len(K - 1L)) {
      t_prev <- t_grid[[k]]
      t_next <- t_grid[[k + 1L]]
      mu_coef <- (1 - t_next) / (1 - t_prev)
      sd_step <- sqrt((t_next - t_prev) * (1 - t_next) / (1 - t_prev))
      bridge[, k + 1L] <- mu_coef * bridge[, k] + rnorm(n, sd = sd_step)
    }
  }

  x <- tcrossprod(y, t_grid) + sigma * bridge
  probabilities <- matrix(0, nrow = n, ncol = K + 1L)
  t_inner <- t_grid[seq_len(K)]
  denominator <- sigma^2 * (1 - t_inner)
  centered <- x[, seq_len(K), drop = FALSE] - tcrossprod(rep(1, n), t_inner / 2)
  log_odds <- tcrossprod(logit_safe(p0), rep(1, K)) +
    sweep(centered, 2, denominator, "/")
  probabilities[, seq_len(K)] <- plogis(log_odds)
  probabilities[, K + 1L] <- y
  probabilities[, 1L] <- p0

  list(p0 = p0, y = y, p = probabilities)
}

observe_feed <- function(probabilities, step = 1L) {
  indices <- seq(1L, ncol(probabilities), by = step)
  if (tail(indices, 1L) != ncol(probabilities)) {
    indices <- c(indices, ncol(probabilities))
  }
  probabilities[, indices, drop = FALSE]
}

u_values_from_paths <- function(probabilities, p0, y) {
  max_probability <- apply(probabilities, 1, max)
  min_probability <- apply(probabilities, 1, min)
  loser_peak <- ifelse(y == 0, max_probability, 1 - min_probability)
  pit_cdf_two_team(loser_peak, pmax(p0, 1 - p0))
}

message("Running discrete-time conservatism under feed coarsening.")
threshold_grid <- seq(0, 1, by = 0.01)
coarsen_steps <- c(1L, 2L, 5L, 10L)

label_coarsen <- function(step) {
  if (step == 1L) {
    return(paste0("every update (K = ", format(K_base, big.mark = ","), ")"))
  }
  paste0("every ", step, " updates (K = ", format(K_base, big.mark = ","), ")")
}

latent <- simulate_bridge_paths(n = n_null, K = K_base)
u_by_step <- lapply(coarsen_steps, function(step) {
  observed <- observe_feed(latent$p, step = step)
  u_values_from_paths(observed, latent$p0, latent$y)
})
names(u_by_step) <- as.character(coarsen_steps)

null_pit_profile <- map_dfr(coarsen_steps, function(step) {
  u <- u_by_step[[as.character(step)]]
  tibble(
    m = step,
    curve = label_coarsen(step),
    t = threshold_grid,
    upper_gap = map_dbl(threshold_grid, ~ .x - mean(u <= .x))
  )
})
coarsening_levels <- map_chr(coarsen_steps, label_coarsen)
null_pit_profile <- null_pit_profile |>
  mutate(curve = factor(curve, levels = coarsening_levels))

null_tail_summary <- map_dfr(coarsen_steps, function(step) {
  u <- u_by_step[[as.character(step)]]
  tibble(
    m = step,
    n_games = length(u),
    pr_u_ge_090 = mean(u >= 0.90),
    pr_u_ge_095 = mean(u >= 0.95),
    pr_u_ge_099 = mean(u >= 0.99)
  )
})

write_csv(null_pit_profile, file.path(out_dir, "null_pit_profile.csv"))
write_csv(null_tail_summary, file.path(out_dir, "null_tail_summary.csv"))

coarsening_colors <- setNames(
  rep(paper_style$ref, length(coarsening_levels)),
  coarsening_levels
)
coarsening_colors[[coarsening_levels[[1L]]]] <- paper_style$ink
coarsening_colors[coarsening_levels[-1L]] <- paper_palette_seq(
  length(coarsening_levels) - 1L,
  begin = 0.30,
  end = 0.90
)

null_conservatism <- ggplot(
  null_pit_profile,
  aes(x = t, y = upper_gap, color = curve)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = paper_style$ref,
    linewidth = 0.85
  ) +
  geom_line(linewidth = 0.95) +
  scale_color_manual(values = coarsening_colors, breaks = coarsening_levels) +
  labs(
    x = "t",
    y = expression(t - hat(F)[U](t)),
    color = NULL,
    title = paste0(
      "PIT signature: feed coarsening (n = ",
      format(n_null, big.mark = ","),
      ")"
    )
  ) +
  paper_theme(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "null_conservatism.png"),
  null_conservatism,
  width = 8.2,
  height = 4.8,
  dpi = 300
)

message("Saved coarsening simulation outputs to: ", out_dir)
