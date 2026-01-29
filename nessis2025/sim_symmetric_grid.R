#################
### LIBRARIES ###
#################

library(ggplot2)
library(parallel)
library(pbapply)
utils::globalVariables(c("..density..", "empirical", "alpha", "identity", "alpha_display", "empirical_display"))

##################
### PARAMETERS ###
##################

# Grid of probabilities and possessions
p_grid = c(0.45, 0.50, 0.55)
N_grid = c(100, 1000, 10000)
n_sims = 10000

# Output directory (match slides location)
out_dir = file.path("nessis2025", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Penn colors (match slides)
PENN_RED  = "#990000"
PENN_BLUE = "#011f5b"

##################
### FUNCTIONS  ###
##################

# Brownian-limit win probability for symmetric case (mu = 0)
win_prob_brownian = function(t_index, d_t, N) {
  t = t_index / N
  sigma = sqrt(0.5)
  X_t_star = d_t / (sigma * sqrt(N))
  denom = sqrt(max(1 - t, 1e-12))
  stats::pnorm(X_t_star / denom)
}

# Precompute WP table for faster lookup
precompute_win_probs = function(N) {
  wp_table = array(NA_real_, dim = c(N + 1, 2 * N + 1))
  pb = utils::txtProgressBar(min = 0, max = N, style = 3)
  for (t in 0:N) {
    n_rem = N - t
    for (d in -n_rem:n_rem) {
      wp_table[t + 1, d + N + 1] = win_prob_brownian(t, d, N)
    }
    if (t %% 10 == 0 || t == N) utils::setTxtProgressBar(pb, t)
  }
  close(pb)
  wp_table
}

# Simulate a single game for general per-possession scoring probability p
sim_game = function(N, p, wp_table = NULL) {
  scoresA = scoresB = numeric(N + 1)
  WPs_A = numeric(N + 1)

  a_scores = rbinom(N, 1, p)
  b_scores = rbinom(N, 1, p)

  WPs_A[1] = 0.5
  for (t in 1:N) {
    scoresA[t + 1] = scoresA[t] + a_scores[t]
    scoresB[t + 1] = scoresB[t] + b_scores[t]
    d_t = scoresA[t + 1] - scoresB[t + 1]
    if (!is.null(wp_table) && abs(d_t) <= (N - t)) {
      WPs_A[t + 1] = wp_table[t + 1, d_t + N + 1]
    } else {
      WPs_A[t + 1] = win_prob_brownian(t, d_t, N)
    }
  }

  final_diff = scoresA[N + 1] - scoresB[N + 1]
  if (final_diff == 0) final_diff = sample(c(-1, 1), 1)
  loser = ifelse(final_diff < 0, "A", "B")
  list(WPs_A = WPs_A, loser = loser)
}

# Run many simulations and return vector of maximum WP for loser
simulate_max_wp = function(n_sims, N, p) {
  wp_table = precompute_win_probs(N)
  Mvals = numeric(n_sims)
  pb = utils::txtProgressBar(min = 0, max = n_sims, style = 3)
  for (j in seq_len(n_sims)) {
    game = sim_game(N, p, wp_table)
    WP_loser = if (game$loser == "A") game$WPs_A else 1 - game$WPs_A
    Mvals[j] = max(WP_loser, na.rm = TRUE)
    if (j %% 10 == 0 || j == n_sims) utils::setTxtProgressBar(pb, j)
  }
  close(pb)
  Mvals
}

# Theoretical density for symmetric case: f(x) = 1/x^2 on [1/2, 1]
theoretical_density = function(x) {
  ifelse(x >= 0.5 & x <= 1, 1 / (x^2), 0)
}

# Plot helpers (Penn colors)
plot_histogram = function(M, N, p, out_path) {
  df = data.frame(M = M)
  mean_M = mean(df$M, na.rm = TRUE)
  sd_M = stats::sd(df$M, na.rm = TRUE)
  p_hist = ggplot(df, aes(M)) +
    geom_histogram(aes(y = ..density..), boundary = 0.5, closed = "left",
                   binwidth = 0.025, fill = PENN_RED, color = "white") +
    coord_cartesian(xlim = c(0.5, 1)) +
    labs(title = sprintf("Distribution of Maximum Win Probability of Loser — N=%d, p=%.2f", N, p),
         x = "Maximum Win Probability Attained", y = "Density",
         caption = sprintf("Mean = %.3f, SD = %.3f", mean_M, sd_M)) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 10))
  ggsave(out_path, p_hist, width = 8, height = 4, dpi = 300)
}

plot_threshold = function(M, N, p, out_path) {
  alpha_values = seq(0.5, 1, by = 0.01)
  empirical_probs = sapply(alpha_values, function(a) mean(M >= a))
  plot_data = data.frame(alpha = alpha_values,
                         empirical = empirical_probs,
                         identity = 1 - alpha_values)

  # quartile target points (0.75, 0.5, 0.25)
  target_probs = c(0.75, 0.5, 0.25)
  points = data.frame(
    empirical = target_probs,
    alpha = sapply(target_probs, function(p0) {
      idx = which(diff(sign(empirical_probs - p0)) != 0)[1]
      if (is.na(idx)) return(NA_real_)
      x1 = alpha_values[idx]; x2 = alpha_values[idx + 1]
      y1 = empirical_probs[idx] - p0; y2 = empirical_probs[idx + 1] - p0
      x1 - y1 * (x2 - x1) / (y2 - y1)
    })
  )
  points = points[!is.na(points$alpha), ]
  points$alpha_display = round(points$alpha, 2)
  points$empirical_display = round(points$empirical, 2)

  p_thr = ggplot(plot_data, aes(x = alpha)) +
    geom_line(aes(y = empirical), color = PENN_RED, size = 1) +
    geom_line(aes(y = identity), color = "gray", linetype = "dashed", size = 1) +
    geom_point(data = points, aes(x = alpha, y = empirical), color = PENN_BLUE, size = 3) +
    geom_text(data = points[points$alpha == 0.50,],
              aes(x = alpha, y = empirical, label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
              hjust = -0.2, color = PENN_BLUE) +
    geom_text(data = points[points$alpha != 0.50,],
              aes(x = alpha, y = empirical, label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
              hjust = -0.2, vjust = -0.5, color = PENN_BLUE) +
    labs(title = sprintf("Proportion of Games Where the Loser's Max Win Probability ≥ α — N=%d, p=%.2f", N, p),
         x = expression(alpha),
         y = "Proportion of Games") +
    xlim(0.5, 1) +
    theme_minimal()
  ggsave(out_path, p_thr, width = 8, height = 4, dpi = 300)
}

##################
### EXECUTION  ###
##################

set.seed(20250926)

all_hist = data.frame()
all_thr = data.frame()

for (N in N_grid) {
  message(sprintf("Running symmetric grid for N = %d", N))
  for (p in p_grid) {
    message(sprintf("  Simulating p = %.2f", p))
    Mvals = simulate_max_wp(n_sims, N, p)

    base = sprintf("%s/symmetric_N%d_p%02d", out_dir, N, round(100 * p))
    plot_histogram(Mvals, N, p, sprintf("%s_hist.png", base))
    plot_threshold(Mvals, N, p, sprintf("%s_threshold.png", base))

    # accumulate for grid plots
    all_hist = rbind(all_hist, data.frame(
      M = Mvals,
      N = factor(N, levels = N_grid, ordered = TRUE),
      p_label = factor(sprintf("p = %.2f", p), levels = sprintf("p = %.2f", p_grid), ordered = TRUE)
    ))

    alpha_values = seq(0.5, 1, by = 0.01)
    empirical_probs = sapply(alpha_values, function(a) mean(Mvals >= a))
    all_thr = rbind(all_thr, data.frame(
      alpha = alpha_values,
      empirical = empirical_probs,
      identity = 1 - alpha_values,
      N = factor(N, levels = N_grid, ordered = TRUE),
      p_label = factor(sprintf("p = %.2f", p), levels = sprintf("p = %.2f", p_grid), ordered = TRUE)
    ))
  }
}

# Export grid plots: rows = N, cols = p
if (nrow(all_hist) > 0) {
  hist_grid = ggplot(all_hist, aes(M)) +
    geom_histogram(aes(y = ..density..), boundary = 0.5, closed = "left",
                   binwidth = 0.025, fill = PENN_RED, color = "white") +
    coord_cartesian(xlim = c(0.5, 1)) +
    labs(title = "Distribution of Maximum Win Probability of Loser",
         x = "Maximum Win Probability Attained", y = "Density") +
    theme_minimal() +
    facet_grid(rows = vars(N), cols = vars(p_label),
               labeller = labeller(
                 N = function(x) paste0("N = ", x),
                 p_label = label_value
               )) +
    theme(strip.text = element_text(size = 11, face = "plain"),
          strip.background = ggplot2::element_blank())
  ggsave(file.path(out_dir, "symmetric_hist_grid.png"), hist_grid, width = 12, height = 8, dpi = 300)
}

if (nrow(all_thr) > 0) {
  thr_grid = ggplot(all_thr, aes(x = alpha)) +
    geom_line(aes(y = empirical), color = PENN_RED, size = 0.9) +
    geom_line(aes(y = identity), color = "gray", linetype = "dashed", size = 0.9) +
    xlim(0.5, 1) +
    labs(title = "Proportion of Games Where the Loser's Max Win Probability ≥ α",
         x = expression(alpha),
         y = "Proportion of Games") +
    theme_minimal() +
    facet_grid(rows = vars(N), cols = vars(p_label),
               labeller = labeller(
                 N = function(x) paste0("N = ", x),
                 p_label = label_value
               )) +
    theme(strip.text = element_text(size = 11, face = "plain"),
          strip.background = ggplot2::element_blank())

  # add quartile points per facet (0.75, 0.5, 0.25)
  quartiles = c(0.75, 0.5, 0.25)
  pts_list = lapply(split(all_thr, list(all_thr$N, all_thr$p_label), drop = TRUE), function(df) {
    if (nrow(df) == 0) return(NULL)
    alpha_values = df$alpha
    empirical = df$empirical
    out = lapply(quartiles, function(p0) {
      idx = which(diff(sign(empirical - p0)) != 0)[1]
      if (is.na(idx)) return(NULL)
      x1 = alpha_values[idx]; x2 = alpha_values[idx + 1]
      y1 = empirical[idx] - p0; y2 = empirical[idx + 1] - p0
      data.frame(alpha = x1 - y1 * (x2 - x1) / (y2 - y1), empirical = p0,
                 N = df$N[1], p_label = df$p_label[1])
    })
    do.call(rbind, out)
  })
  pts_df = do.call(rbind, pts_list)
  if (!is.null(pts_df) && nrow(pts_df) > 0) {
    thr_grid = thr_grid +
      geom_point(data = pts_df, aes(x = alpha, y = empirical), color = PENN_BLUE, size = 2) +
      geom_text(data = pts_df,
                aes(x = alpha, y = empirical,
                    label = sprintf("(%.2f, %.2f)", alpha, empirical)),
                hjust = -0.2, vjust = -0.5, color = PENN_BLUE, size = 3)
  }

  ggsave(file.path(out_dir, "symmetric_threshold_grid.png"), thr_grid, width = 12, height = 8, dpi = 300)
}


