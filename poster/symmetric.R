#################
### LIBRARIES ###
#################

library(nflfastR)
library(tidyverse)

#################
### PARAMETERS ##
#################

N = 10000        # nominal maximum possessions
n_sims = 10000    # number of simulated games (adjust as needed)
p_td = 0.5       # touchdown probability for both teams (symmetric)

set.seed(09042025)

out_dir = "poster/figures"

#################
### FUNCTIONS ###
#################

## Brownian-limit win probability for symmetric case (mu = 0):
## p_t = Phi( X_t* / sqrt(1 - t) ), where X_t* = d_t / (sigma * sqrt(N))
win_prob_brownian = function(t_index, d_t, N) {
  t = t_index / N
  sigma = sqrt(0.5)  # for p = 0.5, variance is 0.5
  X_t_star = d_t / (sigma * sqrt(N))
  denom = sqrt(max(1 - t, 1e-12))
  pnorm(X_t_star / denom)
}

## Precompute win probabilities for symmetric case using normal approx
precompute_win_probs = function(N) {
  wp_table = array(NA_real_, dim = c(N + 1, 2 * N + 1))
  message("Precomputing win probabilities (symmetric)...")
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

# single-game simulation - optimized
sim_game = function(N, wp_table = NULL) {
  d = 0; scoresA = scoresB = numeric(N+1)
  WPs_A = numeric(N+1)

  # pre-generate all random samples for speed
  a_scores = rbinom(N, 1, p_td)
  b_scores = rbinom(N, 1, p_td)

  # t = 0 (no possessions played yet)
  WPs_A[1] = 0.5

  # possessions t = 1 .. N
  for (t in 1:N) {
    scoresA[t + 1] = scoresA[t] + a_scores[t]
    scoresB[t + 1] = scoresB[t] + b_scores[t]

    # score differential AFTER possession t
    d_t   = scoresA[t + 1] - scoresB[t + 1]
    n_rem = N - t

    # look up or compute WP for the new game state
    if (!is.null(wp_table) && abs(d_t) <= n_rem) {
      WPs_A[t + 1] = wp_table[t + 1, d_t + N + 1]
    } else {
      WPs_A[t + 1] = win_prob_brownian(t, d_t, N)
    }
  }

  final_diff = scoresA[N+1] - scoresB[N+1]
  if (final_diff == 0) {
    final_diff = sample(c(-1, 1), 1)
  }
  loser = ifelse(final_diff < 0, "A", "B")
  list(WPs_A = WPs_A, loser = loser)
}

# wrapper to simulate M_N using sequential processing
simulate_max_wp = function(n_sims, N) {
  wp_table = precompute_win_probs(N)
  Mvals = numeric(n_sims)
  message("Starting simulation (symmetric)...")
  pb = utils::txtProgressBar(min = 0, max = n_sims, style = 3)
  for (j in seq_len(n_sims)) {
    game = sim_game(N, wp_table)
    if (game$loser == "A") {
      WP_loser = game$WPs_A
    } else {
      WP_loser = 1 - game$WPs_A
    }
    Mvals[j] = max(WP_loser, na.rm = TRUE)
    if (j %% 10 == 0 || j == n_sims) utils::setTxtProgressBar(pb, j)
  }
  close(pb)
  Mvals
}

##################
### RUN & PLOT ###
##################

# Run simulation for symmetric case
M = simulate_max_wp(n_sims, N)
plot_data = data.frame(M = M)
mean_M = mean(plot_data$M, na.rm = TRUE)
sd_M = sd(plot_data$M, na.rm = TRUE)

# Ensure output directory exists
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Theoretical density for symmetric case: f(x) = 1/x^2 on [1/2, 1]
theoretical_density = function(x) {
  ifelse(x >= 0.5 & x <= 1, 1 / (x^2), 0)
}

# Simple plot: histogram (Penn red) with theoretical curve (Penn blue)
p = ggplot(plot_data, aes(M)) +
  geom_histogram(aes(y = ..density..),
                 boundary = 0.5,
                 closed = "left",
                 binwidth = 0.025,
                 fill = "#990000", color = "white") +
  stat_function(fun = theoretical_density, color = "#011f5b", size = 1) +
  coord_cartesian(xlim = c(0.5, 1)) +
  labs(title = "Distribution of Maximal Functional Value Conditional On Ending Negative",
       x = "Maximal Functional Value Attained",
       y = "Density",
       caption = sprintf("Mean = %.3f, SD = %.3f", mean_M, sd_M)) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 10))

ggsave(file.path(out_dir, "symmetric.png"), p, width = 8, height = 4, dpi = 300)

#######################
### NFL FASTR PLOTS ###
#######################

# Load data for 2002 - 2024
pbp_data = load_pbp(2002:2024)

# Real WP data
wp_data = pbp_data |>
  # filter for games with low spread
  filter(abs(spread_line) < 2) |>
  # group by game
  group_by(game_id) |>
  mutate(
    # get final scores
    home_final = last(home_score),
    away_final = last(away_score)) |> 
  ungroup() |> 
  # filter for games with a winner
  filter(home_final != away_final) |>
  # calculate loser's win probabilities
  mutate(wp_loser = if_else(home_final > away_final, away_wp, home_wp)) |>
  # group by game
  group_by(game_id) |>
  # get maximum win probability of the loser, one row per game
  reframe(home_team = home_team,
          away_team = away_team,
          home_final = home_final,
          away_final = away_final,
          max_wp_loser = max(wp_loser, na.rm = TRUE)) |>
  distinct() |>
  # ensure max_wp_loser is at least 0.5
  mutate(max_wp_loser = ifelse(max_wp_loser < 0.5, 0.5, max_wp_loser))

# Theoretical density for symmetric case: f(x) = 1/x^2 on [1/2, 1]
theoretical_density = function(x) {
  ifelse(x >= 0.5 & x <= 1, 1 / (x^2), 0)
}

# Plot distribution of max WP for Losers with Penn colors and theoretical curve
m_plot_empirical = ggplot(wp_data, aes(max_wp_loser)) +
  geom_histogram(aes(y = ..density..),
                 boundary = 0.5,
                 closed = "left",
                 binwidth = 0.025,
                 fill = "#990000", color = "white") +
  stat_function(fun = theoretical_density, color = "#011f5b", size = 1) +
  coord_cartesian(xlim = c(0.5, 1)) +
  labs(title = "Observed Distribution of Maximum Win Prob. of Loser",
       x = "Maximal Win Prob. Attained by Loser", y = "Density",
       caption = sprintf("Mean = %.3f, SD = %.3f", mean(wp_data$max_wp_loser), sd(wp_data$max_wp_loser))) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 10))

# Save NFL empirical plot
ggsave(file.path(out_dir, "nfl.png"), m_plot_empirical, width = 8, height = 4, dpi = 300)