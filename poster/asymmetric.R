#################
### LIBRARIES ###
#################

library(ggplot2)

#################
### PARAMETERS ##
#################

# Total possessions (max)
N = 10000

# Number of simulated games
n_sims = 10000

# Asymmetric scoring probabilities per possession
p_td_A = 0.505   # Team A touchdown probability
p_td_B = 0.500   # Team B touchdown probability

set.seed(09042025)

#################
### FUNCTIONS ###
#################

# Brownian-limit win probability for asymmetric case (with drift and variance):
# Approximates P(final_diff > 0) for Team A using normal approximation for the
# remaining sum of increments S = \sum_{i=1}^{n_rem} (Bern(pA) - Bern(pB)).
#
# Over the remaining n_rem possessions:
#   E[S] = (pA - pB) * n_rem
#   Var[S] = (pA(1-pA) + pB(1-pB)) * n_rem
# Current differential is d_t = scoreA - scoreB after t possessions.
# So final_diff = d_t + S. We approximate
#   P(final_diff > 0) \approx Phi( (d_t + E[S]) / sqrt(Var[S]) )
# Guard against zero variance when n_rem == 0.
win_prob_brownian_asym = function(t_index, d_t, N, pA, pB) {
  n_rem = N - t_index
  if (n_rem <= 0) {
    return(as.numeric(d_t > 0) + 0.5 * as.numeric(d_t == 0))
  }
  mu_step = pA - pB
  var_step = pA * (1 - pA) + pB * (1 - pB)
  mean_S = mu_step * n_rem
  var_S = var_step * n_rem
  if (var_S <= 0) {
    return(as.numeric(d_t + mean_S > 0) + 0.5 * as.numeric(d_t + mean_S == 0))
  }
  stats::pnorm((d_t + mean_S) / sqrt(var_S))
}

# Precompute win probabilities for faster lookup
precompute_win_probs_asym = function(N, pA, pB) {
  wp_table = array(NA_real_, dim = c(N + 1, 2 * N + 1))
  message("Precomputing win probabilities (asymmetric)...")
  pb = utils::txtProgressBar(min = 0, max = N, style = 3)
  for (t in 0:N) {
    n_rem = N - t
    for (d in -n_rem:n_rem) {
      wp_table[t + 1, d + N + 1] = win_prob_brownian_asym(t, d, N, pA, pB)
    }
    if (t %% 10 == 0 || t == N) utils::setTxtProgressBar(pb, t)
  }
  close(pb)
  wp_table
}

# Single-game simulation
sim_game_asym = function(N, pA, pB, wp_table = NULL) {
  d = 0; scoresA = scoresB = numeric(N + 1)
  WPs_A = numeric(N + 1)

  # Pre-generate all random samples for speed
  a_scores = rbinom(N, 1, pA)
  b_scores = rbinom(N, 1, pB)

  # t = 0 (no possessions played yet)
  WPs_A[1] = stats::pnorm(0)  # 0.5

  # possessions t = 1 .. N
  for (t in 1:N) {
    scoresA[t + 1] = scoresA[t] + a_scores[t]
    scoresB[t + 1] = scoresB[t] + b_scores[t]

    # score differential AFTER possession t
    d_t   = scoresA[t + 1] - scoresB[t + 1]

    # look up or compute WP for the new game state
    if (!is.null(wp_table)) {
      idx = d_t + N + 1
      if (idx >= 1 && idx <= (2 * N + 1)) {
        WPs_A[t + 1] = wp_table[t + 1, idx]
      } else {
        WPs_A[t + 1] = win_prob_brownian_asym(t, d_t, N, pA, pB)
      }
    } else {
      WPs_A[t + 1] = win_prob_brownian_asym(t, d_t, N, pA, pB)
    }
  }

  final_diff = scoresA[N + 1] - scoresB[N + 1]
  if (final_diff == 0) {
    final_diff = sample(c(-1, 1), 1)
  }
  loser = ifelse(final_diff < 0, "A", "B")
  list(WPs_A = WPs_A, loser = loser)
}

 

###########################################
### BASIC ASYMMETRIC (DESIGNATED LOSER) ###
###########################################

# Simulate maximum WP for a designated team, conditional on that team losing
simulate_max_wp_designated_loser = function(n_target, N, pA, pB, designated_team = c("A", "B")) {
  designated_team = match.arg(designated_team)
  wp_table = precompute_win_probs_asym(N, pA, pB)
  Mvals = numeric(n_target)
  collected = 0L
  sims_run = 0L
  message(sprintf("Starting simulation (asymmetric, designated %s loses)...", designated_team))
  pb = utils::txtProgressBar(min = 0, max = n_target, style = 3)
  while (collected < n_target) {
    game = sim_game_asym(N, pA, pB, wp_table)
    sims_run = sims_run + 1L
    if (designated_team == "A" && game$loser == "A") {
      WP_path = game$WPs_A
      collected = collected + 1L
      Mvals[collected] = max(WP_path, na.rm = TRUE)
      if (collected %% 10 == 0 || collected == n_target) utils::setTxtProgressBar(pb, collected)
    } else if (designated_team == "B" && game$loser == "B") {
      # Team B's WP is 1 - WPs_A
      WP_path_B = 1 - game$WPs_A
      collected = collected + 1L
      Mvals[collected] = max(WP_path_B, na.rm = TRUE)
      if (collected %% 10 == 0 || collected == n_target) utils::setTxtProgressBar(pb, collected)
    }
  }
  close(pb)
  message(sprintf("Finished after %d total simulations to collect %d losses.", sims_run, n_target))
  Mvals
}

##################
### RUN & PLOT ###
##################

# Ensure output directory exists
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

 

# 2) Basic asymmetric: designated team conditional on losing (e.g., Team A)
n_conditional = n_sims
M_asym_A_loses = simulate_max_wp_designated_loser(n_conditional, N, p_td_A, p_td_B, designated_team = "A")
plot_data_asym_A = data.frame(M = M_asym_A_loses)

# Theoretical density for designated branch starting at s (Brownian-limit)
# Use per-possession signal-to-noise to approximate s = Phi(mu*)
s0 = stats::pnorm((p_td_A - p_td_B) / sqrt(p_td_A * (1 - p_td_A) + p_td_B * (1 - p_td_B)))
theoretical_density_desig = function(x, s) {
  ifelse(x >= s & x <= 1, (s / (1 - s)) * (1 / (x^2)), 0)
}
p_asym_A = ggplot(plot_data_asym_A, aes(M)) +
  geom_histogram(aes(y = ..density..), boundary = 0.5, closed = "left",
                 binwidth = 0.025, fill = "#990000", color = "white") +
  stat_function(fun = function(x) theoretical_density_desig(x, s0), color = "#011f5b", size = 1) +
  coord_cartesian(xlim = c(0.5, 1)) +
  labs(title = "Max WP of Designated Team (Conditional on Loss)",
       subtitle = sprintf("Team A loses, p_A = %.3f, p_B = %.3f", p_td_A, p_td_B),
       x = "Maximum Win Probability Attained", y = "Density") +
  theme_minimal()
ggsave("figures/max-wp-sim-asym-designated.png", p_asym_A, width = 8, height = 4, dpi = 300)


