#################
### LIBRARIES ###
#################

# install.packages(c("ggplot2", "gganimate", "nflfastR", "parallel", "pbapply", "pracma", "tidyverse"))
library(gganimate)
library(ggplot2)
library(nflfastR)
library(parallel)
library(pbapply)
library(pracma)
library(tidyverse)

#################
### PARAMETERS ##
#################

N = 10000     # nominal maximum possessions
n_sims = 10000   # number of simulated games
n_cores = detectCores() - 1  # use 1 less than available cores
p_td = 0.5     # touchdown probability for both teams

out_dir = "research-note/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#################
### FUNCTIONS ###
#################

# baseline pmf & cdf - vectorized for speed
pmf_base = function(n_rem, z) {
  if (abs(z) > n_rem) return(0)         # return 0 outside the support
  dbinom(n_rem + z, 2 * n_rem, 0.5)
}

cdf_base = function(n_rem, z) {
  if (z < -n_rem) return(0) # entire lower tail is 0
  pbinom(n_rem + z, 2 * n_rem, 0.5)
}

win_prob_base = function(n_rem, d_t) {
  1 - cdf_base(n_rem, -d_t) + 0.5*pmf_base(n_rem, -d_t)
}

# precompute win probabilities for baseline model for speed
precompute_win_probs = function(N) {
  wp_table = array(NA_real_, dim = c(N + 1, 2 * N + 1))
  
  message("Precomputing win probabilities...")
  pb = txtProgressBar(min = 0, max = N, style = 3)

  for (t in 0:N) {                 # start at t = 0
    n_rem = N - t                  # possessions still to play
    for (d in -n_rem:n_rem) {      # only the reachable differentials
      wp_table[t + 1, d + N + 1] = win_prob_base(n_rem, d)
    }
    setTxtProgressBar(pb, t)
  }

  close(pb)
  wp_table
}

# single-game simulation - optimized
sim_game = function(N, wp_table = NULL) {
  # simulate per-possession differentials and running d_t
  d = 0; scoresA = scoresB = numeric(N+1)
  WPs_A = numeric(N+1)  # tracking team A's WP specifically
  
  # pre-generate all random samples for speed
  a_scores = rbinom(N, 1, p_td)
  b_scores = rbinom(N, 1, p_td)
  
  # execute the game

  # t = 0 (no possessions played yet)
  WPs_A[1] = 0.5 # even game at kickoff
  
  # possessions t = 1 .. N
  for (t in 1:N) {
    # update scores with the t-th possession
    scoresA[t + 1] = scoresA[t] + a_scores[t]
    scoresB[t + 1] = scoresB[t] + b_scores[t]
    
    # score differential AFTER possession t
    d_t   = scoresA[t + 1] - scoresB[t + 1]
    n_rem = N - t # possessions still to play
    
    # look up or compute WP for the new game state
    if (!is.null(wp_table) && abs(d_t) <= n_rem) {
      WPs_A[t + 1] = wp_table[t + 1, d_t + N + 1]
    } else {
      WPs_A[t + 1] = win_prob_base(n_rem, d_t)
    }
  }
  final_diff = scoresA[N+1] - scoresB[N+1]
  # In case of tie, break randomly
  if (final_diff == 0) {
    final_diff = sample(c(-1, 1), 1)
  }
  loser = ifelse(final_diff < 0, "A", "B")
  list(WPs_A = WPs_A, loser = loser, final_diff = final_diff, 
       scoresA = scoresA, scoresB = scoresB)
}

# wrapper to simulate M_N and T_N using parallel processing
simulate_max_wp = function(n_sims, N) {
  # precompute baseline table
  wp_table = precompute_win_probs(N)
  
  message("Starting simulation...")
  
  # Use sequential processing for progress bar visibility
  Mvals = Tvals = numeric(n_sims)
  all_games = vector("list", n_sims)
  high_wp_game = NULL  # Store the first game with max WP >= 0.99
  
  # Setup progress bar
  pb = txtProgressBar(min = 0, max = n_sims, style = 3)
  
  for (j in seq_len(n_sims)) {
    successful = FALSE
    max_attempts = 5  # Limit retry attempts
    
    for (attempt in 1:max_attempts) {
      tryCatch({
        game = sim_game(N, wp_table)
        
        # Get the correct WP trajectory for the loser
        if (game$loser == "A") {
          WP_loser = game$WPs_A
        } else {
          WP_loser = 1 - game$WPs_A
        }
        
        # Store results
        Mvals[j] = max(WP_loser, na.rm = TRUE)
        max_idx = which.max(WP_loser) - 1
        if (length(max_idx) > 0) {
          Tvals[j] = max_idx
          all_games[[j]] = game
          
          # Check if this is the first game with max WP between 0.90 and 0.95
          if (is.null(high_wp_game) && Mvals[j] >= 0.95 && Mvals[j] < 0.99) {
            high_wp_game = list(
              game = game,
              max_wp = Mvals[j],
              max_idx = max_idx
            )
          }
          
          successful = TRUE
          break
        }
      }, error = function(e) {
        # Just continue to next attempt
      })
    }
    
    # If all attempts failed, use default values
    if (!successful) {
      Mvals[j] = NA
      Tvals[j] = NA
    }
    
    # Update progress bar
    if (j %% 10 == 0 || j == n_sims) {
      setTxtProgressBar(pb, j)
    }
  }
  
  # Close progress bar
  close(pb)
  
  # Remove any NA values
  valid_idx = !is.na(Mvals) & !is.na(Tvals)
  if (sum(valid_idx) > 0) {
    M_all = Mvals[valid_idx]
    T_all = Tvals[valid_idx]
    all_games = all_games[valid_idx]
  } else {
    M_all = T_all = numeric(0)
    all_games = list()
  }
  
  list(M = M_all, T = T_all, games = all_games, high_wp_game = high_wp_game)
}

################
### SIMULATE ###
################

run_simulation = function() {
  # run simulation
  set.seed(06022025)
  res = simulate_max_wp(n_sims, N)
  
  # Skip if no valid results
  if (length(res$M) == 0) {
    message("No valid results, skipping plots")
    return(NULL)
  }
  
  # Create data frame for plotting
  plot_data = data.frame(M = res$M, T = res$T/N)
  
  # plot empirical M_N
  m_plot = ggplot(plot_data, aes(M)) +
    geom_histogram(aes(y = ..density..),
                   boundary = 0.5,      # left edge of the first bin
                   closed = "left",   # include 0.5 in the first bin
                   binwidth = 0.025,
                   fill = "#FF6B6B", color = "white") +
    labs(title = "Distribution of Maximum Win Prob. of Loser",
         x = "Maximum Win Prob. Attained", y = "Density",
         caption = sprintf("Mean = %.3f, SD = %.3f", mean(res$M), sd(res$M))) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 10))
  print(m_plot)
  
  # plot empirical T/N
  t_plot = ggplot(plot_data, aes(T)) +
    geom_histogram(aes(y = ..count../sum(..count..)),
                   boundary = 0,      # left edge of the first bin
                   closed = "left",   # include 0 in the first bin
                   binwidth = 0.05,
                   fill = "#FF6B6B", color = "white") +
    labs(title = "Distribution of Time of Maximum Win Prob. of Loser",
         x = "Time Maximum Win Prob. is Attained", y = "Proportion",
         caption = sprintf("Mean = %.3f, SD = %.3f", mean(res$T/N), sd(res$T/N))) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 10))
  print(t_plot)
  
  # plot of P(Max WP of Loser > alpha)
  alpha_values = seq(0, 1, by = 0.01)
  empirical_probs = sapply(alpha_values, function(alpha) mean(res$M >= alpha))
  
  # Create main plot data frame with all values
  plot_data = data.frame(
    alpha = alpha_values,
    empirical = empirical_probs,
    identity = 1 - alpha_values
  )
  
  # Find target points and intersection in one go
  target_probs = c(1, 0.75, 0.5, 0.25)
  points = data.frame(
    empirical = target_probs,
    alpha = sapply(target_probs, function(p) {
      idx = which(diff(sign(empirical_probs - p)) != 0)[1]
      if (is.na(idx)) return(NA)
      x1 = alpha_values[idx]
      x2 = alpha_values[idx + 1]
      y1 = empirical_probs[idx] - p
      y2 = empirical_probs[idx + 1] - p
      x1 - y1 * (x2 - x1)/(y2 - y1)
    }),
    is_intersection = FALSE
  )
  
  # Add intersection point
  last_above_idx = tail(which(plot_data$empirical > plot_data$identity), 1)
  if (length(last_above_idx) > 0 && last_above_idx < nrow(plot_data)) {
    cross_idx = last_above_idx
    x1 = alpha_values[cross_idx]
    x2 = alpha_values[cross_idx + 1]
    y1 = plot_data$empirical[cross_idx] - plot_data$identity[cross_idx]
    y2 = plot_data$empirical[cross_idx + 1] - plot_data$identity[cross_idx + 1]
    points = rbind(points, data.frame(
      empirical = mean(res$M >= (x1 - y1 * (x2 - x1)/(y2 - y1))),
      alpha = x1 - y1 * (x2 - x1)/(y2 - y1),
      is_intersection = TRUE
    ))
  }
  
  # Remove NA values and add display values
  points = points[!is.na(points$alpha),]
  points$alpha_display = round(points$alpha, 2)
  points$empirical_display = round(points$empirical, 2)
  
  threshold_plot = ggplot(plot_data, aes(x = alpha)) +
    geom_line(aes(y = empirical), color = "#FF6B6B", size = 1) +
    geom_line(aes(y = identity), color = "gray", linetype = "dashed", size = 1) +
    geom_point(data = points, aes(x = alpha, y = empirical), 
               color = "blue", size = 3) +
    geom_text(data = points[!points$is_intersection & points$alpha == 0.50,], 
              aes(x = alpha, y = empirical, 
                  label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
              hjust = -0.2, color = "blue") +
    geom_text(data = points[!points$is_intersection & points$alpha != 0.50,], 
              aes(x = alpha, y = empirical, 
                  label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
              hjust = -0.2, vjust = -0.5, color = "blue") +
    geom_text(data = points[points$is_intersection,],
              aes(x = alpha, y = empirical,
                  label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
              vjust = -0.5, hjust = -0.2, color = "blue") +
    labs(
      title = expression(paste("Probability of Loser Achieving a Win Prob. of at Least ", alpha)),
      x = expression(alpha),
      y = expression(paste("P(Max Win Prob. of Loser >= ", alpha, ")"))
    ) +
    xlim(0.5, 1) +
    theme_minimal()
    
  print(threshold_plot)
  
  # If we found a high WP game, create animation for it
  if (!is.null(res$high_wp_game)) {
    game = res$high_wp_game$game
    
    # Create animation data
    anim_data = data.frame(
      possession = 0:N,
      score_a = game$scoresA,
      score_b = game$scoresB,
      wp = if(game$loser == "A") game$WPs_A else 1 - game$WPs_A
    )
    
    # Add column to track if this is the max WP point
    anim_data$is_max = anim_data$wp == max(anim_data$wp)
    
    # Data for labels on static plot
    label_data = anim_data[c(which.max(anim_data$wp), nrow(anim_data)), ]
    label_data = unique(label_data)
    
    # Create static plot of the trajectory
    static_plot = ggplot(anim_data, aes(x = possession)) +
      geom_line(aes(y = wp), color = "#FF6B6B", size = 1) +
      geom_point(data = anim_data[anim_data$is_max, ], aes(y = wp), color = "blue", size = 2) +
      geom_text(data = label_data,
                aes(y = wp, label = sprintf("A: %d - B: %d", score_a, score_b)),
                vjust = -1) +
      labs(
        title = sprintf("Game with Maximum Win Prob. of Loser = %.1f%%", res$high_wp_game$max_wp * 100),
        x = "Possession",
        y = sprintf("Win Prob. of Eventual Loser (%s)", game$loser)
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = round(seq(0, N, length.out = 5))) +
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
      coord_cartesian(xlim = c(-N/20, N + N/10), ylim = c(0, 1.05)) +
      theme(legend.position = "none")  # Hide the legend since we don't need it
    
    # Create animated plot
    anim_plot = ggplot(anim_data, aes(x = possession)) +
      geom_line(aes(y = wp), color = "#FF6B6B", size = 1) +
      geom_point(aes(y = wp, color = is_max), size = 1) +
      geom_text(aes(x = possession, y = wp, 
                    label = sprintf("A: %d - B: %d", score_a, score_b)),
                vjust = -0.5) +
      scale_color_manual(values = c("#FF6B6B", "blue")) +
      labs(
        title = sprintf("Game with Maximum Win Prob. of Loser = %.1f%%", res$high_wp_game$max_wp * 100),
        x = "Possession",
        y = sprintf("Win Prob. of Eventual Loser (%s)", game$loser)
      ) +
      theme_minimal() +
      transition_reveal(possession, keep_last = TRUE) +
      ease_aes('linear') +
      scale_x_continuous(breaks = round(seq(0, N, length.out = 5))) +
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
      coord_cartesian(xlim = c(-N/20, N + N/10), ylim = c(0, 1.05)) +
      theme(legend.position = "none")  # Hide the legend since we don't need it
    
    # Save all plots
    ggsave(file.path(out_dir, "max-wp-sim.png"), m_plot, width = 6, height = 5, dpi = 300)
    ggsave(file.path(out_dir, "time-max-wp-sim.png"), t_plot, width = 6, height = 5, dpi = 300)
    ggsave(file.path(out_dir, "threshold-sim.png"), threshold_plot, width = 6, height = 5, dpi = 300)
    ggsave(file.path(out_dir, "high-wp-game.png"), static_plot, width = 6, height = 5, dpi = 300)  # Save static plot
    anim_save(file.path(out_dir, "high-wp-game.gif"), anim_plot, 
              duration = 30, fps = 20, width = 6, height = 5, units = "in", res = 300,
              end_pause = 100)  # 100 frames = 5 seconds at 20 fps
  }
  
  return(res)
}

# run simulation
results = run_simulation()

#######################
### EMPIRICAL PLOTS ###
#######################

# load data for 2002 - 2023
pbp_data = load_pbp(2002:2023)

# real wp data
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

# number of games
num_games = nrow(wp_data)

# plot distribution of max wp for Losers
m_plot_empirical = ggplot(wp_data, aes(max_wp_loser)) +
  geom_histogram(aes(y = ..density..),
                 boundary = 0.5,      # left edge of the first bin
                 closed = "left",   # include 0.5 in the first bin
                 binwidth = 0.025,
                 fill = "#FF6B6B", color = "white") +
  labs(title = "Observed Distribution of Maximum Win Prob. of Loser",
       x = "Maximum Win Prob. Attained", y = "Density",
       caption = sprintf("Mean = %.3f, SD = %.3f", mean(wp_data$max_wp_loser), sd(wp_data$max_wp_loser))) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 10))

# plot of p(max wp of Loser > alpha)
alpha_values = seq(0.5, 1, by = 0.01)  # Changed to start at 0.5
empirical_probs = sapply(alpha_values, function(alpha) mean(wp_data$max_wp_loser >= alpha))

# create main plot data frame with all values
plot_data = data.frame(
  alpha = alpha_values,
  empirical = empirical_probs,
  identity = 1 - alpha_values
)

# find target points and intersection
target_probs = c(1, 0.75, 0.5, 0.25)
points = data.frame(
  empirical = target_probs,
  alpha = sapply(target_probs, function(p) {
    idx = which(diff(sign(empirical_probs - p)) != 0)[1]
    if (is.na(idx)) return(NA)
    x1 = alpha_values[idx]
    x2 = alpha_values[idx + 1]
    y1 = empirical_probs[idx] - p
    y2 = empirical_probs[idx + 1] - p
    x1 - y1 * (x2 - x1)/(y2 - y1)
  }),
  is_intersection = FALSE
)

# add intersection point
last_above_idx = tail(which(plot_data$empirical > plot_data$identity), 1)
if (length(last_above_idx) > 0 && last_above_idx < nrow(plot_data)) {
  cross_idx = last_above_idx
  x1 = alpha_values[cross_idx]
  x2 = alpha_values[cross_idx + 1]
  y1 = plot_data$empirical[cross_idx] - plot_data$identity[cross_idx]
  y2 = plot_data$empirical[cross_idx + 1] - plot_data$identity[cross_idx + 1]
  points = rbind(points, data.frame(
    empirical = mean(wp_data$max_wp_loser >= (x1 - y1 * (x2 - x1)/(y2 - y1))),
    alpha = x1 - y1 * (x2 - x1)/(y2 - y1),
    is_intersection = TRUE
  ))
}

# remove na values and add display values
points = points[!is.na(points$alpha),]
points$alpha_display = round(points$alpha, 2)
points$empirical_display = round(points$empirical, 2)

threshold_plot_empirical = ggplot(plot_data, aes(x = alpha)) +
  geom_line(aes(y = empirical), color = "#FF6B6B", size = 1) +
  geom_line(aes(y = identity), color = "gray", linetype = "dashed", size = 1) +
  geom_point(data = points, aes(x = alpha, y = empirical), 
             color = "blue", size = 3) +
  geom_text(data = points[!points$is_intersection & points$alpha == 0.50,], 
            aes(x = alpha, y = empirical, 
                label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
            hjust = -0.2, color = "blue") +
  geom_text(data = points[!points$is_intersection & points$alpha != 0.50,], 
            aes(x = alpha, y = empirical, 
                label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
            hjust = -0.2, vjust = -0.5, color = "blue") +
  geom_text(data = points[points$is_intersection,],
            aes(x = alpha, y = empirical,
                label = sprintf("(%.2f, %.2f)", alpha_display, empirical_display)),
            vjust = -0.5, hjust = -0.2, color = "blue") +
  labs(
    title = expression(paste("Observed Prop. of Loser Achieving a Win Prob. of at Least ", alpha)),
    x = expression(alpha),
    y = expression(paste("P(Max Win Prob. of Loser >= ", alpha, ")"))
  ) +
  xlim(0.5, 1) +
  theme_minimal()

# save plots
ggsave(file.path(out_dir, "max-wp-emp.png"), m_plot_empirical, width = 6, height = 5, dpi = 300)
ggsave(file.path(out_dir, "threshold-emp.png"), threshold_plot_empirical, width = 6, height = 5, dpi = 300)