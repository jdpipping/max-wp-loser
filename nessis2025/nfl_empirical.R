#################
### LIBRARIES ###
#################

library(nflfastR)
library(tidyverse)

##################
### PARAMETERS ###
##################

out_dir = "figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Penn colors
PENN_RED  = "#990000"
PENN_BLUE = "#011f5b"

##################
### LOAD DATA  ###
##################

# 2002–2024, evenly matched (|spread| < 2)
pbp_data = load_pbp(2002:2024)

wp_data = pbp_data |>
  filter(abs(spread_line) < 2) |>
  group_by(game_id) |>
  mutate(home_final = last(home_score),
         away_final = last(away_score)) |>
  ungroup() |>
  filter(home_final != away_final) |>
  mutate(wp_loser = if_else(home_final > away_final, away_wp, home_wp)) |>
  group_by(game_id) |>
  reframe(home_team = home_team,
          away_team = away_team,
          home_final = home_final,
          away_final = away_final,
          max_wp_loser = max(wp_loser, na.rm = TRUE)) |>
  distinct() |>
  mutate(max_wp_loser = ifelse(max_wp_loser < 0.5, 0.5, max_wp_loser))

#############
### PLOTS ###
#############

# Histogram (Penn red)
m_theoretical_density = function(x) {
  ifelse(x >= 0.5 & x <= 1, 1 / (x^2), 0)
}
m_plot_empirical = ggplot(wp_data, aes(max_wp_loser)) +
  geom_histogram(aes(y = ..density..),
                 boundary = 0.5,
                 closed = "left",
                 binwidth = 0.025,
                 fill = PENN_RED, color = "white") +
  stat_function(fun = m_theoretical_density, color = PENN_BLUE, size = 1) +
  geom_text(data = data.frame(x = 0.95, y = 1.5,
                              label = "f(x) = 1/x^2"),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = PENN_BLUE, size = 3.5, hjust = 0) +
  coord_cartesian(xlim = c(0.5, 1)) +
  labs(title = "Empirical Distribution of Maximum Win Probability of Loser",
       x = "Maximum Win Probability Attained", y = "Density",
       caption = sprintf("Games = %d; Mean = %.3f, SD = %.3f",
                         nrow(wp_data), mean(wp_data$max_wp_loser), sd(wp_data$max_wp_loser))) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 10), legend.position = "none")
ggsave(file.path(out_dir, "nfl_even_hist.png"), m_plot_empirical, width = 8, height = 4, dpi = 300)

# Threshold curve with quartile markers
alpha_values = seq(0.5, 1, by = 0.01)
empirical_probs = sapply(alpha_values, function(a) mean(wp_data$max_wp_loser >= a))
plot_data = data.frame(alpha = alpha_values,
                       empirical = empirical_probs)

# quartile points
quartiles = c(0.75, 0.5, 0.25)
points = data.frame(
  empirical = quartiles,
  alpha = sapply(quartiles, function(p0) {
    idx = which(diff(sign(empirical_probs - p0)) != 0)[1]
    if (is.na(idx)) return(NA_real_)
    x1 = alpha_values[idx]; x2 = alpha_values[idx + 1]
    y1 = empirical_probs[idx] - p0; y2 = empirical_probs[idx + 1] - p0
    x1 - y1 * (x2 - x1) / (y2 - y1)
  })
)
points = points[!is.na(points$alpha), ]

theoretical_cdf = function(x) {
  # transform current curve: new = 1 - old
  old = ifelse(x >= 0.5 & x < 1, 2 - 1/x, NA)
  1 - old
}

threshold_plot_empirical = ggplot(plot_data, aes(x = alpha)) +
  geom_line(aes(y = empirical, color = "Empirical"), size = 1) +
  stat_function(aes(color = "Theoretical"), fun = theoretical_cdf, size = 1) +
  scale_color_manual(values = c("Empirical" = PENN_RED, "Theoretical" = PENN_BLUE)) +
  guides(color = guide_legend(title = NULL)) +
  geom_point(data = points, aes(x = alpha, y = empirical), color = PENN_BLUE, size = 3) +
  geom_text(data = points,
            aes(x = alpha, y = empirical,
                label = sprintf("(%.2f, %.2f)", alpha, empirical)),
            hjust = -0.2, vjust = -0.5, color = PENN_BLUE, size = 3) +
  xlim(0.5, 1) +
  labs(title = "Empirical Proportion of Games Where the Loser's Max Win Probability ≥ α",
       x = expression(alpha), y = "Proportion of Games") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "nfl_even_threshold.png"), threshold_plot_empirical, width = 8, height = 4, dpi = 300)

###########################
### SPREAD = 3 MIXTURE (p0)
###########################

# Filter games with |spread| == 3 and compute favorite win rate p0
fav_win_data = pbp_data |>
  filter(abs(spread_line) == 3) |>
  group_by(game_id) |>
  mutate(home_final = last(home_score),
         away_final = last(away_score)) |>
  ungroup() |>
  filter(home_final != away_final) |>
  distinct(game_id, home_team, away_team, spread_line, home_final, away_final) |>
  mutate(
    home_fav = spread_line > 0,
    fav_won = if_else(home_fav, home_final > away_final, away_final > home_final)
  )

prop_fav_wins = mean(fav_win_data$fav_won, na.rm = TRUE)
p0_mix = prop_fav_wins
lower_support = 1 - p0_mix

wp_data_spread = pbp_data |>
  filter(abs(spread_line) == 3) |>
  group_by(game_id) |>
  mutate(home_final = last(home_score),
         away_final = last(away_score)) |>
  ungroup() |>
  filter(home_final != away_final) |>
  mutate(wp_loser = if_else(home_final > away_final, away_wp, home_wp)) |>
  group_by(game_id) |>
  reframe(home_team = home_team,
          away_team = away_team,
          home_final = home_final,
          away_final = away_final,
          max_wp_loser = max(wp_loser, na.rm = TRUE)) |>
  distinct() |>
  mutate(max_wp_loser = ifelse(max_wp_loser < lower_support, lower_support, max_wp_loser))

# Mixture theoretical density and CDF for p0 >= 0.5
mixture_pdf = function(x, p0) {
  out = rep(0, length(x))
  out[x >= (1 - p0) & x < p0] = (1 - p0) / (x[x >= (1 - p0) & x < p0]^2)
  out[x >= p0 & x <= 1] = 1 / (x[x >= p0 & x <= 1]^2)
  out
}

mixture_cdf_base = function(x, p0) {
  ifelse(x < (1 - p0), 0,
         ifelse(x < p0, 1 - (1 - p0) / x,
                ifelse(x <= 1, 2 - 1 / x, NA)))
}

mixture_cdf = function(x, p0) {
  # transform current curve: new = 1 - old
  1 - mixture_cdf_base(x, p0)
}

# Histogram with mixture overlay
m_plot_spread = ggplot(wp_data_spread, aes(max_wp_loser)) +
  geom_histogram(aes(y = ..density..),
                 boundary = lower_support,
                 closed = "left",
                 binwidth = 0.025,
                 fill = PENN_RED, color = "white") +
  stat_function(fun = function(x) mixture_pdf(x, p0_mix), color = PENN_BLUE, size = 1) +
  geom_text(data = data.frame(x = 0.9, y = 1.5,
                              label = sprintf("mixture (p0=%.2f)", p0_mix)),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = PENN_BLUE, size = 3.5, hjust = 0) +
  coord_cartesian(xlim = c(lower_support, 1)) +
  labs(title = "Empirical Distribution of Maximum Win Probability of Loser",
       x = "Maximum Win Probability Attained", y = "Density",
       caption = sprintf("Games = %d; Mean = %.3f, SD = %.3f",
                         nrow(wp_data_spread), mean(wp_data_spread$max_wp_loser), sd(wp_data_spread$max_wp_loser))) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 10), legend.position = "none")
ggsave(file.path(out_dir, "nfl_spread_hist.png"), m_plot_spread, width = 8, height = 4, dpi = 300)

# Threshold with mixture CDF
alpha_values_mix = seq(lower_support, 1, by = 0.01)
empirical_probs_mix = sapply(alpha_values_mix, function(a) mean(wp_data_spread$max_wp_loser >= a))
plot_data_mix = data.frame(alpha = alpha_values_mix,
                           empirical = empirical_probs_mix)

threshold_plot_spread = ggplot(plot_data_mix, aes(x = alpha)) +
  geom_line(aes(y = empirical, color = "Empirical"), size = 1) +
  stat_function(aes(color = "Theoretical"), fun = function(x) mixture_cdf(x, p0_mix), size = 1) +
  scale_color_manual(values = c("Empirical" = PENN_RED, "Theoretical" = PENN_BLUE)) +
  guides(color = guide_legend(title = NULL)) +
  labs(title = "Empirical Proportion of Games Where the Loser's Max Win Probability ≥ α",
       x = expression(alpha), y = "Proportion of Games") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "nfl_spread_threshold.png"), threshold_plot_spread, width = 8, height = 4, dpi = 300)