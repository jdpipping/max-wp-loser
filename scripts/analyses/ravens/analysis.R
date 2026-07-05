# Ravens-specific max WP analysis: M | Y = 0
# (max WP of a designated team, conditional on that team losing)
#
# Run from repo root: Rscript scripts/analyses/ravens/analysis.R

#################
### LIBRARIES ###
#################

library(nflfastR)
library(tidyverse)
library(viridisLite)
library(knitr)
library(kableExtra)

args_full = commandArgs(trailingOnly = FALSE)
file_arg = args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path = normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path = normalizePath("scripts/analyses/ravens/analysis.R")
}
repo_root = normalizePath(file.path(dirname(script_path), "..", "..", ".."))

##################
### PARAMETERS ###
##################

TEAM = "BAL"
SEASONS = 2008:2025

ERA_PRE   = 2008:2017
ERA_LAMAR = 2018:2025

out_dir = file.path(repo_root, "presentations", "ravens", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

FILL_COLOR = "#440154"
LINE_COLOR = "#FDE725"

THRESHOLDS = c(0.80, 0.90, 0.95, 0.99)

##################
### FUNCTIONS  ###
##################

# Distribution of M | Y = 0 (max WP of a designated team, given they lose)
# If the team's pre-game WP is s, the conditional distribution on [s, 1) is:
#   PDF:      f(x; s) = s / ((1-s) * x^2)
#   CDF:      F(x; s) = 1 - s*(1-x) / (x*(1-s))
#   Survival: P(M >= x | Y=0) = s*(1-x) / (x*(1-s))
#
# This is NOT M_lambda (the paper's mixture over which team loses).
# This is the one-sided conditional for a specific team that we know lost.

cdf_m_given_loss = function(x, s) {
  u = numeric(length(x))
  u[x < s] = 0
  in_range = x >= s & x < 1
  u[in_range] = 1 - s[in_range] * (1 - x[in_range]) / (x[in_range] * (1 - s[in_range]))
  u[x >= 1] = 1
  pmin(pmax(u, 0), 1)
}

pvalue_m_given_loss = function(x, s) {
  p = numeric(length(x))
  p[x < s] = 1
  in_range = x >= s & x < 1
  p[in_range] = s[in_range] * (1 - x[in_range]) / (x[in_range] * (1 - s[in_range]))
  p[x >= 1] = 0
  pmin(pmax(p, 0), 1)
}

pit_analysis_team = function(max_wp, pregame_wp) {
  valid = !is.na(max_wp) & !is.na(pregame_wp)
  max_wp = max_wp[valid]
  pregame_wp = pregame_wp[valid]

  u = cdf_m_given_loss(max_wp, pregame_wp)
  pit_p = pvalue_m_given_loss(max_wp, pregame_wp)
  n = length(u)

  ks_result = tryCatch(
    stats::ks.test(pit_p, "punif", 0, 1, alternative = "greater", exact = FALSE),
    error = function(e) list(statistic = NA_real_, p.value = NA_real_)
  )

  list(n = n, u = u, pit_p = pit_p,
       ks_stat = as.numeric(ks_result$statistic),
       ks_pval = as.numeric(ks_result$p.value))
}

# Theoretical P(M > alpha | Y = 0) averaged over a set of pregame WPs
theoretical_exceed = function(alpha, pregame_wps) {
  mean(pvalue_m_given_loss(rep(alpha, length(pregame_wps)), pregame_wps))
}

##################
### LOAD DATA  ###
##################

message("Loading play-by-play data...")
pbp_data = load_pbp(SEASONS)

team_games = pbp_data |>
  filter(home_team == TEAM | away_team == TEAM) |>
  group_by(game_id) |>
  mutate(
    home_final = last(home_score),
    away_final = last(away_score)
  ) |>
  ungroup() |>
  filter(home_final != away_final) |>
  mutate(
    team_is_home = (home_team == TEAM),
    team_won = if_else(team_is_home, home_final > away_final, away_final > home_final),
    team_wp = if_else(team_is_home, home_wp, away_wp)
  )

pregame = team_games |>
  filter(!is.na(team_wp)) |>
  group_by(game_id) |>
  summarise(pregame_wp = first(team_wp), .groups = "drop")

losses = team_games |>
  filter(!team_won) |>
  group_by(game_id) |>
  summarise(
    season = first(season),
    max_wp = max(team_wp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(pregame, by = "game_id") |>
  mutate(max_wp = pmax(max_wp, pregame_wp))

losses_pre = losses |> filter(season %in% ERA_PRE)
losses_post    = losses |> filter(season %in% ERA_LAMAR)

message(sprintf("\n%s: %d losses total — Pre-Lamar Era %d, Lamar Era %d",
                TEAM, nrow(losses), nrow(losses_pre), nrow(losses_post)))

#################
### ANALYSIS  ###
#################

pit_pre = pit_analysis_team(losses_pre$max_wp, losses_pre$pregame_wp)
pit_post    = pit_analysis_team(losses_post$max_wp, losses_post$pregame_wp)

message(sprintf(
  "\nPre-Lamar Era (%d–%d):  n=%d, KS D=%.4f, p=%.4f",
  min(ERA_PRE), max(ERA_PRE),
  pit_pre$n, pit_pre$ks_stat, pit_pre$ks_pval))
message(sprintf(
  "Lamar Era (%d–%d): n=%d, KS D=%.4f, p=%.4f",
  min(ERA_LAMAR), max(ERA_LAMAR),
  pit_post$n, pit_post$ks_stat, pit_post$ks_pval))

##############################
### THRESHOLD TABLE (kable) ##
##############################

build_era_col = function(losses_df, suffix) {
  n = nrow(losses_df)
  empirical = sapply(THRESHOLDS, function(a) mean(losses_df$max_wp > a))
  theory    = sapply(THRESHOLDS, function(a) theoretical_exceed(a, losses_df$pregame_wp))
  tibble(
    alpha = sprintf("M > %.2f", THRESHOLDS),
    !!paste0("Observed", suffix) := sprintf("%.1f%% (%d/%d)", 100 * empirical, round(empirical * n), n),
    !!paste0("Theory", suffix)   := sprintf("%.1f%%", 100 * theory)
  )
}

pre_label   = sprintf("Pre-Lamar Era (%d–%d)", min(ERA_PRE), max(ERA_PRE))
lamar_label = sprintf("Lamar Era (%d–%d)", min(ERA_LAMAR), max(ERA_LAMAR))

tab_h = build_era_col(losses_pre,  "_pre")
tab_l = build_era_col(losses_post, "_lamar")

tab = left_join(tab_h, tab_l, by = "alpha")

ks_row = tibble(
  alpha = "KS p-value",
  Observed_pre   = sprintf("%.4f", pit_pre$ks_pval),
  Theory_pre     = "",
  Observed_lamar = sprintf("%.4f", pit_post$ks_pval),
  Theory_lamar   = ""
)

tab = bind_rows(tab, ks_row) |>
  rename(` ` = alpha,
         `Observed` = Observed_pre, `Theory` = Theory_pre,
         `Observed ` = Observed_lamar, `Theory ` = Theory_lamar)

kt = kable(tab, format = "html", escape = FALSE, align = c("l", rep("c", 4)),
           caption = sprintf("%s: Proportion of losses where max WP exceeds threshold", TEAM)) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") |>
  add_header_above(setNames(c(1, 2, 2), c(" ", pre_label, lamar_label))) |>
  row_spec(length(THRESHOLDS), extra_css = "border-bottom: 2px solid #333;")

save_kable(kt, file.path(out_dir, "threshold_table.html"))

message("\nThreshold table:")
print(as.data.frame(tab), row.names = FALSE)

#################
### PIT PLOTS ###
#################

plot_pit = function(pit_result, era_label, filename) {
  p = ggplot(data.frame(u = pit_result$u), aes(x = u)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.05, boundary = 0,
                   fill = FILL_COLOR, color = "white") +
    geom_hline(yintercept = 1, color = LINE_COLOR, linewidth = 1) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
    labs(x = expression("PIT" ~ U[i]), y = "Density",
         title = bquote(.(TEAM) ~ .(era_label) ~ ": PIT values for" ~ M ~ "|" ~ Y == 0),
         caption = sprintf("n = %d losses  |  KS D = %.4f, p = %.4f",
                           pit_result$n, pit_result$ks_stat, pit_result$ks_pval)) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 9))
  ggsave(file.path(out_dir, filename), p, width = 7, height = 4.5, dpi = 300)
}

plot_pit(pit_pre,  sprintf("Pre-Lamar Era (%d–%d)",  min(ERA_PRE),  max(ERA_PRE)),
         "pit_pre.png")
plot_pit(pit_post, sprintf("Lamar Era (%d–%d)", min(ERA_LAMAR), max(ERA_LAMAR)),
         "pit_lamar.png")

#########################
### PIT P-VALUE PLOTS ###
#########################

plot_pval = function(pit_result, era_label, filename) {
  p = ggplot(data.frame(p = pit_result$pit_p), aes(x = p)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.05, boundary = 0,
                   fill = FILL_COLOR, color = "white") +
    geom_hline(yintercept = 1, color = LINE_COLOR, linewidth = 1) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
    labs(x = expression(P(M >= m ~ "|" ~ Y == 0 ~ ", s")), y = "Density",
         title = bquote(.(TEAM) ~ .(era_label) ~ ": Right-tail PIT p-values"),
         caption = sprintf("P(p <= 0.10) = %.3f  |  P(p <= 0.05) = %.3f  |  P(p <= 0.01) = %.3f",
                           mean(pit_result$pit_p <= 0.10),
                           mean(pit_result$pit_p <= 0.05),
                           mean(pit_result$pit_p <= 0.01))) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 9))
  ggsave(file.path(out_dir, filename), p, width = 7, height = 4.5, dpi = 300)
}

plot_pval(pit_pre,  sprintf("Pre-Lamar Era (%d–%d)",  min(ERA_PRE),  max(ERA_PRE)),
          "pvalue_pre.png")
plot_pval(pit_post, sprintf("Lamar Era (%d–%d)", min(ERA_LAMAR), max(ERA_LAMAR)),
          "pvalue_lamar.png")

#######################
### SEASON SUMMARY  ###
#######################

season_summary = losses |>
  group_by(season) |>
  summarise(
    n = n(),
    mean_max_wp = mean(max_wp),
    mean_pregame = mean(pregame_wp),
    heartbreakers_10 = sum(pvalue_m_given_loss(max_wp, pregame_wp) <= 0.10),
    .groups = "drop"
  )

write.csv(season_summary, file.path(out_dir, "season_summary.csv"), row.names = FALSE)

writeLines(sprintf(
  paste0("team=%s\n",
         "pre_lamar_era:  n=%d KS_D=%.4f KS_p=%.4f\n",
         "lamar_era:      n=%d KS_D=%.4f KS_p=%.4f\n",
         "distribution=M|Y=0 (designated team, conditional on loss)"),
  TEAM,
  pit_pre$n, pit_pre$ks_stat, pit_pre$ks_pval,
  pit_post$n, pit_post$ks_stat, pit_post$ks_pval
), file.path(out_dir, "pit_summary.txt"))

####################################
### THEORY PLOTS: M_lambda and M|Y=0
####################################

p0_values = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
x_values = seq(0.001, 0.999, by = 0.001)

format_p0_label = function(p0) {
  if (p0 == 0.99 || p0 == 0.95) sprintf("s = %.2f", p0)
  else sprintf("s = %.1f", p0)
}

gradient_colors = viridis(length(p0_values) + 1, option = "viridis")[1:length(p0_values)]

# --- M_lambda (mixture: max WP of eventual loser) ---

mlambda_upper_tail = function(x, p0) {
  if (p0 < 0.5) return(mlambda_upper_tail(x, 1 - p0))
  case_when(
    x < 1 - p0 ~ 1,
    x < p0     ~ (1 - p0) / x,
    x >= p0 & x < 1 ~ (1 - x) / x,
    x >= 1 ~ 0,
    TRUE ~ NA_real_
  )
}

plot_data_mlambda = expand_grid(p0 = p0_values, x = x_values) |>
  mutate(
    upper_tail = map2_dbl(x, p0, mlambda_upper_tail),
    p0_label = factor(map_chr(p0, format_p0_label),
                      levels = map_chr(p0_values, format_p0_label))
  ) |>
  filter(!is.na(upper_tail))

p_mlambda_ut = ggplot(plot_data_mlambda, aes(x = x, y = upper_tail, color = p0_label)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = gradient_colors, name = NULL) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = expression("Upper Tail of" ~ M[lambda] ~ "(Max WP of Eventual Loser)"),
       x = "x", y = expression(P(M[lambda] >= x))) +
  theme_minimal()
ggsave(file.path(out_dir, "theory_mlambda_upper_tail.png"), p_mlambda_ut, width = 8, height = 5, dpi = 300)

# --- M | Y = 0 (max WP of designated team, given loss) ---

m_cond_upper_tail_scalar = function(x, s) {
  if (x < s) return(1)
  if (x >= 1) return(0)
  s * (1 - x) / (x * (1 - s))
}

plot_data_mcond = expand_grid(p0 = p0_values, x = x_values) |>
  mutate(
    upper_tail = map2_dbl(x, p0, m_cond_upper_tail_scalar),
    p0_label = factor(map_chr(p0, format_p0_label),
                      levels = map_chr(p0_values, format_p0_label))
  )

p_mcond_ut = ggplot(plot_data_mcond, aes(x = x, y = upper_tail, color = p0_label)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = gradient_colors, name = NULL) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = expression("Upper Tail of" ~ M ~ "|" ~ Y == 0 ~ "(Max WP Given Loss)"),
       x = "x", y = expression(P(M >= x ~ "|" ~ Y == 0))) +
  theme_minimal()
ggsave(file.path(out_dir, "theory_m_cond_upper_tail.png"), p_mcond_ut, width = 8, height = 5, dpi = 300)

message(sprintf("\nDone! Output saved to %s/", out_dir))
