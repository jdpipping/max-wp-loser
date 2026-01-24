#################
### LIBRARIES ###
#################

library(tidyverse)

##################
### PARAMETERS ###
##################

out_dir = "../figures/nfl"
data_dir = "../data/nfl"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Penn colors
PENN_RED  = "#990000"
PENN_BLUE = "#011f5b"

# Minimum sample size per bin
MIN_SAMPLE_SIZE = 100

# Bin centers and ranges (centered bins: [center - 0.025, center + 0.025))
bin_centers = seq(0.5, 0.95, by = 0.05)
bin_width = 0.05

##################
### FUNCTIONS ###
##################

bin_games = function(game_data) {
  bin_lower = bin_centers - bin_width / 2
  bin_upper = bin_centers + bin_width / 2

  game_data |>
    mutate(
      bin_idx = map_int(starting_wp_favored, function(wp) {
        matches = which(wp >= bin_lower & wp < bin_upper)
        if (length(matches) > 0) matches[1] else NA_integer_
      }),
      bin_center = ifelse(!is.na(bin_idx), bin_centers[bin_idx], NA_real_),
      bin_label = ifelse(!is.na(bin_idx),
                         sprintf("p0=%.2f", bin_centers[bin_idx]),
                         NA_character_)
    ) |>
    filter(!is.na(bin_center))
}

theoretical_cdf = function(x, p0) {
  y = numeric(length(x))
  y[x < (1 - p0)] = 0
  mid = x >= (1 - p0) & x < p0
  y[mid] = 1 - (1 - p0) / x[mid]
  high = x >= p0 & x < 1
  y[high] = 2 - 1 / x[high]
  y[x >= 1] = 1
  pmin(pmax(y, 0), 1)
}

theoretical_pdf = function(x, p0) {
  y = numeric(length(x))
  mid = x >= (1 - p0) & x < p0
  y[mid] = (1 - p0) / (x[mid]^2)
  high = x >= p0 & x < 1
  y[high] = 1 / (x[high]^2)
  y
}


ks_statistic = function(x, p0) {
  n = length(x)
  if (n == 0) return(NA_real_)
  x = sort(x)
  emp_right = (1:n) / n
  emp_left = (0:(n - 1)) / n
  th = theoretical_cdf(x, p0)
  max(c(max(abs(emp_right - th)), max(abs(emp_left - th))))
}


ks_test_pvalue = function(x, p0) {
  if (length(x) == 0) return(NA_real_)
  tryCatch({
    stats::ks.test(x, function(z) theoretical_cdf(z, p0), exact = FALSE)$p.value
  }, error = function(e) NA_real_)
}

kl_divergence = function(x, p0) {
  if (length(x) == 0) return(NA_real_)
  breaks = seq(0, 1, by = bin_width)
  if (tail(breaks, 1) < 1) breaks = c(breaks, 1)
  h = hist(x, breaks = breaks, plot = FALSE)
  emp = h$counts / sum(h$counts)
  th = diff(theoretical_cdf(breaks, p0))
  eps = 1e-12
  emp = pmax(emp, eps)
  th = pmax(th, eps)
  sum(emp * log(emp / th))
}

analyze_binned_data = function(binned_data, out_dir_fig) {
  valid_bins = binned_data |>
    group_by(bin_center) |>
    summarise(n = n(), .groups = "drop") |>
    filter(n >= MIN_SAMPLE_SIZE) |>
    pull(bin_center)

  if (length(valid_bins) == 0) {
    message("No bins with sufficient sample size. Adjust MIN_SAMPLE_SIZE or collect more data.")
    return(NULL)
  }

  summary_stats = binned_data |>
    filter(bin_center %in% valid_bins) |>
    group_by(bin_center) |>
    summarise(
      n = n(),
      mean_max_wp = mean(max_wp_loser, na.rm = TRUE),
      sd_max_wp = sd(max_wp_loser, na.rm = TRUE),
      ks_stat = ks_statistic(max_wp_loser, first(bin_center)),
      ks_pvalue = ks_test_pvalue(max_wp_loser, first(bin_center)),
      kl_div = kl_divergence(max_wp_loser, first(bin_center)),
      .groups = "drop"
    )

  message("\nSummary Statistics by Favored Team Starting WP Bin:")
  print(summary_stats)

  grid_bins = sort(intersect(valid_bins, c(0.50, 0.60, 0.70, 0.80)))
  if (length(grid_bins) > 0) {
    label_map = summary_stats |>
      filter(bin_center %in% grid_bins) |>
      mutate(label = sprintf("p[0]==%.2f*' (n=%d)'", bin_center, as.integer(n))) |>
      select(bin_center, label)

    emp_grid = map_dfr(grid_bins, function(p0) {
      bin_data = binned_data |>
        filter(bin_center == p0)
      ecdf_fn = stats::ecdf(bin_data$max_wp_loser)
      x_vals = sort(unique(bin_data$max_wp_loser))
      label = label_map$label[match(p0, label_map$bin_center)]
      data.frame(
        x = x_vals,
        y = ecdf_fn(x_vals),
        type = "Empirical",
        p0_label = label
      )
    })

    theory_grid = map_dfr(grid_bins, function(p0) {
      x_min = max(0, 1 - p0 - 0.05)
      theory_x = seq(max(0.001, x_min), 0.999, by = 0.001)
      label = label_map$label[match(p0, label_map$bin_center)]
      data.frame(
        x = theory_x,
        y = theoretical_cdf(theory_x, p0),
        type = "Theoretical",
        p0_label = label
      )
    })

    p_cdf_grid = ggplot() +
      geom_step(data = emp_grid, aes(x = x, y = y, color = type), linewidth = 1) +
      geom_line(data = theory_grid, aes(x = x, y = y, color = type), linewidth = 1) +
      facet_wrap(~p0_label, ncol = 2, scales = "free_x", labeller = label_parsed) +
      scale_color_manual(values = c("Empirical" = PENN_RED, "Theoretical" = PENN_BLUE), name = NULL) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
      labs(
        x = "Maximum Win Probability Attained by Eventual Loser",
        y = "Cumulative Probability"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggsave(file.path(out_dir_fig, "cdf_grid.png"),
           p_cdf_grid, width = 8, height = 6, dpi = 300)
  }

  for (p0 in valid_bins) {
    bin_data = binned_data |>
      filter(bin_center == p0)

    tag = sprintf("%.2f", p0)

    x_min = max(0, 1 - p0 - 0.05)
    theory_x = seq(max(0.001, x_min), 0.999, by = 0.001)
    pdf_df = data.frame(x = theory_x, y = theoretical_pdf(theory_x, p0))

    p_hist = ggplot(bin_data, aes(max_wp_loser)) +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = 0.025,
                     fill = PENN_RED, color = "white") +
      geom_line(data = pdf_df, aes(x = x, y = y), color = PENN_BLUE, linewidth = 1, inherit.aes = FALSE) +
      coord_cartesian(xlim = c(x_min, 1)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
      labs(
        x = "Maximum Win Probability Attained by Eventual Loser",
        y = "Density"
      ) +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0, size = 10))

    ecdf_fn = stats::ecdf(bin_data$max_wp_loser)
    x_vals = sort(unique(bin_data$max_wp_loser))
    cdf_df = data.frame(x = x_vals, y = ecdf_fn(x_vals))
    theory_df = data.frame(x = theory_x, y = theoretical_cdf(theory_x, p0))
    p_cdf = ggplot() +
      geom_step(data = cdf_df, aes(x = x, y = y, color = "Empirical"), linewidth = 1) +
      geom_line(data = theory_df, aes(x = x, y = y, color = "Theoretical"), linewidth = 1) +
      scale_color_manual(values = c("Empirical" = PENN_RED, "Theoretical" = PENN_BLUE), name = NULL) +
      coord_cartesian(xlim = c(x_min, 1), ylim = c(0, 1)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
      labs(
        x = "Maximum Win Probability Attained by Eventual Loser",
        y = "Cumulative Probability"
      ) +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0, size = 10),
            legend.position = "none")

    ggsave(file.path(out_dir_fig, sprintf("hist_%s.png", tag)),
           p_hist, width = 5, height = 4, dpi = 300)
    ggsave(file.path(out_dir_fig, sprintf("cdf_%s.png", tag)),
           p_cdf, width = 5, height = 4, dpi = 300)
  }

  return(summary_stats)
}

##################
### MAIN EXECUTION ###
##################

all_games_path = file.path(data_dir, "all_games.csv")
if (!file.exists(all_games_path)) {
  season_files = list.files(data_dir, pattern = "_games\\.csv$", full.names = TRUE)
  season_files = season_files[!grepl("all_games\\.csv$", season_files)]
  if (length(season_files) == 0) {
    stop("No game files found under data/nfl.")
  }
  all_games = bind_rows(lapply(season_files, read.csv))
  write.csv(all_games, all_games_path, row.names = FALSE)
} else {
  all_games = read.csv(all_games_path)
}

binned_data = bin_games(all_games)
write.csv(binned_data, file.path(data_dir, "all_binned.csv"), row.names = FALSE)

summary_stats = analyze_binned_data(binned_data, out_dir)
if (!is.null(summary_stats)) {
  write.csv(summary_stats, file.path(data_dir, "all_binned_summary.csv"), row.names = FALSE)
}

message("\nNFL analysis complete!")
