# Generate distribution figures for the manuscript/AOAS deliverables.
# Run from repo root: Rscript scripts/manuscript/n-player.R

library(tidyverse)

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath("scripts/manuscript/n-player.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(repo_root, "R", "plot-style.R"))

# Define the CDF F(M_omega) for symmetric n-player games
nplayer_cdf = function(x, n) {
  case_when(
    x < 0 ~ 0,
    x >= 0 & x < 1/n ~ (n - 1) * x / (1 - x),
    x >= 1/n ~ 1,
    TRUE ~ NA_real_
  )
}

# Create data for plotting
n_values = c(2, 3, 4, 5, 6, 7, 8, 9, 10)
x_values = seq(0, 1, by = 0.001)

format_n_label = function(n) {
  sprintf("n = %d", n)
}

# CDF plot data
plot_data_cdf = expand_grid(
  n = n_values,
  x = x_values
) %>%
  mutate(
    cdf = map2_dbl(x, n, nplayer_cdf),
    n_label = factor(map_chr(n, format_n_label),
                    levels = map_chr(n_values, format_n_label))
  ) %>%
  filter(!is.na(cdf))

gradient_colors <- paper_palette_seq(length(n_values), begin = 0.20, end = 0.88)

# Create the CDF plot
p_cdf = ggplot(plot_data_cdf, aes(x = x, y = cdf, color = n_label)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = gradient_colors,
    labels = map_chr(n_values, format_n_label),
    name = "Players"
  ) +
  coord_cartesian(xlim = c(0, 0.5), ylim = c(0, 1)) +
  labs(
    title = "Theoretical CDF of Min Win Prob. of Eventual Winner",
    x = "x",
    y = expression("P(M"[omega] <= " x)")
  ) +
  paper_theme(base_size = 11) +
  theme(
    legend.position = "right"
  )

# Output to shared manuscript figures.
out_dir <- file.path(repo_root, "results", "figures", "manuscript", "figures", "distributions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_dir, "nplayer_cdf.png"), p_cdf, width = 8, height = 5, dpi = 300)
message("Saved to ", out_dir, "/nplayer_cdf.png")
