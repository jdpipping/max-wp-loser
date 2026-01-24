library(tidyverse)

# Define the CDF F(M_omega) for symmetric n-player games
# From section 4.2.1: F_{M_omega}(x) = (n-1)x/(1-x) for x in [0, 1/n)
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

# Function to format n labels
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

# Create color palette using viridis magma (generate one extra and drop the brightest)
library(viridisLite)
gradient_colors = viridis(length(n_values) + 1, option = "magma")[1:length(n_values)]

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
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# Save the plot
ggsave("paper/figures/distributions/nplayer_cdf.png", p_cdf, width = 8, height = 5, dpi = 300)

print("Plot saved to paper/figures/distributions/nplayer_cdf.png")
