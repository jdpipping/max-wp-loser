library(tidyverse)

# Define the piecewise upper tail distribution P(M_ℓ ≥ x) for given p_0
mixture_upper_tail = function(x, p0) {
  if (p0 < 0.5) {
    # For p0 < 0.5, swap roles
    return(mixture_upper_tail(x, 1 - p0))
  }
  
  # For p0 >= 0.5
  case_when(
    x < 1 - p0 ~ 1,
    x < p0 ~ (1 - p0) / x,  # CORRECTED: includes contribution from team A losing
    x >= p0 & x < 1 ~ (1 - x) / x,
    x >= 1 ~ 0,
    TRUE ~ NA_real_
  )
}

# Define the PDF (derivative of the CDF, which is 1 - upper tail)
mixture_pdf = function(x, p0) {
  if (p0 < 0.5) {
    return(mixture_pdf(x, 1 - p0))
  }
  
  # For p0 >= 0.5
  # PDF is negative derivative of upper tail
  # For 1-p0 ≤ x < p0: P(M_ℓ ≥ x) = (1-p0)/x, so PDF = (1-p0)/x^2
  # For x ≥ p0: P(M_ℓ ≥ x) = (1-x)/x, so PDF = 1/x^2
  case_when(
    x < 1 - p0 ~ 0,
    x < p0 ~ (1 - p0) / (x^2),
    x >= p0 & x < 1 ~ 1 / (x^2),
    TRUE ~ NA_real_
  )
}

# Create data for plotting
p0_values = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
x_values = seq(0, 0.999, by = 0.001)

# Function to format p0 labels with proper decimal places
format_p0_label = function(p0) {
  if (p0 == 0.99 || p0 == 0.95) {
    sprintf("p_0 = %.2f", p0)
  } else {
    sprintf("p_0 = %.1f", p0)
  }
}

# CDF plot data (CDF = 1 - upper tail)
plot_data_cdf = expand_grid(
  p0 = p0_values,
  x = x_values
) %>%
  mutate(
    cdf = 1 - map2_dbl(x, p0, mixture_upper_tail),
    p0_label = factor(map_chr(p0, format_p0_label), 
                      levels = map_chr(p0_values, format_p0_label))
  ) %>%
  filter(!is.na(cdf))

# PDF plot data
plot_data_pdf = expand_grid(
  p0 = p0_values,
  x = x_values
) %>%
  mutate(
    density = map2_dbl(x, p0, mixture_pdf),
    p0_label = factor(map_chr(p0, format_p0_label), 
                      levels = map_chr(p0_values, format_p0_label))
  ) %>%
  filter(!is.na(density))

# Create color palette using viridis magma (generate one extra and drop the brightest)
library(viridisLite)
gradient_colors = viridis(length(p0_values) + 1, option = "magma")[1:length(p0_values)]

# Create the CDF plot
p_cdf = ggplot(plot_data_cdf, aes(x = x, y = cdf, color = p0_label)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = gradient_colors,
    labels = map_chr(p0_values, format_p0_label),
    name = NULL
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Theoretical CDF of Max Win Prob. of Eventual Loser",
    x = "x",
    y = expression("P(M"[lambda]*" " <= " x)")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# Create the PDF plot
p_pdf = ggplot(plot_data_pdf, aes(x = x, y = density, color = p0_label)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = gradient_colors,
    labels = map_chr(p0_values, format_p0_label),
    name = NULL
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 4)) +
  labs(
    title = "Theoretical PDF of Max Win Prob. of Eventual Loser",
    x = "x",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# Save the plots (paths relative to repo root)
ggsave("paper/figures/distributions/sports_cdf.png", p_cdf, width = 8, height = 5, dpi = 300)
ggsave("paper/figures/distributions/sports_pdf.png", p_pdf, width = 8, height = 5, dpi = 300)

