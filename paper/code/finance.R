library(tidyverse)

# Define the conditional CDF F(M|Y=0) for losing trades
# From Theorem 3 (continuous-conditional): F_{M|Y=0}(x) = 1 - (p_0/(1-p_0)) * ((1-x)/x) for x ∈ [p_0, 1)
conditional_cdf_losing = function(x, p0) {
  case_when(
    x < p0 ~ 0,
    x >= p0 & x < 1 ~ 1 - (p0 / (1 - p0)) * ((1 - x) / x),
    x >= 1 ~ 1,
    TRUE ~ NA_real_
  )
}

# Define the conditional PDF (derivative of CDF)
conditional_pdf_losing = function(x, p0) {
  case_when(
    x < p0 ~ 0,
    x >= p0 & x < 1 ~ (p0 / (1 - p0)) / (x^2),
    TRUE ~ NA_real_
  )
}

# Create data for plotting
p0_values = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
x_values = seq(0, 0.999, by = 0.001)

# Function to format p0 labels with proper decimal places
format_p0_label = function(p0) {
  sprintf("p_0 = %.1f", p0)
}

# CDF plot data
plot_data_cdf = expand_grid(
  p0 = p0_values,
  x = x_values
) %>%
  mutate(
    cdf = map2_dbl(x, p0, conditional_cdf_losing),
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
    density = map2_dbl(x, p0, conditional_pdf_losing),
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
    title = "Theoretical CDF of Max Win Prob. of Losing Trades",
    x = "x",
    y = expression("P(M " <= " x | Y=0)")
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
    title = "Theoretical PDF of Max Win Prob. of Losing Trades",
    x = "x",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# Save the plots (paths relative to code/ directory)
ggsave("../figures/distributions/finance_cdf.png", p_cdf, width = 8, height = 5, dpi = 300)
ggsave("../figures/distributions/finance_pdf.png", p_pdf, width = 8, height = 5, dpi = 300)

