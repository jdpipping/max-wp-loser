# Shared plotting style for manuscript figures.

suppressPackageStartupMessages({
  library(ggplot2)
  library(viridisLite)
})

paper_palette_seq <- function(n,
                              option = "magma",
                              begin = 0.15,
                              end = 0.90,
                              direction = 1) {
  viridisLite::viridis(
    n = n,
    option = option,
    begin = begin,
    end = end,
    direction = direction
  )
}

paper_palette_discrete <- function(labels) {
  setNames(paper_palette_seq(length(labels)), labels)
}

paper_style <- list(
  ink = "#2E3440",
  ref = "#6B7280",
  grid = "#E5E7EB",
  shade = paper_palette_seq(6, begin = 0.35, end = 0.80)[3]
)

paper_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = paper_style$grid, linewidth = 0.35),
      plot.title = element_text(face = "bold"),
      legend.title = element_blank()
    )
}
