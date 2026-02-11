library(tidyverse)

# Load the PDF function from plot_mixture_distribution.R
source("plot_mixture_distribution.R")

# Test PDF values at key points
cat("=== Verifying PDF from plot_mixture_distribution.R ===\n\n")

p0_values = c(0.5, 0.6, 0.7, 0.8, 0.9)

for (p0 in p0_values) {
  cat(sprintf("For p0 = %.1f:\n", p0))
  
  # Test points
  x_before = 1 - p0 - 0.01
  x_at_break1 = 1 - p0
  x_mid_region1 = (1 - p0 + p0) / 2
  x_at_break2 = p0
  x_mid_region2 = (p0 + 1) / 2
  x_near_1 = 0.99
  
  cat(sprintf("  PDF(%.3f) = %.6f (should be 0)\n", x_before, mixture_pdf(x_before, p0)))
  cat(sprintf("  PDF(%.3f) = %.6f\n", x_at_break1, mixture_pdf(x_at_break1, p0)))
  cat(sprintf("  PDF(%.3f) = %.6f (should be (1-p0)/x^2 = %.6f)\n", 
              x_mid_region1, mixture_pdf(x_mid_region1, p0),
              (1-p0)/(x_mid_region1^2)))
  cat(sprintf("  PDF(%.3f) = %.6f (left limit: %.6f, right limit: %.6f)\n", 
              x_at_break2, mixture_pdf(x_at_break2, p0),
              (1-p0)/(p0^2), 1/(p0^2)))
  cat(sprintf("  PDF(%.3f) = %.6f (should be 1/x^2 = %.6f)\n", 
              x_mid_region2, mixture_pdf(x_mid_region2, p0),
              1/(x_mid_region2^2)))
  cat(sprintf("  PDF(%.3f) = %.6f (should be 1/x^2 = %.6f)\n", 
              x_near_1, mixture_pdf(x_near_1, p0),
              1/(x_near_1^2)))
  
  # Check if PDF integrates to 1
  x_seq = seq(max(0.001, 1 - p0 + 0.001), 0.999, by = 0.001)
  pdf_vals = map_dbl(x_seq, ~ mixture_pdf(.x, p0))
  integral = sum(pdf_vals) * 0.001
  cat(sprintf("  Numerical integral ≈ %.6f (should be 1)\n", integral))
  
  cat("\n")
}

cat("=== PDF Verification Complete ===\n")
cat("\nNote: PDF has a jump discontinuity at x = p0:\n")
cat("  Left limit: (1-p0)/p0^2\n")
cat("  Right limit: 1/p0^2\n")
cat("This is correct because the CDF has a corner (non-smooth point) at x = p0.\n")

