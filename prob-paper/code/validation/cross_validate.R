library(tidyverse)

# Functions from plot_mixture_distribution.R
mixture_upper_tail_code = function(x, p0) {
  if (p0 < 0.5) {
    return(mixture_upper_tail_code(x, 1 - p0))
  }
  case_when(
    x < 1 - p0 ~ 1,
    x < p0 ~ (1 - p0) / x,
    x >= p0 & x < 1 ~ (1 - x) / x,
    x >= 1 ~ 0,
    TRUE ~ NA_real_
  )
}

mixture_pdf_code = function(x, p0) {
  if (p0 < 0.5) {
    return(mixture_pdf_code(x, 1 - p0))
  }
  case_when(
    x < 1 - p0 ~ 0,
    x < p0 ~ (1 - p0) / (x^2),
    x >= p0 & x < 1 ~ 1 / (x^2),
    TRUE ~ NA_real_
  )
}

# Functions from validation (derived from paper CDF)
cdf_M_ell = function(x, p0) {
  if (p0 < 0.5) {
    return(cdf_M_ell(x, 1 - p0))
  }
  case_when(
    x < 1 - p0 ~ 0,
    x < p0 ~ 1 - (1 - p0) / x,
    x >= p0 & x < 1 ~ 2 - 1 / x,
    x >= 1 ~ 1,
    TRUE ~ NA_real_
  )
}

upper_tail_from_cdf = function(x, p0) {
  1 - cdf_M_ell(x, p0)
}

pdf_from_cdf = function(x, p0) {
  if (p0 < 0.5) {
    return(pdf_from_cdf(x, 1 - p0))
  }
  case_when(
    x < 1 - p0 ~ 0,
    x < p0 ~ (1 - p0) / (x^2),
    x >= p0 & x < 1 ~ 1 / (x^2),
    TRUE ~ NA_real_
  )
}

# Cross-validation
cat("=== Cross-validating plot_mixture_distribution.R functions with paper formulas ===\n\n")

p0_values = c(0.5, 0.6, 0.7, 0.8, 0.9)
x_test = seq(0.4, 0.99, by = 0.01)

for (p0 in p0_values) {
  cat(sprintf("Testing p0 = %.1f:\n", p0))
  
  # Test upper tail
  upper_tail_diff = map_dbl(x_test, function(x) {
    val1 = mixture_upper_tail_code(x, p0)
    val2 = upper_tail_from_cdf(x, p0)
    if (is.na(val1) || is.na(val2)) return(0)
    abs(val1 - val2)
  })
  max_upper_diff = max(upper_tail_diff, na.rm = TRUE)
  cat(sprintf("  Upper tail max difference: %.2e\n", max_upper_diff))
  
  # Test PDF
  pdf_diff = map_dbl(x_test, function(x) {
    val1 = mixture_pdf_code(x, p0)
    val2 = pdf_from_cdf(x, p0)
    if (is.na(val1) || is.na(val2)) return(0)
    abs(val1 - val2)
  })
  max_pdf_diff = max(pdf_diff, na.rm = TRUE)
  cat(sprintf("  PDF max difference: %.2e\n", max_pdf_diff))
  
  if (max_upper_diff < 1e-10 && max_pdf_diff < 1e-10) {
    cat("  ✓ Functions match perfectly\n\n")
  } else {
    cat("  ✗ Functions do NOT match\n\n")
  }
}

# Verify upper tail formulas match paper
cat("=== Verifying upper tail formulas from paper ===\n\n")
cat("From paper (line 473):\n")
cat("  P(M_ℓ ≥ x) = (1-p0)/x for x ∈ [1-p0, p0)\n")
cat("  P(M_ℓ ≥ x) = (1-x)/x for x ≥ p0\n\n")

p0 = 0.7
x1 = 0.5  # In [1-p0, p0) = [0.3, 0.7)
x2 = 0.8  # In [p0, 1) = [0.7, 1)

cat(sprintf("For p0 = %.1f:\n", p0))
cat(sprintf("  At x = %.1f (in [1-p0, p0)): P(M_ℓ ≥ x) = (1-%.1f)/%.1f = %.6f\n", 
            x1, p0, x1, (1-p0)/x1))
cat(sprintf("    Code gives: %.6f\n", mixture_upper_tail_code(x1, p0)))
cat(sprintf("  At x = %.1f (in [p0, 1)): P(M_ℓ ≥ x) = (1-%.1f)/%.1f = %.6f\n", 
            x2, x2, x2, (1-x2)/x2))
cat(sprintf("    Code gives: %.6f\n", mixture_upper_tail_code(x2, p0)))

cat("\n=== All validations complete ===\n")

