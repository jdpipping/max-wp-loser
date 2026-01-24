library(tidyverse)

# Define the CDF from the paper (equation around line 434)
cdf_M_ell = function(x, p0) {
  if (p0 < 0.5) {
    # For p0 < 0.5, swap roles
    return(cdf_M_ell(x, 1 - p0))
  }
  
  # For p0 >= 0.5
  case_when(
    x < 1 - p0 ~ 0,
    x < p0 ~ 1 - (1 - p0) / x,
    x >= p0 & x < 1 ~ 2 - 1 / x,
    x >= 1 ~ 1,
    TRUE ~ NA_real_
  )
}

# Define the PDF as the derivative of the CDF
pdf_M_ell = function(x, p0) {
  if (p0 < 0.5) {
    return(pdf_M_ell(x, 1 - p0))
  }
  
  # For p0 >= 0.5
  # PDF is derivative of CDF:
  # For 1-p0 ≤ x < p0: F(x) = 1 - (1-p0)/x, so f(x) = (1-p0)/x^2
  # For x ≥ p0: F(x) = 2 - 1/x, so f(x) = 1/x^2
  case_when(
    x < 1 - p0 ~ 0,
    x < p0 ~ (1 - p0) / (x^2),
    x >= p0 & x < 1 ~ 1 / (x^2),
    TRUE ~ NA_real_
  )
}

# Define upper tail (complement of CDF)
upper_tail_M_ell = function(x, p0) {
  1 - cdf_M_ell(x, p0)
}

# Validation checks
validate_distribution = function(p0) {
  cat("\n=== Validating distribution for p0 =", p0, "===\n\n")
  
  # Test points for continuity
  test_points = c(
    1 - p0 - 0.001,  # Just before first breakpoint
    1 - p0,          # First breakpoint
    1 - p0 + 0.001,  # Just after first breakpoint
    (1 - p0 + p0) / 2,  # Midpoint of first region
    p0 - 0.001,      # Just before second breakpoint
    p0,              # Second breakpoint
    p0 + 0.001,      # Just after second breakpoint
    (p0 + 1) / 2,    # Midpoint of second region
    1 - 0.001,       # Just before 1
    1,               # At 1
    1 + 0.001        # Just after 1
  )
  
  # 1. Check continuity at breakpoints
  cat("1. Continuity checks:\n")
  
  # At x = 1-p0
  left_1p0 = cdf_M_ell(1 - p0 - 1e-10, p0)
  right_1p0 = cdf_M_ell(1 - p0 + 1e-10, p0)
  cat(sprintf("   At x = 1-p0 = %.3f: F(%.6f) = %.6f, F(%.6f) = %.6f\n", 
              1 - p0, 1 - p0 - 1e-10, left_1p0, 1 - p0 + 1e-10, right_1p0))
  if (abs(left_1p0 - right_1p0) < 1e-6) {
    cat("   ✓ Continuous at x = 1-p0\n")
  } else {
    cat("   ✗ NOT continuous at x = 1-p0\n")
  }
  
  # At x = p0
  left_p0 = cdf_M_ell(p0 - 1e-10, p0)
  right_p0 = cdf_M_ell(p0 + 1e-10, p0)
  cat(sprintf("   At x = p0 = %.3f: F(%.6f) = %.6f, F(%.6f) = %.6f\n", 
              p0, p0 - 1e-10, left_p0, p0 + 1e-10, right_p0))
  if (abs(left_p0 - right_p0) < 1e-6) {
    cat("   ✓ Continuous at x = p0\n")
  } else {
    cat("   ✗ NOT continuous at x = p0\n")
  }
  
  # At x = 1
  left_1 = cdf_M_ell(1 - 1e-10, p0)
  right_1 = cdf_M_ell(1, p0)
  cat(sprintf("   At x = 1: F(%.6f) = %.6f, F(1) = %.6f\n", 
              1 - 1e-10, left_1, right_1))
  if (abs(left_1 - right_1) < 1e-6) {
    cat("   ✓ Continuous at x = 1\n")
  } else {
    cat("   ✗ NOT continuous at x = 1\n")
  }
  
  # 2. Check boundary conditions
  cat("\n2. Boundary conditions:\n")
  f_at_1p0 = cdf_M_ell(1 - p0, p0)
  f_at_1 = cdf_M_ell(1, p0)
  cat(sprintf("   F(1-p0) = %.6f (should be 0)\n", f_at_1p0))
  cat(sprintf("   F(1) = %.6f (should be 1)\n", f_at_1))
  if (abs(f_at_1p0) < 1e-6 && abs(f_at_1 - 1) < 1e-6) {
    cat("   ✓ Boundary conditions satisfied\n")
  } else {
    cat("   ✗ Boundary conditions NOT satisfied\n")
  }
  
  # 3. Check monotonicity
  cat("\n3. Monotonicity check:\n")
  x_seq = seq(max(0.01, 1 - p0 - 0.1), min(0.99, 1), length.out = 1000)
  cdf_values = map_dbl(x_seq, ~ cdf_M_ell(.x, p0))
  is_monotonic = all(diff(cdf_values) >= -1e-10)
  cat(sprintf("   CDF is monotonic: %s\n", ifelse(is_monotonic, "✓", "✗")))
  
  # 4. Check PDF is derivative of CDF (numerical differentiation)
  cat("\n4. PDF vs derivative of CDF:\n")
  x_test = seq(max(0.01, 1 - p0 + 0.01), min(0.99, p0 - 0.01), length.out = 20)
  max_diff_region1 = 0
  for (x in x_test) {
    h = 1e-6
    cdf_deriv = (cdf_M_ell(x + h, p0) - cdf_M_ell(x - h, p0)) / (2 * h)
    pdf_val = pdf_M_ell(x, p0)
    max_diff_region1 = max(max_diff_region1, abs(cdf_deriv - pdf_val))
  }
  
  x_test2 = seq(max(0.01, p0 + 0.01), 0.99, length.out = 20)
  max_diff_region2 = 0
  for (x in x_test2) {
    h = 1e-6
    cdf_deriv = (cdf_M_ell(x + h, p0) - cdf_M_ell(x - h, p0)) / (2 * h)
    pdf_val = pdf_M_ell(x, p0)
    max_diff_region2 = max(max_diff_region2, abs(cdf_deriv - pdf_val))
  }
  
  cat(sprintf("   Max difference in region [1-p0, p0): %.2e\n", max_diff_region1))
  cat(sprintf("   Max difference in region [p0, 1): %.2e\n", max_diff_region2))
  if (max_diff_region1 < 1e-5 && max_diff_region2 < 1e-5) {
    cat("   ✓ PDF matches derivative of CDF\n")
  } else {
    cat("   ✗ PDF does NOT match derivative of CDF\n")
  }
  
  # 5. Check integration (PDF should integrate to 1)
  cat("\n5. Integration check:\n")
  # Integrate PDF numerically
  x_integrate = seq(max(0.001, 1 - p0), 0.999, by = 0.001)
  pdf_vals = map_dbl(x_integrate, ~ pdf_M_ell(.x, p0))
  integral = sum(pdf_vals) * 0.001  # Simple Riemann sum
  cat(sprintf("   ∫ PDF dx ≈ %.6f (should be 1)\n", integral))
  if (abs(integral - 1) < 0.01) {
    cat("   ✓ PDF integrates to approximately 1\n")
  } else {
    cat("   ✗ PDF does NOT integrate to 1\n")
  }
  
  # 6. Check that CDF matches integration of PDF
  cat("\n6. CDF vs integrated PDF:\n")
  x_check = c((1 - p0 + p0) / 2, (p0 + 1) / 2, 0.99)
  max_cdf_diff = 0
  for (x in x_check) {
    # Integrate PDF from 1-p0 to x
    x_int = seq(max(0.001, 1 - p0), min(0.999, x), by = 0.001)
    pdf_int_vals = map_dbl(x_int, ~ pdf_M_ell(.x, p0))
    cdf_from_pdf = sum(pdf_int_vals) * 0.001
    cdf_direct = cdf_M_ell(x, p0)
    max_cdf_diff = max(max_cdf_diff, abs(cdf_from_pdf - cdf_direct))
  }
  cat(sprintf("   Max difference: %.2e\n", max_cdf_diff))
  if (max_cdf_diff < 0.01) {
    cat("   ✓ CDF matches integrated PDF\n")
  } else {
    cat("   ✗ CDF does NOT match integrated PDF\n")
  }
  
  # 7. Check specific values from paper
  cat("\n7. Specific value checks:\n")
  if (abs(p0 - 0.5) < 1e-6) {
    # Symmetric case
    x_test = 0.75
    expected_cdf = 2 - 1/0.75  # Should be 2 - 4/3 = 2/3
    actual_cdf = cdf_M_ell(x_test, p0)
    cat(sprintf("   For p0=0.5, x=0.75: F(0.75) = %.6f (expected %.6f)\n", 
                actual_cdf, expected_cdf))
    if (abs(actual_cdf - expected_cdf) < 1e-6) {
      cat("   ✓ Matches expected value\n")
    } else {
      cat("   ✗ Does NOT match expected value\n")
    }
  }
}

# Run validation for several p0 values
p0_values = c(0.5, 0.6, 0.7, 0.8, 0.9)

for (p0 in p0_values) {
  validate_distribution(p0)
}

cat("\n=== Validation complete ===\n")

