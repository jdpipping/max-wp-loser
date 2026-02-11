# Analytical verification that PDF integrates to 1

cat("=== Analytical Integration Verification ===\n\n")

# For p0 >= 1/2, the PDF is:
# f(x) = (1-p0)/x^2 for x in [1-p0, p0)
# f(x) = 1/x^2 for x in [p0, 1)

# Integral from 1-p0 to p0:
# ∫[1-p0 to p0] (1-p0)/x^2 dx = (1-p0) * [-1/x]|[1-p0 to p0]
#                            = (1-p0) * (1/(1-p0) - 1/p0)
#                            = 1 - (1-p0)/p0

# Integral from p0 to 1:
# ∫[p0 to 1] 1/x^2 dx = [-1/x]|[p0 to 1]
#                      = 1/p0 - 1
#                      = (1-p0)/p0

# Total = 1 - (1-p0)/p0 + (1-p0)/p0 = 1 ✓

verify_integration = function(p0) {
  cat(sprintf("For p0 = %.2f:\n", p0))
  
  # First region: [1-p0, p0)
  integral1 = (1 - p0) * (1/(1 - p0) - 1/p0)
  cat(sprintf("  ∫[%.2f to %.2f] (1-p0)/x^2 dx = %.6f\n", 1-p0, p0, integral1))
  
  # Second region: [p0, 1)
  integral2 = 1/p0 - 1
  cat(sprintf("  ∫[%.2f to 1] 1/x^2 dx = %.6f\n", p0, integral2))
  
  total = integral1 + integral2
  cat(sprintf("  Total = %.6f + %.6f = %.6f\n", integral1, integral2, total))
  
  if (abs(total - 1) < 1e-10) {
    cat("  ✓ Integrates to exactly 1\n\n")
  } else {
    cat("  ✗ Does NOT integrate to 1\n\n")
  }
}

p0_values = c(0.5, 0.6, 0.7, 0.8, 0.9)
for (p0 in p0_values) {
  verify_integration(p0)
}

cat("=== Verification complete ===\n")

