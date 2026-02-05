# Investigate tolerance scaling with problem size
#
# The ADMM convergence check uses:
#   eps_pri = sqrt(p) * eps_abs + eps_rel * max(||Ax||, ||Bz||)
# where p = m + 2n
#
# This means the effective tolerance scales with sqrt(n), which
# explains why 1e-4 works at n=1000 but fails at n=5000.

devtools::load_all("..")

set.seed(42)

cat("Tolerance Scaling Analysis\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Test: same problem structure at different n
make_problem <- function(n) {
  data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)),
    age = sample(c("18-29", "30-44", "45-59", "60+"), n, replace = TRUE,
                 prob = c(0.25, 0.28, 0.27, 0.20))
  )

  targets <- data.frame(
    variable = c(rep("sex", 2), rep("age", 4)),
    level = c("M", "F", "18-29", "30-44", "45-59", "60+"),
    target = c(0.49, 0.51, 0.20, 0.25, 0.28, 0.27)
  )

  list(data = data, targets = targets)
}

sizes <- c(500, 1000, 2000, 5000, 10000)
m <- 6  # constraints

cat("At eps_abs = eps_rel = 1e-4:\n")
cat(sprintf("%-8s %8s %12s %10s %10s\n", "n", "p", "sqrt(p)", "Iters", "Max Error"))
cat(paste(rep("-", 55), collapse = ""), "\n")

for (n in sizes) {
  p <- m + 2 * n
  sqrt_p <- sqrt(p)

  prob <- make_problem(n)
  f <- ~ rr_exact(sex) + rr_exact(age)

  result <- tryCatch({
    res <- regrake(
      data = prob$data,
      formula = f,
      population_data = prob$targets,
      pop_type = "proportions",
      control = list(eps_abs = 1e-4, eps_rel = 1e-4)
    )
    list(ok = TRUE, iters = res$diagnostics$iterations,
         error = max(abs(res$balance$residual)))
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e))
  })

  if (result$ok) {
    cat(sprintf("%-8d %8d %12.1f %10d %10.2e\n",
                n, p, sqrt_p, result$iters, result$error))
  } else {
    cat(sprintf("%-8d %8d %12.1f %10s\n", n, p, sqrt_p, "ERROR"))
  }
}

cat("\n")
cat("Theory: effective tolerance ~ sqrt(p) * eps_abs ≈ sqrt(2n) * 1e-4\n")
cat("At n=500:   sqrt(1000)  * 1e-4 ≈ 0.003 (0.3%)\n")
cat("At n=5000:  sqrt(10000) * 1e-4 ≈ 0.01  (1%)\n")
cat("At n=10000: sqrt(20000) * 1e-4 ≈ 0.014 (1.4%)\n")
cat("\n")
cat("This explains why 1e-4 tolerance 'works' at small n but fails at large n.\n")
cat("The ADMM residual norms don't directly correspond to margin accuracy.\n")
