# Tolerance experiment: How much do loose tolerances help?
#
# Compare ADMM convergence at different eps_abs/eps_rel values
# and check if the resulting weights are still "good enough" for practical use.

devtools::load_all("..")

set.seed(42)

cat("Tolerance Experiment: Strict vs Loose ADMM Convergence\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Same problem as iteration_cost.R
n <- 1000
data <- data.frame(
  sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)),
  age = sample(c("18-29", "30-44", "45-59", "60+"), n, replace = TRUE,
               prob = c(0.25, 0.28, 0.27, 0.20)),
  region = sample(c("NE", "MW", "S", "W"), n, replace = TRUE,
                  prob = c(0.18, 0.22, 0.38, 0.22)),
  education = sample(c("HS", "Some college", "BA", "Grad"), n, replace = TRUE,
                     prob = c(0.30, 0.28, 0.25, 0.17))
)

pop_targets <- data.frame(
  variable = c(rep("sex", 2), rep("age", 4), rep("region", 4), rep("education", 4)),
  level = c("M", "F",
            "18-29", "30-44", "45-59", "60+",
            "NE", "MW", "S", "W",
            "HS", "Some college", "BA", "Grad"),
  target = c(0.49, 0.51,
             0.20, 0.25, 0.28, 0.27,
             0.17, 0.21, 0.38, 0.24,
             0.27, 0.29, 0.26, 0.18)
)

f <- ~ rr_exact(sex) + rr_exact(age) + rr_exact(region) + rr_exact(education)

# Test different tolerance levels
tolerances <- c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2)

cat(sprintf("%-12s %8s %10s %12s %10s\n",
            "Tolerance", "Iters", "Time (s)", "Max Margin", "Kish deff"))
cat(paste(rep("-", 60), collapse = ""), "\n")

for (tol in tolerances) {
  time <- system.time({
    result <- regrake(
      data = data,
      formula = f,
      population_data = pop_targets,
      pop_type = "proportions",
      control = list(eps_abs = tol, eps_rel = tol)
    )
  })[["elapsed"]]

  # Check max margin error
  max_margin_error <- max(abs(result$balance$residual))

  cat(sprintf("%-12s %8d %10.4f %12.2e %10.3f\n",
              format(tol, scientific = TRUE),
              result$diagnostics$iterations,
              time,
              max_margin_error,
              result$diagnostics$kish_deff))
}

cat("\n")
cat("Key insight: For survey raking, 1e-3 or 1e-4 tolerance is usually sufficient.\n")
cat("Margins within 0.1% (1e-3) are effectively 'perfect' for most applications.\n")
