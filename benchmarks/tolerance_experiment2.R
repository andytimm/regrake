# Tolerance experiment 2: Fine-grained search for optimal default
#
# The first experiment showed 1e-4 is great but 1e-3 is too loose.
# Let's find the sweet spot.

devtools::load_all("..")

set.seed(42)

cat("Fine-grained Tolerance Experiment\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

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

# Fine-grained tolerances between 1e-5 and 1e-3
tolerances <- c(1e-5, 2e-5, 5e-5, 1e-4, 2e-4, 5e-4, 1e-3)

cat(sprintf("%-12s %8s %10s %12s %12s\n",
            "Tolerance", "Iters", "Time (s)", "Max Margin", "Max %err"))
cat(paste(rep("-", 70), collapse = ""), "\n")

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

  max_margin_error <- max(abs(result$balance$residual))

  cat(sprintf("%-12s %8d %10.4f %12.2e %12.4f%%\n",
              format(tol, scientific = TRUE),
              result$diagnostics$iterations,
              time,
              max_margin_error,
              max_margin_error * 100))
}

cat("\n")
cat("For context: IPF packages (autumn/anesrake) use 1% (0.01) tolerance.\n")
cat("A 'pollster acceptable' margin error is probably < 0.1% (1e-3).\n")
