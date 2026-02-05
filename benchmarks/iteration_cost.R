# Per-iteration cost comparison using real implementations
#
# Compare autumn (IPF) vs regrake (ADMM) on a realistic problem
# n=1000 samples, m~20 constraints (4 categorical variables)

library(autumn)
devtools::load_all("..")

set.seed(42)

cat("Per-iteration cost: IPF (autumn) vs ADMM (regrake)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Create realistic survey data: 4 vars with different # of levels
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

# Population targets (14 constraints total: 2 + 4 + 4 + 4)
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

m <- nrow(pop_targets)
cat(sprintf("Problem size: n=%d samples, m=%d constraints\n\n", n, m))

# ── Run autumn (IPF) - multiple times to get measurable time ──
cat("Running autumn (IPF) x100...\n")
autumn_time <- system.time({
  for (i in 1:100) {
    autumn_result <- autumn::harvest(
      data,
      pop_targets,
      verbose = FALSE
    )
  }
})[["elapsed"]] / 100

cat(sprintf("  Single run: %.4fs\n", autumn_time))

# ── Run regrake (ADMM) ──
cat("\nRunning regrake (ADMM)...\n")
f <- ~ rr_exact(sex) + rr_exact(age) + rr_exact(region) + rr_exact(education)

regrake_time <- system.time({
  regrake_result <- regrake(
    data = data,
    formula = f,
    population_data = pop_targets,
    pop_type = "proportions"
  )
})[["elapsed"]]

regrake_iters <- regrake_result$diagnostics$iterations

cat(sprintf("\n%s\n", paste(rep("=", 60), collapse = "")))
cat("Results:\n")
cat(sprintf("  autumn:  %.3fs\n", autumn_time))
cat(sprintf("  regrake: %.3fs (%d ADMM iterations)\n", regrake_time, regrake_iters))
cat(sprintf("  Ratio:   %.1fx slower\n", regrake_time / autumn_time))

# Per-iteration cost (napkin math)
# autumn doesn't expose iteration count easily, but IPF typically converges in ~10-30 iters
# Let's estimate based on typical behavior
cat(sprintf("\n%s\n", paste(rep("-", 60), collapse = "")))
cat("Per-iteration cost estimate:\n")

# Assume autumn takes ~15 iterations (typical for 4 vars)
autumn_iters_est <- 15
autumn_per_iter <- autumn_time / autumn_iters_est
regrake_per_iter <- regrake_time / regrake_iters

cat(sprintf("  autumn:  ~%.4fs/iter (assuming ~%d iterations)\n",
            autumn_per_iter, autumn_iters_est))
cat(sprintf("  regrake: %.4fs/iter (%d iterations)\n",
            regrake_per_iter, regrake_iters))
cat(sprintf("  Per-iter ratio: ~%.1fx\n", regrake_per_iter / autumn_per_iter))

# Verify weights are similar
cat(sprintf("\n%s\n", paste(rep("-", 60), collapse = "")))
cat("Weight comparison:\n")
w_autumn <- autumn_result$weights
w_regrake <- regrake_result$weights

cat(sprintf("  Correlation: %.6f\n", cor(w_autumn, w_regrake)))
cat(sprintf("  Max diff:    %.6f\n", max(abs(w_autumn - w_regrake))))
cat(sprintf("  Kish deff:   autumn=%.3f, regrake=%.3f\n",
            1 + var(w_autumn) / mean(w_autumn)^2,
            regrake_result$diagnostics$kish_deff))
