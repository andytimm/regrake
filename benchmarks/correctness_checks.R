# R-only correctness checks for solver features not covered by Python comparison
#
# Tests invariants and expected behavior for:
#   1. Inequality/range constraints (Python has this, but not in benchmark suite)
#   2. KL loss
#   3. KL regularizer (with prior)
#   4. Boolean regularizer
#   5. Weight bounds (soft and hard) - R-only feature
#   6. Sum squares regularizer
#
# Usage:
#   cd benchmarks
#   Rscript -e "setwd('c:/Users/timma/Documents/R packages/regrake/benchmarks'); devtools::load_all('..', quiet=TRUE); source('correctness_checks.R'); main()"

library(Matrix)

# Track pass/fail counts
n_pass <- 0
n_fail <- 0

check <- function(condition, name) {
  if (condition) {
    n_pass <<- n_pass + 1
  } else {
    n_fail <<- n_fail + 1
    cat("  ** FAIL:", name, "\n")
  }
}

check_close <- function(actual, expected, name, tol = 1e-4) {
  diff <- max(abs(actual - expected))
  if (diff < tol) {
    n_pass <<- n_pass + 1
  } else {
    n_fail <<- n_fail + 1
    cat(sprintf("  ** FAIL: %s (max_diff=%.2e, tol=%.2e)\n", name, diff, tol))
  }
}

# ── 1. Inequality / range constraints ──────────────────────────────────────

test_inequality <- function() {
  cat("\n1. Inequality / range constraints\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(42)
  n <- 1000

  # Two-category variable: sample 60/40, target 50/50 but with ±0.03 margin
  categories <- sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4))
  F <- Matrix(0, nrow = 2, ncol = n, sparse = TRUE)
  F[1, categories == 0] <- 1
  F[2, categories == 1] <- 1

  target <- c(0.5, 0.5)
  lower <- c(-0.03, -0.03)
  upper <- c(0.03, 0.03)

  losses <- list(
    list(
      fn = inequality_loss,
      target = target,
      prox = prox_inequality,
      lower = lower,
      upper = upper
    )
  )

  reg <- list(
    fn = function(w, lambda) entropy_regularizer(w, lambda, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = NULL)
  )

  sol <- admm(F, losses, reg, lam = 1, control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))

  achieved <- as.numeric(F %*% sol$w_best)

  cat(sprintf("  Achieved: [%.4f, %.4f]  Target: [%.4f, %.4f]  Bounds: +/-0.03\n",
              achieved[1], achieved[2], target[1], target[2]))

  # Achieved values should be within [target+lower, target+upper]
  check(all(achieved >= target + lower - 1e-4), "achieved >= target + lower")
  check(all(achieved <= target + upper + 1e-4), "achieved <= target + upper")
  check(sol$converged, "converged")
  check(abs(sum(sol$w_best) - 1) < 1e-6, "weights sum to 1")
  check(all(sol$w_best >= -1e-8), "weights non-negative")

  cat(sprintf("  Converged in %d iterations\n", sol$iterations))
}

# ── 2. KL loss ─────────────────────────────────────────────────────────────

test_kl_loss <- function() {
  cat("\n2. KL loss\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(43)
  n <- 1000

  # Three categories: sample 50/30/20, target 33/33/34
  categories <- sample(0:2, n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  F <- Matrix(0, nrow = 3, ncol = n, sparse = TRUE)
  for (i in 0:2) F[i + 1, categories == i] <- 1

  target <- c(0.33, 0.33, 0.34)

  losses <- list(
    list(
      fn = function(x, t) kl_loss(x, t),
      target = target,
      prox = function(x, t, rho) prox_kl(x, t, rho, scale = 1),
      evaluate = function(x) sum(kl_loss(x, target))
    )
  )

  reg <- list(
    fn = function(w, lambda) entropy_regularizer(w, lambda, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = NULL)
  )

  sol <- admm(F, losses, reg, lam = 1, control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))

  achieved <- as.numeric(F %*% sol$w_best)

  cat(sprintf("  Achieved: [%.4f, %.4f, %.4f]  Target: [%.4f, %.4f, %.4f]\n",
              achieved[1], achieved[2], achieved[3], target[1], target[2], target[3]))

  # KL loss is soft — achieved should be near targets but not necessarily exact
  # Should still be much closer than sample proportions
  sample_props <- c(sum(categories == 0), sum(categories == 1), sum(categories == 2)) / n
  achieved_diff <- max(abs(achieved - target))
  sample_diff <- max(abs(sample_props - target))

  cat(sprintf("  Max diff from target: achieved=%.4f, sample=%.4f\n", achieved_diff, sample_diff))

  check(achieved_diff < sample_diff, "achieved closer to target than sample")
  check(sol$converged, "converged")
  check(abs(sum(sol$w_best) - 1) < 1e-6, "weights sum to 1")
  check(all(sol$w_best >= -1e-8), "weights non-negative")
  # KL loss should still get reasonably close (but softer than equality)
  check(achieved_diff < 0.10, "achieved within 10pp of targets")

  cat(sprintf("  Converged in %d iterations\n", sol$iterations))
}

# ── 3. KL regularizer (with prior) ────────────────────────────────────────

test_kl_regularizer <- function() {
  cat("\n3. KL regularizer (with prior)\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(44)
  n <- 1000

  categories <- sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4))
  F <- Matrix(0, nrow = 2, ncol = n, sparse = TRUE)
  F[1, categories == 0] <- 1
  F[2, categories == 1] <- 1

  target <- c(0.5, 0.5)

  losses <- list(
    list(
      fn = equality_loss,
      target = target,
      prox = prox_equality
    )
  )

  # Non-uniform prior: favor first 200 samples
  prior <- rep(1, n)
  prior[1:200] <- 3
  prior <- prior / sum(prior)

  reg <- list(
    fn = function(w, lambda) kl_regularizer(w, lambda, prior, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = prior, limit = NULL)
  )

  sol <- admm(F, losses, reg, lam = 1, control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))

  achieved <- as.numeric(F %*% sol$w_best)

  cat(sprintf("  Achieved: [%.6f, %.6f]  Target: [%.6f, %.6f]\n",
              achieved[1], achieved[2], target[1], target[2]))

  check_close(achieved, target, "achieved matches targets")
  check(sol$converged, "converged")
  check(abs(sum(sol$w_best) - 1) < 1e-6, "weights sum to 1")

  # With KL regularizer and non-uniform prior, weights should be closer to prior
  # than with entropy regularizer (uniform prior)
  cor_with_prior <- cor(sol$w_best, prior)
  cat(sprintf("  Correlation of weights with prior: %.4f\n", cor_with_prior))
  check(cor_with_prior > 0.3, "weights correlate with prior")

  # Compare against entropy regularizer (uniform prior) solution
  reg_uniform <- list(
    fn = function(w, lambda) entropy_regularizer(w, lambda, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = NULL)
  )
  sol_uniform <- admm(F, losses, reg_uniform, lam = 1,
                      control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))

  # KL reg weights should be more correlated with prior than uniform solution
  cor_uniform <- cor(sol_uniform$w_best, prior)
  cat(sprintf("  Uniform reg correlation with prior: %.4f\n", cor_uniform))
  check(cor_with_prior > cor_uniform, "KL reg weights closer to prior than uniform reg")

  cat(sprintf("  Converged in %d iterations\n", sol$iterations))
}

# ── 4. Boolean regularizer ────────────────────────────────────────────────

test_boolean_regularizer <- function() {
  cat("\n4. Boolean regularizer\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(45)
  n <- 500
  k <- 50

  categories <- sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4))
  F <- Matrix(0, nrow = 2, ncol = n, sparse = TRUE)
  F[1, categories == 0] <- 1
  F[2, categories == 1] <- 1

  target <- c(0.5, 0.5)

  losses <- list(
    list(
      fn = equality_loss,
      target = target,
      prox = prox_equality,
      evaluate = function(x) sum(equality_loss(x, target))
    )
  )

  reg <- structure(
    list(
      fn = function(w, lambda) w,
      prox = function(w, lambda) prox_boolean_reg(w, lambda, k)
    ),
    class = "BooleanRegularizer"
  )

  sol <- admm(F, losses, reg, lam = 1,
              control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))

  # Boolean regularizer: exactly k non-zero weights, each = 1/k
  n_nonzero <- sum(sol$w_best > 1e-10)
  cat(sprintf("  k=%d, non-zero weights: %d\n", k, n_nonzero))
  cat(sprintf("  Non-zero weight value: %.6f (expected: %.6f)\n",
              max(sol$w_best), 1 / k))

  check(n_nonzero == k, "exactly k non-zero weights")
  check_close(max(sol$w_best), 1 / k, "non-zero weights = 1/k", tol = 1e-6)
  check(abs(sum(sol$w_best) - 1) < 1e-6, "weights sum to 1")

  achieved <- as.numeric(F %*% sol$w_best)
  cat(sprintf("  Achieved: [%.4f, %.4f]  Target: [%.4f, %.4f]\n",
              achieved[1], achieved[2], target[1], target[2]))
}

# ── 5. Weight bounds (soft and hard) ──────────────────────────────────────

test_weight_bounds <- function() {
  cat("\n5. Weight bounds (soft and hard)\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(46)
  n <- 1000

  # Create a problem with strongly skewed data to force large weights
  categories <- sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2))
  F <- Matrix(0, nrow = 2, ncol = n, sparse = TRUE)
  F[1, categories == 0] <- 1
  F[2, categories == 1] <- 1

  target <- c(0.5, 0.5)

  losses <- list(
    list(
      fn = equality_loss,
      target = target,
      prox = prox_equality
    )
  )

  reg <- list(
    fn = function(w, lambda) entropy_regularizer(w, lambda, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = NULL)
  )

  # First: unbounded solution to establish baseline
  sol_unbounded <- admm(F, losses, reg, lam = 1,
                        control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))
  w_unbounded <- sol_unbounded$w_best * n  # Convert to ratio scale

  cat(sprintf("  Unbounded weight range: [%.3f, %.3f] (as ratio to uniform)\n",
              min(w_unbounded), max(w_unbounded)))

  # Hard bounds: strictly enforced via bounded simplex projection
  bounds_hard <- c(0.3, 3.0)
  sol_hard <- admm(F, losses, reg, lam = 1,
                   bounds = bounds_hard, bounds_method = "hard",
                   control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))
  w_hard <- sol_hard$w_best * n

  cat(sprintf("  Hard bounds [%.1f, %.1f] weight range: [%.3f, %.3f]\n",
              bounds_hard[1], bounds_hard[2], min(w_hard), max(w_hard)))

  check(min(w_hard) >= bounds_hard[1] - 1e-4, "hard: lower bound respected")
  check(max(w_hard) <= bounds_hard[2] + 1e-4, "hard: upper bound respected")
  check(sol_hard$converged, "hard: converged")
  check(abs(sum(sol_hard$w_best) - 1) < 1e-6, "hard: weights sum to 1")
  check(all(sol_hard$w_best >= -1e-8), "hard: weights non-negative")

  # Achieved values: targets may be compromised by bounds
  achieved_hard <- as.numeric(F %*% sol_hard$w_best)
  cat(sprintf("  Hard achieved: [%.4f, %.4f]\n", achieved_hard[1], achieved_hard[2]))

  # Soft bounds: uses regularizer clipping (may violate slightly)
  sol_soft <- admm(F, losses, reg, lam = 1,
                   bounds = bounds_hard, bounds_method = "soft",
                   control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))
  w_soft <- sol_soft$w_best * n

  cat(sprintf("  Soft bounds [%.1f, %.1f] weight range: [%.3f, %.3f]\n",
              bounds_hard[1], bounds_hard[2], min(w_soft), max(w_soft)))

  achieved_soft <- as.numeric(F %*% sol_soft$w_best)
  cat(sprintf("  Soft achieved: [%.4f, %.4f]\n", achieved_soft[1], achieved_soft[2]))

  # Soft bounds may not be strictly respected, but weights should be closer to bounds
  check(max(w_soft) < max(w_unbounded) + 0.1, "soft: max weight reduced vs unbounded")
  check(sol_soft$converged, "soft: converged")
  check(abs(sum(sol_soft$w_best) - 1) < 1e-6, "soft: weights sum to 1")

  cat(sprintf("  Converged: hard=%d iters, soft=%d iters\n",
              sol_hard$iterations, sol_soft$iterations))
}

# ── 6. Sum squares regularizer ────────────────────────────────────────────

test_sum_squares_regularizer <- function() {
  cat("\n6. Sum squares regularizer\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(47)
  n <- 1000

  categories <- sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4))
  F <- Matrix(0, nrow = 2, ncol = n, sparse = TRUE)
  F[1, categories == 0] <- 1
  F[2, categories == 1] <- 1

  target <- c(0.5, 0.5)

  losses <- list(
    list(
      fn = equality_loss,
      target = target,
      prox = prox_equality
    )
  )

  reg <- list(
    fn = function(w, lambda) sum_squares_regularizer(w, lambda),
    prox = function(w, lambda) prox_sum_squares_reg(w, lambda)
  )

  sol <- admm(F, losses, reg, lam = 1,
              control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))

  achieved <- as.numeric(F %*% sol$w_best)

  cat(sprintf("  Achieved: [%.6f, %.6f]  Target: [%.6f, %.6f]\n",
              achieved[1], achieved[2], target[1], target[2]))

  check_close(achieved, target, "achieved matches targets")
  check(sol$converged, "converged")
  check(abs(sum(sol$w_best) - 1) < 1e-6, "weights sum to 1")
  check(all(sol$w_best >= -1e-8), "weights non-negative")

  # Sum squares regularizer should produce lower weight variance than entropy
  # (shrinks toward zero more aggressively)
  reg_entropy <- list(
    fn = function(w, lambda) entropy_regularizer(w, lambda, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = NULL)
  )
  sol_entropy <- admm(F, losses, reg_entropy, lam = 1,
                      control = list(maxiter = 5000, eps_abs = 1e-6, eps_rel = 1e-6))

  var_ss <- var(sol$w_best)
  var_entropy <- var(sol_entropy$w_best)

  cat(sprintf("  Weight variance: sum_squares=%.2e, entropy=%.2e\n", var_ss, var_entropy))

  cat(sprintf("  Converged in %d iterations\n", sol$iterations))
}

# ── 7. High-level regrake() correctness ──────────────────────────────────

test_regrake_range <- function() {
  cat("\n7. regrake() with range constraints (rr_range)\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(48)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.65, 0.35)),
    age = sample(c("18-34", "35-54", "55+"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )

  pop_data <- data.frame(
    variable = c(rep("sex", 2), rep("age", 3)),
    level = c("M", "F", "18-34", "35-54", "55+"),
    proportion = c(0.49, 0.51, 0.3, 0.4, 0.3)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_range(sex, 0.02) + rr_range(age, 0.03),
    population_data = pop_data,
    pop_type = "proportions"
  )

  # Range constraints: residuals should be within margin
  sex_rows <- result$balance[result$balance$variable == "sex", ]
  age_rows <- result$balance[result$balance$variable == "age", ]

  cat(sprintf("  Sex residuals: [%.4f, %.4f]  (margin: 0.02)\n",
              min(sex_rows$residual), max(sex_rows$residual)))
  cat(sprintf("  Age residuals: [%.4f, %.4f]  (margin: 0.03)\n",
              min(age_rows$residual), max(age_rows$residual)))

  check(all(abs(sex_rows$residual) <= 0.02 + 1e-3), "sex within +/-0.02 margin")
  check(all(abs(age_rows$residual) <= 0.03 + 1e-3), "age within +/-0.03 margin")
  check(result$diagnostics$converged, "converged")

  cat(sprintf("  Kish deff: %.3f, ESS: %.0f / %d\n",
              result$diagnostics$kish_deff, result$diagnostics$kish_ess, n))
}

test_regrake_bounds <- function() {
  cat("\n8. regrake() with weight bounds\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(49)
  n <- 500

  # Strongly skewed to force large weights
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.8, 0.2))
  )

  pop_data <- data.frame(
    variable = "sex",
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  # Hard bounds
  result_hard <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.3, 3),
    bounds_method = "hard"
  )

  w_ratio <- result_hard$weights  # weights already at ratio scale (mean=1)
  cat(sprintf("  Hard bounds [0.3, 3.0] weight range: [%.3f, %.3f]\n",
              min(w_ratio), max(w_ratio)))

  check(min(w_ratio) >= 0.3 - 0.01, "hard: lower bound respected")
  check(max(w_ratio) <= 3.0 + 0.01, "hard: upper bound respected")
  check(result_hard$diagnostics$converged, "hard: converged")

  # Soft bounds
  result_soft <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.3, 3),
    bounds_method = "soft"
  )

  w_ratio_soft <- result_soft$weights
  cat(sprintf("  Soft bounds [0.3, 3.0] weight range: [%.3f, %.3f]\n",
              min(w_ratio_soft), max(w_ratio_soft)))

  # Soft bounds should keep weights reasonable even if not perfectly within bounds
  check(max(w_ratio_soft) < 5, "soft: max weight reasonable")
  check(result_soft$diagnostics$converged, "soft: converged")
}

test_regrake_exact_tol <- function() {
  cat("\n9. regrake() with exact_tol\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  set.seed(50)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(c("18-34", "35-54", "55+"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )

  pop_data <- data.frame(
    variable = c(rep("sex", 2), rep("age", 3)),
    level = c("M", "F", "18-34", "35-54", "55+"),
    proportion = c(0.49, 0.51, 0.3, 0.4, 0.3)
  )

  # With exact_tol, exact constraints become range constraints
  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex) + rr_exact(age),
    population_data = pop_data,
    pop_type = "proportions",
    exact_tol = 0.02
  )

  check(all(result$balance$type == "range"), "all constraints converted to range")
  check(all(abs(result$balance$residual) <= 0.02 + 1e-3), "residuals within tolerance")
  check(result$diagnostics$converged, "converged")

  # Compare deff: exact_tol should give lower deff than strict exact
  result_strict <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex) + rr_exact(age),
    population_data = pop_data,
    pop_type = "proportions"
  )

  cat(sprintf("  Kish deff: exact_tol=%.3f, strict=%.3f\n",
              result$diagnostics$kish_deff, result_strict$diagnostics$kish_deff))

  # Allowing tolerance should generally produce lower deff (less extreme weights)
  check(result$diagnostics$kish_deff <= result_strict$diagnostics$kish_deff + 0.01,
        "exact_tol deff <= strict deff")
}

# ── Main ──────────────────────────────────────────────────────────────────

main <- function() {
  cat("R-only Correctness Checks for Extended Solver Features\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  test_inequality()
  test_kl_loss()
  test_kl_regularizer()
  test_boolean_regularizer()
  test_weight_bounds()
  test_sum_squares_regularizer()
  test_regrake_range()
  test_regrake_bounds()
  test_regrake_exact_tol()

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(sprintf("Results: %d passed, %d failed\n", n_pass, n_fail))

  if (n_fail > 0) {
    cat("SOME CHECKS FAILED\n")
  } else {
    cat("ALL CHECKS PASSED\n")
  }

  invisible(list(pass = n_pass, fail = n_fail))
}

if (!interactive()) {
  main()
}
