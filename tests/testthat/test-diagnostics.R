# Tests for convergence diagnostics, Kish deff/ESS, print/summary, and exact_tol

# Helper: create a minimal regrake result for reuse
make_test_result <- function(exact_tol = NULL, maxiter = 5000) {
  regrake(
    data = make_sample_sex_age(),
    formula = ~ rr_exact(sex) + rr_exact(age),
    population_data = make_pop_sex_age(),
    pop_type = "proportions",
    exact_tol = exact_tol,
    control = list(maxiter = maxiter)
  )
}

# Convergence diagnostics from solver ----------------------------------------

test_that("admm() returns convergence diagnostics", {
  set.seed(42)
  n <- 100
  m <- 2
  F <- matrix(0, nrow = m, ncol = n)
  F[1, 1:50] <- 1
  F[2, 51:100] <- 1

  losses <- list(
    list(
      fn = equality_loss,
      target = 0.5,
      prox = prox_equality,
      evaluate = function(x) sum(equality_loss(x, 0.5))
    ),
    list(
      fn = equality_loss,
      target = 0.5,
      prox = prox_equality,
      evaluate = function(x) sum(equality_loss(x, 0.5))
    )
  )

  reg <- list(
    fn = function(w, lambda) entropy_regularizer(w, lambda, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = NULL)
  )

  result <- admm(F, losses, reg, lam = 1)

  expect_true(result$converged)
  expect_true(is.numeric(result$iterations))
  expect_true(result$iterations > 0)
  expect_true(result$iterations <= 5000)
  expect_true(is.numeric(result$primal_residual))
  expect_true(is.numeric(result$dual_residual))
  expect_true(result$primal_residual >= 0)
  expect_true(result$dual_residual >= 0)
})

test_that("admm() reports non-convergence when maxiter is too low", {
  set.seed(42)
  n <- 100
  m <- 2
  F <- matrix(0, nrow = m, ncol = n)
  F[1, 1:50] <- 1
  F[2, 51:100] <- 1

  losses <- list(
    list(
      fn = equality_loss,
      target = 0.5,
      prox = prox_equality,
      evaluate = function(x) sum(equality_loss(x, 0.5))
    ),
    list(
      fn = equality_loss,
      target = 0.5,
      prox = prox_equality,
      evaluate = function(x) sum(equality_loss(x, 0.5))
    )
  )

  reg <- list(
    fn = function(w, lambda) entropy_regularizer(w, lambda, limit = NULL),
    prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = NULL)
  )

  result <- admm(F, losses, reg, lam = 1, control = list(maxiter = 2))

  expect_false(result$converged)
  expect_equal(result$iterations, 2)
})

# Non-convergence warning in regrake() --------------------------------------

test_that("regrake() warns when solver does not converge", {
  # maxiter = 10 is enough to avoid zero-weight errors but not enough to converge
  expect_warning(
    make_test_result(maxiter = 10),
    "did not converge"
  )
})

test_that("regrake() does not warn when solver converges", {
  expect_no_warning(make_test_result())
})

# Convergence info in diagnostics --------------------------------------------

test_that("diagnostics contain convergence info", {
  result <- make_test_result()
  d <- result$diagnostics

  expect_true(d$converged)
  expect_true(is.numeric(d$iterations))
  expect_true(d$iterations > 0)
  expect_true(is.numeric(d$primal_residual))
  expect_true(is.numeric(d$dual_residual))
})

# Kish deff / ESS -----------------------------------------------------------

test_that("diagnostics contain Kish deff and ESS", {
  result <- make_test_result()
  d <- result$diagnostics

  expect_true(is.numeric(d$kish_deff))
  expect_true(is.numeric(d$kish_ess))
  expect_true(d$kish_deff >= 1)
  expect_true(d$kish_ess <= length(result$weights))
  expect_true(d$kish_ess > 0)
})

test_that("Kish deff/ESS are computed correctly", {
  result <- make_test_result()
  w <- result$weights
  n <- length(w)

  expected_deff <- 1 + var(w) / mean(w)^2
  expected_ess <- n / expected_deff

  expect_equal(result$diagnostics$kish_deff, expected_deff)
  expect_equal(result$diagnostics$kish_ess, expected_ess)
})

# print.regrake() -----------------------------------------------------------

test_that("print.regrake() produces expected output", {
  result <- make_test_result()
  output <- capture.output(print(result))
  output_str <- paste(output, collapse = "\n")

  expect_match(output_str, "Regrake result")
  expect_match(output_str, "Sample size")
  expect_match(output_str, "Constraints")
  expect_match(output_str, "Regularizer")
  expect_match(output_str, "Converged")
  expect_match(output_str, "Weight range")
  expect_match(output_str, "Kish ESS")
  expect_match(output_str, "Max margin diff")
})

test_that("print.regrake() returns invisible result", {
  result <- make_test_result()
  ret <- withVisible(print(result))
  expect_false(ret$visible)
  expect_identical(ret$value, result)
})

test_that("print.regrake() shows exact_tol when set", {
  result <- make_test_result(exact_tol = 0.02)
  output <- capture.output(print(result))
  output_str <- paste(output, collapse = "\n")

  expect_match(output_str, "Exact tolerance")
  expect_match(output_str, "0.02")
})

test_that("print.regrake() does not show exact_tol when NULL", {
  result <- make_test_result()
  output <- capture.output(print(result))
  output_str <- paste(output, collapse = "\n")

  expect_no_match(output_str, "Exact tolerance")
})

# summary.regrake() ---------------------------------------------------------

test_that("summary.regrake() produces expected output", {
  result <- make_test_result()
  output <- capture.output(summary(result))
  output_str <- paste(output, collapse = "\n")

  # Should contain print output
  expect_match(output_str, "Regrake result")
  # Should contain weight distribution
  expect_match(output_str, "Weight distribution")
  expect_match(output_str, "Median")
  # Should contain balance table
  expect_match(output_str, "Balance")
  expect_match(output_str, "achieved")
  expect_match(output_str, "target")
})

test_that("summary.regrake() returns invisible result", {
  result <- make_test_result()
  ret <- withVisible(summary(result))
  expect_false(ret$visible)
  expect_identical(ret$value, result)
})

# exact_tol parameter --------------------------------------------------------

test_that("exact_tol converts exact constraints to range", {
  result <- make_test_result(exact_tol = 0.02)

  # All constraint types should be "range" (converted from "exact")
  expect_true(all(result$balance$type == "range"))

  # Margins should be within or very near the tolerance
  # (ADMM may have small constraint violations near convergence)
  expect_true(all(abs(result$balance$residual) <= 0.02 + 1e-3))
})

test_that("exact_tol = NULL preserves exact constraints", {
  result <- make_test_result(exact_tol = NULL)

  # All constraint types should still be "exact"
  expect_true(all(result$balance$type == "exact"))
})

test_that("exact_tol is stored on result object", {
  result1 <- make_test_result(exact_tol = 0.05)
  expect_equal(result1$exact_tol, 0.05)

  result2 <- make_test_result()
  expect_null(result2$exact_tol)
})

test_that("invalid exact_tol values are rejected", {
  expect_error(make_test_result(exact_tol = -0.01), "exact_tol")
  expect_error(make_test_result(exact_tol = 0), "exact_tol")
  expect_error(make_test_result(exact_tol = "abc"), "exact_tol")
  expect_error(make_test_result(exact_tol = c(0.01, 0.02)), "exact_tol")
})

# Result object fields -------------------------------------------------------

test_that("result object contains formula, regularizer, and lambda", {
  result <- make_test_result()

  expect_true(inherits(result$formula, "formula"))
  expect_equal(result$regularizer, "entropy")
  expect_equal(result$lambda, 1)
})

test_that("exact_tol does not affect non-exact constraints", {
  # Use l2 for age, exact for sex. exact_tol should only affect sex.
  result <- regrake(
    data = make_sample_sex_age(),
    formula = ~ rr_exact(sex) + rr_l2(age),
    population_data = make_pop_sex_age(),
    pop_type = "proportions",
    exact_tol = 0.02
  )

  # sex should be converted to range
  sex_rows <- result$balance[result$balance$variable == "sex", ]
  expect_true(all(sex_rows$type == "range"))

  # age should still be l2

  age_rows <- result$balance[result$balance$variable == "age", ]
  expect_true(all(age_rows$type == "l2"))
})

# margin_tol parameter tests ---------------------------------------------------

test_that("margin_tol scales eps_abs/eps_rel based on problem size", {
  set.seed(42)
  n <- 1000
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4))
  )

  pop_data <- data.frame(
    variable = "sex",
    level = c("M", "F"),
    proportion = c(0.49, 0.51)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    control = list(margin_tol = 0.001)
  )

  expect_true(result$diagnostics$converged)
  expect_lt(max(abs(result$balance$residual)), 0.001)
})

test_that("default margin_tol = 1e-4 is used", {
  result <- make_test_result()
  expect_equal(result$margin_tol, 1e-4)
})

test_that("margin_tol is stored on result object", {
  set.seed(42)
  n <- 500
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4))
  )

  pop_data <- data.frame(
    variable = "sex",
    level = c("M", "F"),
    proportion = c(0.49, 0.51)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    control = list(margin_tol = 0.001)
  )

  expect_equal(result$margin_tol, 0.001)
})

test_that("explicit eps_abs/eps_rel opts out of margin_tol", {
  set.seed(42)
  n <- 500
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4))
  )

  pop_data <- data.frame(
    variable = "sex",
    level = c("M", "F"),
    proportion = c(0.49, 0.51)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    control = list(eps_abs = 1e-6, eps_rel = 1e-6)
  )

  expect_null(result$margin_tol)
})

test_that("margin_tol = NULL uses raw eps_abs/eps_rel", {
  set.seed(42)
  n <- 500
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4))
  )

  pop_data <- data.frame(
    variable = "sex",
    level = c("M", "F"),
    proportion = c(0.49, 0.51)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    control = list(margin_tol = NULL, eps_abs = 1e-6, eps_rel = 1e-6)
  )

  expect_null(result$margin_tol)
  expect_true(result$diagnostics$converged)
})

test_that("invalid margin_tol values are rejected", {
  set.seed(42)
  n <- 100
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE)
  )

  pop_data <- data.frame(
    variable = "sex",
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  expect_error(
    regrake(sample_data, ~ rr_exact(sex), pop_data, "proportions",
            control = list(margin_tol = -0.01)),
    "margin_tol must be a single positive number"
  )

  expect_error(
    regrake(sample_data, ~ rr_exact(sex), pop_data, "proportions",
            control = list(margin_tol = 0)),
    "margin_tol must be a single positive number"
  )

  expect_error(
    regrake(sample_data, ~ rr_exact(sex), pop_data, "proportions",
            control = list(margin_tol = c(0.01, 0.02))),
    "margin_tol must be a single positive number"
  )

  expect_error(
    regrake(sample_data, ~ rr_exact(sex), pop_data, "proportions",
            control = list(margin_tol = "0.01")),
    "margin_tol must be a single positive number"
  )
})
