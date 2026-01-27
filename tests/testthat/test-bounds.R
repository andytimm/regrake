# Tests for weight bounds functionality

# Test projection_bounded_simplex -------------------------------------------

test_that("projection_bounded_simplex maintains sum constraint", {
  set.seed(605)
  n <- 100
  v <- rnorm(n)
  lower <- 0.005
  upper <- 0.02

  result <- projection_bounded_simplex(v, lower, upper)

  expect_equal(sum(result), 1, tolerance = 1e-6)
  expect_true(all(result >= lower - 1e-6))
  expect_true(all(result <= upper + 1e-6))
})

test_that("projection_bounded_simplex handles vector bounds", {
  set.seed(123)
  n <- 50
  v <- runif(n)
  lower <- rep(0.01, n)
  upper <- rep(0.03, n)

  result <- projection_bounded_simplex(v, lower, upper)

  expect_equal(sum(result), 1, tolerance = 1e-6)
  expect_true(all(result >= lower - 1e-6))
  expect_true(all(result <= upper + 1e-6))
})

test_that("projection_bounded_simplex warns on infeasible lower bounds", {
  v <- c(0.5, 0.3, 0.2)
  lower <- 0.4 # sum(lower) = 1.2 > 1, infeasible
  upper <- 1.0

  expect_warning(
    result <- projection_bounded_simplex(v, lower, upper),
    "infeasible"
  )
  # Should still return something that sums to 1
  expect_equal(sum(result), 1, tolerance = 1e-6)
})

test_that("projection_bounded_simplex warns on infeasible upper bounds", {
  v <- c(0.5, 0.3, 0.2)
  lower <- 0
  upper <- 0.2 # sum(upper) = 0.6 < 1, infeasible

  expect_warning(
    result <- projection_bounded_simplex(v, lower, upper),
    "infeasible"
  )
  expect_equal(sum(result), 1, tolerance = 1e-6)
})

test_that("projection_bounded_simplex reduces to simplex when bounds don't constrain", {
  v <- c(2, 1, 0, -1)
  result_bounded <- projection_bounded_simplex(v, lower = 0, upper = 1)
  result_standard <- projection_simplex(v)

  expect_equal(result_bounded, result_standard, tolerance = 1e-6)
})

test_that("projection_bounded_simplex returns clipped vector when already feasible", {
  v <- c(0.3, 0.3, 0.2, 0.2) # Already sums to 1
  lower <- 0.1
  upper <- 0.4

  result <- projection_bounded_simplex(v, lower, upper)

  expect_equal(result, v)
})

# Test helper functions -----------------------------------------------------

test_that("convert_bounds_to_limit works correctly", {
  # Symmetric bounds
  expect_equal(convert_bounds_to_limit(c(0.5, 2)), 2)
  expect_equal(convert_bounds_to_limit(c(1 / 3, 3)), 3) # Exact 1/3

  # Asymmetric bounds - takes max
  expect_equal(convert_bounds_to_limit(c(0.2, 3)), 5) # 1/0.2 = 5 > 3
  expect_equal(convert_bounds_to_limit(c(0.5, 5)), 5) # 1/0.5 = 2 < 5
})

test_that("is_symmetric_bounds detects symmetric bounds", {
  expect_true(is_symmetric_bounds(c(0.5, 2)))
  expect_true(is_symmetric_bounds(c(0.25, 4)))
  expect_true(is_symmetric_bounds(c(1, 1))) # Edge case

  expect_false(is_symmetric_bounds(c(0.3, 3)))
  expect_false(is_symmetric_bounds(c(0.5, 5)))
})

test_that("check_bounds_violated works correctly", {
  # Within bounds
  expect_false(check_bounds_violated(c(0.5, 1, 1.5), c(0.3, 2)))

  # Below lower

  expect_true(check_bounds_violated(c(0.2, 1, 1.5), c(0.3, 2)))

  # Above upper
  expect_true(check_bounds_violated(c(0.5, 1, 2.5), c(0.3, 2)))
})

# Test bounds in regrake ----------------------------------------------------

test_that("regrake with soft bounds works", {
  set.seed(42)
  n <- 200

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.3, 3),
    bounds_method = "soft"
  )

  # Should converge and return weights
  expect_equal(length(result$weights), n)
  expect_equal(sum(result$weights), n, tolerance = 1e-4)

  # Diagnostics should include bounds info
  expect_equal(result$diagnostics$bounds, c(0.3, 3))
  expect_equal(result$diagnostics$bounds_method, "soft")
})

test_that("regrake with hard bounds strictly enforces bounds", {
  set.seed(42)
  n <- 200

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.5, 2),
    bounds_method = "hard"
  )

  # Hard bounds should be strictly enforced
  expect_true(all(result$weights >= 0.5 - 1e-6))
  expect_true(all(result$weights <= 2 + 1e-6))

  # Diagnostics should show bounds not violated
  expect_false(result$diagnostics$bounds_violated)
})

test_that("regrake hard bounds enforced even when targets conflict", {
  set.seed(42)
  n <- 500

  # Very imbalanced sample
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.9, 0.1))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.33, 3), # Tight bounds that conflict with targets
    bounds_method = "hard"
  )

  # Hard bounds must be enforced even though targets can't be hit exactly
  expect_true(all(result$weights >= 0.33 - 1e-6))
  expect_true(all(result$weights <= 3 + 1e-6))

  # Target won't be hit exactly due to binding bounds
  achieved_m <- sum(result$weights[sample_data$sex == "M"]) / n
  # Error should be nonzero but not huge
  expect_true(abs(achieved_m - 0.5) > 0.01) # Won't be exact
  expect_true(abs(achieved_m - 0.5) < 0.3) # But reasonably close
})

test_that("soft bounds may be violated when targets conflict", {
  set.seed(42)
  n <- 500

  # Very imbalanced sample
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.9, 0.1))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.33, 3), # Bounds will be violated to hit targets
    bounds_method = "soft"
  )

  # Soft bounds: targets should be hit closely
  achieved_m <- sum(result$weights[sample_data$sex == "M"]) / n
  expect_equal(achieved_m, 0.5, tolerance = 0.01)

  # But bounds may be violated
  # (diagnostics should indicate this)
  expect_true(result$diagnostics$bounds_violated)
})

test_that("diagnostics includes bounds information", {
  set.seed(42)
  n <- 100

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE)
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.2, 5),
    bounds_method = "soft"
  )

  # Check diagnostics structure
  expect_true("bounds" %in% names(result$diagnostics))
  expect_true("bounds_method" %in% names(result$diagnostics))
  expect_true("bounds_violated" %in% names(result$diagnostics))

  expect_equal(result$diagnostics$bounds, c(0.2, 5))
  expect_equal(result$diagnostics$bounds_method, "soft")
  expect_type(result$diagnostics$bounds_violated, "logical")
})

test_that("bounds_method defaults to soft",
{
  set.seed(42)
  n <- 100

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE)
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  # Don't specify bounds_method - should default to "soft"
  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.2, 5)
  )

  expect_equal(result$diagnostics$bounds_method, "soft")
})
