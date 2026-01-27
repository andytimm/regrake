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
# These tests are consolidated to minimize slow regrake() calls

test_that("regrake with soft bounds works correctly", {
  # Use moderately imbalanced sample to test bounds violation behavior
  set.seed(42)
  n <- 100
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  # Test with tight bounds that will be violated to hit targets
  # With 70/30 sample and 50/50 target, weights need to be ~0.71 and ~1.67
  # Bounds of (0.8, 1.5) are too tight, so soft bounds will be violated
  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.8, 1.5)
    # bounds_method not specified - tests default to "soft"
  )

  # Basic convergence

  expect_equal(length(result$weights), n)
  expect_equal(sum(result$weights), n, tolerance = 1e-4)

  # Soft bounds: targets should be hit closely (bounds may be violated)
  achieved_m <- sum(result$weights[sample_data$sex == "M"]) / n
  expect_equal(achieved_m, 0.5, tolerance = 0.01)

  # Diagnostics structure and values
  expect_true("bounds" %in% names(result$diagnostics))
  expect_true("bounds_method" %in% names(result$diagnostics))
  expect_true("bounds_violated" %in% names(result$diagnostics))
  expect_equal(result$diagnostics$bounds, c(0.8, 1.5))
  expect_equal(result$diagnostics$bounds_method, "soft") # Tests default
  expect_type(result$diagnostics$bounds_violated, "logical")

  # With this imbalanced sample, bounds should be violated to hit targets
  expect_true(result$diagnostics$bounds_violated)
})

test_that("regrake with hard bounds strictly enforces bounds", {
  # Use moderately imbalanced sample to test that bounds are enforced
  # even when targets can't be hit exactly
  set.seed(42)
  n <- 100
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    proportion = c(0.5, 0.5)
  )

  # With 70/30 sample and 50/50 target, weights need ~0.71 and ~1.67
  # Bounds of (0.8, 1.5) will bind and prevent exact target achievement
  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.8, 1.5),
    bounds_method = "hard"
  )

  # Hard bounds must be strictly enforced
  expect_true(all(result$weights >= 0.8 - 1e-6))
  expect_true(all(result$weights <= 1.5 + 1e-6))

  # Diagnostics should show bounds NOT violated (hard enforcement worked)
  expect_false(result$diagnostics$bounds_violated)
  expect_equal(result$diagnostics$bounds_method, "hard")

  # Target won't be hit exactly due to binding bounds
  achieved_m <- sum(result$weights[sample_data$sex == "M"]) / n
  expect_true(abs(achieved_m - 0.5) > 0.01) # Won't be exact
  expect_true(abs(achieved_m - 0.5) < 0.3) # But reasonably close
})
