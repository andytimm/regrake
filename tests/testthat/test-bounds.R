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
# Realistic survey scenarios with reasonable bounds

test_that("regrake with bounds handles realistic multi-variable raking", {
  # Realistic scenario: sample slightly off on sex and age
  # This is what a survey researcher would actually encounter
  set.seed(42)
  n <- 200
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)),
    age = sample(c("18-34", "35-54", "55+"), n, replace = TRUE, prob = c(0.20, 0.35, 0.45))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex", "age", "age", "age"),
    level = c("M", "F", "18-34", "35-54", "55+"),
    proportion = c(0.49, 0.51, 0.28, 0.35, 0.37)
  )

  # Soft bounds with realistic (0.3, 3) range
  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex) + rr_exact(age),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.3, 3)
    # bounds_method not specified - tests default to "soft"
  )

  # Basic convergence
  expect_equal(length(result$weights), n)
  expect_equal(sum(result$weights), n, tolerance = 1e-4)

  # Targets should be hit closely
  achieved_sex_m <- sum(result$weights[sample_data$sex == "M"]) / sum(result$weights)
  achieved_age_young <- sum(result$weights[sample_data$age == "18-34"]) / sum(result$weights)
  expect_equal(achieved_sex_m, 0.49, tolerance = 0.01)
  expect_equal(achieved_age_young, 0.28, tolerance = 0.01)

  # With reasonable bounds, should not be violated
  expect_false(result$diagnostics$bounds_violated)

  # Weights should be reasonable (Kish deff < 1.3 for mild imbalance)
  w_norm <- result$weights * n / sum(result$weights)
  kish_deff <- 1 + var(w_norm) / mean(w_norm)^2
  expect_lt(kish_deff, 1.3)

  # Diagnostics
  expect_equal(result$diagnostics$bounds, c(0.3, 3))
  expect_equal(result$diagnostics$bounds_method, "soft")
})

test_that("regrake with hard bounds enforces limits", {
  # Same realistic scenario but with hard bounds
  set.seed(42)
  n <- 200
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)),
    age = sample(c("18-34", "35-54", "55+"), n, replace = TRUE, prob = c(0.20, 0.35, 0.45))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex", "age", "age", "age"),
    level = c("M", "F", "18-34", "35-54", "55+"),
    proportion = c(0.49, 0.51, 0.28, 0.35, 0.37)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex) + rr_exact(age),
    population_data = pop_data,
    pop_type = "proportions",
    bounds = c(0.3, 3),
    bounds_method = "hard"
  )

  # Bounds must be strictly enforced
  expect_true(all(result$weights >= 0.3 - 1e-6))
  expect_true(all(result$weights <= 3 + 1e-6))

  # Diagnostics
  expect_false(result$diagnostics$bounds_violated)
  expect_equal(result$diagnostics$bounds_method, "hard")

  # Targets should still be reasonably close
  achieved_sex_m <- sum(result$weights[sample_data$sex == "M"]) / sum(result$weights)
  expect_true(abs(achieved_sex_m - 0.49) < 0.05)
})

test_that("regrake errors on infeasible hard bounds", {
  sample_data <- make_sample_sex_age(n = 120, seed = 321)
  pop_data <- make_pop_sex_age()

  expect_error(
    regrake(
      data = sample_data,
      formula = ~ rr_exact(sex) + rr_exact(age),
      population_data = pop_data,
      pop_type = "proportions",
      bounds = c(1.2, 2.0),
      bounds_method = "hard"
    ),
    "Hard bounds are infeasible"
  )
})
