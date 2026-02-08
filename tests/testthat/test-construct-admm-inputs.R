# Test construct_admm_inputs -------------------------------------------------------

test_that("construct_admm_inputs handles basic case with single exact term", {
  # Setup test data
  data <- data.frame(
    x = factor(c("a", "b", "a", "b", "a")),
    y = factor(c("1", "1", "2", "2", "1"))
  )

  # Create formula spec (simulating output from parse_raking_formula)
  formula_spec <- list(
    formula = ~x,
    terms = list(
      list(
        type = "exact",
        variables = "x",
        interaction = NULL
      )
    )
  )

  # Create target values
  target_values <- list(
    targets = list(
      exact_x = c(a = 0.6, b = 0.4)
    )
  )

  # Run function
  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Check structure
  expect_type(result, "list")
  expect_true("design_matrix" %in% names(result))
  expect_true("losses" %in% names(result))

  # Check design matrix
  expect_true(inherits(result$design_matrix, "Matrix"))
  expect_equal(dim(result$design_matrix), c(2, 5)) # 2 levels, 5 samples
  expect_equal(as.vector(result$design_matrix[1, ]), c(1, 0, 1, 0, 1)) # a's
  expect_equal(as.vector(result$design_matrix[2, ]), c(0, 1, 0, 1, 0)) # b's

  # Check losses
  expect_length(result$losses, 1)
  expect_true(all(c("fn", "target", "prox") %in% names(result$losses[[1]])))
  expect_equal(result$losses[[1]]$fn, equality_loss)
  expect_equal(result$losses[[1]]$prox, prox_equality)
  expect_equal(result$losses[[1]]$target, c(a = 0.6, b = 0.4))
})

test_that("construct_admm_inputs handles l2 terms", {
  # Setup test data
  data <- data.frame(
    x = factor(c("a", "b", "a", "b", "a"))
  )

  formula_spec <- list(
    formula = ~x,
    terms = list(
      list(
        type = "l2",
        variables = "x",
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      l2_x = c(a = 0.6, b = 0.4)
    )
  )

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Check loss function type
  expect_equal(result$losses[[1]]$fn, least_squares_loss)
  expect_equal(result$losses[[1]]$prox, prox_least_squares)
})

test_that("construct_admm_inputs handles interactions", {
  # Setup test data
  data <- data.frame(
    x = factor(c("a", "b", "a", "b", "a")),
    y = factor(c("1", "1", "2", "2", "1"))
  )

  formula_spec <- list(
    formula = ~ x:y,
    terms = list(
      list(
        type = "exact",
        variables = c("x", "y"),
        interaction = TRUE
      )
    )
  )

  target_values <- list(
    targets = list(
      "exact_x:y" = c("a:1" = 0.3, "a:2" = 0.3, "b:1" = 0.2, "b:2" = 0.2)
    )
  )

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Check design matrix dimensions (4 interaction levels, 5 samples)
  expect_equal(dim(result$design_matrix), c(4, 5))

  # Verify interaction encoding - must use Matrix::colSums explicitly for sparse matrices
  # to ensure correct method dispatch (base::colSums may not work as expected)
  expect_equal(Matrix::colSums(result$design_matrix), rep(1, 5)) # Each sample in exactly one category
})

test_that("construct_admm_inputs handles multiple terms", {
  # Setup test data
  data <- data.frame(
    x = factor(c("a", "b", "a", "b", "a")),
    y = factor(c("1", "1", "2", "2", "1"))
  )

  formula_spec <- list(
    formula = ~ x + y,
    terms = list(
      list(
        type = "exact",
        variables = "x",
        interaction = NULL
      ),
      list(
        type = "l2",
        variables = "y",
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      exact_x = c(a = 0.6, b = 0.4),
      l2_y = c("1" = 0.6, "2" = 0.4)
    )
  )

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Check structure
  expect_length(result$losses, 2)
  expect_equal(result$losses[[1]]$fn, equality_loss)
  expect_equal(result$losses[[2]]$fn, least_squares_loss)

  # Check design matrix dimensions (2 levels for x + 2 levels for y = 4 rows)
  expect_equal(dim(result$design_matrix), c(4, 5))
})

test_that("construct_admm_inputs errors on missing target values", {
  data <- data.frame(x = factor(c("a", "b")))

  formula_spec <- list(
    formula = ~x,
    terms = list(
      list(
        type = "exact",
        variables = "x",
        interaction = NULL
      )
    )
  )

  target_values <- list(targets = list()) # Empty targets

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "Missing target values for term: x"
  )
})

test_that("construct_admm_inputs handles character variables", {
  # Setup test data with character columns
  data <- data.frame(
    x = c("a", "b", "a", "b", "a"),
    y = c("1", "1", "2", "2", "1"),
    stringsAsFactors = FALSE
  )

  formula_spec <- list(
    formula = ~x,
    terms = list(
      list(
        type = "exact",
        variables = "x",
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      exact_x = c(a = 0.6, b = 0.4)
    )
  )

  # Should convert characters to factors internally
  expect_error(construct_admm_inputs(data, formula_spec, target_values), NA)
})

test_that("construct_admm_inputs errors on unknown term type", {
  data <- data.frame(x = factor(c("a", "b")))

  formula_spec <- list(
    formula = ~x,
    terms = list(
      list(
        type = "unknown",
        variables = "x",
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      unknown_x = c(a = 0.6, b = 0.4)
    )
  )

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "Unknown term type: unknown"
  )
})

test_that("construct_admm_inputs handles mixed exact and l2 loss types", {
  # This test covers the bug where ~ rr_exact(sex) + rr_l2(age) would fail
  # because model.frame with the original formula created columns named
  # "rr_exact(sex)" and "rr_l2(age)" instead of "sex" and "age"
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F", "M")),
    age = factor(c("young", "old", "young", "old", "young"))
  )

  # Simulate what parse_raking_formula returns for ~ rr_exact(sex) + rr_l2(age)
  formula_spec <- list(
    formula = ~ rr_exact(sex) + rr_l2(age), # The wrapped formula
    terms = list(
      list(
        type = "exact",
        variables = "sex", # Variables are unwrapped
        interaction = NULL
      ),
      list(
        type = "l2",
        variables = "age", # Variables are unwrapped
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      exact_sex = c(M = 0.5, F = 0.5),
      l2_age = c(young = 0.6, old = 0.4)
    )
  )

  # Should not error - this was the original bug
  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Verify structure
  expect_type(result, "list")
  expect_true("design_matrix" %in% names(result))
  expect_true("losses" %in% names(result))

  # Verify loss functions are correct for each term
  expect_equal(result$losses[[1]]$fn, equality_loss)
  expect_equal(result$losses[[2]]$fn, least_squares_loss)

  # Verify design matrix dimensions (2 sex levels + 2 age levels = 4 rows, 5 samples)
  expect_equal(dim(result$design_matrix), c(4, 5))
})

test_that("construct_admm_inputs handles continuous variables", {
  # Test that numeric variables get actual values in design matrix, not indicators
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F", "M")),
    income = c(50000, 75000, 45000, 80000, 55000) # Continuous
  )

  formula_spec <- list(
    formula = ~ sex + income,
    terms = list(
      list(
        type = "exact",
        variables = "sex",
        interaction = NULL
      ),
      list(
        type = "exact",
        variables = "income",
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      exact_sex = c(M = 0.5, F = 0.5),
      exact_income = c(mean = 60000) # Target mean income
    )
  )

  # Use normalize = FALSE to test raw values in design matrix
  result <- suppressWarnings(construct_admm_inputs(
    data,
    formula_spec,
    target_values,
    normalize = FALSE
  ))

  # Verify structure
  expect_type(result, "list")
  expect_true("design_matrix" %in% names(result))
  expect_true("losses" %in% names(result))

  # Verify design matrix dimensions (2 sex levels + 1 income row = 3 rows, 5 samples)
  expect_equal(dim(result$design_matrix), c(3, 5))

  # The income row should contain actual values, not 0/1 indicators
  # Row 3 (income) should be [50000, 75000, 45000, 80000, 55000]
  income_row <- as.vector(result$design_matrix[3, ])
  expect_equal(income_row, c(50000, 75000, 45000, 80000, 55000))

  # The sex rows should still be indicators
  expect_equal(as.vector(result$design_matrix[1, ]), c(0, 1, 0, 1, 0)) # F
  expect_equal(as.vector(result$design_matrix[2, ]), c(1, 0, 1, 0, 1)) # M
})

test_that("construct_admm_inputs handles continuous-only formula", {
  # Test case with only continuous variables
  data <- data.frame(
    age = c(25, 35, 45, 30, 40),
    income = c(50000, 75000, 45000, 80000, 55000)
  )

  formula_spec <- list(
    formula = ~ age + income,
    terms = list(
      list(
        type = "exact",
        variables = "age",
        interaction = NULL
      ),
      list(
        type = "exact",
        variables = "income",
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      exact_age = c(mean = 35),
      exact_income = c(mean = 60000)
    )
  )

  # Use normalize = FALSE to test raw values
  result <- construct_admm_inputs(
    data,
    formula_spec,
    target_values,
    normalize = FALSE
  )

  # Design matrix should have 2 rows (one per continuous variable)
  expect_equal(dim(result$design_matrix), c(2, 5))

  # Row 1 (age) should contain actual ages
  expect_equal(as.vector(result$design_matrix[1, ]), c(25, 35, 45, 30, 40))

  # Row 2 (income) should contain actual incomes
  expect_equal(
    as.vector(result$design_matrix[2, ]),
    c(50000, 75000, 45000, 80000, 55000)
  )
})

test_that("construct_admm_inputs normalizes continuous variables by default", {
  # Test that normalize = TRUE (default) scales continuous values by target
  data <- data.frame(
    income = c(50000, 75000, 45000, 80000, 55000)
  )

  formula_spec <- list(
    formula = ~income,
    terms = list(
      list(
        type = "exact",
        variables = "income",
        interaction = NULL
      )
    )
  )

  target_values <- list(
    targets = list(
      exact_income = c(mean = 60000)
    )
  )

  # With normalization (default)
  result_norm <- construct_admm_inputs(data, formula_spec, target_values)

  # Values should be scaled by target (60000)
  expected_normalized <- c(50000, 75000, 45000, 80000, 55000) / 60000
  expect_equal(as.vector(result_norm$design_matrix[1, ]), expected_normalized)

  # Target should be 1.0 (normalized)
  expect_equal(unname(result_norm$losses[[1]]$target), 1.0)
  expect_equal(names(result_norm$losses[[1]]$target), "mean")

  # Original target should be preserved
  expect_equal(unname(result_norm$losses[[1]]$original_target), 60000)

  # Scale factors should be tracked
  expect_true(!is.null(result_norm$scale_factors))
  expect_equal(unname(result_norm$scale_factors[[1]]$scale), 60000)
  expect_equal(result_norm$scale_factors[[1]]$index, 1)
})

test_that("construct_admm_inputs errors on continuous in interaction", {
  data <- data.frame(
    sex = factor(c("M", "F", "M")),
    age = c(25, 35, 45) # Continuous
  )

  formula_spec <- list(
    formula = ~ sex:age,
    terms = list(list(
      type = "exact",
      variables = c("sex", "age"),
      interaction = TRUE
    ))
  )

  target_values <- list(
    targets = list("exact_sex:age" = c(0.25, 0.25, 0.25, 0.25))
  )

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "Interactions with continuous variables are not supported"
  )
})

test_that("construct_admm_inputs handles variance constraint", {
  data <- data.frame(age = c(20, 30, 40, 50, 60))
  # Mean = 40, Variance = sum((x - 40)^2) / 5 = (400+100+0+100+400)/5 = 200

  formula_spec <- list(
    formula = ~age,
    terms = list(list(
      type = "var",
      variables = "age",
      interaction = NULL
    ))
  )

  # Target variance of 250
  target_values <- list(targets = list(var_age = c(var = 250)))

  # Without normalization to check raw values
  result <- suppressWarnings(construct_admm_inputs(
    data,
    formula_spec,
    target_values,
    normalize = FALSE
  ))

  # Verify structure
  expect_type(result, "list")
  expect_true("design_matrix" %in% names(result))
  expect_true("losses" %in% names(result))

  # Design matrix row should be (x - mean(x))^2
  mean_age <- mean(data$age) # 40
  expected <- (data$age - mean_age)^2 # c(400, 100, 0, 100, 400)
  expect_equal(as.vector(result$design_matrix[1, ]), expected)

  # Loss function should use equality (exact match on variance)
  expect_equal(result$losses[[1]]$fn, equality_loss)
  expect_equal(result$losses[[1]]$prox, prox_equality)
  expect_equal(unname(result$losses[[1]]$target), 250)
})

test_that("construct_admm_inputs centers rr_var on target mean when rr_mean exists", {
  data <- data.frame(age = c(20, 30, 40, 50, 60))

  formula_spec <- list(
    formula = ~age,
    terms = list(
      list(type = "exact", variables = "age", interaction = NULL),
      list(type = "var", variables = "age", interaction = NULL)
    )
  )

  target_values <- list(targets = list(
    exact_age = c(mean = 35),
    var_age = c(var = 250)
  ))

  result <- suppressWarnings(construct_admm_inputs(
    data,
    formula_spec,
    target_values,
    normalize = FALSE
  ))

  # Row 1 is mean constraint values, row 2 is variance values centered at target mean 35.
  expect_equal(as.vector(result$design_matrix[2, ]), c(225, 25, 25, 225, 625))
})

test_that("construct_admm_inputs warns and uses sample mean when rr_var has no rr_mean", {
  data <- data.frame(age = c(20, 30, 40, 50, 60))

  formula_spec <- list(
    formula = ~age,
    terms = list(list(
      type = "var",
      variables = "age",
      interaction = NULL
    ))
  )

  target_values <- list(targets = list(var_age = c(var = 250)))

  expect_warning(
    result <- construct_admm_inputs(
      data,
      formula_spec,
      target_values,
      normalize = FALSE
    ),
    "Centering on the sample mean"
  )

  # Centered at sample mean 40.
  expect_equal(as.vector(result$design_matrix[1, ]), c(400, 100, 0, 100, 400))
})

test_that("construct_admm_inputs normalizes variance constraint", {
  data <- data.frame(age = c(20, 30, 40, 50, 60))
  # Mean = 40, (x - mean)^2 = c(400, 100, 0, 100, 400)

  formula_spec <- list(
    formula = ~age,
    terms = list(list(
      type = "var",
      variables = "age",
      interaction = NULL
    ))
  )

  target_values <- list(targets = list(var_age = c(var = 200)))

  # With normalization (default)
  result <- suppressWarnings(construct_admm_inputs(data, formula_spec, target_values))

  # Values should be scaled by target
  mean_age <- mean(data$age)
  raw_values <- (data$age - mean_age)^2
  expected_normalized <- raw_values / 200
  expect_equal(as.vector(result$design_matrix[1, ]), expected_normalized)

  # Target should be 1.0 (normalized)
  expect_equal(unname(result$losses[[1]]$target), 1.0)
  expect_true(tolower(names(result$losses[[1]]$target)) %in% c("var", "variance"))

  # Original target should be preserved
  expect_equal(unname(result$losses[[1]]$original_target), 200)
})

test_that("construct_admm_inputs handles quantile constraint", {
  data <- data.frame(income = c(30000, 40000, 50000, 60000, 70000))
  # Target: 50% of weighted data should be <= 45000

  formula_spec <- list(
    formula = ~income,
    terms = list(list(
      type = "quantile",
      variables = "income",
      interaction = NULL,
      params = list(p = 0.5)
    ))
  )

  # Target quantile value is 45000, probability is 0.5
  target_values <- list(targets = list(quantile_income = c(q50 = 45000)))

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Verify structure
  expect_type(result, "list")
  expect_true("design_matrix" %in% names(result))
  expect_true("losses" %in% names(result))

  # Design matrix row should be indicators I(x <= 45000)
  # income values: 30000, 40000, 50000, 60000, 70000
  # indicators:    1,     1,     0,     0,     0  (30000 <= 45000, 40000 <= 45000)
  expected <- c(1, 1, 0, 0, 0)
  expect_equal(as.vector(result$design_matrix[1, ]), expected)

  # Loss function should use equality
  expect_equal(result$losses[[1]]$fn, equality_loss)
  expect_equal(result$losses[[1]]$prox, prox_equality)

  # Target should be the probability p, not the quantile value
  expect_equal(unname(result$losses[[1]]$target), 0.5)
  expect_equal(unname(result$losses[[1]]$original_target), 0.5)
  expect_equal(tolower(names(result$losses[[1]]$target)), "q50")
  expect_equal(tolower(names(result$losses[[1]]$original_target)), "q50")
})

test_that("construct_admm_inputs errors on multiple quantile targets", {
  data <- data.frame(income = c(30000, 40000, 50000))

  formula_spec <- list(
    formula = ~income,
    terms = list(list(
      type = "quantile",
      variables = "income",
      interaction = NULL,
      params = list(p = 0.5)
    ))
  )

  # Two targets for same variable - should error
  target_values <- list(
    targets = list(
      quantile_income = c(q25 = 35000, q50 = 45000)
    )
  )

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "Quantile constraint requires exactly one target value"
  )
})

# =============================================================================
# Tests for target ordering (bug fix verification)
# =============================================================================
# These tests verify that targets are correctly matched to design matrix rows
# regardless of the order in which levels appear in population_data.
# The design matrix rows follow R's alphabetical factor ordering.

test_that("targets are correctly ordered when pop_data order differs from alphabetical", {

  # Data with levels that would be ordered M, F alphabetically as F, M
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F", "M"))
  )

  formula_spec <- list(
    formula = ~sex,
    terms = list(list(
      type = "exact",
      variables = "sex",
      interaction = NULL
    ))
  )

  # Pop_data style: M first, F second (non-alphabetical)
  target_values <- list(
    targets = list(
      exact_sex = c(M = 0.49, F = 0.51)
    )
  )

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Design matrix rows should be in alphabetical order: F, M
  # Row 1 = F (samples 2, 4), Row 2 = M (samples 1, 3, 5)
  expect_equal(as.vector(result$design_matrix[1, ]), c(0, 1, 0, 1, 0)) # F
  expect_equal(as.vector(result$design_matrix[2, ]), c(1, 0, 1, 0, 1)) # M

  # Targets should be reordered to match: F=0.51, M=0.49
  expect_equal(unname(result$losses[[1]]$target), c(0.51, 0.49))
  expect_equal(names(result$losses[[1]]$target), c("F", "M"))
})

test_that("targets are correctly ordered for multi-level factors", {
  data <- data.frame(
    age = factor(c("55+", "18-34", "35-54", "18-34", "55+"))
  )

  formula_spec <- list(
    formula = ~age,
    terms = list(list(
      type = "exact",
      variables = "age",
      interaction = NULL
    ))
  )

  # Pop_data order: 18-34, 35-54, 55+ (happens to match alphabetical for these)
  # But let's use a different order to test
  target_values <- list(
    targets = list(
      exact_age = c("55+" = 0.37, "18-34" = 0.28, "35-54" = 0.35)
    )
  )

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Alphabetical order: 18-34, 35-54, 55+
  expect_equal(names(result$losses[[1]]$target), c("18-34", "35-54", "55+"))
  expect_equal(unname(result$losses[[1]]$target), c(0.28, 0.35, 0.37))
})

test_that("interaction targets are correctly ordered", {
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F")),
    region = factor(c("S", "N", "N", "S"))
  )

  formula_spec <- list(
    formula = ~ sex:region,
    terms = list(list(
      type = "exact",
      variables = c("sex", "region"),
      interaction = TRUE
    ))
  )

  # Pop_data order: M:N, M:S, F:N, F:S (non-alphabetical)
  target_values <- list(
    targets = list(
      "exact_sex:region" = c("M:N" = 0.20, "M:S" = 0.30, "F:N" = 0.25, "F:S" = 0.25)
    )
  )

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # R orders interactions by iterating through second var levels, then first var levels
  # So: region N (sex F, M), region S (sex F, M) → F:N, M:N, F:S, M:S
  expect_equal(names(result$losses[[1]]$target), c("F:N", "M:N", "F:S", "M:S"))
  expect_equal(unname(result$losses[[1]]$target), c(0.25, 0.20, 0.25, 0.30))
})

test_that("end-to-end raking applies targets to correct levels", {
  # This is the key test: verify that the achieved proportions match

  # the INTENDED targets, not swapped targets
  set.seed(123)
  n <- 500

  # Create sample with known imbalance
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  # Target: M should be 40%, F should be 60% (opposite of sample)
  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),  # M listed first
    proportion = c(0.40, 0.60)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions"
  )

  # Verify achieved proportions match INTENDED targets
  w <- result$weights
  achieved_M <- sum(w[sample_data$sex == "M"]) / sum(w)
  achieved_F <- sum(w[sample_data$sex == "F"]) / sum(w)

  # M should be ~40% (down from ~70%), F should be ~60% (up from ~30%)
  expect_equal(achieved_M, 0.40, tolerance = 1e-4)
  expect_equal(achieved_F, 0.60, tolerance = 1e-4)

  # Also verify via balance table
  balance_M <- result$balance$achieved[result$balance$level == "M"]
  balance_F <- result$balance$achieved[result$balance$level == "F"]
  expect_equal(balance_M, 0.40, tolerance = 1e-4)
  expect_equal(balance_F, 0.60, tolerance = 1e-4)
})

test_that("end-to-end raking with interactions applies targets correctly", {
  set.seed(456)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    region = sample(c("N", "S"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  # Targets with specific joint distribution
  pop_data <- data.frame(
    variable = c("sex", "sex", "region", "region", rep("sex:region", 4)),
    level = c("M", "F", "N", "S", "M:N", "M:S", "F:N", "F:S"),
    target = c(0.5, 0.5, 0.5, 0.5, 0.20, 0.30, 0.30, 0.20)
  )

  suppressWarnings({
    result <- regrake(
      data = sample_data,
      formula = ~ sex + region + sex:region,
      population_data = pop_data,
      pop_type = "proportions"
    )
  })

  w <- result$weights

  # Verify joint distribution targets are hit correctly
  achieved_MN <- sum(w[sample_data$sex == "M" & sample_data$region == "N"]) / sum(w)
  achieved_MS <- sum(w[sample_data$sex == "M" & sample_data$region == "S"]) / sum(w)
  achieved_FN <- sum(w[sample_data$sex == "F" & sample_data$region == "N"]) / sum(w)
  achieved_FS <- sum(w[sample_data$sex == "F" & sample_data$region == "S"]) / sum(w)

  expect_equal(achieved_MN, 0.20, tolerance = 1e-3)
  expect_equal(achieved_MS, 0.30, tolerance = 1e-3)
  expect_equal(achieved_FN, 0.30, tolerance = 1e-3)
  expect_equal(achieved_FS, 0.20, tolerance = 1e-3)
})

# =============================================================================
# Tests for NA handling
# =============================================================================

test_that("construct_admm_inputs errors when data contains NAs", {
  data <- data.frame(
    sex = factor(c("M", "F", NA, "M")),
    age = c(25, 35, 45, 30)
  )

  formula_spec <- list(
    formula = ~ sex,
    terms = list(list(
      type = "exact",
      variables = "sex",
      interaction = NULL
    ))
  )

  target_values <- list(
    targets = list(exact_sex = c(M = 0.5, F = 0.5))
  )

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "row.*contain missing values"
  )
})

test_that("construct_admm_inputs error message identifies NA variables", {
  data <- data.frame(
    sex = factor(c("M", "F", "M")),
    age = c(25, NA, 45)
  )

  formula_spec <- list(
    formula = ~ sex + age,
    terms = list(
      list(type = "exact", variables = "sex", interaction = NULL),
      list(type = "exact", variables = "age", interaction = NULL)
    )
  )

  target_values <- list(
    targets = list(
      exact_sex = c(M = 0.6, F = 0.4),
      exact_age = c(mean = 35)
    )
  )

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "age"
  )
})

test_that("construct_admm_inputs works with complete data", {
  data <- data.frame(
    sex = factor(c("M", "F", "M")),
    age = c(25, 35, 45)
  )

  formula_spec <- list(
    formula = ~ sex,
    terms = list(list(
      type = "exact",
      variables = "sex",
      interaction = NULL
    ))
  )

  target_values <- list(
    targets = list(exact_sex = c(M = 0.6, F = 0.4))
  )

  # Should not error
  result <- construct_admm_inputs(data, formula_spec, target_values)
  expect_true("design_matrix" %in% names(result))
})

# =============================================================================
# Tests for data/target level validation
# =============================================================================

test_that("construct_admm_inputs errors when data level has no target", {
  data <- data.frame(
    region = factor(c("N", "S", "E", "W")) # E and W exist in data
  )

  formula_spec <- list(
    formula = ~ region,
    terms = list(list(
      type = "exact",
      variables = "region",
      interaction = NULL
    ))
  )

  # Only N and S have targets
  target_values <- list(
    targets = list(exact_region = c(N = 0.5, S = 0.5))
  )

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "Data contains level.*that have no targets.*E.*W"
  )
})

test_that("construct_admm_inputs errors when hard-constraint target level not in data", {
  data <- data.frame(
    region = factor(c("N", "N", "S", "S")) # Only N and S
  )

  formula_spec <- list(
    formula = ~ region,
    terms = list(list(
      type = "exact",
      variables = "region",
      interaction = NULL
    ))
  )

  # Targets include E which doesn't exist in data
  target_values <- list(
    targets = list(exact_region = c(N = 0.4, S = 0.4, E = 0.2))
  )

  expect_error(
    {
      construct_admm_inputs(data, formula_spec, target_values)
    },
    "Infeasible hard constraint.*not present in data.*E"
  )
})

test_that("construct_admm_inputs still warns for soft-constraint unused targets", {
  data <- data.frame(
    region = factor(c("N", "N", "S", "S")) # Only N and S
  )

  formula_spec <- list(
    formula = ~ region,
    terms = list(list(
      type = "l2",
      variables = "region",
      interaction = NULL
    ))
  )

  target_values <- list(
    targets = list(l2_region = c(N = 0.4, S = 0.4, E = 0.2))
  )

  expect_warning(
    construct_admm_inputs(data, formula_spec, target_values),
    "levels not present in data.*E"
  )
})

test_that("construct_admm_inputs validates interaction levels", {
  data <- data.frame(
    sex = factor(c("M", "F")),
    region = factor(c("N", "S"))
  )

  formula_spec <- list(
    formula = ~ sex:region,
    terms = list(list(
      type = "exact",
      variables = c("sex", "region"),
      interaction = TRUE
    ))
  )

  # Missing M:S and F:N combinations in targets
  target_values <- list(
    targets = list("exact_sex:region" = c("M:N" = 0.5, "F:S" = 0.5))
  )

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "that have no targets"
  )
})

# =============================================================================
# Tests for range (inequality) constraints
# =============================================================================

test_that("construct_admm_inputs handles range constraint for continuous variable (margin mode)", {
  data <- data.frame(income = c(50000, 75000, 45000, 80000, 55000))

  formula_spec <- list(
    formula = ~income,
    terms = list(list(
      type = "range",
      variables = "income",
      interaction = NULL,
      params = list(mode = "margin", margin = 5000)
    ))
  )

  target_values <- list(targets = list(range_income = c(mean = 60000)))

  # With normalization (default)
  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Verify loss uses inequality prox
  expect_equal(result$losses[[1]]$fn, inequality_loss)
  expect_equal(result$losses[[1]]$prox, prox_inequality)

  # Verify lower/upper bounds are set (scaled by target)
  # Original: lower = -5000, upper = 5000
  # Normalized: lower = -5000/60000, upper = 5000/60000
  expect_equal(unname(result$losses[[1]]$lower), -5000 / 60000, tolerance = 1e-10)
  expect_equal(unname(result$losses[[1]]$upper), 5000 / 60000, tolerance = 1e-10)

  # Target should be normalized to 1.0
  expect_equal(unname(result$losses[[1]]$target), 1.0)
})

test_that("construct_admm_inputs errors on infeasible hard continuous exact target", {
  data <- data.frame(income = c(50000, 60000, 70000))

  formula_spec <- list(
    formula = ~income,
    terms = list(list(
      type = "exact",
      variables = "income",
      interaction = NULL
    ))
  )

  target_values <- list(targets = list(exact_income = c(mean = 100000)))

  expect_error(
    {
      construct_admm_inputs(data, formula_spec, target_values)
    },
    "outside achievable range"
  )
})

test_that("construct_admm_inputs errors on infeasible hard continuous range target", {
  data <- data.frame(income = c(50000, 60000, 70000))

  formula_spec <- list(
    formula = ~income,
    terms = list(list(
      type = "range",
      variables = "income",
      interaction = NULL,
      params = list(mode = "margin", margin = 1000)
    ))
  )

  target_values <- list(targets = list(range_income = c(mean = 100000)))

  expect_error(
    {
      construct_admm_inputs(data, formula_spec, target_values)
    },
    "does not overlap achievable range"
  )
})

test_that("construct_admm_inputs errors on infeasible quantile target threshold", {
  data <- data.frame(income = c(50000, 60000, 70000))

  formula_spec <- list(
    formula = ~income,
    terms = list(list(
      type = "quantile",
      variables = "income",
      interaction = NULL,
      params = list(p = 0.5)
    ))
  )

  # Threshold below min(income) => indicator is always 0, so p=0.5 is infeasible.
  target_values <- list(targets = list(quantile_income = c(q50 = 10000)))

  expect_error(
    {
      construct_admm_inputs(data, formula_spec, target_values)
    },
    "outside achievable range"
  )
})

test_that("construct_admm_inputs handles range constraint for continuous variable (bounds mode)", {
  data <- data.frame(age = c(30, 35, 40, 45, 50))

  formula_spec <- list(
    formula = ~age,
    terms = list(list(
      type = "range",
      variables = "age",
      interaction = NULL,
      params = list(mode = "bounds", lower = 35, upper = 45)
    ))
  )

  # Target from population data
  target_values <- list(targets = list(range_age = c(mean = 40)))

  result <- construct_admm_inputs(data, formula_spec, target_values, normalize = FALSE)

  # Verify bounds are computed as offsets from target
  # lower = 35 - 40 = -5, upper = 45 - 40 = 5
  expect_equal(unname(result$losses[[1]]$lower), -5)
  expect_equal(unname(result$losses[[1]]$upper), 5)
  expect_equal(unname(result$losses[[1]]$target), 40)
})

test_that("construct_admm_inputs handles range constraint for categorical variable (margin mode)", {
  data <- data.frame(sex = factor(c("M", "F", "M", "F", "M")))

  formula_spec <- list(
    formula = ~sex,
    terms = list(list(
      type = "range",
      variables = "sex",
      interaction = NULL,
      params = list(mode = "margin", margin = 0.05)
    ))
  )

  target_values <- list(targets = list(range_sex = c(F = 0.51, M = 0.49)))

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Verify loss uses inequality prox
  expect_equal(result$losses[[1]]$fn, inequality_loss)
  expect_equal(result$losses[[1]]$prox, prox_inequality)

  # Verify lower/upper bounds (same for all levels with single margin)
  # Targets are reordered to F, M (alphabetical)
  expect_equal(result$losses[[1]]$lower, c(-0.05, -0.05))
  expect_equal(result$losses[[1]]$upper, c(0.05, 0.05))
  expect_equal(unname(result$losses[[1]]$target), c(0.51, 0.49))
})

test_that("construct_admm_inputs handles range constraint with named vector margin", {
  data <- data.frame(sex = factor(c("M", "F", "M", "F", "M")))

  formula_spec <- list(
    formula = ~sex,
    terms = list(list(
      type = "range",
      variables = "sex",
      interaction = NULL,
      params = list(mode = "margin", margin = c(F = 0.02, M = 0.03))
    ))
  )

  target_values <- list(targets = list(range_sex = c(F = 0.51, M = 0.49)))

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Verify level-specific bounds (reordered to F, M)
  expect_equal(unname(result$losses[[1]]$lower), c(-0.02, -0.03))
  expect_equal(unname(result$losses[[1]]$upper), c(0.02, 0.03))
})

test_that("construct_admm_inputs handles range constraint for interaction", {
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F")),
    region = factor(c("N", "S", "S", "N"))
  )

  formula_spec <- list(
    formula = ~ sex:region,
    terms = list(list(
      type = "range",
      variables = c("sex", "region"),
      interaction = TRUE,
      params = list(mode = "margin", margin = 0.02)
    ))
  )

  target_values <- list(
    targets = list("range_sex:region" = c(
      "F:N" = 0.25, "F:S" = 0.25,
      "M:N" = 0.25, "M:S" = 0.25
    ))
  )

  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Verify loss uses inequality prox
  expect_equal(result$losses[[1]]$fn, inequality_loss)
  expect_equal(result$losses[[1]]$prox, prox_inequality)

  # Verify bounds are set for all 4 interaction levels
  expect_length(result$losses[[1]]$lower, 4)
  expect_length(result$losses[[1]]$upper, 4)
  expect_true(all(result$losses[[1]]$lower == -0.02))
  expect_true(all(result$losses[[1]]$upper == 0.02))
})

test_that("construct_admm_inputs errors on named vector margin with missing levels", {
  data <- data.frame(region = factor(c("N", "S", "E")))

  formula_spec <- list(
    formula = ~region,
    terms = list(list(
      type = "range",
      variables = "region",
      interaction = NULL,
      params = list(mode = "margin", margin = c(N = 0.02, S = 0.02))
    ))
  )

  target_values <- list(targets = list(range_region = c(N = 0.4, S = 0.4, E = 0.2)))

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "Named margin vector missing levels"
  )
})
