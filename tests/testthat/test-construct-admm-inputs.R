# Test construct_admm_inputs -------------------------------------------------------

test_that("construct_admm_inputs handles basic case with single exact term", {
  # Setup test data
  data <- data.frame(
    x = factor(c("a", "b", "a", "b", "a")),
    y = factor(c("1", "1", "2", "2", "1"))
  )

  # Create formula spec (simulating output from parse_raking_formula)
  formula_spec <- list(
    formula = ~ x,
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
  expect_equal(dim(result$design_matrix), c(2, 5))  # 2 levels, 5 samples
  expect_equal(as.vector(result$design_matrix[1,]), c(1,0,1,0,1))  # a's
  expect_equal(as.vector(result$design_matrix[2,]), c(0,1,0,1,0))  # b's

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
    formula = ~ x,
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
  expect_equal(Matrix::colSums(result$design_matrix), rep(1, 5))  # Each sample in exactly one category
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
    formula = ~ x,
    terms = list(
      list(
        type = "exact",
        variables = "x",
        interaction = NULL
      )
    )
  )

  target_values <- list(targets = list())  # Empty targets

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
    formula = ~ x,
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
    formula = ~ x,
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
    formula = ~ rr_exact(sex) + rr_l2(age),  # The wrapped formula
    terms = list(
      list(
        type = "exact",
        variables = "sex",  # Variables are unwrapped
        interaction = NULL
      ),
      list(
        type = "l2",
        variables = "age",  # Variables are unwrapped
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
    income = c(50000, 75000, 45000, 80000, 55000)  # Continuous
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
      exact_income = c(mean = 60000)  # Target mean income
    )
  )

  # Use normalize = FALSE to test raw values in design matrix
  result <- construct_admm_inputs(data, formula_spec, target_values, normalize = FALSE)

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
  expect_equal(as.vector(result$design_matrix[1, ]), c(0, 1, 0, 1, 0))  # F
  expect_equal(as.vector(result$design_matrix[2, ]), c(1, 0, 1, 0, 1))  # M
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
  result <- construct_admm_inputs(data, formula_spec, target_values, normalize = FALSE)

  # Design matrix should have 2 rows (one per continuous variable)
  expect_equal(dim(result$design_matrix), c(2, 5))

  # Row 1 (age) should contain actual ages
  expect_equal(as.vector(result$design_matrix[1, ]), c(25, 35, 45, 30, 40))

  # Row 2 (income) should contain actual incomes
  expect_equal(as.vector(result$design_matrix[2, ]), c(50000, 75000, 45000, 80000, 55000))
})

test_that("construct_admm_inputs normalizes continuous variables by default", {
  # Test that normalize = TRUE (default) scales continuous values by target
  data <- data.frame(
    income = c(50000, 75000, 45000, 80000, 55000)
  )

  formula_spec <- list(
    formula = ~ income,
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
    age = c(25, 35, 45)  # Continuous
  )

  formula_spec <- list(
    formula = ~ sex:age,
    terms = list(list(
      type = "exact",
      variables = c("sex", "age"),
      interaction = TRUE
    ))
  )

  target_values <- list(targets = list("exact_sex:age" = c(0.25, 0.25, 0.25, 0.25)))

  expect_error(
    construct_admm_inputs(data, formula_spec, target_values),
    "Interactions with continuous variables are not supported"
  )
})

test_that("construct_admm_inputs handles variance constraint", {
  data <- data.frame(age = c(20, 30, 40, 50, 60))
  # Mean = 40, Variance = sum((x - 40)^2) / 5 = (400+100+0+100+400)/5 = 200

  formula_spec <- list(
    formula = ~ age,
    terms = list(list(
      type = "var",
      variables = "age",
      interaction = NULL
    ))
  )

  # Target variance of 250
  target_values <- list(targets = list(var_age = c(var = 250)))

  # Without normalization to check raw values
  result <- construct_admm_inputs(data, formula_spec, target_values, normalize = FALSE)

  # Verify structure
  expect_type(result, "list")
  expect_true("design_matrix" %in% names(result))
  expect_true("losses" %in% names(result))

  # Design matrix row should be (x - mean(x))^2
  mean_age <- mean(data$age)  # 40
  expected <- (data$age - mean_age)^2  # c(400, 100, 0, 100, 400)
  expect_equal(as.vector(result$design_matrix[1, ]), expected)

  # Loss function should use equality (exact match on variance)
  expect_equal(result$losses[[1]]$fn, equality_loss)
  expect_equal(result$losses[[1]]$prox, prox_equality)
  expect_equal(unname(result$losses[[1]]$target), 250)
})

test_that("construct_admm_inputs normalizes variance constraint", {
  data <- data.frame(age = c(20, 30, 40, 50, 60))
  # Mean = 40, (x - mean)^2 = c(400, 100, 0, 100, 400)

  formula_spec <- list(
    formula = ~ age,
    terms = list(list(
      type = "var",
      variables = "age",
      interaction = NULL
    ))
  )

  target_values <- list(targets = list(var_age = c(var = 200)))

  # With normalization (default)
  result <- construct_admm_inputs(data, formula_spec, target_values)

  # Values should be scaled by target
  mean_age <- mean(data$age)
  raw_values <- (data$age - mean_age)^2
  expected_normalized <- raw_values / 200
  expect_equal(as.vector(result$design_matrix[1, ]), expected_normalized)

  # Target should be 1.0 (normalized)
  expect_equal(unname(result$losses[[1]]$target), 1.0)

  # Original target should be preserved
  expect_equal(unname(result$losses[[1]]$original_target), 200)
})