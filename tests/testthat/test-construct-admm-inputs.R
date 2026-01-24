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
  expect_named(result, c("design_matrix", "losses"))

  # Check design matrix
  expect_true(inherits(result$design_matrix, "Matrix"))
  expect_equal(dim(result$design_matrix), c(2, 5))  # 2 levels, 5 samples
  expect_equal(as.vector(result$design_matrix[1,]), c(1,0,1,0,1))  # a's
  expect_equal(as.vector(result$design_matrix[2,]), c(0,1,0,1,0))  # b's

  # Check losses
  expect_length(result$losses, 1)
  expect_named(result$losses[[1]], c("fn", "target", "prox"))
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
  # This test covers the bug where ~ exact(sex) + l2(age) would fail
  # because model.frame with the original formula created columns named

  # "exact(sex)" and "l2(age)" instead of "sex" and "age"
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F", "M")),
    age = factor(c("young", "old", "young", "old", "young"))
  )

  # Simulate what parse_raking_formula returns for ~ exact(sex) + l2(age)
  formula_spec <- list(
    formula = ~ exact(sex) + l2(age),  # The wrapped formula
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
  expect_named(result, c("design_matrix", "losses"))

  # Verify loss functions are correct for each term
  expect_equal(result$losses[[1]]$fn, equality_loss)
  expect_equal(result$losses[[2]]$fn, least_squares_loss)

  # Verify design matrix dimensions (2 sex levels + 2 age levels = 4 rows, 5 samples)
  expect_equal(dim(result$design_matrix), c(4, 5))
})