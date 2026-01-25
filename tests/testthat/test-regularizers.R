library(testthat)

test_that("zero regularizer returns input unchanged", {
  set.seed(605)
  w <- rnorm(10)
  lambda <- 0.5

  result <- zero_regularizer(w, lambda)
  expect_equal(result, w)
})

test_that("entropy regularizer matches optimization solution", {
  set.seed(605)
  w <- rnorm(10)
  lambda <- 0.5

  # Direct calculation
  result <- entropy_regularizer(w, lambda)

  # Optimization solution using CVXR
  library(CVXR)
  what <- Variable(10)
  objective <- Minimize(
    -sum(entr(what)) + 1 / (2 * lambda) * sum_squares(what - w)
  )
  problem <- Problem(objective)
  solution <- CVXR::solve(problem)
  opt_what <- as.vector(solution$getValue(what))

  expect_equal(result, opt_what, tolerance = 1e-4)
})

test_that("kl regularizer matches optimization solution", {
  set.seed(605)
  w <- rnorm(10)
  prior <- runif(10)
  prior <- prior / sum(prior)
  lambda <- 0.5

  # Direct calculation
  result <- kl_regularizer(w, lambda, prior)

  # Optimization solution using CVXR
  library(CVXR)
  what <- Variable(10)
  objective <- Minimize(
    -sum(entr(what)) -
      sum(what * log(prior)) +
      1 / (2 * lambda) * sum_squares(what - w)
  )
  problem <- Problem(objective)
  solution <- CVXR::solve(problem)
  opt_what <- as.vector(solution$getValue(what))

  expect_equal(result, opt_what, tolerance = 1e-4)
})

test_that("sum squares regularizer matches optimization solution", {
  set.seed(605)
  w <- rnorm(10)
  lambda <- 0.5

  # Direct calculation
  result <- sum_squares_regularizer(w, lambda)

  # Optimization solution using CVXR
  library(CVXR)
  what <- Variable(10)
  objective <- Minimize(
    sum_squares(what) + 1 / (2 * lambda) * sum_squares(what - w)
  )
  problem <- Problem(objective)
  solution <- CVXR::solve(problem)
  opt_what <- as.vector(solution$getValue(what))

  expect_equal(result, opt_what, tolerance = 1e-4)
})

test_that("boolean regularizer selects top-k weights", {
  set.seed(605)
  w <- c(0.5, 0.1, 0.8, 0.3, 0.9, 0.2, 0.7, 0.4, 0.6, 0.05)
  k <- 3
  lambda <- 0.5 # Unused but kept for interface consistency

  # Direct calculation
  result <- prox_boolean_reg(w, lambda, k)

  # Check that exactly k weights are non-zero
  expect_equal(sum(result > 0), k)

  # Check that each non-zero weight equals 1/k
  expect_equal(unique(result[result > 0]), 1 / k)

  # Check that the top-k weights by original values are selected
  top_k_idx <- order(w, decreasing = TRUE)[1:k]
  expect_true(all(result[top_k_idx] == 1 / k))

  # Check that all other weights are zero
  other_idx <- setdiff(1:length(w), top_k_idx)
  expect_true(all(result[other_idx] == 0))

  # Check that result sums to 1
  expect_equal(sum(result), 1)
})

test_that("boolean regularizer handles edge cases", {
  # k = 1 selects only the maximum
  w <- c(0.1, 0.5, 0.3)
  result <- prox_boolean_reg(w, 0.5, k = 1)
  expect_equal(result, c(0, 1, 0))

  # k = n selects all with equal weight
  w <- c(0.1, 0.5, 0.3)
  result <- prox_boolean_reg(w, 0.5, k = 3)
  expect_equal(result, rep(1 / 3, 3))

  # Error on invalid k
  expect_error(prox_boolean_reg(w, 0.5, k = 0), "k must be between")
  expect_error(prox_boolean_reg(w, 0.5, k = 4), "k must be between")
})
