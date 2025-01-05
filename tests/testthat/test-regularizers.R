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
  objective <- Minimize(-sum(entr(what)) + 1/(2 * lambda) * sum_squares(what - w))
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
  objective <- Minimize(-sum(entr(what)) - sum(what * log(prior)) +
                   1/(2 * lambda) * sum_squares(what - w))
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
  objective <- Minimize(sum_squares(what) + 1/(2 * lambda) * sum_squares(what - w))
  problem <- Problem(objective)
  solution <- CVXR::solve(problem)
  opt_what <- as.vector(solution$getValue(what))

  expect_equal(result, opt_what, tolerance = 1e-4)
})