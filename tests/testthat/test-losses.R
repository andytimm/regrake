library(testthat)

test_that("equality loss matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  lambda <- 1

  # Direct loss calculation
  direct_loss <- sum(equality_loss(f, fdes))

  # Optimization solution using CVXR
  library(CVXR)
  x <- Variable(m)
  objective <- Minimize(sum(abs(x - fdes)))
  problem <- Problem(objective)
  result <- CVXR::solve(problem)
  opt_x <- as.vector(result$getValue(x))

  expect_equal(opt_x, fdes, tolerance = 1e-5)
  expect_equal(sum(equality_loss(opt_x, fdes)), 0, tolerance = 1e-5)
})

test_that("least squares loss matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  lambda <- 1

  # Direct loss calculation
  direct_loss <- sum(least_squares_loss(f, fdes))

  # Optimization solution using CVXR
  library(CVXR)
  x <- Variable(m)
  objective <- Minimize(sum((x - fdes)^2))  # Squared error for L2
  problem <- Problem(objective)
  result <- CVXR::solve(problem)
  opt_x <- as.vector(result$getValue(x))

  expect_equal(opt_x, fdes, tolerance = 1e-5)
  expect_equal(sum(least_squares_loss(opt_x, fdes)), 0, tolerance = 1e-5)
})

test_that("kl loss matches optimization solution", {
  set.seed(605)
  m <- 10
  # Use positive values for KL divergence
  f <- abs(rnorm(m)) + 0.1
  fdes <- abs(rnorm(m)) + 0.1
  lambda <- 1

  # Direct loss calculation
  direct_loss <- sum(kl_loss(f, fdes))

  # Optimization solution using CVXR
  library(CVXR)
  x <- Variable(m)
  # Rewrite objective to be DCP compliant
  objective <- Minimize(sum(-entr(x) - x * log(fdes) - x + fdes))
  constraints <- list(x >= 0)
  problem <- Problem(objective, constraints)
  result <- CVXR::solve(problem)
  opt_x <- as.vector(result$getValue(x))

  expect_equal(opt_x, fdes, tolerance = 1e-5)
  expect_equal(sum(kl_loss(opt_x, fdes)), 0, tolerance = 1e-5)
})

test_that("loss functions handle zero-length inputs", {
  x <- numeric(0)
  target <- numeric(0)

  expect_equal(length(equality_loss(x, target)), 0)
  expect_equal(length(least_squares_loss(x, target)), 0)
  expect_equal(length(kl_loss(x, target)), 0)
})

test_that("loss functions handle mismatched lengths", {
  x <- c(0.1, 0.2)
  target <- 0.1

  expect_error(equality_loss(x, target))
  expect_error(least_squares_loss(x, target))
  expect_error(kl_loss(x, target))
})

test_that("kl_loss handles edge cases correctly", {
  # Test non-positive values
  expect_equal(kl_loss(0, 1), Inf)
  expect_equal(kl_loss(1, 0), Inf)
  expect_equal(kl_loss(0, 0), Inf)

  # Test vectors with mixed valid/invalid values
  x <- c(0.1, 0, 0.3)
  target <- c(0.2, 0.1, 0)
  result <- kl_loss(x, target)

  expect_equal(length(result), 3)
  expect_true(is.infinite(result[2]))  # x = 0
  expect_true(is.infinite(result[3]))  # target = 0
  expect_false(is.infinite(result[1])) # both positive
})

test_that("loss functions handle NA/NaN values", {
  x <- c(0.1, NA, NaN)
  target <- c(0.2, 0.3, 0.4)

  expect_true(any(is.na(equality_loss(x, target))))
  expect_true(any(is.na(least_squares_loss(x, target))))
  expect_true(any(is.na(kl_loss(x, target))))
})

# Test proximal operators --------------------------------------------------------

test_that("equality prox matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  rho <- 1

  # Our prox
  prox_result <- prox_equality(f, fdes, rho)

  # CVXR solution
  library(CVXR)
  fhat <- CVXR::Variable(m)
  objective <- CVXR::Minimize((1/rho) * CVXR::sum_squares(fhat - f))
  constraints <- list(fhat == fdes)
  prob <- CVXR::Problem(objective, constraints)
  result <- CVXR::solve(prob)

  expect_equal(as.numeric(result$getValue(fhat)), prox_result, tolerance = 1e-5)
})

test_that("least squares prox matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  rho <- 1

  # Our prox
  prox_result <- prox_least_squares(f, fdes, rho)

  # CVXR solution
  library(CVXR)
  fhat <- CVXR::Variable(m)
  objective <- CVXR::Minimize(0.5 * CVXR::sum_squares(fhat - fdes) +
                       1/(2 * rho) * CVXR::sum_squares(fhat - f))
  prob <- CVXR::Problem(objective)
  result <- CVXR::solve(prob)

  expect_equal(as.numeric(result$getValue(fhat)), prox_result, tolerance = 1e-5)
})

test_that("inequality prox matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  rho <- 1
  lower <- rep(-0.3, m)
  upper <- rep(0.3, m)

  # Our prox
  prox_result <- prox_inequality(f, fdes, rho, lower, upper)

  # CVXR solution
  library(CVXR)
  fhat <- CVXR::Variable(m)
  objective <- CVXR::Minimize((1/rho) * CVXR::sum_squares(fhat - f))
  constraints <- list(lower <= fhat - fdes, fhat - fdes <= upper)
  prob <- CVXR::Problem(objective, constraints)
  result <- CVXR::solve(prob)

  expect_equal(as.numeric(result$getValue(fhat)), prox_result, tolerance = 1e-5)
})

test_that("kl prox matches optimization solution", {
  set.seed(605)
  m <- 10
  # Use positive values and normalize
  f <- abs(rnorm(m)) + 0.1
  f <- f/sum(f)
  fdes <- abs(rnorm(m)) + 0.1
  fdes <- fdes/sum(fdes)
  rho <- 1

  # Our prox
  prox_result <- prox_kl(f, fdes, rho)

  # CVXR solution
  library(CVXR)
  fhat <- CVXR::Variable(m, nonneg = TRUE)
  # Match Python test formulation
  objective <- CVXR::Minimize(0.5 * (-CVXR::sum_entries(CVXR::entr(fhat)) -
                                    CVXR::sum_entries(fhat * log(fdes))) +
                       1/(2 * rho) * CVXR::sum_squares(fhat - f))
  prob <- CVXR::Problem(objective)
  result <- CVXR::solve(prob, solver = "ECOS")

  expect_equal(as.numeric(result$getValue(fhat)), prox_result, tolerance = 1e-4)
})

test_that("proximal operators handle edge cases", {
  x <- numeric(0)
  target <- numeric(0)
  rho <- 1

  # Test zero-length inputs
  expect_equal(length(prox_equality(x, target, rho)), 0)
  expect_equal(length(prox_least_squares(x, target, rho)), 0)
  expect_equal(length(prox_kl(x, target, rho)), 0)

  # Test with NA/NaN
  x_na <- c(1, NA, NaN)
  target_na <- c(1, 2, 3)
  expect_true(any(is.na(prox_least_squares(x_na, target_na, rho))))
  expect_true(any(is.na(prox_kl(x_na, target_na, rho))))

  # Test KL with non-positive values
  expect_error(prox_kl(c(0, 1), c(1, 1), rho), class = "regrake_domain_error")
  expect_error(prox_kl(c(1, 1), c(0, 1), rho), class = "regrake_domain_error")
})

# Test weighted least squares --------------------------------------------------------

test_that("weighted least squares loss matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  diag_weight <- abs(rnorm(m)) + 0.1  # Positive weights

  # Direct loss calculation
  direct_loss <- sum(least_squares_loss(f, fdes, diag_weight))

  # Optimization solution using CVXR
  library(CVXR)
  x <- Variable(m)
  objective <- Minimize(sum((diag_weight * (x - fdes))^2))
  problem <- Problem(objective)
  result <- CVXR::solve(problem)
  opt_x <- as.vector(result$getValue(x))

  expect_equal(opt_x, fdes, tolerance = 1e-5)
  expect_equal(sum(least_squares_loss(opt_x, fdes, diag_weight)), 0, tolerance = 1e-5)
})

test_that("weighted least squares prox matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  tau <- 1  # lam in Python
  diag_weight <- abs(rnorm(m)) + 0.1  # Positive weights

  # Our prox with weighting
  prox_result <- prox_least_squares(f, fdes, tau, diag_weight)

  # CVXR solution: minimize sum((dw * (x - fdes))^2) + (1/tau) * sum((x - f)^2)
  library(CVXR)
  fhat <- CVXR::Variable(m)
  objective <- CVXR::Minimize(
    0.5 * CVXR::sum_squares(diag_weight * (fhat - fdes)) +
    1/(2 * tau) * CVXR::sum_squares(fhat - f)
  )
  prob <- CVXR::Problem(objective)
  result <- CVXR::solve(prob)

  expect_equal(as.numeric(result$getValue(fhat)), prox_result, tolerance = 1e-5)
})

test_that("weighted least squares with scalar weight equals uniform scaling", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  tau <- 1.5
  scalar_weight <- 2.5

  # Scalar weight applied uniformly
  result_scalar <- prox_least_squares(f, fdes, tau, scalar_weight)

  # Vector of same weights should give same result
  result_vector <- prox_least_squares(f, fdes, tau, rep(scalar_weight, m))

  expect_equal(result_scalar, result_vector)
})

test_that("weighted least squares with weight=1 equals unweighted", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  tau <- 1.5

  # Default (unweighted)
  result_default <- prox_least_squares(f, fdes, tau)

  # Explicit weight=1
  result_weighted <- prox_least_squares(f, fdes, tau, diag_weight = 1)

  expect_equal(result_default, result_weighted)
})
