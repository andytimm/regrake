# Test projection_simplex --------------------------------------------------------

test_that("projection_simplex maintains sum constraint", {
  set.seed(605)
  n <- 1000
  v <- rnorm(n)
  z <- 1.5  # test non-unit sum constraint

  result <- projection_simplex(v, z)

  expect_equal(sum(result), z, tolerance = 1e-6)
  expect_true(all(result >= 0))
  expect_equal(length(result), length(v))
})

test_that("projection_simplex handles edge cases", {
  # All negative values - should put weight on least negative
  v1 <- c(-1, -2, -3)
  expect_equal(projection_simplex(v1), c(1, 0, 0))

  # Already a probability vector
  v2 <- c(0.3, 0.5, 0.2)
  expect_equal(projection_simplex(v2), v2)

  # Single very large value
  v3 <- c(100, 0.1, 0.1)
  result3 <- projection_simplex(v3)
  expect_true(result3[1] > result3[2])
  expect_equal(sum(result3), 1)
})

# Test compute_norms_and_epsilons ------------------------------------------------

test_that("compute_norms_and_epsilons basic functionality", {
  # Setup test data
  n <- 5  # number of samples
  m <- 3  # number of constraints
  set.seed(42)

  # Create test inputs
  F <- matrix(rnorm(m * n), nrow = m)
  w <- runif(n)
  w <- w / sum(w)  # normalize to sum to 1
  w_old <- runif(n)
  w_old <- w_old / sum(w_old)
  f <- rnorm(m)
  y <- rnorm(m)
  z <- rnorm(n)
  u <- rnorm(n)
  rho <- 50
  eps_abs <- 1e-5
  eps_rel <- 1e-5

  # Compute norms and epsilons
  result <- compute_norms_and_epsilons(f, w, w_old, y, z, u, F, rho, eps_abs, eps_rel)

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("s_norm", "r_norm", "eps_pri", "eps_dual"))

  # Test values are non-negative
  expect_true(result$s_norm >= 0)
  expect_true(result$r_norm >= 0)
  expect_true(result$eps_pri >= 0)
  expect_true(result$eps_dual >= 0)

  # Test manual calculations
  Fw <- drop(as.matrix(F %*% w))
  # Residuals exactly as in Python
  s <- rho * c(Fw - f, w - w_old, w - w_old)
  r <- c(f - Fw, w - w, w - w)
  expect_equal(result$s_norm, sqrt(sum(s^2)))
  expect_equal(result$r_norm, sqrt(sum(r^2)))

  # Test epsilon calculations
  p <- nrow(F) + 2 * length(w)
  Ax_k_norm <- sqrt(sum(c(Fw, w, w)^2))
  Bz_k_norm <- sqrt(sum(c(f, w, w)^2))
  ATy_k_norm <- sqrt(sum((rho * c(y, z, u))^2))
  expect_equal(result$eps_pri,
              sqrt(p) * eps_abs + eps_rel * max(Ax_k_norm, Bz_k_norm))
  expect_equal(result$eps_dual,
              sqrt(p) * eps_abs + eps_rel * ATy_k_norm)
})

test_that("compute_norms_and_epsilons handles edge cases", {
  n <- 3
  m <- 2

  # Test with zero vectors
  zero_result <- compute_norms_and_epsilons(
    f = rep(0, m),
    w = rep(0, n),
    w_old = rep(0, n),
    y = rep(0, m),
    z = rep(0, n),
    u = rep(0, n),
    F = matrix(0, nrow = m, ncol = n),
    rho = 1,
    eps_abs = 1e-5,
    eps_rel = 1e-5
  )

  expect_equal(zero_result$s_norm, 0)
  expect_equal(zero_result$r_norm, 0)
  expect_true(zero_result$eps_pri > 0)  # Should still be positive due to eps_abs
  expect_true(zero_result$eps_dual > 0)

  # Test with sparse matrix input
  sparse_F <- Matrix::Matrix(matrix(c(1,0,0,1), nrow=2), sparse=TRUE)
  sparse_result <- compute_norms_and_epsilons(
    f = c(1,1),
    w = c(1,1),
    w_old = c(0,0),
    y = c(0,0),
    z = c(0,0),
    u = c(0,0),
    F = sparse_F,
    rho = 1,
    eps_abs = 1e-5,
    eps_rel = 1e-5
  )

  expect_type(sparse_result, "list")
  expect_true(all(sapply(sparse_result, is.numeric)))
})

test_that("compute_norms_and_epsilons is invariant to scaling (F fixed)", {
  n <- 4
  m <- 3
  set.seed(42)

  # Base F and vectors
  F <- matrix(rnorm(m * n), nrow = m)
  w <- runif(n)
  w_old <- runif(n)
  f <- rnorm(m)
  y <- rnorm(m)
  z <- rnorm(n)
  u <- rnorm(n)

  eps_abs_val <- 1e-5
  eps_rel_val <- 1e-5

  base_result <- compute_norms_and_epsilons(
    f, w, w_old, y, z, u, F, rho = 1,
    eps_abs = eps_abs_val, eps_rel = eps_rel_val
  )

  # Scale only the vectors by alpha (F is fixed)
  alpha <- 2
  scaled_result <- compute_norms_and_epsilons(
    alpha * f, alpha * w, alpha * w_old,
    alpha * y, alpha * z, alpha * u,
    F, rho = 1,
    eps_abs = eps_abs_val, eps_rel = eps_rel_val
  )

  p <- nrow(F) + 2 * length(w)

  # For eps thresholds, only the relative term scales:
  #   base: eps_pri = sqrt(p)*eps_abs + eps_rel * R
  #   scaled: eps_pri = sqrt(p)*eps_abs + eps_rel * (α * R)
  # So expected scaled_eps_pri = sqrt(p)*eps_abs + α*(base_eps_pri - sqrt(p)*eps_abs)
  expected_eps_pri <- sqrt(p) * eps_abs_val + alpha * (base_result$eps_pri - sqrt(p) * eps_abs_val)
  expected_eps_dual <- sqrt(p) * eps_abs_val + alpha * (base_result$eps_dual - sqrt(p) * eps_abs_val)

  expect_equal(scaled_result$r_norm, alpha * base_result$r_norm, tolerance = 1e-10)
  expect_equal(scaled_result$s_norm, alpha * base_result$s_norm, tolerance = 1e-10)
  expect_equal(scaled_result$eps_pri, expected_eps_pri, tolerance = 1e-10)
  expect_equal(scaled_result$eps_dual, expected_eps_dual, tolerance = 1e-10)
})

# Test ADMM solver (placeholder) ------------------------------------------------

# TODO: Add full solver tests similar to test_solver.py, including:
# 1. Basic convergence test
# 2. Comparison with CVXR solution
# 3. Edge cases and numerical stability tests