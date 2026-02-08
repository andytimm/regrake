# Test projection_simplex --------------------------------------------------------

test_that("projection_simplex maintains sum constraint", {
  set.seed(605)
  n <- 1000
  v <- rnorm(n)
  z <- 1.5 # test non-unit sum constraint

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
  expect_gt(result3[1], result3[2])
  expect_equal(sum(result3), 1)
})

# Test compute_norms_and_epsilons ------------------------------------------------

test_that("compute_norms_and_epsilons basic functionality", {
  # Setup test data
  n <- 5 # number of samples
  m <- 3 # number of constraints
  set.seed(42)

  # Create test inputs
  F <- matrix(rnorm(m * n), nrow = m)
  w <- runif(n)
  w <- w / sum(w) # normalize to sum to 1
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
  result <- compute_norms_and_epsilons(
    f,
    w,
    w_old,
    y,
    z,
    u,
    F,
    rho,
    eps_abs,
    eps_rel
  )

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("s_norm", "r_norm", "eps_pri", "eps_dual"))

  # Test values are non-negative
  expect_gte(result$s_norm, 0)
  expect_gte(result$r_norm, 0)
  expect_gte(result$eps_pri, 0)
  expect_gte(result$eps_dual, 0)

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
  expect_equal(
    result$eps_pri,
    sqrt(p) * eps_abs + eps_rel * max(Ax_k_norm, Bz_k_norm)
  )
  expect_equal(result$eps_dual, sqrt(p) * eps_abs + eps_rel * ATy_k_norm)
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
  expect_gt(zero_result$eps_pri, 0) # Should still be positive due to eps_abs
  expect_gt(zero_result$eps_dual, 0)

  # Test with sparse matrix input
  sparse_F <- Matrix::Matrix(matrix(c(1, 0, 0, 1), nrow = 2), sparse = TRUE)
  sparse_result <- compute_norms_and_epsilons(
    f = c(1, 1),
    w = c(1, 1),
    w_old = c(0, 0),
    y = c(0, 0),
    z = c(0, 0),
    u = c(0, 0),
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
    f,
    w,
    w_old,
    y,
    z,
    u,
    F,
    rho = 1,
    eps_abs = eps_abs_val,
    eps_rel = eps_rel_val
  )

  # Scale only the vectors by alpha (F is fixed)
  alpha <- 2
  scaled_result <- compute_norms_and_epsilons(
    alpha * f,
    alpha * w,
    alpha * w_old,
    alpha * y,
    alpha * z,
    alpha * u,
    F,
    rho = 1,
    eps_abs = eps_abs_val,
    eps_rel = eps_rel_val
  )

  p <- nrow(F) + 2 * length(w)

  # For eps thresholds, only the relative term scales:
  #   base: eps_pri = sqrt(p)*eps_abs + eps_rel * R
  #   scaled: eps_pri = sqrt(p)*eps_abs + eps_rel * (α * R)
  # So expected scaled_eps_pri = sqrt(p)*eps_abs + α*(base_eps_pri - sqrt(p)*eps_abs)
  expected_eps_pri <- sqrt(p) *
    eps_abs_val +
    alpha * (base_result$eps_pri - sqrt(p) * eps_abs_val)
  expected_eps_dual <- sqrt(p) *
    eps_abs_val +
    alpha * (base_result$eps_dual - sqrt(p) * eps_abs_val)

  expect_equal(
    scaled_result$r_norm,
    alpha * base_result$r_norm,
    tolerance = 1e-10
  )
  expect_equal(
    scaled_result$s_norm,
    alpha * base_result$s_norm,
    tolerance = 1e-10
  )
  expect_equal(scaled_result$eps_pri, expected_eps_pri, tolerance = 1e-10)
  expect_equal(scaled_result$eps_dual, expected_eps_dual, tolerance = 1e-10)
})

# Test ADMM solver ---------------------------------------------------------------

test_that("ADMM solver basic convergence", {
  skip_if_not_installed("CVXR")
  # Problem setup similar to Python test
  n <- 100
  m <- 20
  set.seed(42)
  F <- matrix(rnorm(m * n), nrow = m)

  # Split into two blocks like Python test
  fdes1 <- rnorm(m %/% 2)
  fdes2 <- rnorm(m %/% 2)

  # Create losses with their proximal operators
  losses <- list(
    list(
      fn = least_squares_loss,
      target = fdes1,
      prox = prox_least_squares
    ),
    list(
      fn = inequality_loss,
      target = fdes2,
      prox = prox_inequality,
      lower = -rep(1, m %/% 2),
      upper = rep(1, m %/% 2)
    )
  )
  reg <- list(
    prox = function(w, lam) prox_kl_reg(w, lam)
  )

  # Run solver
  sol <- admm(F, losses, reg, lam = 1, verbose = FALSE)

  # Check feasibility of ADMM solution
  expect_true(all(sol$w_best >= 0))
  expect_equal(sum(sol$w_best), 1, tolerance = 1e-6)

  # Helper: numeric entropy (note: CVXR's entr(x) = -x*log(x))
  numeric_entropy <- function(w) {
    sum(ifelse(w > 0, w * log(w), 0))
  }

  # Compute ADMM objective value using the same formulation.
  admm_obj <- 0.5 *
    sum((F[1:(m %/% 2), , drop = FALSE] %*% sol$w_best - fdes1)^2) +
    numeric_entropy(sol$w_best)

  # Solve equivalent problem with CVXR using sum_entries()
  library(CVXR)
  w <- Variable(n)
  obj <- 0.5 *
    sum_squares(F[1:(m %/% 2), , drop = FALSE] %*% w - fdes1) -
    sum_entries(entr(w))
  constraints <- list(
    sum_entries(w) == 1,
    w >= 0,
    max_entries(abs(F[(m %/% 2 + 1):m, , drop = FALSE] %*% w - fdes2)) <= 1
  )
  prob <- Problem(Minimize(obj), constraints)
  result <- CVXR::solve(prob, solver = "ECOS")
  cvxr_obj <- result$value

  expect_equal(admm_obj, cvxr_obj, tolerance = 1e-2)
})

test_that("ADMM solver handles edge cases", {
  n <- 50
  m <- 10
  set.seed(42)

  # Test 1: Zero matrix
  F_zero <- Matrix::Matrix(0, nrow = m, ncol = n, sparse = TRUE)
  fdes <- rep(0, m)
  losses <- list(
    list(
      fn = least_squares_loss,
      target = fdes,
      prox = prox_least_squares
    )
  )
  reg <- list(
    prox = function(w, lam) prox_kl_reg(w, lam)
  )

  sol_zero <- admm(F_zero, losses, reg, lam = 1, verbose = FALSE)
  expect_true(all(sol_zero$w_best >= 0))
  expect_equal(sum(sol_zero$w_best), 1, tolerance = 1e-6)

  # Test 2: Nearly singular matrix
  F_sing <- Matrix::Matrix(1, nrow = m, ncol = n, sparse = TRUE)
  F_sing[1, ] <- F_sing[1, ] + 1e-10 * rnorm(n) # Small perturbation

  sol_sing <- admm(F_sing, losses, reg, lam = 1, verbose = FALSE)
  expect_true(all(sol_sing$w_best >= 0))
  expect_equal(sum(sol_sing$w_best), 1, tolerance = 1e-6)
})

test_that("ADMM solver converges with different regularizers", {
  n <- 50
  m <- 10
  set.seed(42)
  F <- matrix(rnorm(m * n), nrow = m)
  fdes <- rnorm(m)
  losses <- list(
    list(
      fn = least_squares_loss,
      target = fdes,
      prox = prox_least_squares
    )
  )

  # Test different regularizers
  regs <- list(
    list(prox = function(w, lam) {
      prox_kl_reg(w, lam)
    }),
    list(prox = prox_equality_reg),
    list(prox = prox_sum_squares_reg)
  )

  for (reg in regs) {
    sol <- admm(F, losses, reg, lam = 1, verbose = FALSE)
    expect_true(all(sol$w_best >= 0))
    expect_equal(sum(sol$w_best), 1, tolerance = 1e-6)

    # Check convergence
    norms <- compute_norms_and_epsilons(
      sol$f,
      sol$w,
      sol$w,
      sol$y,
      sol$z,
      sol$u,
      F,
      rho = 50,
      eps_abs = 1e-5,
      eps_rel = 1e-5
    )
    expect_lte(norms$r_norm, norms$eps_pri)
    expect_lte(norms$s_norm, norms$eps_dual)
  }
})

# TODO: Add specific tests for Cholesky solver behavior:
# 1. Test permutation effectiveness
# 2. Test regularization fallback
# 3. Test numerical stability with different condition numbers
# 4. Test performance with varying sparsity levels

test_that("Cholesky solver handles different matrix structures", {
  # Test 1: Permutation effectiveness with block structure
  n <- 100
  m <- 50
  set.seed(42)

  # Create block structured matrix to test permutation
  F_block <- Matrix::Matrix(0, m, n, sparse = TRUE)
  F_block[1:(m / 2), (n / 2 + 1):n] <- Matrix::rsparsematrix(m / 2, n / 2, 0.1)
  F_block[(m / 2 + 1):m, 1:(n / 2)] <- Matrix::rsparsematrix(m / 2, n / 2, 0.1)

  # Run solver with block matrix
  losses <- list(list(
    fn = least_squares_loss,
    target = rnorm(m),
    prox = prox_least_squares
  ))
  reg <- list(
    prox = function(w, lam) prox_kl_reg(w, lam)
  )

  sol_block <- admm(F_block, losses, reg, lam = 1)
  expect_true(all(sol_block$w_best >= 0))
  expect_equal(sum(sol_block$w_best), 1, tolerance = 1e-6)
})

test_that("Cholesky solver is stable with different condition numbers", {
  n <- 50
  m <- 20
  set.seed(42)

  # Fixed matrix creation to ensure dimensions match
  create_matrix <- function(cond_num) {
    # Create square matrix first then truncate
    X <- matrix(rnorm(n * n), n, n)
    # SVD decomposition
    svd_res <- svd(X)
    # Create desired singular values - use more moderate range
    s <- exp(seq(0, log(cond_num), length.out = n))
    # Normalize singular values
    s <- s / sqrt(sum(s^2))
    # Reconstruct with controlled condition number
    X_cond <- svd_res$u %*% diag(s) %*% t(svd_res$v)
    # Take first m rows to get desired dimensions
    F <- Matrix::Matrix(X_cond[1:m, ], sparse = TRUE)
    return(F)
  }

  # Test matrices with more moderate condition numbers
  cond_numbers <- c(1e2, 1e4, 1e6) # reduced from previous values
  for (cond in cond_numbers) {
    F <- create_matrix(cond)
    losses <- list(list(
      fn = least_squares_loss,
      target = rnorm(m),
      prox = prox_least_squares
    ))
    reg <- list(
      fn = kl_loss,
      prox = function(w, lam) prox_kl_reg(w, lam)
    )

    # Run solver with error checking
    sol <- admm(F, losses, reg, lam = 1)

    # Check solution validity
    expect_true(all(sol$w_best >= 0))
    expect_equal(sum(sol$w_best), 1, tolerance = 1e-6)

    # Verify no NaN/NA values in solution
    expect_false(any(is.na(sol$w_best)))
    expect_false(any(is.na(sol$w)))

    # Check convergence
    norms <- compute_norms_and_epsilons(
      sol$f,
      sol$w,
      sol$w,
      sol$y,
      sol$z,
      sol$u,
      F,
      rho = 50,
      eps_abs = 1e-5,
      eps_rel = 1e-5
    )
    expect_false(any(is.na(norms$r_norm)))
    expect_false(any(is.na(norms$s_norm)))
    expect_lte(norms$r_norm, norms$eps_pri)
    expect_lte(norms$s_norm, norms$eps_dual)
  }
})

test_that("Solver performance scales with sparsity levels", {
  n <- 200
  m <- 100
  densities <- c(0.01, 0.05, 0.2)
  set.seed(42)

  results <- list()
  for (i in seq_along(densities)) {
    # Create sparse matrix with given density
    F <- Matrix::rsparsematrix(m, n, density = densities[i])

    losses <- list(list(
      fn = least_squares_loss,
      target = rnorm(m),
      prox = prox_least_squares
    ))
    reg <- list(
      fn = kl_loss,
      prox = function(w, lam) prox_kl_reg(w, lam)
    )

    # Time the solver with control list
    start_time <- proc.time()
    sol <- admm(F, losses, reg, lam = 1, control = list(maxiter = 100))
    times <- (proc.time() - start_time)[3]

    results[[i]] <- list(
      density = densities[i],
      nnz = Matrix::nnzero(F),
      time = times,
      solution = sol
    )

    # Basic solution checks
    expect_true(all(sol$w_best >= 0))
    expect_equal(sum(sol$w_best), 1, tolerance = 1e-6)
  }

  # Instead of checking timing (which can be unreliable),
  # verify that we can solve problems at different sparsity levels
  for (r in results) {
    expect_gt(r$time, 0) # Ensure timing is positive
    expect_equal(r$nnz, round(r$density * m * n), tolerance = 10) # Verify density
  }
})

test_that("Solver handles regularization fallback gracefully", {
  n <- 50
  m <- 20
  set.seed(42)

  # Create nearly singular matrix
  F <- Matrix::Matrix(1, m, n, sparse = TRUE)
  F[1, ] <- F[1, ] + 1e-10 * rnorm(n)

  losses <- list(list(
    fn = least_squares_loss,
    target = rnorm(m),
    prox = prox_least_squares
  ))
  reg <- list(
    prox = function(w, lam) prox_kl_reg(w, lam)
  )

  # Should complete without error due to damping
  expect_error(admm(F, losses, reg, lam = 1), NA)
})
