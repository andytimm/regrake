# This file uses a dummy regularizer to test the ADMM solver; this was helpful
# in debugging the solver early on, but I'm leaving it around because it's a good way
# to test components 1 by 1.

test_that("dummy regularizer identity works", {
  # Create a dummy reg that does nothing. Notice the prox takes exactly 2 parameters.
  dummy_reg <- list(
    fn = function(x) x,
    prox = function(w, lam) {
      w
    }
  )

  # Check that calling dummy_reg$prox returns its first argument.
  x <- rnorm(10)
  lam <- 1
  expect_equal(dummy_reg$prox(x, lam), x)
})

test_that("ADMM converges with dummy regularizer", {
  # Create a dummy reg that does nothing. Notice the prox takes exactly 2 parameters.
  dummy_reg <- list(
    fn = function(x) x,
    prox = function(w, lam) {
      w
    }
  )

  # Check that calling dummy_reg$prox returns its first argument.
  x <- rnorm(10)
  lam <- 1
  expect_equal(dummy_reg$prox(x, lam), x)

  # A simple problem to test ADMM
  n <- 20
  m <- 10
  set.seed(123)
  F <- matrix(rnorm(m * n), nrow = m)

  # Create a single least squares loss block
  fdes <- rnorm(m)
  losses <- list(
    list(
      fn = least_squares_loss,
      target = fdes,
      prox = prox_least_squares # For a loss, this is OK; its interface is fixed in the solver f-update.
    )
  )

  # Use the dummy_reg instead of one of your more sophisticated regularizers.
  sol <- admm(F, losses, dummy_reg, lam = 1, verbose = FALSE)

  # Check convergence criteria using compute_norms_and_epsilons.
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

  # Also check that the weight vector is a probability distribution.
  expect_true(all(sol$w_best >= 0))
  expect_equal(sum(sol$w_best), 1, tolerance = 1e-6)
})
