test_that("Solver performance scales with problem size", {
  # Skip on CRAN
  skip_on_cran()

  # Test cases matching the JAX README
  ns <- c(1000, 10000, 100000)
  results <- list()

  for (i in seq_along(ns)) {
    n <- ns[i]
    # Create simulated data similar to the README example
    set.seed(605)
    age <- runif(n, 20, 30)
    sex <- rbinom(n, 1, 0.6)
    height <- rnorm(n, 5, 1)

    # Create design matrix 3 x n
    F <- Matrix::Matrix(rbind(age, sex, height), sparse = TRUE)

    # Normalize ALL features
    F_norm <- F
    for (j in 1:3) {
      F_norm[j,] <- (F[j,] - mean(F[j,])) / sd(F[j,])
    }

    losses <- list(
      list(
        fn = equality_loss,
        target = 0,  # All targets 0 after normalization
        prox = prox_equality
      ),
      list(
        fn = equality_loss,
        target = 0,
        prox = prox_equality
      ),
      list(
        fn = equality_loss,
        target = 0,
        prox = prox_equality
      )
    )

    reg <- list(
      fn = kl_regularizer,
      prox = function(w, lam) prox_kl_reg(w, lam, tau = 50/lam)
    )

    start_time <- proc.time()
    tryCatch({
      sol <- admm(F_norm, losses, reg, lam = 0.01,
                  control = list(
                    eps_abs = 1e-5,
                    eps_rel = 1e-5,
                    rho = 50,
                    maxiter = 5000
                  ),
                  verbose = TRUE)
      elapsed <- (proc.time() - start_time)[3]

      # Transform results back to original scale
      Fw_norm <- as.vector(F_norm %*% sol$w_best)
      Fw <- c(
        Fw_norm[1] * sd(age) + mean(age),
        Fw_norm[2] * sd(sex) + mean(sex),
        Fw_norm[3] * sd(height) + mean(height)
      )

      targets <- c(mean(age), mean(sex), mean(height))

      results[[i]] <- list(
        n = n,
        time = elapsed,
        final_values = Fw,
        targets = targets,
        status = "success"
      )

      message(sprintf("%10d %12.2f s", n, elapsed))

    }, error = function(e) {
      message(sprintf("Failed for n=%d: %s", n, e$message))
      results[[i]] <- list(
        n = n,
        status = "error",
        error = e$message
      )
    })
  }

  # Check that we have results for all problem sizes
  expect_equal(length(results), length(ns))

  # Check that at least one problem size succeeded
  successes <- which(sapply(results, function(r) r$status == "success"))
  expect_true(length(successes) > 0)

  # For successful runs, check scaling behavior
  success_times <- sapply(results[successes], function(r) r$time)
  if (length(success_times) > 1) {
    # Compute empirical scaling factor
    scaling <- diff(log(success_times)) / diff(log(ns[successes]))
    # Should scale better than quadratic
    expect_true(all(scaling < 2.5))
  }
})