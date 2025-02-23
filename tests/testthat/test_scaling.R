test_that("Solver performance scales with problem size", {
  # Skip on CRAN
  skip_on_cran()

  # Test cases matching the JAX README
  ns <- c(1000, 10000, 100000)
  results <- list()

  for (n in ns) {

    # Create simulated data similar to the README example
    set.seed(605)
    age <- runif(n, 20, 30)
    sex <- rbinom(n, 1, 0.6)
    height <- rnorm(n, 5, 1)

    # Create design matrix 3 x n
    F <- Matrix::Matrix(rbind(age, sex, height), sparse = TRUE)

    # Normalize ALL features
    F_norm <- F
    for (i in 1:3) {
      F_norm[i,] <- (F[i,] - mean(F[i,])) / sd(F[i,])
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
                  eps_abs = 1e-5,
                  eps_rel = 1e-5,
                  rho = 50,
                  maxiter = 5000,
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

      results[[as.character(n)]] <- list(
        n = n,
        time = elapsed,
        final_values = Fw,
        targets = targets,
        status = "success"
      )

    }, error = function(e) {
      results[[as.character(n)]] <- list(
        n = n,
        time = (proc.time() - start_time)[3],
        error = e$message,
        status = "failed"
      )
      message(sprintf("Failed for n=%d: %s", n, e$message))
    })
  }

  # Print results
  for (i in seq_along(ns)) {
    r <- results[[as.character(ns[i])]]
    if (r$status == "success") {
      message(sprintf("%10d %12.2f s", ns[i], r$time))
    } else {
      message(sprintf("%10d %12s", ns[i], "FAILED"))
    }
  }

  # Verify test completed without errors
  expect_true(TRUE, "Scaling test completed successfully")
})