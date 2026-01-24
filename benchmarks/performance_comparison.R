# Performance comparison: R solver vs Python implementations
#
# This script compares timing between the R ADMM solver and the Python
# implementations (rswjax and rsw_original) on the same test cases.
#
# Prerequisites:
#   1. Run generate_test_cases.py first to create test cases and Python timings
#   2. Run run_r_solver.R to generate R results
#
# Usage:
#   cd benchmarks
#   Rscript performance_comparison.R

library(Matrix)

# Load regrake package (from parent directory)
devtools::load_all("..")

# Helper to load a test case and time R solver
benchmark_test_case <- function(case_name) {
  case_dir <- file.path("test_cases", case_name)

  if (!dir.exists(case_dir)) {
    return(NULL)
  }

  # Load design matrix
  F <- as.matrix(read.csv(file.path(case_dir, "F.csv"), header = FALSE))
  F <- Matrix::Matrix(F, sparse = TRUE)

  # Load losses specification
  losses_json <- jsonlite::fromJSON(file.path(case_dir, "losses.json"))

  # Load regularizer specification
  reg_json <- jsonlite::fromJSON(file.path(case_dir, "regularizer.json"))

  # Load lambda
  lam <- as.numeric(readLines(file.path(case_dir, "lambda.txt")))

  # Build losses
  losses <- make_losses(losses_json)

  # Build regularizer
  reg <- make_regularizer(reg_json)

  # Time R solver (average of 3 runs for stability)
  times <- numeric(3)
  for (i in 1:3) {
    start <- proc.time()
    sol <- admm(
      F = F,
      losses = losses,
      reg = reg,
      lam = lam,
      control = list(
        maxiter = 5000,
        eps_abs = 1e-6,
        eps_rel = 1e-6,
        rho = 50
      ),
      verbose = FALSE
    )
    times[i] <- (proc.time() - start)[3]
  }

  r_time <- median(times)

  # Try to load Python timing if available
  rswjax_time <- NA
  rsw_orig_time <- NA
  timing_file <- file.path(case_dir, "timing.json")
  if (file.exists(timing_file)) {
    timing <- jsonlite::fromJSON(timing_file)
    rswjax_time <- timing$rswjax_time
    rsw_orig_time <- timing$rsw_original_time
  }

  list(
    case = case_name,
    r_time = r_time,
    rswjax_time = rswjax_time,
    rsw_orig_time = rsw_orig_time,
    n_samples = ncol(F),
    n_constraints = nrow(F)
  )
}

# Convert losses spec to R loss objects (same as run_r_solver.R)
make_losses <- function(losses_spec) {
  if (is.data.frame(losses_spec)) {
    losses_list <- lapply(seq_len(nrow(losses_spec)), function(i) {
      list(type = losses_spec$type[i], target = losses_spec$target[[i]])
    })
  } else {
    losses_list <- losses_spec
  }

  lapply(losses_list, function(spec) {
    target <- spec$target
    switch(spec$type,
      "equality" = list(
        fn = equality_loss,
        target = target,
        prox = prox_equality
      ),
      "least_squares" = list(
        fn = least_squares_loss,
        target = target,
        prox = prox_least_squares
      ),
      stop("Unknown loss type: ", spec$type)
    )
  })
}

# Convert regularizer spec to R regularizer object
make_regularizer <- function(reg_spec) {
  switch(reg_spec$type,
    "entropy" = list(
      fn = function(w, lambda) entropy_regularizer(w, lambda, reg_spec$limit),
      prox = function(w, lambda) prox_kl_reg(w, lambda, prior = NULL, limit = reg_spec$limit)
    ),
    "zero" = list(
      fn = zero_regularizer,
      prox = prox_equality_reg
    ),
    stop("Unknown regularizer type: ", reg_spec$type)
  )
}

# Main
main <- function() {
  # Find all test cases
  test_dirs <- list.dirs("test_cases", recursive = FALSE, full.names = FALSE)

  if (length(test_dirs) == 0) {
    stop("No test cases found. Run generate_test_cases.py first.")
  }

  cat("Performance Comparison: R vs Python\n")
  cat("====================================\n\n")

  results <- list()
  for (case_name in sort(test_dirs)) {
    cat("Benchmarking:", case_name, "...")
    result <- tryCatch({
      benchmark_test_case(case_name)
    }, error = function(e) {
      cat(" ERROR:", conditionMessage(e), "\n")
      NULL
    })

    if (!is.null(result)) {
      results[[case_name]] <- result
      cat(sprintf(" R: %.3fs", result$r_time))
      if (!is.na(result$rswjax_time)) {
        cat(sprintf(" | JAX: %.3fs", result$rswjax_time))
      }
      if (!is.na(result$rsw_orig_time)) {
        cat(sprintf(" | Orig: %.3fs", result$rsw_orig_time))
      }
      cat("\n")
    }
  }

  # Summary table
  cat("\n")
  cat("Summary Table\n")
  cat("=============\n")
  cat(sprintf("%-22s %8s %6s %8s %8s %8s %8s %8s\n",
              "Test Case", "Samples", "Cstrs", "R (s)", "JAX (s)", "Orig (s)", "R/JAX", "R/Orig"))
  cat(paste(rep("-", 90), collapse = ""), "\n")

  for (r in results) {
    jax_str <- if (is.na(r$rswjax_time)) "N/A" else sprintf("%.3f", r$rswjax_time)
    orig_str <- if (is.na(r$rsw_orig_time)) "N/A" else sprintf("%.3f", r$rsw_orig_time)
    r_jax_str <- if (is.na(r$rswjax_time)) "N/A" else sprintf("%.2fx", r$r_time / r$rswjax_time)
    r_orig_str <- if (is.na(r$rsw_orig_time)) "N/A" else sprintf("%.2fx", r$r_time / r$rsw_orig_time)
    cat(sprintf("%-22s %8d %6d %8.3f %8s %8s %8s %8s\n",
                r$case, r$n_samples, r$n_constraints, r$r_time, jax_str, orig_str, r_jax_str, r_orig_str))
  }

  # Overall summary
  if (length(results) > 0) {
    r_times <- sapply(results, function(x) x$r_time)
    cat("\n")
    cat(sprintf("R solver: min=%.3fs, median=%.3fs, max=%.3fs\n",
                min(r_times), median(r_times), max(r_times)))

    jax_times <- sapply(results, function(x) x$rswjax_time)
    orig_times <- sapply(results, function(x) x$rsw_orig_time)

    if (any(!is.na(jax_times))) {
      valid <- !is.na(jax_times)
      ratios <- r_times[valid] / jax_times[valid]
      cat(sprintf("R/JAX ratio: min=%.2fx, median=%.2fx, max=%.2fx\n",
                  min(ratios), median(ratios), max(ratios)))
    }

    if (any(!is.na(orig_times))) {
      valid <- !is.na(orig_times)
      ratios <- r_times[valid] / orig_times[valid]
      cat(sprintf("R/Original ratio: min=%.2fx, median=%.2fx, max=%.2fx\n",
                  min(ratios), median(ratios), max(ratios)))
    }
  }

  invisible(results)
}

# Run if executed directly
if (!interactive()) {
  main()
}
