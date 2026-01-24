# Compare R solver results against Python reference implementations
#
# This script loads results from test_cases/ and compares:
# - R results vs rswjax (your Python implementation)
# - R results vs rsw_original (paper authors' implementation)
# - rswjax vs rsw_original (to verify they match)

library(jsonlite)

compare_vectors <- function(v1, v2, name1, name2, tol = 1e-4) {
  if (is.null(v1) || is.null(v2)) {
    return(list(
      compared = FALSE,
      reason = paste0(if (is.null(v1)) name1 else name2, " missing")
    ))
  }

  max_diff <- max(abs(v1 - v2))
  mean_diff <- mean(abs(v1 - v2))
  correlation <- cor(v1, v2)

  list(
    compared = TRUE,
    max_diff = max_diff,
    mean_diff = mean_diff,
    correlation = correlation,
    pass = max_diff < tol
  )
}

load_results <- function(case_name) {
  case_dir <- file.path("test_cases", case_name)

  results <- list(name = case_name)

  # Load R results
  r_weights_file <- file.path(case_dir, "weights_r.csv")
  if (file.exists(r_weights_file)) {
    results$r <- list(
      weights = as.numeric(read.csv(r_weights_file)[[1]]),
      achieved = as.numeric(read.csv(file.path(case_dir, "achieved_r.csv"))[[1]])
    )
  }

  # Load rswjax results
  rswjax_file <- file.path(case_dir, "weights_rswjax.csv")
  if (file.exists(rswjax_file)) {
    results$rswjax <- list(
      weights = as.numeric(read.csv(rswjax_file, header = FALSE)[[1]]),
      achieved = as.numeric(read.csv(file.path(case_dir, "achieved_rswjax.csv"),
                                     header = FALSE)[[1]])
    )
  }

  # Load rsw_original results
  orig_file <- file.path(case_dir, "weights_rsw_original.csv")
  if (file.exists(orig_file)) {
    results$rsw_original <- list(
      weights = as.numeric(read.csv(orig_file, header = FALSE)[[1]]),
      achieved = as.numeric(read.csv(file.path(case_dir, "achieved_rsw_original.csv"),
                                     header = FALSE)[[1]])
    )
  }

  # Load targets for reference
  losses <- fromJSON(file.path(case_dir, "losses.json"))
  results$targets <- unlist(lapply(losses, function(l) l$target))

  results
}

compare_case <- function(case_name, tol = 1e-4) {
  results <- load_results(case_name)

  comparisons <- list(name = case_name)

  # R vs rswjax
  if (!is.null(results$r) && !is.null(results$rswjax)) {
    comparisons$r_vs_rswjax <- list(
      weights = compare_vectors(results$r$weights, results$rswjax$weights,
                                "R", "rswjax", tol),
      achieved = compare_vectors(results$r$achieved, results$rswjax$achieved,
                                 "R", "rswjax", tol)
    )
  }

  # R vs rsw_original
  if (!is.null(results$r) && !is.null(results$rsw_original)) {
    comparisons$r_vs_original <- list(
      weights = compare_vectors(results$r$weights, results$rsw_original$weights,
                                "R", "rsw_original", tol),
      achieved = compare_vectors(results$r$achieved, results$rsw_original$achieved,
                                 "R", "rsw_original", tol)
    )
  }

  # rswjax vs rsw_original (sanity check)
  if (!is.null(results$rswjax) && !is.null(results$rsw_original)) {
    comparisons$rswjax_vs_original <- list(
      weights = compare_vectors(results$rswjax$weights, results$rsw_original$weights,
                                "rswjax", "rsw_original", tol),
      achieved = compare_vectors(results$rswjax$achieved, results$rsw_original$achieved,
                                 "rswjax", "rsw_original", tol)
    )
  }

  # Check achieved vs targets
  if (!is.null(results$r)) {
    comparisons$r_vs_targets <- compare_vectors(
      results$r$achieved, results$targets, "R achieved", "targets", tol
    )
  }

  comparisons
}

print_comparison <- function(comp, name) {
  if (is.null(comp)) {
    cat("  ", name, ": NOT AVAILABLE\n")
    return()
  }

  if (!comp$compared) {
    cat("  ", name, ": SKIPPED (", comp$reason, ")\n")
    return()
  }

  status <- if (comp$pass) "PASS" else "FAIL"
  cat(sprintf("  %s: %s (max_diff=%.2e, corr=%.6f)\n",
              name, status, comp$max_diff, comp$correlation))
}

main <- function(tol = 1e-4) {
  # Find all test cases
  test_dirs <- list.dirs("test_cases", recursive = FALSE, full.names = FALSE)

  if (length(test_dirs) == 0) {
    stop("No test cases found. Run generate_test_cases.py and run_r_solver.R first.")
  }

  cat("Comparing results across implementations\n")
  cat("Tolerance:", tol, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  all_pass <- TRUE
  summary <- list()

  for (case_name in sort(test_dirs)) {
    cat(case_name, "\n")

    comp <- compare_case(case_name, tol)
    summary[[case_name]] <- comp

    # Print R vs Python comparisons
    if (!is.null(comp$r_vs_rswjax)) {
      print_comparison(comp$r_vs_rswjax$weights, "R vs rswjax (weights)")
      print_comparison(comp$r_vs_rswjax$achieved, "R vs rswjax (achieved)")

      if (!is.null(comp$r_vs_rswjax$weights$pass) && !comp$r_vs_rswjax$weights$pass) {
        all_pass <- FALSE
      }
    }

    if (!is.null(comp$r_vs_original)) {
      print_comparison(comp$r_vs_original$weights, "R vs original (weights)")
    }

    # Sanity check: do the two Python implementations agree?
    if (!is.null(comp$rswjax_vs_original)) {
      print_comparison(comp$rswjax_vs_original$weights, "rswjax vs original")
    }

    # Did we hit our targets?
    if (!is.null(comp$r_vs_targets)) {
      print_comparison(comp$r_vs_targets, "R achieved vs targets")
    }

    cat("\n")
  }

  cat(paste(rep("=", 60), collapse = ""), "\n")
  if (all_pass) {
    cat("ALL TESTS PASSED\n")
  } else {
    cat("SOME TESTS FAILED\n")
  }

  invisible(summary)
}

# Run if executed directly
if (!interactive()) {
  main()
}
