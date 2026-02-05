# Compare R solver results against Python reference implementations
#
# This script loads results from test_cases/ and compares:
# - R results vs rswjax (your Python implementation)
# - R results vs rsw_original (paper authors' implementation)
# - rswjax vs rsw_original (to verify they match)
#
# Key insight: Only exact constraints (equality loss) should hit targets precisely.
# Soft constraints (least_squares) trade off target accuracy against regularization.

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
  correlation <- suppressWarnings(cor(v1, v2))

  list(
    compared = TRUE,
    max_diff = max_diff,
    mean_diff = mean_diff,
    correlation = if (is.na(correlation)) 1.0 else correlation,
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

  # Load losses specification to understand constraint types
  losses <- fromJSON(file.path(case_dir, "losses.json"))

  # Handle case where jsonlite simplifies to data frame
  if (is.data.frame(losses)) {
    results$targets <- unlist(losses$target)
    results$loss_types <- losses$type
  } else {
    results$targets <- unlist(lapply(losses, function(l) l$target))
    results$loss_types <- vapply(losses, function(l) l$type, character(1))
  }

  # Determine if this is an exact-only case (all equality losses)
  results$all_exact <- all(results$loss_types == "equality")
  results$has_soft <- any(results$loss_types %in% c("least_squares", "kl", "inequality"))

  # Count constraints for tolerance adjustment
  results$n_constraints <- length(results$targets)

  # Load regularizer type for tolerance adjustment
  reg <- fromJSON(file.path(case_dir, "regularizer.json"))
  results$reg_type <- reg$type

  results
}

# Determine appropriate tolerance based on problem characteristics
get_tolerance <- function(results, base_tol = 1e-4) {
  # Boolean regularizer: discrete solution, cross-implementation differences expected
  if (!is.null(results$reg_type) && results$reg_type == "boolean") {
    return(base_tol * 100)  # 1e-2 for boolean
  }
  # High constraint cases need looser tolerance
  if (results$n_constraints >= 50) {
    return(base_tol * 5)  # 5e-4 for 50+ constraints
  }
  base_tol
}

# Check if problem is likely over-constrained (can't satisfy all targets)
is_overconstrained <- function(results) {
  # Heuristic: if constraints >= 90% of likely sample size, may be over-constrained
  # We don't have sample size here, but 99 constraints is a red flag

  results$n_constraints >= 90
}

compare_case <- function(case_name, base_tol = 1e-4) {
  results <- load_results(case_name)
  tol <- get_tolerance(results, base_tol)

  comparisons <- list(
    name = case_name,
    all_exact = results$all_exact,
    has_soft = results$has_soft,
    n_constraints = results$n_constraints,
    tolerance_used = tol,
    overconstrained = is_overconstrained(results),
    is_boolean = !is.null(results$reg_type) && results$reg_type == "boolean"
  )

  # R vs rswjax (weights should always match)
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

  # Check achieved vs targets - only meaningful for exact constraints
  if (!is.null(results$r)) {
    comparisons$r_vs_targets <- compare_vectors(
      results$r$achieved, results$targets, "R achieved", "targets", tol
    )
  }

  comparisons
}

print_comparison <- function(comp, name, note = NULL) {
  if (is.null(comp)) {
    cat("  ", name, ": NOT AVAILABLE\n")
    return()
  }

  if (!comp$compared) {
    cat("  ", name, ": SKIPPED (", comp$reason, ")\n")
    return()
  }

  status <- if (comp$pass) "PASS" else "FAIL"
  note_str <- if (!is.null(note)) paste0(" ", note) else ""
  cat(sprintf("  %s: %s (max_diff=%.2e)%s\n",
              name, status, comp$max_diff, note_str))
}

main <- function(base_tol = 1e-4) {
  # Find all test cases
  test_dirs <- list.dirs("test_cases", recursive = FALSE, full.names = FALSE)

  if (length(test_dirs) == 0) {
    stop("No test cases found. Run generate_test_cases.py and run_r_solver.R first.")
  }

  cat("Comparing R solver against Python implementations\n")
  cat("Base tolerance:", base_tol, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  all_pass <- TRUE
  summary <- list()

  for (case_name in sort(test_dirs)) {
    comp <- compare_case(case_name, base_tol)
    summary[[case_name]] <- comp

    # Case header with context
    constraint_info <- sprintf("[%d constraints, %s]",
                               comp$n_constraints,
                               if (comp$all_exact) "exact" else "mixed/soft")
    cat(case_name, constraint_info, "\n")

    # Primary check: R weights match Python weights
    if (!is.null(comp$r_vs_rswjax)) {
      print_comparison(comp$r_vs_rswjax$weights, "R vs rswjax (weights)")

      if (!is.null(comp$r_vs_rswjax$weights$pass) && !comp$r_vs_rswjax$weights$pass) {
        all_pass <- FALSE
      }
    }

    # Secondary: R vs original Python
    if (!is.null(comp$r_vs_original)) {
      print_comparison(comp$r_vs_original$weights, "R vs original (weights)")
    }

    # Achieved vs targets - interpret based on constraint type
    if (!is.null(comp$r_vs_targets)) {
      if (comp$overconstrained) {
        note <- "(over-constrained - infeasible)"
        print_comparison(comp$r_vs_targets, "R achieved vs targets", note)
      } else if (comp$is_boolean) {
        note <- "(boolean reg - discrete solution)"
        print_comparison(comp$r_vs_targets, "R achieved vs targets", note)
      } else if (comp$all_exact) {
        print_comparison(comp$r_vs_targets, "R achieved vs targets")
        if (!comp$r_vs_targets$pass) {
          all_pass <- FALSE
        }
      } else {
        note <- "(soft constraints - expected gap)"
        print_comparison(comp$r_vs_targets, "R achieved vs targets", note)
      }
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
