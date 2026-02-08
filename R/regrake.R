#' Optimal representative sample weighting
#'
#' @param data A data.frame or tibble containing the sample data
#' @param formula A formula specifying the raking constraints (e.g., `~ rr_exact(sex) + rr_l2(age)`)
#' @param population_data Population data: a data.frame, list, or survey.design object (see `pop_type`)
#' @param pop_type How population data is specified:
#'   - "raw": Raw population data (one row per unit)
#'   - "weighted": Population data with weights column
#'   - "proportions": Direct specification of target proportions (variable, level, target columns)
#'   - "anesrake": List of named numeric vectors (anesrake package format)
#'   - "survey": Data frame with margin, category, value columns
#'   - "survey_design": survey package design object
#' @param pop_weights Column name in population_data containing weights (if pop_type = "weighted")
#' @param regularizer Regularization method ("entropy", "zero", "kl", or "boolean")
#' @param lambda Regularization strength (default = 1)
#' @param prior Optional prior weights used when `regularizer = "kl"`.
#'   Must be a positive numeric vector of length `nrow(data)`. If it does not
#'   sum to 1, it is normalized internally.
#' @param k Number of samples to select (required for regularizer = "boolean")
#' @param bounds Numeric vector of length 2 specifying (min, max) allowed weight values.
#'   Weights returned sum to n (sample size), so `bounds = c(0.3, 3)` means each weight
#'   is between 0.3 and 3 times the "average" weight of 1. Default is `c(0.1, 10)`.
#' @param bounds_method How to enforce bounds:
#'   \describe{
#'     \item{"soft"}{(Default) Uses regularizer clipping. Fast but bounds may be
#'       slightly violated when targets conflict with bounds. Asymmetric bounds
#'       are approximated as symmetric.}
#'     \item{"hard"}{Uses bounded simplex projection. Bounds are strictly enforced
#'       but optimization may be slower and targets may be less closely matched
#'       when bounds are binding.}
#'   }
#' @param exact_tol Optional tolerance for exact constraints. When non-NULL, all
#'   `rr_exact()` (and `rr_mean()`) constraints are converted to `rr_range()`
#'   with this margin. For example, `exact_tol = 0.02` means categorical proportions
#'   must be within +/- 2 percentage points of targets, and continuous means within
#'   +/- 0.02 of targets. Use `rr_range()` directly in the formula for per-variable
#'   control. Default is NULL (exact constraints enforced strictly).
#' @param normalize Logical. If TRUE (default), continuous variables are automatically
#'   scaled by their target value for numerical stability. The achieved values are
#'   reported in original units. Set to FALSE to disable this behavior.
#' @param control List of control parameters for the ADMM solver:
#'   \describe{
#'     \item{margin_tol}{Margin-based convergence tolerance (default 1e-4). The ADMM
#'       solver scales this by problem size to achieve approximately this level of
#'       margin accuracy regardless of sample size or constraint count. For example,
#'       `margin_tol = 0.001` targets ~0.1% max margin error. Internally computes
#'       `eps = margin_tol / sqrt(m + 2*n)`. Set to NULL to use raw eps_abs/eps_rel
#'       instead.}
#'     \item{eps_abs}{Absolute convergence tolerance for ADMM residuals. Only used
#'       when margin_tol is NULL. Note: the effective tolerance scales with
#'       `sqrt(m + 2*n)`, so the same value behaves differently at different
#'       problem sizes. Default 1e-5.}
#'     \item{eps_rel}{Relative convergence tolerance for ADMM residuals. Only used
#'       when margin_tol is NULL. Default 1e-5.}
#'     \item{rho}{ADMM penalty parameter (default 50).}
#'     \item{maxiter}{Maximum ADMM iterations (default 5000).}
#'   }
#' @param verbose Whether to print progress information
#' @param ... Additional arguments passed to methods
#'
#' @return An object of class `"regrake"` containing:
#'   \item{weights}{The optimal weights (sum to n)}
#'   \item{balance}{Data frame comparing achieved vs target values with columns:
#'     constraint (e.g., "exact_sex"), type ("exact" or "l2"), variable,
#'     level, achieved, target, residual}
#'   \item{solution}{Full solution details from solver}
#'   \item{diagnostics}{Weight, convergence, and margin matching diagnostics}
#' @examples
#' \donttest{
#' set.seed(42)
#' sample_data <- data.frame(
#'   sex = sample(c("M", "F"), 200, replace = TRUE, prob = c(0.6, 0.4)),
#'   age = sample(c("young", "old"), 200, replace = TRUE, prob = c(0.7, 0.3))
#' )
#' pop_targets <- data.frame(
#'   variable = c("sex", "sex", "age", "age"),
#'   level = c("M", "F", "young", "old"),
#'   target = c(0.49, 0.51, 0.45, 0.55)
#' )
#' result <- regrake(
#'   data = sample_data,
#'   formula = ~ rr_exact(sex) + rr_exact(age),
#'   population_data = pop_targets,
#'   pop_type = "proportions"
#' )
#' result
#' result$balance
#' }
#' @export
regrake <- function(
  data,
  formula,
  population_data,
  pop_type = c("raw", "weighted", "proportions", "anesrake", "survey", "survey_design"),
  pop_weights = NULL,
  regularizer = "entropy",
  lambda = 1,
  prior = NULL,
  k = NULL,
  bounds = c(0.1, 10),
  bounds_method = c("soft", "hard"),
  exact_tol = NULL,
  normalize = TRUE,
  control = list(),
  verbose = FALSE,
  ...
) {
  # Early input validation
  pop_type <- match.arg(pop_type)
  bounds_method <- match.arg(bounds_method)
  regularizer <- match.arg(regularizer, c("entropy", "zero", "kl", "boolean"))
  validate_inputs(formula, population_data, pop_type, pop_weights, bounds, bounds_method)

  if (!is.null(exact_tol)) {
    if (!is.numeric(exact_tol) || length(exact_tol) != 1 || exact_tol <= 0) {
      stop("exact_tol must be a single positive number or NULL", call. = FALSE)
    }
  }

  prior <- validate_and_prepare_prior(prior, nrow(data), regularizer)

  # Step 1: Parse formula into specification
  # This step determines what variables and interactions we need
  # and what type of constraints they represent
  formula_spec <- parse_raking_formula(formula)
  if (verbose) {
    cat("Formula parsed with", length(formula_spec$terms), "raking terms\n")
  }

  # Step 1b: Convert exact constraints to range if exact_tol is specified
  if (!is.null(exact_tol)) {
    for (i in seq_along(formula_spec$terms)) {
      if (formula_spec$terms[[i]]$type == "exact") {
        formula_spec$terms[[i]]$type <- "range"
        formula_spec$terms[[i]]$params <- list(mode = "margin", margin = exact_tol)
      }
    }
    if (verbose) {
      cat(sprintf(
        "exact_tol = %g: exact constraints converted to range (+/- %g)\n",
        exact_tol,
        exact_tol
      ))
    }
  }

  # Step 2: Process population data based on formula requirements
  # This step computes target values for each term in the formula
  # handling different population data formats consistently
  target_values <- compute_target_values(
    population_data = population_data,
    formula_spec = formula_spec,
    pop_type = pop_type,
    pop_weights = pop_weights
  )
  if (verbose) {
    cat("Population data processed, targets computed\n")
  }

  # Step 3: Build design matrix and losses for ADMM
  # This creates the inputs needed for the ADMM solver:
  # - design matrix (F)
  # - loss functions with their targets
  admm_inputs <- construct_admm_inputs(
    data = data,
    formula_spec = formula_spec,
    target_values = target_values,
    normalize = normalize
  )
  if (verbose) {
    cat(
      "Design matrix constructed with",
      nrow(admm_inputs$design_matrix),
      "constraints\n"
    )
  }

  # Step 4: Set up solver parameters
  ctrl <- list(
    maxiter = 5000,
    rho = 50,
    margin_tol = 1e-4,
    eps_rel = 1e-5,
    eps_abs = 1e-5
  )
  # If user explicitly provides eps_abs or eps_rel, opt out of margin_tol
  if (("eps_abs" %in% names(control) || "eps_rel" %in% names(control)) &&
      !("margin_tol" %in% names(control))) {
    ctrl$margin_tol <- NULL
  }
  ctrl[names(control)] <- control

  # Validate margin_tol if set
  if (!is.null(ctrl$margin_tol)) {
    if (!is.numeric(ctrl$margin_tol) || length(ctrl$margin_tol) != 1 || ctrl$margin_tol <= 0) {
      stop("control$margin_tol must be a single positive number or NULL", call. = FALSE)
    }
  }

  # Apply margin_tol scaling: compute eps from margin_tol and problem size
  if (!is.null(ctrl$margin_tol)) {
    n_samples <- nrow(data)
    n_constraints <- nrow(admm_inputs$design_matrix)
    p <- n_constraints + 2 * n_samples
    scaled_eps <- ctrl$margin_tol / sqrt(p)
    ctrl$eps_abs <- scaled_eps
    ctrl$eps_rel <- scaled_eps
    if (verbose) {
      cat(sprintf(
        "margin_tol=%.1e -> scaled eps=%.2e (p=%d, sqrt(p)=%.1f)\n",
        ctrl$margin_tol, scaled_eps, p, sqrt(p)
      ))
    }
  }

  # Step 5: Convert bounds to limit for soft method
  limit <- NULL
  if (bounds_method == "soft" && !is.null(bounds)) {
    limit <- convert_bounds_to_limit(bounds)
    if (verbose && !is_symmetric_bounds(bounds)) {
      message(
        "Note: Asymmetric bounds approximated for soft method. ",
        "Use bounds_method='hard' for exact enforcement."
      )
    }
  }

  # Step 6: Call solver
  if (verbose) {
    start_time <- proc.time()
  }

  solution <- admm(
    F = admm_inputs$design_matrix,
    losses = admm_inputs$losses,
    reg = create_regularizer(regularizer, prior = prior, k = k, limit = limit),
    lam = lambda,
    control = ctrl,
    verbose = verbose,
    bounds = if (bounds_method == "hard") bounds else NULL,
    bounds_method = bounds_method
  )

  if (verbose) {
    end_time <- proc.time()
    cat(sprintf("ADMM took %.3f seconds\n", (end_time - start_time)["elapsed"]))
  }

  if (!solution$converged) {
    warning(
      "ADMM solver did not converge within ", ctrl$maxiter, " iterations. ",
      "Results may be unreliable. Consider increasing maxiter or adjusting lambda/rho.",
      call. = FALSE
    )
  }

  # Step 7: Process results and compute diagnostics
  results <- process_admm_results(
    solution, admm_inputs, formula_spec, verbose, normalize, regularizer
  )

  # Step 8: Add bounds information to diagnostics
  results$diagnostics$bounds <- bounds
  results$diagnostics$bounds_method <- bounds_method
  results$diagnostics$bounds_violated <- check_bounds_violated(results$weights, bounds)

  if (verbose && results$diagnostics$bounds_violated) {
    message(
      "Note: Some weights violate the specified bounds. ",
      "This can happen with bounds_method='soft' when targets conflict with bounds."
    )
  }

  # Return results with appropriate class for methods dispatch
  structure(
    list(
      data = data,
      weights = results$weights,
      balance = results$balance,
      solution = solution,
      diagnostics = results$diagnostics,
      formula = formula,
      regularizer = regularizer,
      lambda = lambda,
      prior = if (regularizer == "kl") prior else NULL,
      exact_tol = exact_tol,
      margin_tol = ctrl$margin_tol,
      call = match.call()
    ),
    class = "regrake"
  )
}

# Helper to validate and normalize prior weights for KL regularization
validate_and_prepare_prior <- function(prior, n_rows, regularizer) {
  if (regularizer != "kl") {
    if (!is.null(prior)) {
      rlang::warn(
        c(
          "Argument `prior` is ignored unless regularizer = 'kl'.",
          i = "Proceeding without prior weights."
        ),
        class = "regrake_prior_ignored"
      )
    }
    return(NULL)
  }

  if (is.null(prior)) {
    stop(
      "prior must be provided when regularizer = 'kl'.",
      call. = FALSE
    )
  }
  if (!is.numeric(prior) || length(prior) != n_rows || any(!is.finite(prior))) {
    stop(
      "prior must be a finite numeric vector with length equal to nrow(data).",
      call. = FALSE
    )
  }
  if (any(prior <= 0)) {
    stop("prior values must all be strictly positive.", call. = FALSE)
  }

  s <- sum(prior)
  if (abs(s - 1) > 1e-8) {
    rlang::warn(
      c(
        "prior does not sum to 1; normalizing internally.",
        i = paste0("Provided sum was ", format(s, digits = 6), ".")
      ),
      class = "regrake_prior_normalized"
    )
    prior <- prior / s
  }

  prior
}

# Helper function for input validation
validate_inputs <- function(
  formula,
  population_data,
  pop_type,
  pop_weights,
  bounds,
  bounds_method
) {
  if (is.null(formula)) {
    stop(
      "Formula must be specified. For direct solver access, use admm() instead."
    )
  }

  if (is.null(population_data)) {
    stop("Population data must be provided")
  }

  if (pop_type == "weighted" && is.null(pop_weights)) {
    stop("pop_weights must be specified when pop_type = 'weighted'")
  }

  if (length(bounds) != 2 || bounds[1] >= bounds[2] || bounds[1] <= 0) {
    stop("bounds must be a vector of length 2 with 0 < min < max")
  }

  if (bounds_method == "hard" && (bounds[1] > 1 || bounds[2] < 1)) {
    stop(
      "Hard bounds are infeasible with sum-to-n weights: require min <= 1 <= max.",
      call. = FALSE
    )
  }
}

# Helper to create regularizer object from string specification
create_regularizer <- function(
  regularizer,
  prior = NULL,
  limit = NULL,
  k = NULL
) {
  # Validate regularizer type
  regularizer <- match.arg(
    regularizer,
    choices = c("entropy", "zero", "kl", "boolean"),
    several.ok = FALSE
  )

  # Create regularizer object based on type
  switch(
    regularizer,
    "zero" = list(
      prox = prox_equality_reg # zero regularizer uses equality prox
    ),
    "entropy" = list(
      prox = function(w, lambda) {
        prox_kl_reg(w, lambda, prior = NULL, limit = limit)
      }
    ),
    "kl" = {
      if (is.null(prior)) {
        stop("Prior weights must be provided for KL regularization")
      }
      list(
        prox = function(w, lambda) {
          prox_kl_reg(w, lambda, prior = prior, limit = limit)
        }
      )
    },
    "boolean" = {
      if (is.null(k)) {
        stop(
          "k (number of samples to select) must be provided for boolean regularization"
        )
      }
      reg <- list(
        prox = function(w, lambda) prox_boolean_reg(w, lambda, k)
      )
      class(reg) <- c("BooleanRegularizer", class(reg))
      reg
    }
  )
}

# Helper to process solution and compute diagnostics
process_admm_results <- function(
  solution,
  admm_inputs,
  formula_spec,
  verbose,
  normalize = TRUE,
  regularizer = "entropy"
) {
  # Extract best weights from solution and scale to sum to sample size
  weights <- solution$w_best
  n <- length(weights)
  weights <- weights * n # Scale up to sum to sample size

  # Safety net: error if any weights are exactly zero (except for boolean regularizer)
  if (regularizer != "boolean" && any(weights == 0)) {
    n_zero <- sum(weights == 0)
    zero_rows <- which(weights == 0)
    stop(
      n_zero, " observation(s) received zero weight (rows: ",
      paste(utils::head(zero_rows, 10), collapse = ", "),
      if (n_zero > 10) ", ..." else "",
      "). Possible causes: (1) tolerance too loose for problem size ",
      "(try smaller eps_abs/eps_rel in control), ",
      "(2) data has observations with no overlap with targets, ",
      "(3) targets are infeasible given the data.",
      call. = FALSE
    )
  }

  # Calculate achieved values using design matrix
  # Result is weighted totals; divide by n to get proportions/means on same scale as targets
  achieved_values <- drop(as.matrix(admm_inputs$design_matrix %*% weights)) / n

  # De-normalize continuous variable achieved values if normalization was applied
  if (normalize && !is.null(admm_inputs$scale_factors)) {
    for (i in seq_along(admm_inputs$scale_factors)) {
      idx <- admm_inputs$scale_factors[[i]]$index
      scale <- admm_inputs$scale_factors[[i]]$scale
      achieved_values[idx] <- achieved_values[idx] * scale
    }
  }

  # Extract targets in the same order as achieved values (original, un-normalized)
  targets_flat <- unlist(lapply(admm_inputs$losses, function(l) l$original_target))

  # Build balance data frame
  balance <- build_balance_df(formula_spec, admm_inputs$losses, achieved_values, targets_flat)

  # Compute diagnostics
  kish_deff <- 1 + stats::var(weights) / mean(weights)^2
  diagnostics <- list(
    # Weight properties
    weight_range = range(weights),
    weight_mean = mean(weights),
    weight_sd = sd(weights),
    kish_deff = kish_deff,
    kish_ess = length(weights) / kish_deff,

    # Convergence
    converged = solution$converged,
    iterations = solution$iterations,
    primal_residual = solution$primal_residual,
    dual_residual = solution$dual_residual,

    # Margin matching quality (achieved_values already on proportion/mean scale)
    max_abs_diff = max(abs(achieved_values - targets_flat)),
    max_pct_diff = max_pct_diff(achieved_values, targets_flat)
  )

  if (verbose) {
    cat("\nDiagnostics:\n")
    cat(sprintf(
      "Weight range: [%.3f, %.3f]\n",
      diagnostics$weight_range[1],
      diagnostics$weight_range[2]
    ))
    cat(sprintf(
      "Maximum absolute difference in margins: %.2e\n",
      diagnostics$max_abs_diff
    ))
    cat(sprintf(
      "Maximum percent difference in margins: %.2f%%\n",
      diagnostics$max_pct_diff
    ))
  }

  list(
    weights = weights,
    balance = balance,
    diagnostics = diagnostics
  )
}

# Helper to calculate max percent difference
max_pct_diff <- function(achieved, desired) {
  diff <- abs(achieved - desired)
  pct_diff <- ifelse(desired != 0, (diff / abs(desired)) * 100, NA)
  max(pct_diff, na.rm = TRUE)
}

# Helper to build balance data frame from formula_spec and results
build_balance_df <- function(formula_spec, losses, achieved_values, targets_flat) {
  # Build rows for each term
  rows <- vector("list", length(formula_spec$terms))
  start <- 1


  for (i in seq_along(formula_spec$terms)) {
    term <- formula_spec$terms[[i]]
    loss <- losses[[i]]
    n_levels <- length(loss$target)
    end <- start + n_levels - 1

    # Get constraint name (e.g., "exact_sex" or "l2_region:educ")
    variable <- if (is.null(term$interaction)) {
      term$variables
    } else {
      paste(term$variables, collapse = ":")
    }
    constraint <- paste0(term$type, "_", variable)

    # Get level names from targets
    level_names <- names(loss$target)
    if (is.null(level_names)) {
      level_names <- as.character(seq_len(n_levels))
    }

    rows[[i]] <- data.frame(
      constraint = constraint,
      type = term$type,
      variable = variable,
      level = level_names,
      achieved = achieved_values[start:end],
      target = targets_flat[start:end],
      stringsAsFactors = FALSE
    )

    start <- end + 1
  }

  # Combine all rows
  balance <- do.call(rbind, rows)
  balance$residual <- balance$achieved - balance$target
  rownames(balance) <- NULL
  balance
}

# Helper to convert user-specified bounds to regularizer limit
# Bounds are [min, max] where weights sum to n (so "average" weight is 1)
# Limit is symmetric: weights clipped to [1/limit, limit] * (1/n)
# To cover both bounds: limit >= max AND limit >= 1/min
convert_bounds_to_limit <- function(bounds) {
  max(bounds[2], 1 / bounds[1])
}

# Check if bounds are symmetric around 1 (e.g., c(0.5, 2) or c(0.33, 3))
is_symmetric_bounds <- function(bounds) {
  abs(bounds[1] * bounds[2] - 1) < 1e-6
}

# Check if any weights violate bounds (for diagnostics)
check_bounds_violated <- function(weights, bounds) {
  any(weights < bounds[1] - 1e-6) || any(weights > bounds[2] + 1e-6)
}

#' Print method for regrake objects
#'
#' @param x A regrake object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.regrake <- function(x, ...) {
  d <- x$diagnostics
  n <- length(x$weights)

  # Constraint type summary
  types <- x$balance$type
  type_counts <- table(types)
  type_str <- paste(
    vapply(
      names(type_counts),
      function(t) sprintf("%d %s", type_counts[[t]], t),
      character(1)
    ),
    collapse = ", "
  )
  n_constraints <- nrow(x$balance)

  cat("Regrake result\n")
  cat(sprintf("  Sample size:      %d\n", n))
  cat(sprintf("  Constraints:      %d (%s)\n", n_constraints, type_str))
  cat(sprintf("  Regularizer:      %s (lambda = %g)\n", x$regularizer, x$lambda))
  if (!is.null(x$exact_tol)) {
    cat(sprintf("  Exact tolerance:  %g\n", x$exact_tol))
  }
  if (!identical(x$margin_tol, 1e-4)) {
    if (is.null(x$margin_tol)) {
      cat("  Margin tolerance: off (using raw eps_abs/eps_rel)\n")
    } else {
      cat(sprintf("  Margin tolerance: %g\n", x$margin_tol))
    }
  }
  if (d$converged) {
    cat(sprintf("  Converged:        Yes (%d iterations)\n", d$iterations))
  } else {
    cat(sprintf("  Converged:        No (hit %d iterations)\n", d$iterations))
  }
  cat(sprintf(
    "  Weight range:     [%.3f, %.3f]\n",
    d$weight_range[1],
    d$weight_range[2]
  ))
  cat(sprintf(
    "  Kish ESS:         %.0f / %d (deff = %.2f)\n",
    d$kish_ess,
    n,
    d$kish_deff
  ))
  cat(sprintf("  Max margin diff:  %.2e\n", d$max_abs_diff))

  invisible(x)
}

#' Summary method for regrake objects
#'
#' @param object A regrake object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
summary.regrake <- function(object, ...) {
  # Print compact overview first
  print.regrake(object)

  d <- object$diagnostics
  w <- object$weights

  # Weight distribution
  wq <- stats::quantile(w, probs = c(0, 0.25, 0.5, 0.75, 1))
  cat("\nWeight distribution:\n")
  cat(sprintf(
    "  Min.   Q1     Median Q3     Max.\n  %-6.3f %-6.3f %-6.3f %-6.3f %.3f\n",
    wq[1], wq[2], wq[3], wq[4], wq[5]
  ))

  # Bounds info
  if (!is.null(d$bounds)) {
    cat(sprintf(
      "\nBounds: [%.2f, %.2f] (%s)",
      d$bounds[1],
      d$bounds[2],
      d$bounds_method
    ))
    if (d$bounds_violated) {
      cat(" -- VIOLATED")
    }
    cat("\n")
  }

  # Balance table
  cat("\nBalance:\n")
  bal <- object$balance
  bal$achieved <- round(bal$achieved, 6)
  bal$target <- round(bal$target, 6)
  bal$residual <- round(bal$residual, 6)
  print(bal, row.names = FALSE)

  invisible(object)
}
