#' Optimal representative sample weighting
#'
#' @param data A data.frame or tibble containing the sample data
#' @param formula A formula specifying the raking constraints using exact() and l2() terms
#' @param population_data A data.frame containing population data, or pre-computed target proportions
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
#' @param normalize Logical. If TRUE (default), continuous variables are automatically
#'   scaled by their target value for numerical stability. The achieved values are
#'   reported in original units. Set to FALSE to disable this behavior.
#' @param control List of control parameters for the solver
#' @param verbose Whether to print progress information
#' @param ... Additional arguments passed to methods
#'
#' @return A list containing:
#'   \item{weights}{The optimal weights (sum to n)}
#'   \item{balance}{Data frame comparing achieved vs target values with columns:
#'     constraint (e.g., "exact_sex"), type ("exact" or "l2"), variable,
#'     level, achieved, target, residual}
#'   \item{solution}{Full solution details from solver}
#'   \item{diagnostics}{Weight and margin matching diagnostics}
#' @export
regrake <- function(
  data,
  formula,
  population_data,
  pop_type = c("raw", "weighted", "proportions", "anesrake", "survey", "survey_design"),
  pop_weights = NULL,
  regularizer = "entropy",
  lambda = 1,
  k = NULL,
  bounds = c(0.1, 10),
  bounds_method = c("soft", "hard"),
  normalize = TRUE,
  control = list(),
  verbose = FALSE,
  ...
) {
  # Early input validation
  pop_type <- match.arg(pop_type)
  bounds_method <- match.arg(bounds_method)
  validate_inputs(formula, population_data, pop_type, pop_weights, bounds)

  # Step 1: Parse formula into specification
  # This step determines what variables and interactions we need
  # and what type of constraints they represent
  formula_spec <- parse_raking_formula(formula)
  if (verbose) {
    cat("Formula parsed with", length(formula_spec$terms), "raking terms\n")
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
    eps_rel = 1e-5,
    eps_abs = 1e-5
  )
  ctrl[names(control)] <- control

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
    reg = create_regularizer(regularizer, k = k, limit = limit),
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
      call = match.call() # Store call for reproducibility
    ),
    class = "regrake"
  )
}

# Helper function for input validation
validate_inputs <- function(
  formula,
  population_data,
  pop_type,
  pop_weights,
  bounds
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
      fn = zero_regularizer,
      prox = prox_equality_reg # zero regularizer uses equality prox
    ),
    "entropy" = list(
      fn = function(w, lambda) entropy_regularizer(w, lambda, limit),
      prox = function(w, lambda) {
        prox_kl_reg(w, lambda, prior = NULL, limit = limit)
      }
    ),
    "kl" = {
      if (is.null(prior)) {
        stop("Prior weights must be provided for KL regularization")
      }
      list(
        fn = function(w, lambda) kl_regularizer(w, lambda, prior, limit),
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
        fn = function(w, lambda) prox_boolean_reg(w, lambda, k),
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
      "). This should not happen with valid data/targets. ",
      "Please report this as a bug if you believe your inputs are correct.",
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
  # Fallback to target if original_target not present (for backwards compatibility)
  if (is.null(targets_flat)) {
    targets_flat <- unlist(lapply(admm_inputs$losses, function(l) l$target))
  }

  # Build balance data frame
  balance <- build_balance_df(formula_spec, admm_inputs$losses, achieved_values, targets_flat)

  # Compute diagnostics
  diagnostics <- list(
    # Weight properties
    weight_range = range(weights),
    weight_mean = mean(weights),
    weight_sd = sd(weights),

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
