#' Optimal representative sample weighting
#'
#' @param data A data.frame or tibble containing the sample data
#' @param formula A formula specifying the raking constraints using exact() and l2() terms
#' @param population_data A data.frame containing population data, or pre-computed target proportions
#' @param pop_type How population data is specified:
#'   - "raw": Raw population data (one row per unit)
#'   - "weighted": Population data with weights column
#'   - "proportions": Direct specification of target proportions
#' @param pop_weights Column name in population_data containing weights (if pop_type = "weighted")
#' @param regularizer Regularization method ("entropy", "zero", "kl", or "boolean")
#' @param lambda Regularization strength (default = 1)
#' @param bounds Numeric vector of length 2 specifying (min, max) allowed weight values
#' @param control List of control parameters for the solver
#' @param verbose Whether to print progress information
#' @param ... Additional arguments passed to methods
#'
#' @return A list containing:
#'   \item{weights}{The optimal weights}
#'   \item{achieved}{The achieved margins}
#'   \item{solution}{Full solution details from solver}
#' @export
regrake <- function(data,
                   formula,
                   population_data,
                   pop_type = c("raw", "weighted", "proportions"),
                   pop_weights = NULL,
                   regularizer = "entropy",
                   lambda = 1,
                   bounds = c(0.1, 10),
                   control = list(),
                   verbose = FALSE,
                   ...) {

  # Early input validation
  pop_type <- match.arg(pop_type)
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
    target_values = target_values
  )
  if (verbose) {
    cat("Design matrix constructed with", nrow(admm_inputs$design_matrix), "constraints\n")
  }

  # Step 4: Set up solver parameters
  ctrl <- list(
    maxiter = 5000,
    rho = 50,
    eps_rel = 1e-5,
    eps_abs = 1e-5
  )
  ctrl[names(control)] <- control

  # Step 5: Call solver
  if (verbose) start_time <- proc.time()

  solution <- admm(
    F = admm_inputs$design_matrix,
    losses = admm_inputs$losses,
    regularizer = create_regularizer(regularizer),
    lambda = lambda,
    bounds = bounds,
    control = ctrl,
    verbose = verbose
  )

  if (verbose) {
    end_time <- proc.time()
    cat(sprintf("ADMM took %.3f seconds\n",
                (end_time - start_time)["elapsed"]))
  }

  # Step 6: Process results and compute diagnostics
  results <- process_admm_results(solution, admm_inputs, verbose)

  # Return results with appropriate class for methods dispatch
  structure(
    list(
      data = data,
      weights = results$weights,
      achieved = results$achieved,
      solution = solution,
      diagnostics = results$diagnostics,
      call = match.call()  # Store call for reproducibility
    ),
    class = "regrake"
  )
}

# Helper function for input validation
validate_inputs <- function(formula, population_data, pop_type, pop_weights, bounds) {
  if (is.null(formula)) {
    stop("Formula must be specified. For direct solver access, use admm() instead.")
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
create_regularizer <- function(regularizer) {
  # Validate regularizer type
  regularizer <- match.arg(
    regularizer,
    choices = c("entropy", "zero", "kl", "boolean"),
    several.ok = FALSE
  )

  # TODO: Implement regularizers
  # entropy: -sum(w * log(w))  [default]
  # zero: no regularization
  # kl: KL divergence from initial weights
  # boolean: boolean/discrete regularization for subset selection

  # For now, error with clear message
  stop(
    "Regularizer '", regularizer, "' not yet implemented\n",
    "Available regularizers will be: entropy, zero, kl, boolean",
    call. = FALSE
  )
}

# Helper to process solution and compute diagnostics
process_admm_results <- function(solution, admm_inputs, verbose) {
  # TODO: Implement solution processing
  stop("Not implemented")
}

# Helper to calculate max percent difference
max_pct_diff <- function(achieved, desired) {
  diff <- abs(achieved - desired)
  pct_diff <- ifelse(desired != 0, (diff / abs(desired)) * 100, NA)
  max(pct_diff, na.rm = TRUE)
}

# Helper to split achieved values by loss functions
split_by_losses <- function(means, losses) {
  start <- 1
  result <- vector("list", length(losses))
  for (i in seq_along(losses)) {
    end <- start + length(losses[[i]]$targets) - 1
    result[[i]] <- means[start:end]
    start <- end + 1
  }
  result
}