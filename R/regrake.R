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

  # Input validation
  pop_type <- match.arg(pop_type)

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

  # Parse formula and compute targets
  parsed <- parse_raking_formula(
    formula = formula,
    data = data,
    population_data = population_data,
    pop_type = pop_type,
    pop_weights = pop_weights
  )

  F <- parsed$design_matrix
  losses <- parsed$losses

  # Handle missing values
  na_idx <- which(is.na(F), arr.ind = TRUE)
  if (length(na_idx) > 0) {
    desired <- unlist(lapply(losses, function(l) l$targets))
    for (i in unique(na_idx[,1])) {
      cols <- na_idx[na_idx[,1] == i, 2]
      F[i, cols] <- desired[i]
    }
  }

  # Validate dimensions
  total_targets <- sum(sapply(losses, function(l) length(l$targets)))
  if (nrow(F) > total_targets) {
    warning("A loss is not defined for all columns. Check inputs carefully.")
  }
  if (nrow(F) < total_targets) {
    warning("More losses specified than columns. Check inputs carefully.")
  }

  # Set up solver control parameters
  ctrl <- list(
    maxiter = 5000,
    rho = 50,
    eps_rel = 1e-5,
    eps_abs = 1e-5
  )
  ctrl[names(control)] <- control

  # Call solver
  if (verbose) start_time <- proc.time()

  solution <- admm(
    F = F,
    losses = losses,
    regularizer = regularizer,
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

  # Calculate achieved margins
  means <- as.vector(F %*% solution$weights)
  achieved <- split_by_losses(means, losses)

  if (verbose) {
    # Calculate max percent difference
    targets <- unlist(lapply(losses, function(l) l$targets))
    max_diff <- max_pct_diff(achieved, targets)
    cat(sprintf("Largest percent difference between desired and achieved: %.2f%%\n",
                max_diff))
  }

  # Return results with sample data and weights
  structure(
    list(
      data = data,
      weights = solution$weights,
      achieved = achieved,
      solution = solution
    ),
    class = "regrake"
  )
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