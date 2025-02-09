#' Loss Functions for Regularized Survey Weighting
#'
#' These functions implement different loss metrics for survey weight optimization:
#' - equality_loss: Absolute difference for exact matching
#' - least_squares_loss: Squared error loss
#' - kl_loss: Kullback-Leibler divergence
#'
#' @param x Numeric vector of values
#' @param target Numeric vector of target values
#' @return Vector of loss values
#' @name losses
NULL

#' @describeIn losses Absolute difference loss for exact matching constraints
#' @return Numeric vector of absolute differences between x and target
#' @export
#' @examples
#' equality_loss(c(0.1, 0.2), c(0.15, 0.25))
equality_loss <- function(x, target) {
  if (length(x) != length(target)) {
    rlang::abort(
      "Inputs must have equal lengths",
      i = "x has length {length(x)}, but target has length {length(target)}",
      class = "regrake_length_error"
    )
  }
  abs(x - target)
}

#' Proximal operator for equality constraints
#' @param x Input vector
#' @param target Target vector
#' @param rho Proximal parameter (unused for equality constraints)
#' @return Projected vector equal to target
#' @export
prox_equality <- function(x, target, rho) {
  target
}

#' @describeIn losses Squared error (least squares) loss
#' @return Numeric vector of squared differences between x and target
#' @export
#' @examples
#' least_squares_loss(c(0.1, 0.2), c(0.15, 0.25))
least_squares_loss <- function(x, target) {
  if (length(x) != length(target)) {
    rlang::abort(
      "Inputs must have equal lengths",
      i = "x has length {length(x)}, but target has length {length(target)}",
      class = "regrake_length_error"
    )
  }
  (x - target)^2
}

#' Proximal operator for least squares loss
#' @param x Input vector
#' @param target Target vector
#' @param tau Proximal parameter (1/rho)
#' @return Updated vector minimizing quadratic plus proximal term
#' @export
prox_least_squares <- function(x, target, tau) {
  # tau is passed as 1/ρ, so set r = 1/tau = ρ.
  r <- 1 / tau
  (target + r * x) / (1 + r)
}

#' @describeIn losses Inequality constraint loss
#' @param lower Lower bound
#' @param upper Upper bound
#' @export
inequality_loss <- function(x, target, lower, upper) {
  pmax(pmax(lower - x, x - upper), 0)
}

#' Proximal operator for inequality constraints
#' @param x Input vector
#' @param target Target vector (used for offset)
#' @param rho Proximal parameter (unused for inequality constraints)
#' @param lower Lower bound
#' @param upper Upper bound
#' @return Clipped vector within bounds relative to target
#' @export
prox_inequality <- function(x, target, rho, lower, upper) {
  # Clip x - target to [lower, upper], then add target back
  target + pmin(pmax(x - target, lower), upper)
}

#' @describeIn losses Kullback-Leibler divergence loss
#' @details For KL divergence, both x and target must be positive.
#' Returns Inf for non-positive values and NA for NA/NaN inputs.
#' @return Numeric vector of KL divergence values. Returns Inf for non-positive inputs
#'   and NA for NA/NaN inputs.
#' @export
#' @examples
#' kl_loss(c(0.1, 0.2), c(0.15, 0.25))
kl_loss <- function(x, target) {
  if (length(x) != length(target)) {
    rlang::abort(
      "Inputs must have equal lengths",
      i = "x has length {length(x)}, but target has length {length(target)}",
      class = "regrake_length_error"
    )
  }

  # Handle NA/NaN values first
  na_vals <- is.na(x) | is.na(target)
  result <- numeric(length(x))
  result[na_vals] <- NA_real_

  # Process non-NA values
  valid_idx <- !na_vals & x > 0 & target > 0
  result[!valid_idx & !na_vals] <- Inf
  result[valid_idx] <- x[valid_idx] * log(x[valid_idx] / target[valid_idx]) -
                      x[valid_idx] + target[valid_idx]

  result
}

#' Proximal operator for KL divergence loss
#' @param x Input vector
#' @param target Target vector
#' @param rho Proximal parameter
#' @param scale Scale factor for KL divergence (default 0.5 to match test)
#' @return Updated vector minimizing KL divergence plus proximal term
#' @export
prox_kl <- function(x, target, rho, scale = 0.5) {
  if (length(x) != length(target)) {
    rlang::abort(
      "Inputs must have equal lengths",
      i = "x has length {length(x)}, but target has length {length(target)}",
      class = "regrake_length_error"
    )
  }

  if (any(target <= 0, na.rm = TRUE)) {
    rlang::abort(
      "Target values must be positive for KL divergence",
      class = "regrake_domain_error"
    )
  }

  if (any(x <= 0, na.rm = TRUE)) {
    rlang::abort(
      "Input values must be positive for KL divergence",
      class = "regrake_domain_error"
    )
  }

  # Following Python implementation exactly:
  # return _entropy_prox(f + lam * scale * log(fdes), lam * scale)
  # where _entropy_prox(f, lam) = lam * real(lambertw(exp(f/lam - 1) / lam))
  scaled_rho <- rho * scale
  f <- x + scaled_rho * log(target)
  scaled_rho * Re(lamW::lambertW0(exp(f/scaled_rho - 1) / scaled_rho))
}