#' Loss Functions for Regularized Survey Weighting
#'
#' These functions implement different loss metrics for survey weight optimization:
#' - equality_loss: Absolute difference for exact matching
#' - l2_loss: Squared error loss
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

#' @describeIn losses Squared error loss
#' @return Numeric vector of squared differences between x and target
#' @export
#' @examples
#' l2_loss(c(0.1, 0.2), c(0.15, 0.25))
l2_loss <- function(x, target) {
  if (length(x) != length(target)) {
    rlang::abort(
      "Inputs must have equal lengths",
      i = "x has length {length(x)}, but target has length {length(target)}",
      class = "regrake_length_error"
    )
  }
  (x - target)^2
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