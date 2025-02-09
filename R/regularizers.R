#' Regularizers for Survey Weight Optimization
#'
#' These functions implement different regularization methods:
#' - zero_regularizer: No regularization
#' - entropy_regularizer: Negative entropy regularization
#' - kl_regularizer: KL divergence from prior
#' - sum_squares_regularizer: Sum of squared weights
#'
#' @param w Numeric vector of weights
#' @param lambda Regularization strength
#' @return Vector of regularized weights
#' @name regularizers
NULL

#' @describeIn regularizers No regularization (identity function)
#' @export
#' @examples
#' zero_regularizer(c(0.1, 0.2), 0.5)
zero_regularizer <- function(w, lambda) {
  if (!is.numeric(w)) {
    rlang::abort(
      "Input must be numeric",
      i = "w is of class {class(w)}",
      class = "regrake_type_error"
    )
  }
  w
}

#' @describeIn regularizers Negative entropy regularization
#' @param limit Optional upper bound on weight magnitudes
#' @export
#' @examples
#' entropy_regularizer(c(0.1, 0.2), 0.5)
entropy_regularizer <- function(w, lambda, limit = NULL) {
  if (!is.numeric(w)) {
    rlang::abort(
      "Input must be numeric",
      i = "w is of class {class(w)}",
      class = "regrake_type_error"
    )
  }
  if (!is.null(limit) && limit <= 1) {
    rlang::abort(
      "Limit must be greater than 1",
      i = "limit is {limit}",
      class = "regrake_value_error"
    )
  }

  what <- lambda * lamW::lambertW0(exp(w / lambda - 1) / lambda)
  if (!is.null(limit)) {
    what <- pmin(pmax(what, 1 / (limit * length(w))), limit / length(w))
  }
  what
}

#' @describeIn regularizers KL divergence from prior regularization
#' @param prior Numeric vector of prior weights (must sum to 1)
#' @param limit Optional upper bound on weight magnitudes
#' @export
kl_regularizer <- function(w, lambda, prior, limit = NULL) {
  if (!is.numeric(prior)) {
    rlang::abort("Prior must be numeric", class = "regrake_type_error")
  }
  if (abs(sum(prior) - 1) > 1e-6) {
    rlang::abort("Prior must sum to 1", class = "regrake_value_error")
  }
  entropy_regularizer(w + lambda * log(prior), lambda, limit)
}

#' @describeIn regularizers Equality regularization
#' @export
equality_regularizer <- function(w, lambda) {
  w
}

#' @describeIn regularizers Least squares regularization
#' @export
sum_squares_regularizer <- function(w, lambda) {
  w / (1 + 2 * lambda)
}

#' Proximal operator for KL regularizer
#' @param w Input vector
#' @param lam Regularization parameter
#' @param tau Proximal parameter
#' @param prior Prior weights (default uniform)
#' @param limit Optional upper bound on weight magnitudes
#' @return Updated vector minimizing KL divergence plus proximal term
#' @export
prox_kl_reg <- function(w, lam, tau = 1, prior = NULL, limit = NULL) {
  if (is.null(prior)) {
    prior <- rep(1/length(w), length(w))
  }
  # Exactly match Python implementation
  what <- lam * Re(lamW::lambertW0(exp((w + lam * log(prior)) / lam - 1) / lam))
  if (!is.null(limit)) {
    what <- pmin(pmax(what, 1/(limit * length(w))), limit/length(w))
  }
  what
}

#' Proximal operator for equality regularizer
#' @param w Input vector
#' @param lam Regularization parameter
#' @return Original vector (identity operation)
#' @export
prox_equality_reg <- function(w, lam) {
  w
}

#' Proximal operator for sum squares regularizer
#' @param w Input vector
#' @param lam Regularization parameter
#' @return Updated vector minimizing sum squares plus proximal term
#' @export
prox_sum_squares_reg <- function(w, lam) {
  w / (1 + 2 * lam)
}