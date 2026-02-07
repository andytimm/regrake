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
  w
}

#' @describeIn regularizers Negative entropy regularization
#' @param limit Optional upper bound on weight magnitudes
#' @export
#' @examples
#' entropy_regularizer(c(0.1, 0.2), 0.5)
entropy_regularizer <- function(w, lambda, limit = NULL) {
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
#' @examples
#' kl_regularizer(c(0.1, 0.2), 0.5, prior = c(0.5, 0.5))
kl_regularizer <- function(w, lambda, prior, limit = NULL) {
  if (!is.numeric(prior)) {
    rlang::abort("Prior must be numeric", class = "regrake_type_error")
  }
  if (abs(sum(prior) - 1) > 1e-6) {
    rlang::abort("Prior must sum to 1", class = "regrake_value_error")
  }
  entropy_regularizer(w + lambda * log(prior), lambda, limit)
}

#' @describeIn regularizers Least squares regularization
#' @export
#' @examples
#' sum_squares_regularizer(c(0.1, 0.2), 0.5)
sum_squares_regularizer <- function(w, lambda) {
  w / (1 + 2 * lambda)
}

#' Proximal operator for KL regularizer
#' @param w Input vector
#' @param lam Regularization parameter
#' @param prior Prior weights (default uniform)
#' @param limit Optional upper bound on weight magnitudes
#' @return Updated vector minimizing KL divergence plus proximal term
#' @export
#' @examples
#' prox_kl_reg(c(0.1, 0.2), lam = 0.5)
prox_kl_reg <- function(w, lam, prior = NULL, limit = NULL) {
  if (is.null(prior)) {
    prior <- rep(1 / length(w), length(w))
  }
  # Exactly match Python implementation
  what <- lam * Re(lamW::lambertW0(exp((w + lam * log(prior)) / lam - 1) / lam))
  if (!is.null(limit)) {
    what <- pmin(pmax(what, 1 / (limit * length(w))), limit / length(w))
  }
  what
}

#' Proximal operator for equality regularizer
#' @param w Input vector
#' @param lam Regularization parameter
#' @return Original vector (identity operation)
#' @export
#' @examples
#' prox_equality_reg(c(0.1, 0.2), lam = 0.5)
prox_equality_reg <- function(w, lam) {
  w
}

#' Proximal operator for sum squares regularizer
#' @param w Input vector
#' @param lam Regularization parameter
#' @return Updated vector minimizing sum squares plus proximal term
#' @export
#' @examples
#' prox_sum_squares_reg(c(0.1, 0.2), lam = 0.5)
prox_sum_squares_reg <- function(w, lam) {
  w / (1 + 2 * lam)
}

#' Proximal operator for boolean regularizer
#'
#' Selects top-k weights and assigns them equal weight 1/k.
#' All other weights are set to zero. This is used for representative
#' sample selection where exactly k samples should be selected.
#'
#' @param w Input vector of weights
#' @param lam Regularization parameter (unused, kept for interface consistency)
#' @param k Number of samples to select
#' @return Vector with k non-zero entries, each equal to 1/k
#' @export
#' @examples
#' prox_boolean_reg(c(0.3, 0.1, 0.6), lam = 0.5, k = 2)
prox_boolean_reg <- function(w, lam, k) {
  n <- length(w)
  if (k < 1 || k > n) {
    rlang::abort("k must be between 1 and length(w)")
  }
  result <- rep(0, n)
  top_k_idx <- order(w, decreasing = TRUE)[1:k]
  result[top_k_idx] <- 1 / k
  result
}
