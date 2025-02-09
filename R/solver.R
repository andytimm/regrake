#' Project vector onto probability simplex
#'
#' Projects a vector onto the probability simplex using a sort-based algorithm.
#' While algorithms with better theoretical complexity exist (e.g., Blondel
#' 2014, Condat 2016, Dai/Chen 2022), with a decent bit of testing I found this
#' sort-based approach is hard to beat in practice.
#'
#' @param v Numeric vector to project
#' @param z Target sum (default 1)
#' @return Projected vector that sums to z with non-negative elements
#' @keywords internal
projection_simplex <- function(v, z = 1) {
  n <- length(v)
  u <- sort(v, decreasing = TRUE)
  cssv <- cumsum(u)
  rho <- max(which(u > (cssv - z) / (1:n)))
  theta <- (cssv[rho] - z) / rho
  pmax(v - theta, 0)
}

#' Compute ADMM convergence metrics
#'
#' Computes primal and dual residual norms and their corresponding epsilon thresholds
#' for ADMM convergence checking. Implementation follows Boyd et al. (2011) ADMM paper.
#'
#' @param f Current f vector
#' @param w Current w vector
#' @param w_old Previous w vector
#' @param y Current y vector
#' @param z Current z vector
#' @param u Current u vector
#' @param F Design matrix (sparse)
#' @param rho ADMM penalty parameter
#' @param eps_abs Absolute convergence tolerance
#' @param eps_rel Relative convergence tolerance
#' @return List containing:
#'   \item{s_norm}{Dual residual norm}
#'   \item{r_norm}{Primal residual norm}
#'   \item{eps_pri}{Primal feasibility threshold}
#'   \item{eps_dual}{Dual feasibility threshold}
#' @keywords internal
compute_norms_and_epsilons <- function(f, w, w_old, y, z, u, F, rho, eps_abs, eps_rel) {
  # Compute residuals using dense operations for numerical stability
  Fw <- drop(as.matrix(F %*% w))

  # Dual residual using differences from previous iteration
  s <- rho * c(Fw - f, w - w_old, w - w_old)
  s_norm <- sqrt(sum(s^2))

  # Primal residual (only f - Fw block matters)
  r <- c(f - Fw, rep(0, length(w)), rep(0, length(w)))
  r_norm <- sqrt(sum(r^2))

  # Total number of residual components
  p <- nrow(F) + 2 * length(w)

  # Compute epsilon thresholds following Boyd et al.
  Ax <- c(f, w, w)
  Ax_k_norm <- sqrt(sum(Ax^2))
  Bz <- c(w, w, w)
  Bz_k_norm <- sqrt(sum(Bz^2))
  ATy <- rho * c(y, z, u)
  ATy_k_norm <- sqrt(sum(ATy^2))

  eps_pri <- sqrt(p) * eps_abs + eps_rel * max(Ax_k_norm, Bz_k_norm)
  eps_dual <- sqrt(p) * eps_abs + eps_rel * ATy_k_norm

  list(
    s_norm = s_norm,
    r_norm = r_norm,
    eps_pri = eps_pri,
    eps_dual = eps_dual
  )
}

#' ADMM solver for regularized survey weighting
#'
#' Implements the ADMM (Alternating Direction Method of Multipliers) algorithm for
#' solving regularized survey weighting problems. The algorithm minimizes a sum of loss
#' functions subject to simplex constraints and regularization.
#'
#' The implementation uses sparse matrix operations and cached Cholesky factorization
#' for efficiency. Numerical stability is ensured through careful matrix conditioning
#' and damping.
#'
#' @param F Design matrix (converted to sparse internally)
#' @param losses List of loss functions, each containing:
#'   \item{fn}{Loss function}
#'   \item{target}{Target values}
#'   \item{prox}{Proximal operator}
#'   \item{lower,upper}{Optional bounds for inequality constraints}
#' @param reg Regularizer object with:
#'   \item{fn}{Regularization function}
#'   \item{prox}{Proximal operator}
#' @param lam Regularization strength parameter
#' @param rho ADMM penalty parameter (default 50)
#' @param maxiter Maximum iterations (default 5000)
#' @param warm_start List of initial values for variables (optional)
#' @param verbose Print convergence progress (default FALSE)
#' @param eps_abs Absolute convergence tolerance (default 1e-5)
#' @param eps_rel Relative convergence tolerance (default 1e-5)
#' @return List containing:
#'   \item{f}{Final f vector}
#'   \item{w}{Final weights}
#'   \item{w_bar}{Projected weights}
#'   \item{w_tilde}{Regularized weights}
#'   \item{y,z,u}{Final dual variables}
#'   \item{w_best}{Best solution found (w_bar or w_tilde for boolean regularizer)}
#' @export
admm <- function(F, losses, reg, lam, rho = 50, maxiter = 5000,
                warm_start = list(), verbose = FALSE,
                eps_abs = 1e-5, eps_rel = 1e-5) {
  # Convert F to sparse matrix first
  if (!inherits(F, "Matrix")) {
    F <- Matrix::Matrix(F, sparse = TRUE)
  }

  m <- nrow(F)
  n <- ncol(F)

  # Pre-allocate vectors
  rhs <- numeric(n + m)

  # Initialize warm start values
  f <- if (is.null(warm_start$f)) Matrix::rowMeans(F) else warm_start$f
  w <- if (is.null(warm_start$w)) rep(1/n, n) else warm_start$w
  w_bar <- if (is.null(warm_start$w_bar)) rep(1/n, n) else warm_start$w_bar
  w_tilde <- if (is.null(warm_start$w_tilde)) rep(1/n, n) else warm_start$w_tilde
  y <- if (is.null(warm_start$y)) rep(0, m) else warm_start$y
  z <- if (is.null(warm_start$z)) rep(0, n) else warm_start$z
  u <- if (is.null(warm_start$u)) rep(0, n) else warm_start$u

  # Cache matrix operations
  # Note: This is a key difference from the Python implementation which uses QDLDL.
  # While both are LDL^T factorizations, QDLDL (used in Python) is specifically
  # designed for quasi-definite matrices and includes built-in regularization.
  # Here we use R's sparse Cholesky with explicit damping for stability:
  # 1. Construct the KKT matrix Q
  Q <- rbind(
    cbind(2 * Matrix::Diagonal(n), Matrix::t(F)),
    cbind(F, -Matrix::Diagonal(m))
  )
  # 2. Add small diagonal perturbation for numerical stability
  damp <- max(1e-8, max(Matrix::rowSums(abs(Q))) * 1e-6)
  Q <- Q + damp * Matrix::Diagonal(nrow(Q))
  # 3. Compute LDL^T factorization with permutation for sparsity
  Q_factor <- Matrix::Cholesky(Q, LDL = TRUE, perm = TRUE)

  # Pre-process losses
  ct_cum <- 0
  for (i in seq_along(losses)) {
    losses[[i]]$m <- length(losses[[i]]$target)
    losses[[i]]$start <- ct_cum + 1
    losses[[i]]$end <- ct_cum + losses[[i]]$m
    ct_cum <- ct_cum + losses[[i]]$m
  }
  if (ct_cum != m) stop("Loss dimensions mismatch")

  w_best <- NULL
  best_objective_value <- Inf

  for (k in seq_len(maxiter)) {
    # Update f block (vectorized)
    f_updates <- lapply(losses, function(l) {
      idx <- seq(l$start, l$end)
      Fw <- Matrix::rowSums(F[idx, , drop = FALSE] * rep(w, each = length(idx)))
      if (!is.null(l$lower) || !is.null(l$upper)) {
        l$prox(Fw - y[idx], l$target, 1/rho, l$lower, l$upper)
      } else {
        l$prox(Fw - y[idx], l$target, 1/rho)
      }
    })
    f <- do.call(c, f_updates)

    # Update w_tilde and w_bar
    w_tilde <- reg$prox(w - z, lam/rho)
    w_bar <- projection_simplex(w - u)

    # Solve for w_new using cached factorization
    Ft_fy <- Matrix::crossprod(F, f + y)
    rhs[1:n] <- Ft_fy + w_tilde + z + w_bar + u
    rhs[(n+1):(n+m)] <- 0
    w_new <- Matrix::solve(Q_factor, rhs)[1:n]

    w_old <- w
    w <- w_new

    # Dual updates (optimized)
    Fw <- Matrix::rowSums(F * rep(w, each = m))
    y <- f - Fw + y
    z <- z + (w_tilde - w)
    u <- u + (w_bar - w)

    # Check convergence
    norms <- compute_norms_and_epsilons(
      f = f,
      w = w,
      w_old = w_old,
      y = y,
      z = z,
      u = u,
      F = F,
      rho = rho,
      eps_abs = eps_abs,
      eps_rel = eps_rel
    )

    if (verbose && k %% 50 == 0) {
      message(sprintf("It %03d / %03d | %8.5e | %8.5e",
                     k, maxiter,
                     norms$r_norm/norms$eps_pri,
                     norms$s_norm/norms$eps_dual))
    }

    # Track best solution for boolean regularizer
    if (inherits(reg, "BooleanRegularizer")) {
      ct_cum <- 0
      objective <- 0
      for (l in losses) {
        idx <- seq(ct_cum + 1, ct_cum + l$m)
        objective <- objective + l$evaluate(F[idx, , drop = FALSE] %*% w_tilde)
        ct_cum <- ct_cum + l$m
      }
      if (objective < best_objective_value) {
        if (verbose) {
          message(sprintf("Found better objective value: %3.5f -> %3.5f",
                         best_objective_value, objective))
        }
        best_objective_value <- objective
        w_best <- w_tilde
      }
    }

    # Check convergence
    if (norms$r_norm <= norms$eps_pri && norms$s_norm <= norms$eps_dual) {
      break
    }

    # Check for numerical issues
    if (is.nan(norms$r_norm) || is.nan(norms$s_norm)) {
      stop("Numerical error in optimization. This usually indicates a poorly formulated ",
           "problem. Common causes include excessively high or low values of lambda or rho.")
    }
  }

  if (!inherits(reg, "BooleanRegularizer")) {
    w_best <- w_bar
  }

  # Return results
  list(f = f, w = w, w_bar = w_bar, w_tilde = w_tilde,
       y = y, z = z, u = u, w_best = w_best)
}