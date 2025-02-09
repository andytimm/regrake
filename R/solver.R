#' Project vector onto probability simplex
#'
#' Projects a vector onto the probability simplex using a sort-based algorithm.
#' While algorithms with better theoretical complexity exist (e.g., Blondel
#'  2014, Condat 2016, Dai/Chen 2022), with a decent bit of testing I found this
#'  sort-based approach is hard to beat in practice.
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
#' for ADMM convergence checking.
#'
#' @param f Current f vector
#' @param w Current w vector
#' @param w_old Previous w vector
#' @param y Current y vector
#' @param z Current z vector
#' @param u Current u vector
#' @param F Design matrix
#' @param rho ADMM penalty parameter
#' @param eps_abs Absolute convergence tolerance
#' @param eps_rel Relative convergence tolerance
#' @return List containing s_norm, r_norm, eps_pri, eps_dual
#' @keywords internal
compute_norms_and_epsilons <- function(f, w, w_old, y, z, u, F, rho, eps_abs, eps_rel) {
  # Use dense form for calculations (if F is sparse)
  Fw <- drop(as.matrix(F %*% w))

  # s uses differences from the previous iteration.
  s <- rho * c(Fw - f, w - w_old, w - w_old)
  s_norm <- sqrt(sum(s^2))

  # Dual residual: only the block f - Fw matters.
  r <- c(f - Fw, rep(0, length(w)), rep(0, length(w)))
  r_norm <- sqrt(sum(r^2))

  # p is the total number of components for the residuals.
  p <- nrow(F) + 2 * length(w)
  # Change epsilon definitions to follow the Python code.
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
#' Implements the ADMM algorithm for solving the regularized survey weighting problem.
#'
#' @param F Design matrix
#' @param losses List of loss functions
#' @param reg Regularizer object
#' @param lam Regularization parameter
#' @param rho ADMM penalty parameter (default 50)
#' @param maxiter Maximum iterations (default 5000)
#' @param warm_start List of initial values (optional)
#' @param verbose Print progress (default FALSE)
#' @param eps_abs Absolute convergence tolerance (default 1e-5)
#' @param eps_rel Relative convergence tolerance (default 1e-5)
#' @return List containing solution vectors and convergence information
#' @export
admm <- function(F, losses, reg, lam, rho = 50, maxiter = 5000,
                warm_start = list(), verbose = FALSE,
                eps_abs = 1e-5, eps_rel = 1e-5) {

  # Get dimensions
  m <- nrow(F)
  n <- ncol(F)
  ms <- sapply(losses, function(l) l$m)

  # Initialize warm start values
  f <- warm_start$f %||% rep(mean(F), m)
  w <- warm_start$w %||% rep(1/n, n)
  w_bar <- warm_start$w_bar %||% rep(1/n, n)
  w_tilde <- warm_start$w_tilde %||% rep(1/n, n)
  y <- warm_start$y %||% rep(0, m)
  z <- warm_start$z %||% rep(0, n)
  u <- warm_start$u %||% rep(0, n)

  # Convert F to sparse matrix if not already
  if (!inherits(F, "Matrix")) {
    F <- Matrix::Matrix(F, sparse = TRUE)
  }

  # Construct and factorize Q matrix
  Q <- Matrix::bdiag(2 * Matrix::Diagonal(n), t(F)) %*%
       Matrix::bdiag(Matrix::Diagonal(n), -Matrix::Diagonal(m))

  # Initialize best solution tracking
  w_best <- NULL
  best_objective_value <- Inf

  if (verbose) {
    message("Iteration     | ||r||/eps_pri | ||s||/eps_dual")
  }

  # Main ADMM loop
  for (k in seq_len(maxiter)) {
    ct_cum <- 0

    # Update f block
    for (l in losses) {
      idx <- seq(ct_cum + 1, ct_cum + l$m)
      f_update <- l$prox(
        F[idx, , drop = FALSE] %*% w - y[idx],
        1/rho
      )
      f[idx] <- f_update
      ct_cum <- ct_cum + l$m
    }

    # Update w_tilde and w_bar
    w_tilde <- reg$prox(w - z, lam/rho)
    w_bar <- projection_simplex(w - u)

    # Solve for w_new
    rhs <- c(
      t(F) %*% (f + y) + w_tilde + z + w_bar + u,
      rep(0, m)
    )

    # Use sparse solver for w update
    # Note: Need to implement or find equivalent to qdldl for R
    # For now using Matrix::solve, but should be replaced with more efficient solver
    sol <- try(Matrix::solve(Q, rhs), silent = TRUE)
    if (inherits(sol, "try-error")) {
      warning("Matrix solve failed, trying with regularization")
      sol <- Matrix::solve(Q + 1e-8 * Matrix::Diagonal(nrow(Q)), rhs)
    }
    w_new <- sol[1:n]

    # Store old w and update
    w_old <- w
    w <- w_new

    # Dual updates
    y <- y + (f - F %*% w)
    z <- z + (w_tilde - w)
    u <- u + (w_bar - w)

    # Check convergence
    norms <- compute_norms_and_epsilons(f, w, w_old, y, z, u, F, rho, eps_abs, eps_rel)

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

  # Set final best weights
  if (!inherits(reg, "BooleanRegularizer")) {
    w_best <- w_bar
  }

  # Return results
  list(
    f = f,
    w = w,
    w_bar = w_bar,
    w_tilde = w_tilde,
    y = y,
    z = z,
    u = u,
    w_best = w_best
  )
}