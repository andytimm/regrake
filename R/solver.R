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

#' Project vector onto bounded probability simplex
#'
#' Projects a vector onto the probability simplex with per-element bounds.
#' Uses a sorting-based algorithm that finds the exact threshold by binary
#' search over breakpoints where elements transition between bounds.
#'
#' @param v Numeric vector to project
#' @param lower Lower bound for each element (scalar or vector)
#' @param upper Upper bound for each element (scalar or vector)
#' @param z Target sum (default 1)
#' @return Projected vector that sums to z with elements in `[lower, upper]`
#' @keywords internal
projection_bounded_simplex <- function(v, lower, upper, z = 1) {
  n <- length(v)

  # Handle scalar bounds
  if (length(lower) == 1) lower <- rep(lower, n)
  if (length(upper) == 1) upper <- rep(upper, n)

  # Feasibility check: sum(lower) <= z <= sum(upper)
  if (sum(lower) > z + 1e-10) {
    rlang::warn(
      c(
        "Weight bounds are infeasible: sum of lower bounds exceeds target sum.",
        i = "Returning best-effort projection with rescaled clipped values."
      ),
      class = "regrake_bounds_infeasible"
    )
    result <- pmax(pmin(rep(z / n, n), upper), lower)
    return(result * z / sum(result))
  }
  if (sum(upper) < z - 1e-10) {
    rlang::warn(
      c(
        "Weight bounds are infeasible: sum of upper bounds below target sum.",
        i = "Returning best-effort projection with rescaled clipped values."
      ),
      class = "regrake_bounds_infeasible"
    )
    result <- pmax(pmin(rep(z / n, n), upper), lower)
    return(result * z / sum(result))
  }

  # Quick check: if simple clipping satisfies sum constraint, we're done
  w_clipped <- pmin(pmax(v, lower), upper)
  if (abs(sum(w_clipped) - z) < 1e-10) {
    return(w_clipped)
  }

  # Sorting-based approach: find threshold theta where sum(clip(v - theta)) = z
  # The function f(theta) = sum(clip(v - theta, lower, upper)) is piecewise linear
  # Breakpoints occur where elements hit their bounds

  # Compute breakpoints: theta values where element i transitions
  theta_upper <- v - upper # element i hits upper bound when theta < this

  theta_lower <- v - lower # element i hits lower bound when theta > this

  # Sort all breakpoints
  breakpoints <- sort(c(theta_lower, theta_upper))

  # Binary search for interval containing solution
  # f(theta) is monotonically decreasing in theta
  left <- 1L

  right <- 2L * n

  while (right - left > 1L) {
    mid <- (left + right) %/% 2L
    s <- sum(pmax(pmin(v - breakpoints[mid], upper), lower))
    if (s > z) {
      left <- mid
    } else {
      right <- mid
    }
  }

  # Exact solution within interval [breakpoints[left], breakpoints[right]]
  # f(theta) is linear here, so we can interpolate
  theta_lo <- breakpoints[left]
  theta_hi <- breakpoints[right]
  s_lo <- sum(pmax(pmin(v - theta_lo, upper), lower))
  s_hi <- sum(pmax(pmin(v - theta_hi, upper), lower))

  # Linear interpolation to find exact theta
  if (abs(s_lo - s_hi) < 1e-15) {
    theta <- theta_lo
  } else {
    theta <- theta_lo + (s_lo - z) * (theta_hi - theta_lo) / (s_lo - s_hi)
  }

  pmax(pmin(v - theta, upper), lower)
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
#' @return List containing s_norm (dual residual norm), r_norm (primal residual
#'   norm), eps_pri (primal feasibility threshold), and eps_dual (dual
#'   feasibility threshold)
#' @keywords internal
compute_norms_and_epsilons <- function(
  f,
  w,
  w_old,
  y,
  z,
  u,
  F,
  rho,
  eps_abs,
  eps_rel,
  Fw = NULL
) {
  if (is.null(Fw)) {
    Fw <- drop(as.matrix(F %*% w))
  }

  # Compute differences once
  diff_fw <- Fw - f
  diff_w <- w - w_old

  # Cache shared computation
  ss_fw <- sum(diff_fw^2)

  # Dual residual: ||rho * [Fw-f, w-w_old, w-w_old]||
  s_norm <- rho * sqrt(ss_fw + 2 * sum(diff_w^2))

  # Primal residual: ||[f-Fw, 0, 0]|| - zeros don't contribute
  r_norm <- sqrt(ss_fw)

  # Total number of residual components
  p <- length(f) + 2 * length(w)

  # Compute norms directly without concatenation
  w_sq_sum <- sum(w^2)
  Ax_k_norm <- sqrt(sum(Fw^2) + 2 * w_sq_sum) # ||[Fw, w, w]||
  Bz_k_norm <- sqrt(sum(f^2) + 2 * w_sq_sum) # ||[f, w, w]||
  ATy_k_norm <- rho * sqrt(sum(y^2) + sum(z^2) + sum(u^2)) # ||rho * [y, z, u]||

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
#' @param losses List of loss functions, each containing fn (loss function),
#'   target (target values), prox (proximal operator), and optionally
#'   lower/upper (bounds for inequality constraints)
#' @param reg Regularizer object with prox (proximal operator function)
#' @param lam Regularization strength parameter
#' @param control List of control parameters: rho (ADMM penalty, default 50),
#'   maxiter (max iterations, default 5000), eps_abs and eps_rel (convergence
#'   tolerances, default 1e-5)
#' @param verbose Print convergence progress (default FALSE)
#' @param bounds Optional numeric vector of length 2 specifying (min, max) weight
#'   bounds as ratios to uniform weight. Only used when bounds_method = "hard".
#' @param bounds_method Method for enforcing bounds: "soft" (via regularizer limit,
#'   default) or "hard" (via bounded simplex projection).
#' @return List containing f (final f vector), w (final weights), w_bar
#'   (projected weights), w_tilde (regularized weights), y/z/u (dual variables),
#'   and w_best (best solution found)
#' @keywords internal
admm <- function(
  F,
  losses,
  reg,
  lam,
  control = list(),
  verbose = FALSE,
  bounds = NULL,
  bounds_method = "soft"
) {
  # Set defaults for control parameters
  ctrl <- list(
    rho = 50,
    maxiter = 5000,
    eps_abs = 1e-5,
    eps_rel = 1e-5
  )
  ctrl[names(control)] <- control # Override defaults with user values

  # Convert F to sparse matrix first
  if (!inherits(F, "Matrix")) {
    F <- Matrix::Matrix(F, sparse = TRUE)
  }

  m <- nrow(F)
  n <- ncol(F)

  # Pre-allocate vectors
  rhs <- numeric(n + m)

  # Initialize warm start values
  f <- Matrix::rowMeans(F)
  w <- rep(1 / n, n)
  w_bar <- rep(1 / n, n)
  w_tilde <- rep(1 / n, n)
  y <- rep(0, m)
  z <- rep(0, n)
  u <- rep(0, n)

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
  if (ct_cum != m) {
    stop("Loss dimensions mismatch")
  }

  w_best <- NULL
  best_objective_value <- Inf
  converged <- FALSE

  # Pre-compute F %*% w for first iteration's f update
  Fw_all <- drop(as.matrix(F %*% w))

  for (k in seq_len(ctrl$maxiter)) {
    # f update - use pre-computed Fw_all and index (much faster than row extraction)
    for (i in seq_along(losses)) {
      l <- losses[[i]]
      idx <- seq(l$start, l$end)
      Fw <- Fw_all[idx]
      if (!is.null(l$lower) || !is.null(l$upper)) {
        f[idx] <- l$prox(Fw - y[idx], l$target, 1 / ctrl$rho, l$lower, l$upper)
      } else {
        f[idx] <- l$prox(Fw - y[idx], l$target, 1 / ctrl$rho)
      }
    }

    # Update w_tilde and w_bar
    w_tilde <- reg$prox(w - z, lam / ctrl$rho)

    # Use bounded simplex projection if hard bounds requested
    if (!is.null(bounds) && bounds_method == "hard") {
      w_bar <- projection_bounded_simplex(w - u, bounds[1] / n, bounds[2] / n)
    } else {
      w_bar <- projection_simplex(w - u)
    }

    # Solve for w_new using cached factorization
    Ft_fy <- Matrix::crossprod(F, f + y)
    rhs[1:n] <- Ft_fy + w_tilde + z + w_bar + u
    w_new <- Matrix::solve(Q_factor, rhs)[1:n]

    w_old <- w
    w <- w_new

    # Compute F %*% w once - used for dual updates, convergence, and next iteration's f update
    Fw_all <- drop(as.matrix(F %*% w))

    # Dual updates
    y <- f - Fw_all + y
    z <- z + (w_tilde - w)
    u <- u + (w_bar - w)

    # Check convergence, passing the cached Fw_all
    norms <- compute_norms_and_epsilons(
      f = f,
      w = w,
      w_old = w_old,
      y = y,
      z = z,
      u = u,
      F = F,
      rho = ctrl$rho,
      eps_abs = ctrl$eps_abs,
      eps_rel = ctrl$eps_rel,
      Fw = Fw_all
    )

    if (verbose && k %% 50 == 0) {
      message(sprintf(
        "It %03d / %03d | %8.5e | %8.5e",
        k,
        ctrl$maxiter,
        norms$r_norm / norms$eps_pri,
        norms$s_norm / norms$eps_dual
      ))
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
          message(sprintf(
            "Found better objective value: %3.5f -> %3.5f",
            best_objective_value,
            objective
          ))
        }
        best_objective_value <- objective
        w_best <- w_tilde
      }
    }

    # Check for numerical issues (before convergence so NaN doesn't slip through)
    if (is.nan(norms$r_norm) || is.nan(norms$s_norm)) {
      stop(
        "Numerical error in optimization. This usually indicates a poorly formulated ",
        "problem. Common causes include excessively high or low values of lambda or rho."
      )
    }

    # Check convergence
    if (norms$r_norm <= norms$eps_pri && norms$s_norm <= norms$eps_dual) {
      converged <- TRUE
      break
    }
  }

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
    w_best = w_best,
    converged = converged,
    iterations = k,
    primal_residual = norms$r_norm,
    dual_residual = norms$s_norm
  )
}
