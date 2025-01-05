#' Project vector onto probability simplex
#'
#' Projects a vector onto the probability simplex using a sort-based algorithm.
#' While algorithms with better theoretical complexity exist (e.g., Blondel 2014,
#' Condat 2016, Dai/Chen 2022), extensive testing shows this sort-based approach
#' is hard to beat in practice due to highly optimized sorting implementations.
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