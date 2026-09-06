
#' Compute a diversity coefficient
#'
#' Computes Rao's quadratic diversity coefficient for each column of a data
#' frame, optionally using a provided distance matrix.
#'
#' @param x a data frame or matrix of non-negative values (e.g. abundances).
#'   Rows correspond to entities, columns to samples.
#' @param dis optional object of class `dist`. If `NULL`, a default
#'   Euclidean distance is used, in which case the coefficient reduces to the
#'   Gini-Simpson index \eqn{1 - \sum p_i^2}.
#' @param normalize logical; if `TRUE`, the diversity is scaled by its
#'   theoretical maximum
#' @param na.rm logical; if `TRUE`, columns containing missing values
#'   yield `NA` instead of aborting
#' @param tol numeric convergence tolerance for the iterative maximisation
#'   used by `normalize = TRUE`
#'
#' @return a numeric vector of diversity coefficients, one per column
#'
#' @details
#' The diversity coefficient is defined as
#' \deqn{D = \frac{x^T D^2 x}{2 (\sum x)^2}}{D = t(x) \%*\% D^2 \%*\% x / (2 * sum(x)^2)}
#' where \eqn{x} is a column of `x` and \eqn{D} is the distance matrix.
#'
#' If `normalize = TRUE`, values are divided by the maximum achievable
#' diversity under the given distance matrix. That maximum is found by a
#' fixed-point iteration over the simplex, which is a heuristic: it is not
#' guaranteed to reach the global optimum for an arbitrary distance matrix.
#' A warning is issued when the iteration has not converged within
#' `tol`.
#'
#' @examples
#' set.seed(1)
#' x <- matrix(runif(20), ncol = 4)
#' d <- dist(matrix(rnorm(10), ncol = 2))
#'
#' divCoef(x, d)
#' divCoef(x, d, normalize = TRUE)
#'
#' # without a distance matrix this is the Gini-Simpson index
#' divCoef(matrix(c(1, 1, 1, 1, 0, 0), ncol = 2))
#'
#' @family inequality
#' @concept diversity
#' @concept concentration-index
#' @export
divCoef <- function(x, dis = NULL, normalize = FALSE, na.rm = FALSE,
                    tol = 1e-8) {

  # --- input checks ---
  # 'df' as an argument name masked stats::df and read as "data frame"
  # where a matrix of abundances is meant
  if (!is.matrix(x) && !is.data.frame(x))
    stop("'x' must be a matrix or data.frame")

  x <- as.matrix(x)

  if (!is.numeric(x))
    stop("'x' must be numeric")

  if (any(x < 0, na.rm = TRUE))
    stop("Negative values in 'x' not allowed")

  n <- nrow(x)

  # --- distance handling ---
  if (is.null(dis)) {
    # default: Euclidean distances on simplex vertices
    d2 <- matrix(2, n, n)
    diag(d2) <- 0
    d2 <- d2 / 2
  } else {
    if (!inherits(dis, "dist"))
      stop("'dis' must be of class 'dist'")

    if (!isEuclid(dis))
      warning("Distance matrix is not Euclidean")

    dmat <- as.matrix(dis)
    if (nrow(dmat) != n)
      stop("'x' and 'dis' have incompatible dimensions")

    d2 <- dmat^2 / 2
  }

  # --- compute diversity (vectorized) ---
  colSum <- colSums(x)

  # A logical index containing NA is an error in subassignment, so columns
  # with missing values have to be resolved before div[valid] <- ... is
  # reached: previously any NA in the input died on
  # "NAs are not allowed in subscripted assignments".
  isNA <- is.na(colSum)

  if (any(isNA) && !na.rm)
    stop("'x' contains missing values; use na.rm = TRUE to return NA for ",
         "the affected columns")

  valid <- !isNA & colSum > .Machine$double.eps

  div <- rep(0, ncol(x))
  div[isNA] <- NA_real_

  if (any(valid)) {
    xv <- x[, valid, drop = FALSE]
    div[valid] <- colSums(xv * (d2 %*% xv)) / (colSum[valid]^2)
  }

  # --- normalization ---
  if (normalize) {
    maxRes <- .divCoefMax(d2, tol = tol)

    if (!maxRes$converged)
      warning("the maximisation of the diversity coefficient did not ",
              "converge; the normalized values may be too large")

    if (maxRes$value <= 0)
      stop("the maximum diversity is not positive; 'dis' is degenerate")

    div <- div / maxRes$value
  }

  return(div)
}



#' @noRd
.divCoefMax <- function(d2, tol = 1e-8, maxit = 1000) {

  n <- nrow(d2)

  # start with uniform weights
  x <- rep(1 / n, n)
  converged <- FALSE

  for (i in seq_len(maxit)) {
    xNew <- as.vector(d2 %*% x)

    # projection onto simplex
    xNew <- pmax(xNew, 0)
    s <- sum(xNew)

    if (s == 0) {
      xNew <- rep(1 / n, n)
    } else {
      xNew <- xNew / s
    }

    if (max(abs(x - xNew)) < tol) {
      x <- xNew
      converged <- TRUE
      break
    }

    x <- xNew
  }

  list(
    value = as.numeric(t(x) %*% d2 %*% x),
    weights = x,
    iterations = i,
    converged = converged
  )
}
