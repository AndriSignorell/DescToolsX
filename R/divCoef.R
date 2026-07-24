
#' Compute a diversity coefficient
#'
#' Computes a quadratic diversity coefficient for each column of a data frame,
#' optionally using a provided distance matrix.
#'
#' @param df a data frame or matrix of non-negative values (e.g. abundances).
#'   Rows correspond to entities, columns to samples.
#' @param dis optional object of class \code{dist}. If \code{NULL}, a default
#'   Euclidean distance is used.
#' @param normalize logical; if \code{TRUE}, the diversity is scaled by its
#'   theoretical maximum
#' @param tol numeric tolerance used for numerical checks
#'
#' @return a numeric vector of diversity coefficients, one per column
#'
#' @details
#' The diversity coefficient is defined as
#' \deqn{D = \frac{x^T D^2 x}{2 (\sum x)^2}}
#' where \eqn{x} is a column of \code{df} and \eqn{D} is the distance matrix.
#'
#' If \code{normalize = TRUE}, values are divided by the maximum achievable
#' diversity under the given distance matrix.
#'
#' @examples
#' set.seed(1)
#' df <- matrix(runif(20), ncol = 4)
#' d <- dist(matrix(rnorm(10), ncol = 2))
#'
#' divCoef(df, d)
#' divCoef(df, d, normalize = TRUE)
#'



#' @family assoc.nominal  
#' @concept association-measure  
#' @concept nominal
#'
#'
#' @export
divCoef <- function(df, dis = NULL, normalize = FALSE, tol = 1e-8) {
  
  # --- input checks ---
  if (!is.matrix(df) && !is.data.frame(df))
    stop("df must be a matrix or data.frame")
  
  df <- as.matrix(df)
  
  if (any(df < 0, na.rm = TRUE))
    stop("Negative values in df not allowed")
  
  n <- nrow(df)
  
  # --- distance handling ---
  if (is.null(dis)) {
    # default: Euclidean distances on simplex vertices
    D2 <- matrix(2, n, n)
    diag(D2) <- 0
    D2 <- D2 / 2
  } else {
    if (!inherits(dis, "dist"))
      stop("dis must be of class 'dist'")
    
    if (!isEuclid(dis))
      warning("Distance matrix is not Euclidean")
    
    D <- as.matrix(dis)
    if (nrow(D) != n)
      stop("df and dis have incompatible dimensions")
    
    D2 <- D^2 / 2
  }
  
  # --- compute diversity (vectorized) ---
  col_sums <- colSums(df)
  valid <- col_sums > .Machine$double.eps
  
  div <- rep(0, ncol(df))
  
  if (any(valid)) {
    X <- df[, valid, drop = FALSE]
    div[valid] <- colSums(X * (D2 %*% X)) / (col_sums[valid]^2)
  }
  
  # --- normalization ---
  if (normalize) {
    max_val <- .divCoefMax(D2, tol = tol)$value
    div <- div / max_val
  }
  
  return(div)
}




.divCoefMax <- function(D2, tol = 1e-8, maxit = 1000) {
  
  n <- nrow(D2)
  
  # start with uniform weights
  x <- rep(1 / n, n)
  
  for (i in seq_len(maxit)) {
    x_new <- D2 %*% x
    
    # projection onto simplex
    x_new <- pmax(x_new, 0)
    s <- sum(x_new)
    
    if (s == 0) {
      x_new <- rep(1 / n, n)
    } else {
      x_new <- x_new / s
    }
    
    if (max(abs(x - x_new)) < tol)
      break
    
    x <- x_new
  }
  
  value <- as.numeric(t(x) %*% D2 %*% x)
  
  list(
    value = value,
    weights = x,
    iterations = i,
    converged = (i < maxit)
  )
}

