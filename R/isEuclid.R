
#' Test if a distance matrix is Euclidean
#'
#' Checks whether a distance matrix corresponds to Euclidean distances.
#'
#' @param distmat An object of class \code{dist}.
#' @param tol Numeric tolerance for detecting negative eigenvalues.
#'
#' @return A logical scalar. Returns \code{TRUE} if the distance matrix is
#'   (approximately) Euclidean, otherwise \code{FALSE}.
#'
#' @details
#' The test is based on the eigenvalues of the double-centered squared
#' distance matrix \eqn{B = -\frac{1}{2} J D^2 J}. A distance matrix is
#' Euclidean if and only if \eqn{B} is positive semi-definite, i.e., all
#' eigenvalues are non-negative (within numerical tolerance).
#'
#' The returned logical value carries additional diagnostic information
#' as attributes:
#' \itemize{
#'   \item \code{eigenvalues}: Eigenvalues of the centered matrix
#'   \item \code{min_eigenvalue}: Smallest eigenvalue
#'   \item \code{tol}: Tolerance used for the test
#' }
#'
#' @examples
#' d <- dist(matrix(rnorm(20), ncol = 2))
#' res <- isEuclid(d)
#' res
#'
#' # Access diagnostics
#' attr(res, "eigenvalues")
#' attr(res, "min_eigenvalue")
#'



#' @export
isEuclid <- function(distmat, tol = 1e-07) {
  if (!inherits(distmat, "dist"))
    stop("Object of class 'dist' expected")
  
  D <- as.matrix(distmat)
  n <- nrow(D)
  
  # double centering
  J <- diag(n) - 1 / n
  B <- -0.5 * J %*% (D^2) %*% J
  
  lambda <- eigen(B, symmetric = TRUE, only.values = TRUE)$values
  
  res <- min(lambda) > -tol
  
  # attach diagnostics
  attr(res, "eigenvalues") <- lambda
  attr(res, "min_eigenvalue") <- min(lambda)
  attr(res, "tol") <- tol
  
  return(res)
}

