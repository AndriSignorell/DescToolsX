
#' Partial Correlation Matrix via Schur Complement
#'
#' Computes the partial correlation matrix of a set of variables \code{x}
#' while controlling for another set of variables \code{y}, based on a
#' correlation matrix or raw data.
#'
#' If \code{m} is not a square matrix, it is interpreted as a data matrix
#' (observations in rows, variables in columns), and a correlation matrix
#' is computed internally using \code{cor(..., use = "pairwise.complete.obs")}.
#'
#' @param m A numeric matrix. Either:
#'   \itemize{
#'     \item a square correlation matrix, or
#'     \item a data matrix (observations × variables)
#'   }
#' @param x Integer vector of indices specifying the variables of interest
#'   for which partial correlations are computed.
#' @param y Integer vector of indices specifying the control variables
#'   (conditioning set).
#'
#' @return A symmetric numeric matrix containing the partial correlations
#'   among variables in \code{x}, adjusted for variables in \code{y}.
#'   Row and column names correspond to \code{colnames(m)[x]}.
#'
#' @details
#' The function computes the partial correlation matrix using the
#' Schur complement:
#'
#' \deqn{
#' \Sigma_{xx \cdot y} = \Sigma_{xx} - \Sigma_{xy} \Sigma_{yy}^{-1} \Sigma_{yx}
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{\Sigma_{xx}} is the submatrix for variables \code{x}
#'   \item \eqn{\Sigma_{yy}} is the submatrix for variables \code{y}
#'   \item \eqn{\Sigma_{xy}} is the cross-covariance block
#' }
#'
#' The resulting matrix is then scaled to unit diagonal to yield a
#' correlation matrix.
#'
#' @section Numerical considerations:
#' \itemize{
#'   \item If \eqn{\Sigma_{yy}} (denoted as \code{phi}) is singular or
#'     ill-conditioned (e.g., due to collinearity in \code{y}), the
#'     function stops with an error.
#'   \item If the resulting residual covariance matrix is not positive
#'     definite (e.g., negative or zero diagonal elements), the function
#'     stops with an error.
#'   \item Pairwise correlations may introduce inconsistencies if missing
#'     data are present.
#' }
#'
#' @examples
#' # Simulated data
#' set.seed(1)
#' X <- matrix(rnorm(100 * 5), ncol = 5)
#' colnames(X) <- paste0("V", 1:5)
#'
#' # Partial correlations of V1, V2 controlling for V3, V4
#' corPart(X, x = 1:2, y = 3:4)
#'
#' # Using a correlation matrix directly
#' C <- cor(X)
#' corPart(C, x = 1:2, y = 3:4)
#'
#' @seealso \code{\link[stats]{cor}}, \code{\link[stats]{cov}},
#'   \code{\link[MASS]{ginv}} for generalized inverse
#'
#' @family correlation
#' @concept partial-correlation
#'


#' @export
corPart <- function(m, x, y) {
  
  if (!is.matrix(m)) m <- as.matrix(m)
  
  # treat non-square input as data matrix
  if (nrow(m) != ncol(m)) {
    m <- cor(m, use = "pairwise.complete.obs")
  }
  
  p <- ncol(m)
  
  # --- index checks ---
  if (any(!is.finite(x)) || any(!is.finite(y)) ||
      any(x %% 1 != 0) || any(y %% 1 != 0) ||
      any(x < 1) || any(y < 1) ||
      any(x > p) || any(y > p)) {
    stop("x and y must be integer indices in 1:ncol(m)")
  }
  
  xy <- c(x, y)
  numx <- length(x)
  numy <- length(y)
  
  # --- reorder ---
  reorder <- m[xy, xy, drop = FALSE]
  
  X <- reorder[1:numx, 1:numx, drop = FALSE]
  Y <- reorder[1:numx, (numx + 1):(numx + numy), drop = FALSE]
  phi <- reorder[(numx + 1):(numx + numy),
                 (numx + 1):(numx + numy),
                 drop = FALSE]
  
  # --- solve system (robust) ---
  tmp <- tryCatch(
    solve(phi, t(Y)),
    error = function(e) {
      stop("phi matrix is singular or ill-conditioned (collinearity in 'y')")
    }
  )
  
  X.resid <- X - Y %*% tmp
  
  # --- normalize to correlation matrix ---
  d <- diag(X.resid)
  
  if (any(!is.finite(d)) || any(d <= 0)) {
    stop("Partial correlation matrix is not positive definite; check inputs")
  }
  
  sc <- diag(1 / sqrt(d))
  X.resid <- sc %*% X.resid %*% sc
  
  colnames(X.resid) <- rownames(X.resid) <- colnames(m)[x]
  
  return(X.resid)
}

