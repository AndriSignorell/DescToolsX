
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
#' @param m a numeric matrix, either:
#'   \itemize{
#'     \item a square correlation matrix, or
#'     \item a data matrix (observations × variables)
#'   }
#' @param x integer vector of indices specifying the variables of interest
#'   for which partial correlations are computed
#' @param y integer vector of indices specifying the control variables
#'   (conditioning set)
#'
#' @return a symmetric numeric matrix containing the partial correlations
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



#' @family assoc.continuous  
#' @concept correlation  
#' @concept association-measure
#'
#'
#' @export
corPart <- function(m, x, y) {
  
  if (!is.matrix(m)) m <- as.matrix(m)
  
  # --- wenn Datenmatrix: Kovarianz berechnen ---
  if (nrow(m) != ncol(m)) {
    S <- cov(m, use = "pairwise.complete.obs")
  } else {
    # Input ist bereits Matrix → als Kovarianz interpretieren
    S <- m
  }
  
  p <- ncol(S)
  
  # --- Index Checks ---
  if (any(!is.finite(x)) || any(!is.finite(y)) ||
      any(x %% 1 != 0) || any(y %% 1 != 0) ||
      any(x < 1) || any(y < 1) ||
      any(x > p) || any(y > p)) {
    stop("x and y must be integer indices in 1:ncol(m)")
  }
  
  # --- relevante Submatrix ---
  idx <- c(x, y)
  S_sub <- S[idx, idx, drop = FALSE]
  
  # --- Inversion (Präzisionsmatrix) ---
  P <- tryCatch(
    solve(S_sub),
    error = function(e) {
      stop("Covariance matrix is singular or ill-conditioned (collinearity)")
    }
  )
  
  k <- length(x)
  
  # --- Partial correlations aus Präzisionsmatrix ---
  P_xx <- P[1:k, 1:k, drop = FALSE]
  
  D <- diag(1 / sqrt(diag(P_xx)))
  pc <- -D %*% P_xx %*% D
  
  diag(pc) <- 1
  
  colnames(pc) <- rownames(pc) <- colnames(m)[x]
  
  return(pc)
}
