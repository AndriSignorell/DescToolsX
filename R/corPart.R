
#' Partial Correlation Matrix
#'
#' Computes the partial correlation matrix of a set of variables `x`
#' while controlling for another set of variables `y`, based on a
#' covariance/correlation matrix or on raw data.
#'
#' @param m a numeric matrix, either:
#'   \itemize{
#'     \item a square, symmetric covariance or correlation matrix, or
#'     \item a data matrix (observations in rows, variables in columns)
#'   }
#'   The two are told apart by symmetry, not by shape alone - a data
#'   matrix with as many rows as columns would otherwise be mistaken for
#'   a correlation matrix.
#' @param x integer vector of indices specifying the variables of interest
#'   for which partial correlations are computed
#' @param y integer vector of indices specifying the control variables
#'   (conditioning set)
#'
#' @return a symmetric numeric matrix containing the partial correlations
#'   among variables in `x`, adjusted for variables in `y`.
#'   Row and column names correspond to `colnames(m)[x]`.
#'
#' @details
#' Partial correlations are read off the precision matrix. Let \eqn{K} be
#' the inverse of the joint covariance matrix of \eqn{(x, y)}; then
#'
#' \deqn{\rho_{ij \cdot y} = - K_{ij} / \sqrt{K_{ii} K_{jj}}}{
#'   rho_ij.y = -K_ij / sqrt(K_ii * K_jj)}
#'
#' for \eqn{i, j} in \eqn{x}. This is algebraically equivalent to forming
#' the Schur complement
#' \eqn{\Sigma_{xx} - \Sigma_{xy}\Sigma_{yy}^{-1}\Sigma_{yx}} and scaling
#' it to unit diagonal, but needs a single inversion instead of two.
#'
#' Because the result is scaled to unit diagonal, it makes no difference
#' whether `m` is a covariance or a correlation matrix.
#'
#' @section Numerical considerations:
#' \itemize{
#'   \item The joint submatrix of `x` and `y` must be invertible.
#'     Near-singularity from collinearity among the control variables is
#'     detected via the reciprocal condition number, not merely by a
#'     failure of [base::solve()], which succeeds and returns
#'     nonsense well before the matrix is numerically singular.
#'   \item `x` and `y` must not overlap.
#'   \item For raw data, correlations are computed pairwise, which can
#'     produce a non-positive-definite matrix when values are missing.
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
#' # a single variable of interest is allowed and returns a 1x1 matrix
#' corPart(C, x = 1, y = 3:4)
#'
#' @seealso [stats::cor()], [stats::cov()]
#'
#' @family assoc.continuous
#' @concept correlation
#' @concept association-measure
#' @export
corPart <- function(m, x, y) {

  if (!is.matrix(m)) m <- as.matrix(m)

  if (!is.numeric(m))
    stop("'m' must be numeric")

  # Squareness alone is not enough to identify a covariance matrix: a data
  # set with as many observations as variables (5 subjects, 5 items) was
  # silently taken to be one, with no error anywhere downstream.
  isCovMat <- nrow(m) == ncol(m) && isSymmetric(unname(m))

  S <- if (isCovMat) m else cov(m, use = "pairwise.complete.obs")

  p <- ncol(S)

  # --- index checks ---
  if (length(x) == 0L || length(y) == 0L)
    stop("'x' and 'y' must each name at least one variable")

  if (any(!is.finite(x)) || any(!is.finite(y)) ||
      any(x %% 1 != 0) || any(y %% 1 != 0) ||
      any(x < 1) || any(y < 1) ||
      any(x > p) || any(y > p)) {
    stop("x and y must be integer indices in 1:ncol(m)")
  }

  if (length(intersect(x, y)) > 0L)
    stop("'x' and 'y' must not overlap - a variable cannot be both of ",
         "interest and a control")

  # --- relevant submatrix ---
  idx <- c(x, y)
  S_sub <- S[idx, idx, drop = FALSE]

  if (anyNA(S_sub))
    stop("the covariance matrix contains missing values; too few complete ",
         "pairs in 'm'")

  # --- inversion (precision matrix) ---
  # solve() only errors below its own tolerance and happily returns
  # garbage for a merely ill-conditioned matrix, which the documentation
  # nevertheless promised to catch
  if (rcond(S_sub) < .Machine$double.eps^0.5)
    stop("Covariance matrix is singular or ill-conditioned (collinearity)")

  P <- solve(S_sub)

  k <- length(x)

  # --- partial correlations from the precision matrix ---
  P_xx <- P[seq_len(k), seq_len(k), drop = FALSE]

  # diag(v) with a length-1 v builds an identity matrix of size round(v)
  # instead of a 1x1 matrix - corPart(m, x = 1, y = ...) died on a
  # non-conformable multiplication. nrow= forces the intended reading.
  dv <- 1 / sqrt(diag(P_xx))
  D  <- diag(dv, nrow = k)

  pc <- -D %*% P_xx %*% D

  diag(pc) <- 1

  colnames(pc) <- rownames(pc) <- colnames(m)[x]

  return(pc)
}
