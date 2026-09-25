
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
#' @param x column indices (whole numbers, no duplicates) of the variables
#'   of interest for which partial correlations are computed
#' @param y column indices (whole numbers, no duplicates) of the control
#'   variables (conditioning set); must not overlap with `x`
#'
#' @return a symmetric numeric matrix containing the partial correlations
#'   among variables in `x`, adjusted for variables in `y`.
#'   Row and column names correspond to `colnames(m)[x]`.
#'
#' @details
#' Only the variables in `y` are controlled for. Let \eqn{S} be the
#' joint covariance matrix of the selected variables. The residual covariance
#' matrix of `x` after linear adjustment for `y` is the Schur complement
#'
#' \deqn{V = S_{xx} - S_{xy} S_{yy}^{-1} S_{yx}.}{
#'   V = S_xx - S_xy * solve(S_yy, S_yx)}
#'
#' The result has entries \eqn{V_{ij}/\sqrt{V_{ii} V_{jj}}}.
#' With complete raw data, this equals the correlation matrix of the
#' residuals from regressing each variable in `x` on all variables in `y`,
#' including an intercept. Adding another variable to `x` does not change
#' the correlations between the previously selected variables.
#'
#' Normalizing the inverse of the joint covariance matrix would also control
#' for the other variables in `x`. That is a different quantity when
#' `length(x) > 2`.
#'
#' Because the result is scaled to unit diagonal, it makes no difference
#' whether `m` is a covariance or a correlation matrix.
#'
#' @section Numerical considerations:
#' \itemize{
#'   \item The joint submatrix of `x` and `y` must be invertible, and
#'     every selected variable must have positive variance.
#'     Near-singularity from collinearity is detected via the reciprocal
#'     condition number, not merely by a failure of [base::solve()],
#'     which succeeds and returns nonsense well before the matrix is
#'     numerically singular. The condition number is taken on the
#'     correlation scale, so variables measured in very different units
#'     are not mistaken for collinear ones.
#'   \item For raw data only the selected columns enter [stats::cov()].
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

  p <- ncol(m)

  # --- index checks, against m itself: raw data then only needs the
  # selected columns in cov(), not all of them ---
  if (length(x) == 0L || length(y) == 0L)
    stop("'x' and 'y' must each name at least one variable")

  # is.numeric() first: TRUE passed every arithmetic test below as index 1
  if (!is.numeric(x) || !is.numeric(y) ||
      any(!is.finite(x)) || any(!is.finite(y)) ||
      any(x %% 1 != 0) || any(y %% 1 != 0) ||
      any(x < 1) || any(y < 1) ||
      any(x > p) || any(y > p)) {
    stop("x and y must be integer indices in 1:ncol(m)")
  }

  # a repeated index duplicates a row and column of the submatrix, which
  # then failed below as "collinearity" - true, but not the user's mistake
  if (anyDuplicated(x) || anyDuplicated(y))
    stop("'x' and 'y' must not contain duplicate indices")

  if (length(intersect(x, y)) > 0L)
    stop("'x' and 'y' must not overlap - a variable cannot be both of ",
         "interest and a control")

  # --- relevant submatrix ---
  # pairwise deletion works pair by pair, so the covariance of the selected
  # columns equals the corresponding block of the full pairwise matrix
  idx <- c(x, y)
  S_sub <- if (isCovMat) m[idx, idx, drop = FALSE]
           else cov(m[, idx, drop = FALSE], use = "pairwise.complete.obs")

  if (anyNA(S_sub))
    stop("the covariance matrix contains missing values; too few complete ",
         "pairs in 'm'")

  if (any(!is.finite(S_sub)))
    stop("the covariance matrix must contain only finite values")

  if (any(diag(S_sub) <= 0))
    stop("the variables with index ",
         paste(idx[diag(S_sub) <= 0], collapse = ", "),
         " have no positive variance")

  # The result is scale-free, the condition number is not: on the covariance
  # scale, uncorrelated variables in metres and in micrometres alone gave
  # rcond() ~ 1e-14 and a spurious "collinearity" error. On the correlation
  # scale the check measures collinearity and nothing else; the partial
  # correlations are identical either way (and solve() is better
  # conditioned, too).
  S_sub <- cov2cor(S_sub)

  # singular or ill-conditioned joint matrix
  if (rcond(S_sub) < .Machine$double.eps^0.5)
    stop("Covariance matrix is singular or ill-conditioned (collinearity)")

  # Control ONLY for y, irrespective of the number of variables in x.
  # solve(A, B) avoids constructing an explicit inverse.
  k <- length(x)
  ix <- seq_len(k)
  iy <- k + seq_along(y)
  S_xx <- S_sub[ix, ix, drop = FALSE]
  S_xy <- S_sub[ix, iy, drop = FALSE]
  S_yy <- S_sub[iy, iy, drop = FALSE]
  residualCov <- S_xx - S_xy %*% solve(S_yy, t(S_xy))
  residualCov <- (residualCov + t(residualCov)) / 2

  if (any(!is.finite(residualCov)) || any(diag(residualCov) <= 0))
    stop("Residual variances must be finite and positive")

  pc <- cov2cor(residualCov)

  colnames(pc) <- rownames(pc) <- colnames(m)[x]

  return(pc)
}
