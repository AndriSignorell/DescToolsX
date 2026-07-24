
#' Hoeffding's D Statistic (Fast Computation)
#'
#' Computes Hoeffding's D statistic for testing independence between two variables
#' using an efficient \eqn{O(n \log n)} algorithm based on rank statistics and
#' Fenwick trees (Even-Zohar & Leng, 2020).
#'
#' This implementation is substantially faster than classical \eqn{O(n^2)} approaches
#' and yields results numerically identical to those from \code{Hmisc::hoeffd()}
#' for continuous data without ties.
#'
#' @param x numeric vector
#' @param y numeric vector with the same length as \code{x}
#' @param jitter logical. If \code{TRUE}, small random noise is added to \code{y}
#'   to break ties. This is useful when the data contain ties, since the fast
#'   algorithm assumes continuous data.
#' @param eps optional numeric magnitude of the jitter noise; defaults to
#'   \code{1e-10 * sd(y)} if not specified
#' @param seed optional integer random seed for reproducibility when
#'   \code{jitter = TRUE}
#'
#' @return numeric scalar containing Hoeffding's D statistic. The statistic lies approximately in
#' the interval \eqn{[-1/60, 1/30]}, where values near 0 indicate independence.
#'
#' @details
#' The algorithm requires a strict ordering of the data and therefore assumes
#' no ties. If ties are present and \code{jitter = FALSE}, a warning is issued
#' and results may be biased.
#'
#' Setting \code{jitter = TRUE} resolves ties by adding small random noise,
#' yielding a fast and practical approximation.
#'
#' In contrast, \code{Hmisc::hoeffd()} handles ties via midranks but uses a
#' slower algorithm.
#'
#' @references
#' Even-Zohar, C., & Leng, C. (2020).
#' Fast computation of Hoeffding’s D statistic.
#'
#' Hollander, M., Wolfe, D. A., & Chicken, E. (2013).
#' Nonparametric Statistical Methods (3rd ed.).
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(200)
#' y <- x^2 + rnorm(200)
#'
#' # fast computation
#' hoeffdingD(x, y)
#'
#' # with ties
#' y2 <- round(y, 1)
#' hoeffdingD(x, y2)               # warning
#' hoeffdingD(x, y2, jitter=TRUE) # recommended
#'

#' @family assoc.continuous  
#' @concept association-measure  
#' @concept nonlinear-association
#'
#'
#' @export
hoeffdingD <- function(x, y, jitter = FALSE, eps = NULL, seed = NULL) {
  
  if (length(x) != length(y))
    stop("x and y must have same length")
  
  n <- length(x)
  if (n < 5)
    stop("Need at least 5 observations")
  
  has_ties <- any(duplicated(x)) || any(duplicated(y))
  
  if (has_ties && !jitter) {
    warning(
      "Ties detected. The fast algorithm assumes continuous data.\n",
      "Use jitter = TRUE to break ties (recommended), ",
      "or expect small bias."
    )
  }
  
  if (jitter) {
    if (!is.null(seed)) set.seed(seed)
    
    if (is.null(eps))
      eps <- 1e-10 * sd(y)
    
    y <- y + runif(n, -eps, eps)
  }
  
  ord_x <- order(x)
  rank_y <- rank(y, ties.method = "first")
  perm <- rank_y[ord_x] - 1
  
  hoeffdingD_cpp(perm)
  
}

