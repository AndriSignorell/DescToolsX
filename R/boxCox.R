
#' Box-Cox Transformation
#'
#' `boxCox()` applies the Box-Cox transformation to a numeric vector.
#' \cr `boxCoxInv()` reverses the transformation.
#'
#' The Box-Cox transformation is defined for strictly positive values of
#' `x` and is given by
#'
#' \deqn{
#' f_\lambda(x) = \left\{ \begin{array}{ll}
#'   (x^\lambda - 1) / \lambda & \mbox{if } \lambda \neq 0 \\
#'   \log(x)                   & \mbox{if } \lambda = 0
#' \end{array} \right.
#' }{f(x) = (x^lambda - 1)/lambda for lambda != 0, and log(x) for lambda = 0}
#'
#' @name boxCox
#' @aliases boxCox boxCoxInv
#'
#' @param x a numeric vector. Must contain strictly positive values
#'   (except `NA`s).
#' @param lambda a single numeric transformation parameter
#' @param tol numeric tolerance for detecting the special case
#'   \eqn{\lambda \approx 0}
#'
#' @return a numeric vector of the same length as `x`. An input
#'   consisting only of `NA` is an error.
#'
#' @details
#' The transformation requires strictly positive input values. If
#' `|lambda| < tol`, the logarithmic transformation is used instead
#' for numerical stability.
#'
#' The inverse transformation recovers the original data (up to numerical
#' precision) when the same `lambda` and `tol` are used.
#'
#' @references
#' Box, G. E. P. and Cox, D. R. (1964).
#' An analysis of transformations.
#' *Journal of the Royal Statistical Society, Series B*,
#' **26**(2), 211--252.
#'
#' @seealso [boxCoxLambda()]
#'
#' @examples
#' set.seed(1)
#' x <- rlnorm(500, 1, 0.5)
#'
#' y <- boxCox(x, lambda = 0.5)
#' xBack <- boxCoxInv(y, lambda = 0.5)
#'
#' # check inversion
#' max(abs(x - xBack))
#'
#' # log-transform (lambda ~ 0)
#' y0 <- boxCox(x, lambda = 0)
#'
#' @family transform
#' @concept transformation
#' @concept variance-stabilization
#' @export
boxCox <- function(x, lambda, tol = 1e-6) {

  x <- .checkBoxCoxInput(x, lambda)

  if (length(x) == 0L)
    return(x)

  if (all(is.na(x)))
    stop("'x' contains only NA values")

  if (any(x <= 0, na.rm = TRUE))
    stop("Box-Cox requires strictly positive values")

  if (abs(lambda) < tol) {
    log(x)
  } else {
    (x^lambda - 1) / lambda
  }
}


#' @rdname boxCox
#' @export
boxCoxInv <- function(x, lambda, tol = 1e-6) {

  x <- .checkBoxCoxInput(x, lambda)

  if (length(x) == 0L)
    return(x)

  if (all(is.na(x)))
    stop("'x' contains only NA values")

  if (abs(lambda) < tol)
    return(exp(x))

  tmp <- lambda * x + 1

  if (any(tmp <= 0, na.rm = TRUE))
    stop("lambda * x + 1 must be positive")

  tmp^(1 / lambda)
}


# Shared input contract for boxCox()/boxCoxInv().
#
# The original branch
#
#   if (!is.numeric(x)) { if (!all(is.na(x))) stop("x must be numeric")
#                         x <- as.numeric(x) }
#
# was NOT dead code, as I first claimed. rep(NA, n) and a bare NA are
# LOGICAL, so an all-NA input is not numeric - and the branch exists so
# that such input reaches the informative "only NA" error instead of
# being turned away as "must be numeric". Only the assignment was
# pointless, since the coerced value is never used before the stop().
# Rewritten so both messages survive and nothing is assigned in vain.
#' @noRd
.checkBoxCoxInput <- function(x, lambda) {

  if (!is.numeric(x) && !all(is.na(x)))
    stop("'x' must be numeric")

  if (!is.numeric(lambda) || length(lambda) != 1L || !is.finite(lambda))
    stop("'lambda' must be a single finite number")

  x
}
