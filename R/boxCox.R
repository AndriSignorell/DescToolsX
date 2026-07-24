
#' Box-Cox Transformation
#'
#' \code{boxCox()} applies the Box-Cox transformation to a numeric vector.
#' \cr \code{boxCoxInv()} reverses the transformation.
#'
#' The Box-Cox transformation is defined for strictly positive values of
#' \code{x} and is given by
#'
#' \deqn{
#' f_\lambda(x) =
#' \begin{cases}
#' \frac{x^\lambda - 1}{\lambda} & \text{if } \lambda \neq 0 \\
#' \log(x) & \text{if } \lambda = 0
#' \end{cases}
#' }
#'
#' @name boxCox
#' @aliases boxCox boxCoxInv
#'
#' @param x a numeric vector. Must contain strictly positive values
#'   (except \code{NA}s).
#' @param lambda a single numeric transformation parameter
#' @param tol numeric tolerance for detecting the special case
#'   \eqn{\lambda \approx 0}
#'
#' @return a numeric vector of the same length as \code{x}
#'
#' @details
#' The transformation requires strictly positive input values. If
#' \code{|lambda| < tol}, the logarithmic transformation is used instead
#' for numerical stability.
#'
#' The inverse transformation recovers the original data (up to numerical
#' precision) when the same \code{lambda} and \code{tol} are used.
#'
#' @references
#' Box, G. E. P. and Cox, D. R. (1964).
#' An analysis of transformations.
#' \emph{Journal of the Royal Statistical Society, Series B},
#' \bold{26}(2), 211--252.
#'
#' @examples
#' set.seed(1)
#' x <- rlnorm(500, 1, 0.5)
#'
#' y <- boxCox(x, lambda = 0.5)
#' x_back <- boxCoxInv(y, lambda = 0.5)
#'
#' # Check inversion
#' max(abs(x - x_back))
#'
#' # Log-transform (lambda ~ 0)
#' y0 <- boxCox(x, lambda = 0)
#'
#'
#' @family transform  
#' @concept transformation  
#' @concept variance-stabilization
#'
#'
#' @export
boxCox <- function(x, lambda, tol = 1e-6) {
  
  if (!is.numeric(x)) {
    if (!all(is.na(x)))
      stop("x must be numeric")
    x <- as.numeric(x)
  }
  
  if (!is.numeric(lambda) || length(lambda) != 1 || !is.finite(lambda))
    stop("lambda must be a single finite number")
  
  if (length(x) == 0)
    return(x)
  
  if (all(is.na(x)))
    stop("x contains only NA values")
  
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
  
  if (!is.numeric(x)) {
    if (!all(is.na(x)))
      stop("x must be numeric")
    x <- as.numeric(x)
  }
  
  if (!is.numeric(lambda) || length(lambda) != 1 || !is.finite(lambda))
    stop("lambda must be a single finite number")
  
  if (length(x) == 0)
    return(x)
  
  if (all(is.na(x)))
    stop("x contains only NA values")
  
  if (abs(lambda) < tol) {
    return(exp(x))
  }
  
  tmp <- lambda * x + 1
  
  if (all(is.na(tmp)))
    stop("All values lead to invalid inverse transformation")
  
  if (any(tmp <= 0, na.rm = TRUE))
    stop("lambda * x + 1 must be positive")
  
  tmp^(1 / lambda)
}
