
#' Yeo-Johnson Transformation
#'
#' \code{yeoJohnson()} applies the Yeo-Johnson transformation to a numeric
#' vector. \cr \code{yeoJohnsonInv()} reverses the transformation.
#'
#' The Yeo-Johnson transformation extends the Box-Cox transformation to allow
#' for zero and negative values.
#'
#' \deqn{
#' f_\lambda(x) =
#' \begin{cases}
#' \frac{(x+1)^\lambda - 1}{\lambda} & \text{if } x \ge 0,\ \lambda \neq 0 \\
#' \log(x+1) & \text{if } x \ge 0,\ \lambda = 0 \\
#' -\frac{(-x+1)^{2-\lambda} - 1}{2-\lambda} & \text{if } x < 0,\ \lambda \neq 2 \\
#' -\log(-x+1) & \text{if } x < 0,\ \lambda = 2
#' \end{cases}
#' }
#'
#' @name yeoJohnson
#' @aliases yeoJohnson yeoJohnsonInv
#'
#' @param x a numeric vector
#' @param lambda a single numeric transformation parameter
#' @param tol numeric tolerance for detecting the special cases
#'   \eqn{\lambda \approx 0} and \eqn{\lambda \approx 2}
#'
#' @return a numeric vector of the same length as \code{x}
#'
#' @details
#' The transformation is defined for all real-valued inputs and is continuous
#' and differentiable for all \eqn{x}. It is commonly used as an alternative to
#' the Box-Cox transformation when the data include zero or negative values.
#'
#' The inverse transformation recovers the original data (up to numerical
#' precision) when the same \code{lambda} and \code{tol} are used.
#'
#' @references
#' Yeo, I.-K. and Johnson, R. A. (2000).
#' A new family of power transformations to improve normality or symmetry.
#' \emph{Biometrika}, \bold{87}(4), 954--959.
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(500)
#'
#' y <- yeoJohnson(x, lambda = 0.5)
#' x_back <- yeoJohnsonInv(y, lambda = 0.5)
#'
#' # Check inversion
#' max(abs(x - x_back))
#'
#' # Compare with log-like transformation
#' y0 <- yeoJohnson(x, lambda = 0)
#'
#'
#' @family transform  
#' @concept transformation  
#' @concept variance-stabilization
#'
#'
#' @export
yeoJohnson <- function(x, lambda, tol = 1e-6) {
  
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
  
  out <- numeric(length(x))
  
  pos <- x >= 0 | is.na(x)
  neg <- !pos
  
  # --- x >= 0 ---
  if (abs(lambda) < tol) {
    out[pos] <- log(x[pos] + 1)
  } else {
    out[pos] <- ((x[pos] + 1)^lambda - 1) / lambda
  }
  
  # --- x < 0 ---
  if (abs(lambda - 2) < tol) {
    out[neg] <- -log(-x[neg] + 1)
  } else {
    out[neg] <- -(((-x[neg] + 1)^(2 - lambda) - 1) / (2 - lambda))
  }
  
  out
}


#' @rdname yeoJohnson
#' @export
yeoJohnsonInv <- function(x, lambda, tol = 1e-6) {
  
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
  
  out <- numeric(length(x))
  
  pos <- x >= 0 | is.na(x)
  neg <- !pos
  
  # --- inverse for x >= 0 ---
  if (abs(lambda) < tol) {
    out[pos] <- exp(x[pos]) - 1
  } else {
    tmp <- lambda * x[pos] + 1
    if (any(tmp <= 0, na.rm = TRUE))
      stop("Invalid values in inverse transformation")
    out[pos] <- tmp^(1 / lambda) - 1
  }
  
  # --- inverse for x < 0 ---
  if (abs(lambda - 2) < tol) {
    out[neg] <- 1 - exp(-x[neg])
  } else {
    tmp <- (2 - lambda) * (-x[neg]) + 1
    if (any(tmp <= 0, na.rm = TRUE))
      stop("Invalid values in inverse transformation")
    out[neg] <- 1 - tmp^(1 / (2 - lambda))
  }
  
  out
}

