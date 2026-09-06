#' Yeo-Johnson Transformation
#'
#' `yeoJohnson()` applies the Yeo-Johnson transformation to a numeric
#' vector. \cr `yeoJohnsonInv()` reverses the transformation.
#'
#' The Yeo-Johnson transformation extends the Box-Cox transformation to allow
#' for zero and negative values. It is defined piecewise:
#'
#' \deqn{f_\lambda(x) = \frac{(x+1)^\lambda - 1}{\lambda}}{
#'       f(x) = ((x+1)^lambda - 1) / lambda}
#' for \eqn{x \ge 0} and \eqn{\lambda \ne 0},
#'
#' \deqn{f_\lambda(x) = \log(x+1)}{f(x) = log(x+1)}
#' for \eqn{x \ge 0} and \eqn{\lambda = 0},
#'
#' \deqn{f_\lambda(x) = -\frac{(-x+1)^{2-\lambda} - 1}{2-\lambda}}{
#'       f(x) = -((-x+1)^(2-lambda) - 1) / (2-lambda)}
#' for \eqn{x < 0} and \eqn{\lambda \ne 2}, and
#'
#' \deqn{f_\lambda(x) = -\log(-x+1)}{f(x) = -log(-x+1)}
#' for \eqn{x < 0} and \eqn{\lambda = 2}.
#'
#' @name yeoJohnson
#' @aliases yeoJohnson yeoJohnsonInv
#'
#' @param x a numeric vector
#' @param lambda a single numeric transformation parameter
#' @param tol numeric tolerance for detecting the special cases
#'   \eqn{\lambda \approx 0} and \eqn{\lambda \approx 2}
#'
#' @return a numeric vector of the same length as `x`. Names and
#'   dimensions of `x` are preserved.
#'
#' @details
#' The transformation is defined for all real-valued inputs and is continuous
#' and differentiable for all \eqn{x}. It is commonly used as an alternative to
#' the Box-Cox transformation when the data include zero or negative values.
#'
#' The transformation is strictly increasing and maps 0 to 0, so the sign of
#' the transformed value identifies the branch to be inverted.
#'
#' The inverse transformation recovers the original data (up to numerical
#' precision) when the same `lambda` and `tol` are used. For
#' \eqn{\lambda < 0} the image of the transformation is bounded above by
#' \eqn{-1/\lambda} (and correspondingly for \eqn{\lambda > 2} below); values
#' outside that range have no preimage and are reported as an error.
#'
#' @references
#' Yeo, I.-K. and Johnson, R. A. (2000).
#' A new family of power transformations to improve normality or symmetry.
#' *Biometrika*, **87**(4), 954--959.
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
#' @export
yeoJohnson <- function(x, lambda, tol = 1e-6) {

  x <- .yjCheck(x, lambda, tol)
  if (length(x) == 0L)
    return(x)

  out <- .yjLike(x)

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

  x <- .yjCheck(x, lambda, tol)
  if (length(x) == 0L)
    return(x)

  out <- .yjLike(x)

  # the transformation is increasing with f(0) = 0, so the sign of the
  # transformed value selects the branch
  pos <- x >= 0 | is.na(x)
  neg <- !pos

  # --- inverse for x >= 0 ---
  if (abs(lambda) < tol) {
    out[pos] <- exp(x[pos]) - 1
  } else {
    tmp <- lambda * x[pos] + 1
    .yjStopOutOfRange(tmp, lambda)
    out[pos] <- tmp^(1 / lambda) - 1
  }

  # --- inverse for x < 0 ---
  if (abs(lambda - 2) < tol) {
    out[neg] <- 1 - exp(-x[neg])
  } else {
    tmp <- (2 - lambda) * (-x[neg]) + 1
    .yjStopOutOfRange(tmp, lambda)
    out[neg] <- 1 - tmp^(1 / (2 - lambda))
  }

  out
}


# common argument checks for yeoJohnson() and yeoJohnsonInv()
.yjCheck <- function(x, lambda, tol) {

  if (!is.numeric(x)) {
    # an all-NA logical vector is the typical result of subsetting an empty
    # data set; it gets the informative "only NA" message below instead of
    # "must be numeric" (same rationale as in boxCox())
    if (!all(is.na(x)))
      stop("x must be numeric")
    x <- as.numeric(x)
  }

  if (!is.numeric(lambda) || length(lambda) != 1 || !is.finite(lambda))
    stop("lambda must be a single finite number")

  if (!is.numeric(tol) || length(tol) != 1 || !is.finite(tol) || tol < 0)
    stop("tol must be a single non-negative number")

  if (length(x) == 0)
    return(x)

  if (all(is.na(x)))
    stop("x contains only NA values")

  x
}


# a numeric container with the shape (names / dim) of x
.yjLike <- function(x) {
  out <- numeric(length(x))
  if (!is.null(dim(x))) {
    dim(out) <- dim(x)
    dimnames(out) <- dimnames(x)
  } else {
    names(out) <- names(x)
  }
  out
}


.yjStopOutOfRange <- function(tmp, lambda) {
  bad <- which(tmp <= 0)
  if (length(bad))
    stop(gettextf(
      "%d value(s) outside the image of the transformation for lambda = %s; %s",
      length(bad), format(lambda),
      "they have no preimage and cannot be inverted"))
  invisible(TRUE)
}
