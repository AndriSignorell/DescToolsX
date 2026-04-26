
#' Fisher's z-Transformation
#'
#' Convert a Pearson correlation coefficient to Fisher's \eqn{z} scale
#' and back. The transformation stabilizes the variance of the correlation
#' coefficient and yields approximately normally distributed values.
#'
#' The forward transformation is defined as
#' \deqn{
#' z = \tanh^{-1}(r) = \frac{1}{2}\log\left(\frac{1 + r}{1 - r}\right),
#' }
#' and the inverse transformation as
#' \deqn{
#' r = \tanh(z).
#' }
#'
#' @param rho Numeric vector. Pearson correlation coefficient(s), typically
#'   in the interval \eqn{[-1, 1]}. Values of \eqn{\pm 1} are mapped to
#'   \eqn{\pm \infty}.
#' @param z Numeric vector. Fisher \eqn{z}-transformed values.
#'
#' @return
#' \describe{
#'   \item{fisherZ}{Numeric vector of Fisher \eqn{z}-transformed values.}
#'   \item{fisherZInv}{Numeric vector of correlation coefficients.}
#' }
#'
#' @details
#' Fisher's \eqn{z}-transformation is commonly used to construct confidence
#' intervals and perform hypothesis tests for correlation coefficients.
#'
#' @seealso \code{\link{corCI}}, \code{\link{cor.test}}
#'
#' @examples
#' # Forward and inverse transformation
#' r <- seq(-0.9, 0.9, by = 0.1)
#' z <- fisherZ(r)
#' fisherZInv(z)
#'
#' # Round-trip accuracy
#' all.equal(r, fisherZInv(fisherZ(r)))
#'


#' @rdname fisherZ
#' @export
fisherZ <- function(rho) {
  atanh(rho)
}


#' @rdname fisherZ
#' @export
fisherZInv <- function(z) {
  tanh(z)
}

