
#' Median Absolute Deviation
#' 
#' Compute the median absolute deviation, i.e., the (lo-/hi-) median of the
#' absolute deviations from the median, and (by default) adjust by a factor for
#' asymptotically normal consistency. This function wraps the specific base R
#' function \code{\link{mad}} and extends it for the use of weights.
#' 
#' The actual value calculated is \code{constant * cMedian(abs(x - center))}
#' with the default value of \code{center} being \code{median(x)}, and
#' \code{cMedian} being the usual, the \sQuote{low} or \sQuote{high} median,
#' see the arguments description for \code{low} and \code{high} above.
#' 
#' The default \code{constant = 1.4826} (approximately \eqn{1/\Phi^{-1}(\frac 3
#' 4)}{1/ \Phi^(-1)(3/4)} = \code{1/qnorm(3/4)}) ensures consistency, i.e.,
#' \deqn{E[mad(X_1,\dots,X_n)] = \sigma} for \eqn{X_i} distributed as
#' \eqn{N(\mu, \sigma^2)} and large \eqn{n}.
#' 
#' If \code{na.rm} is \code{TRUE} then \code{NA} values are stripped from
#' \code{x} before computation takes place.  If this is not done then an
#' \code{NA} value in \code{x} will cause \code{madX} to return \code{NA}.
#' 
#' @param x a numeric vector.
#' @param weights a numerical vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}.
#' @param center the centre given either as numeric value or as a function to
#' be applied to \code{x} (defaults to the \code{medianX(x)}). Note
#' in cases when weights are defined to provide a function that also support
#' weights. If this is not possible fall back to a numeric value.
#' @param constant scale factor (default is \code{1.4826})
#' @param median.type if \code{TRUE}, compute the \sQuote{lo-median}, i.e., for
#' even sample size, do not average the two middle values, but take the smaller
#' one, if \code{TRUE}, compute the \sQuote{hi-median}, i.e., take the larger
#' of the two middle values for even sample size.
#' @param na.rm if \code{TRUE} then \code{NA} values are stripped from \code{x}
#' before computation takes place.
#' @seealso \code{\link{IQR}} which is simpler but less robust,
#' \code{\link{iqrX}} for the same using weights,\cr \code{\link{mad}},
#' \code{\link{median}}, \code{\link{var}} the base R equivalents
#' \cr\code{\link{madCI}} (confidence intervals).
#' @keywords univar robust
#' @examples
#' 
#' madX(c(1:9))
#' print(madX(c(1:9),     constant = 1)) ==
#'       madX(c(1:8, 100), constant = 1)       # = 2 ; TRUE
#' x <- c(1,2,3,5,7,8)
#' sort(abs(x - median(x)))
#' c(madX(x, constant = 1, median.type="standard"),
#'   madX(x, constant = 1, median.type="low"),
#'   madX(x, constant = 1, median.type="high"))
#' 
#' # use weights
#' x <- sample(20, 30, replace = TRUE)
#' z <- as.numeric(names(w <- table(x)))
#' 
#' (m1 <- madX(z, weights=w))
#' (m2 <- madX(x))
#' stopifnot(identical(m1, m2))
#' 


#' @export
madX <- function(x,
                weights = NULL,
                center = medianX,
                constant = 1.4826,
                median.type = c("standard", "low", "high"),
                na.rm = FALSE) {
  
  median.type <- match.arg(median.type)
  
  ## NA handling
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
    if (!is.null(weights))
      weights <- weights[ok]
  }
  
  ## determine center 
  if (is.function(center)) {
    center <- if (is.null(weights)) {
      center(x)
    } else {
      center(x, weights = weights)
    }
  }
  
  ## deviations
  d <- abs(x - center)
  
  ## weights
  if (is.null(weights))
    weights <- rep(1, length(d))
  
  z <- .NormWeights(d, weights, na.rm = FALSE, zero.rm = TRUE)
  
  ## Median-Index
  n <- length(z$x)
  
  if (median.type == "standard" || n %% 2 == 1) {
    m <- medianX(z$x, z$weights)
  } else {
    k <- n %/% 2
    o <- order(z$x)
    m <- if (median.type == "low")
      z$x[o[k]]
    else
      z$x[o[k + 1]]
  }
  
  return(constant * m)
  
}
