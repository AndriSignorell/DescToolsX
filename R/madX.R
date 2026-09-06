
#' Median Absolute Deviation
#' 
#' Compute the median absolute deviation, i.e., the (lo-/hi-) median of the
#' absolute deviations from the median, and (by default) adjust by a factor for
#' asymptotically normal consistency. This function wraps the specific base R
#' function [mad()] and extends it for the use of weights.
#' 
#' The actual value calculated is `constant * cMedian(abs(x - center))`
#' with the default value of `center` being `median(x)`, and
#' `cMedian` being the usual, the \sQuote{low} or \sQuote{high} median,
#' see the arguments description for `low` and `high` above.
#' 
#' The default `constant = 1.4826` (approximately \eqn{1/\Phi^{-1}(\frac 3
#' 4)}{1/ \Phi^(-1)(3/4)} = `1/qnorm(3/4)`) ensures consistency, i.e.,
#' \deqn{E[mad(X_1,\dots,X_n)] = \sigma} for \eqn{X_i} distributed as
#' \eqn{N(\mu, \sigma^2)} and large \eqn{n}.
#' 
#' If `na.rm` is `TRUE` then `NA` values are stripped from
#' `x` before computation takes place.  If this is not done then an
#' `NA` value in `x` will cause `madX` to return `NA`.
#' 
#' Confidence intervals are provided by [lumen::madCI()].
#' 
#' @param x a numeric vector
#' @param weights a numerical vector of weights the same length as `x`
#' giving the weights to use for elements of `x`
#' @param center a numeric center or a function applied to `x`. When
#' weights are supplied, the function must support a `weights` argument.
#' Defaults to `medianX`.
#' @param constant scale factor (default is `1.4826`)
#' @param medianType character string selecting the `"standard"`,
#' `"low"`, or `"high"` median for even sample sizes
#' @param na.rm if `TRUE` then `NA` values are stripped from `x`
#' before computation takes place
#' @return a numeric scalar containing the scaled median absolute deviation
#' @seealso [IQR()] which is simpler but less robust,
#' [iqrX()] for the same using weights,\cr [mad()],
#' [median()], [var()] the base R equivalents
#' \cr[lumen::madCI()] (confidence intervals).
#' 
#' @examples
#' 
#' madX(c(1:9))
#' print(madX(c(1:9),     constant = 1)) ==
#'       madX(c(1:8, 100), constant = 1)       # = 2 ; TRUE
#' x <- c(1,2,3,5,7,8)
#' sort(abs(x - median(x)))
#' c(madX(x, constant = 1, medianType="standard"),
#'   madX(x, constant = 1, medianType="low"),
#'   madX(x, constant = 1, medianType="high"))
#' 
#' # use weights
#' x <- sample(20, 30, replace = TRUE)
#' z <- as.numeric(names(w <- table(x)))
#' 
#' (m1 <- madX(z, weights=w))
#' (m2 <- madX(x))
#' stopifnot(identical(m1, m2))
#' 
#'
#' @family dispersion
#' @concept dispersion
#' @concept robust-statistics
#' @export
madX <- function(x,
                weights = NULL,
                center = medianX,
                constant = 1.4826,
                medianType = c("standard", "low", "high"),
                na.rm = FALSE) {
  
  medianType <- match.arg(medianType)
  
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
  
  z <- .normWeights(d, weights, na.rm = FALSE)
  
  ## Median-Index
  n <- length(z$x)
  
  if (medianType == "standard") {

    m <- medianX(z$x, z$weights)

  } else {

    # The low/high branch used to take the plain order statistics
    # z$x[o[k]] and z$x[o[k+1]] with k = n %/% 2, IGNORING z$weights
    # entirely - so madX(x, weights = w, medianType = "low") silently
    # dropped the weights it had just been given and normalized. It also
    # keyed off n %% 2, but with weights the relevant question is not the
    # number of observations, it is whether the cumulative weight lands
    # exactly on half.
    #
    # The weighted low/high median: order by value, accumulate weight,
    # and take the first observation whose cumulative share reaches
    # (low) or exceeds (high) one half. With equal weights this
    # reproduces the previous indices exactly.
    o  <- order(z$x)
    cw <- cumsum(z$weights[o]) / sum(z$weights)

    idx <- if (medianType == "low")
      which(cw >= 0.5)[1L]
    else
      which(cw >  0.5)[1L]

    # cw ends at 1, so "low" always finds a position; "high" does not
    # when the last observation alone tips the balance
    if (is.na(idx)) idx <- length(o)

    m <- z$x[o[idx]]
  }
  
  return(constant * m)
  
}
