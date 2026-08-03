#' (Weighted) Variance and Standard Deviation
#'
#' \code{varX()} computes the variance of \code{x}, allowing the definition of
#' weights (unlike base R's \code{\link{var}()} function). Using the estimator
#' \code{ml} returns the uncorrected sample variance (which is a biased
#' estimator for the sample variance). \cr\code{sdX} yields the standard
#' deviation following the same logic.
#'
#' Using estimator \code{"unbiased"} the denominator \eqn{n - 1} is used (known
#' as "Bessel's correction") which gives an unbiased estimator of the
#' (co)variance for i.i.d. observations.\cr \code{"ml"} yields the biased
#' version using the denominator \eqn{n}. With frequency weights \eqn{n} is the
#' sum of the weights.
#'
#' These functions return \code{\link{NA}} when there is only one observation
#' and \code{NA} when \code{x} has length zero.
#'
#' \bold{Note:}\verb{ } Analytic (precision) weights are not supported. For
#' likelihood-based weighted variance estimation, see
#' \code{\link[stats]{cov.wt}}.
#'
#' @name varX
#' @aliases varX varX.default varX.Freq sdX
#'
#' @param x a numeric vector, matrix, or data frame
#' @param estimator determines the estimator type; if \code{"unbiased"} (the
#' default) then the usual unbiased estimate (using \eqn{n - 1} as denominator)
#' is returned, if \code{"ml"} then it is the maximum likelihood estimate for a
#' Gaussian distribution (denominator \eqn{n}).
#' @param weights non-negative numeric vector of weights the same length as
#' \code{x}, interpreted as frequency (replication) weights. Observations with
#' larger weights contribute more strongly to the empirical distribution.
#' Weights are supported for vector input only.
#' @param na.rm logical. Should missing values be removed?
#' @param breaks breaks for calculating the variance for classified data as
#' composed by \code{\link{freq}}
#' @param \dots further arguments passed to or from other methods
#'
#' @return
#' \describe{
#'   \item{\code{varX()}}{a numeric scalar for vector input or a covariance
#'     matrix for a matrix or data frame}
#'   \item{\code{sdX()}}{a numeric scalar containing the standard deviation}
#' }
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}.  Wadsworth & Brooks/Cole.
#'
#' @seealso \code{\link[lumen]{varCI}} for confidence intervals,
#' \code{\link[lumen]{varTest}} for tests and base R's implementations
#' \code{\link{var}}, \code{\link{sd}}, \code{\link{cov}}
#'
#' @examples
#'
#' varX(1:10)                 # 9.166667
#' sdX(1:10)
#'
#' # frequency weights replicate the observations, so the result is the
#' # variance of the expanded vector c(1, 2,2, 3,3,3, 4,4,4,4, 5,5,5,5,5)
#' varX(1:5, weights=1:5)     # 1.666667
#' varX(rep(1:5, times=1:5))  # 1.666667
#'
#' # weighted Variance
#' set.seed(45)
#' (z <- as.numeric(names(w <- table(x <- sample(-10:20, size=50, replace=TRUE)))))
#' varX(z, weights=w)
#' sdX(z, weights=w)
#'
#' # check!
#' all.equal(varX(x), varX(z, weights=w))
#'
#'
#' # Variance for frequency tables
#' varX(freq(as.table(c(6,16,24,25,17))),
#'           breaks=c(0, 10, 20, 30, 40, 50))
#'
#'
#' @rdname varX
#'
#' @family dispersion
#' @concept dispersion
#'
#' @export
sdX <- function (x, estimator = c("unbiased", "ml"),
                 weights = NULL, na.rm = FALSE, ...) {

  # classified data: everything is handled by the Freq method, the
  # arguments of which are passed through the dots (breaks= in particular)
  if (inherits(x, "Freq"))
    return(sqrt(varX(x, ...)))

  sqrt(varX(if (is.vector(x) || is.factor(x)) x else as.double(x),
            estimator = estimator, weights = weights, na.rm = na.rm, ...))
}


#' @rdname varX
#' @export
varX <- function (x, ...)
  UseMethod("varX")



#' @rdname varX
#' @export
varX.default <- function(x, estimator = c("unbiased", "ml"),
                         weights = NULL, na.rm = FALSE, ...) {

  estimator <- match.arg(estimator)

  ## matrix / data frame: return the covariance matrix, do not subset
  ## (x[ok] would silently drop dim and turn the result into a scalar)
  if (is.matrix(x) || is.data.frame(x)) {

    if (!is.null(weights))
      stop("Weights are not supported for matrix or data frame input.")

    res <- var(x, na.rm = na.rm)

    if (estimator == "ml") {
      n <- if (na.rm) sum(stats::complete.cases(x)) else nrow(x)
      res <- res * (n - 1) / n
    }

    return(res)
  }

  if (!is.numeric(x) && !is.logical(x))
    stop("Argument 'x' must be numeric.")

  ## NA-Handling
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
    if (!is.null(weights))
      weights <- weights[ok]
  }

  ## Weights?
  if (is.null(weights)) {

    res <- var(x = x, na.rm = na.rm)

    if (estimator == "ml") {
      # n must be derived from x - 'ok' does not exist unless na.rm was TRUE
      n <- sum(!is.na(x))
      if (n > 0L)
        res <- res * (n - 1) / n
    }

  } else {

    if (length(weights) != length(x))
      stop("Argument 'weights' must have the same length as 'x'.")
    if (anyNA(weights) || any(weights < 0))
      stop("Argument 'weights' must be non-negative and must not contain NAs.")

    z <- .normWeights(x, weights, na.rm = FALSE)

    if (estimator == "ml"){
      res <- as.numeric(stats::cov.wt(cbind(z$x), z$weights, method = "ML")$cov)

    } else {

      if (z$wsum <= 1)
        return(NA_real_)

      # use z$x throughout - x and z$x need not be the same vector
      xbar <- sum(z$weights * z$x) / z$wsum
      res <- sum(z$weights * ((z$x - xbar)^2)) / (z$wsum - 1)
    }

  }

  return( res )

}



#' @rdname varX
#' @export
varX.Freq <- function(x, breaks, estimator = c("unbiased", "ml"), ...)  {

  estimator <- match.arg(estimator)

  if (missing(breaks))
    stop("Argument 'breaks' is required for objects of class 'Freq'.")
  if (!is.numeric(breaks) || length(breaks) != nrow(x) + 1L)
    stop("Argument 'breaks' must be numeric of length nrow(x) + 1.")

  n   <- sum(x$freq)
  if (n < 2)
    return(NA_real_)

  # class midpoints
  mid <- head(moveAvg(breaks, order = 2, align = "left"), -1)

  mu  <- sum(mid * x$perc)
  ss  <- sum(mid^2 * x$freq) - n * mu^2

  if (estimator == "ml") ss / n else ss / (n - 1)

}
