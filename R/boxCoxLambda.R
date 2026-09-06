
#' Automatic Selection of Box-Cox Transformation Parameter
#'
#' Selects the Box-Cox transformation parameter automatically, using either
#' Guerrero's method or the profile log likelihood.\cr Guerrero's (1993)
#' method yields a lambda which
#' minimizes the coefficient of variation for subseries of `x`.  For
#' method `"loglik"`, the value of lambda is chosen to maximize the
#' profile log likelihood of a linear model fitted to `x`.  For
#' non-seasonal data, a linear time trend is fitted while for seasonal data, a
#' linear time trend with seasonal dummy variables is used.
#'
#'
#' @param x a numeric vector or univariate time series. All values must be
#' strictly positive and finite, as the Box-Cox transformation is undefined
#' otherwise; missing values are not removed but rejected, since subsetting
#' would strip a `ts` of its frequency and cycle positions.
#' @param method method to be used in calculating lambda. Can be either
#' `"guerrero"` (default) or `"loglik"`.
#' @param lower lower limit for possible lambda values; defaults to -1
#' @param upper upper limit for possible lambda values; defaults to 2
#' @param nonseasonalLength number of observations per subseries used by the
#' `"guerrero"` method for non-seasonal data, default is 2. Must be a
#' whole number \eqn{\ge 2}. For seasonal time series the series' own
#' frequency is used instead, whenever it is larger.
#' @return a numeric scalar containing the estimated Box-Cox transformation
#' parameter
#'
#' @details
#' Seasonality is taken from `x` itself: a [stats::ts()] object
#' with `frequency(x) > 1` is treated as seasonal, anything else
#' (including a plain numeric vector) as non-seasonal. For method
#' `"loglik"` the profile log likelihood is therefore computed from
#' `lm(x ~ trend)` for non-seasonal data and from
#' `lm(x ~ trend + factor(cycle(x)))` for seasonal data. Both methods
#' optimise lambda continuously over `[lower, upper]` via
#' [stats::optimize()].
#'
#' Both methods need enough data to identify their criterion, and signal an
#' error rather than falling back silently when they do not have it:
#' `"loglik"` requires at least three observations, and more than
#' `frequency(x) + 1` for a seasonal series, that being the number of
#' parameters in the seasonal model (intercept, trend and
#' `frequency(x) - 1` dummies); `"guerrero"` requires at least two
#' complete subseries. Constant series are rejected by both, since the
#' coefficient of variation degenerates to \eqn{0/0} and the profile log
#' likelihood is singular.
#'
#' @note Based on code by Leanne Chhay and Rob J Hyndman previously
#' published as `BoxCox.lambda()` in the \pkg{forecast} package, adapted
#' to conform to package standards.
#'
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. *JRSS B* **26** 211--246.
#'
#' Guerrero, V.M. (1993) Time-series analysis supported by power
#' transformations. *Journal of Forecasting*, **12**, 37--48.
#'
#' @examples
#' lambda <- boxCoxLambda(AirPassengers)
#'
#' # profile log likelihood, seasonal trend model
#' boxCoxLambda(AirPassengers, method = "loglik")
#'
#' # plain numeric vector, treated as non-seasonal
#' set.seed(1)
#' boxCoxLambda(rlnorm(100), method = "loglik")
#'
#' @family transform
#' @concept transformation
#' @concept variance-stabilization
#' @export
boxCoxLambda <- function(x, method = c("guerrero", "loglik"),
                         lower = -1, upper = 2, nonseasonalLength = 2) {

  method <- match.arg(method)

  if (!is.numeric(x) || (!is.null(dim(x)) && !identical(length(dim(x)), 1L)))
    stop("'x' must be a numeric vector or a univariate time series")

  # Non-finite values are rejected rather than dropped: subsetting a ts
  # silently strips its class, so frequency, cycle positions and the
  # spacing of the series would all be lost - a monthly series would
  # thereafter be treated as non-seasonal. Callers should decide how to
  # impute or trim, since only they know whether the gap is meaningful.
  if (any(!is.finite(x)))
    stop("'x' must not contain missing or non-finite values")

  # The Box-Cox transformation is defined for strictly positive x only:
  # at lambda == 0 it is log(x), and for lambda != 0 the power x^lambda
  # is not real-valued for x < 0. Clamping lower to 0 (as an earlier
  # version did) does not rescue non-positive input - it merely picks the
  # log branch, which is undefined just the same. Callers needing to
  # handle non-positive data should shift the series themselves.
  if (any(x <= 0))
    stop("all values of 'x' must be positive")

  # method "loglik" fits intercept + trend, so two observations are
  # interpolated exactly (rss == 0, log(rss) == -Inf). Three is the
  # minimum leaving a residual degree of freedom.
  nMin <- if (method == "loglik") 3L else 2L
  if (length(x) < nMin)
    stop(gettextf("'x' must contain at least %d values for method \"%s\"",
                  nMin, method), domain = NA)

  # A constant series has zero variation: Guerrero's ratio degenerates to
  # 0/0 and the profile log likelihood is singular for every lambda.
  if (isTRUE(all.equal(max(x), min(x))))
    stop("'x' must not be constant")

  .checkScalarNum <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
      stop(gettextf("'%s' must be a single finite numeric value", name),
           domain = NA)
  }
  .checkScalarNum(lower, "lower")
  .checkScalarNum(upper, "upper")

  if (lower >= upper)
    stop("'lower' must be strictly less than 'upper'")

  # %% 1, not as.integer(): as.integer() overflows to NA (with a warning)
  # for anything beyond .Machine$integer.max, and the NA then propagated
  # into the if() condition as "missing value where TRUE/FALSE needed".
  if (!is.numeric(nonseasonalLength) || length(nonseasonalLength) != 1L ||
      !is.finite(nonseasonalLength) || nonseasonalLength < 2 ||
      nonseasonalLength %% 1 != 0)
    stop("'nonseasonalLength' must be a single whole number >= 2")

  if (method == "loglik")
    .bcLogLik(x, lower = lower, upper = upper)
  else
    .guerrero(x, lower = lower, upper = upper,
              nonseasonalLength = nonseasonalLength)
}


# == internal helper functions ===========================================

# Seasonality is a property of x, not a user argument: a ts with
# frequency > 1 is seasonal, anything else (plain numeric included) is
# not. Both helpers below need this, so it lives in one place.
.seasonalPeriod <- function(x) {
  period <- if (stats::is.ts(x)) stats::frequency(x) else 1
  # round(), not as.integer(): truncating a frequency of e.g. 1.9 would
  # yield 1 and silently declare the series non-seasonal. forecast's
  # original implementation rounds as well.
  if (!is.finite(period) || period < 1) 1L else as.integer(round(period))
}


.guerrero <- function(x, lower = -1, upper = 2, nonseasonalLength = 2) {

  # Guerrero extracts the required lambda
  # Input: x = original time series as a time series object
  # Output: lambda that minimises the coefficient of variation

  period <- max(as.integer(nonseasonalLength), .seasonalPeriod(x))
  nObs <- length(x)
  nSub <- nObs %/% period

  # With fewer than two complete subseries there is no between-subseries
  # variation left to minimise, and matrix() below would recycle silently.
  if (nSub < 2L)
    stop(gettextf(
      "'x' is too short for method \"guerrero\": needs at least %d observations for a period of %d",
      2L * period, period), domain = NA)

  # guerCv computes the coefficient of variation
  # Input:
  #             lam  = lambda
  #             xMat = x arranged as a period x nSub subseries matrix
  # Output: coefficient of variation
  guerCv <- function(lam, xMat) {
    xMean <- colMeans(xMat, na.rm = TRUE)
    xSd <- apply(xMat, 2, stats::sd, na.rm = TRUE)
    xRat <- xSd / xMean^(1 - lam)
    ratMean <- mean(xRat, na.rm = TRUE)

    # Subseries without variation give xSd == 0 throughout, so the ratio
    # collapses to 0/0. The constant-input check in boxCoxLambda() covers
    # the global case; this catches locally degenerate subseries.
    if (!is.finite(ratMean) || ratMean == 0)
      stop("the coefficient of variation is not finite; subseries of 'x' show no variation")

    stats::sd(xRat, na.rm = TRUE) / ratMean
  }

  # Trailing observations are used: an incomplete leading subseries is
  # dropped, so the most recent complete cycles drive the estimate.
  nUsed <- nSub * period
  xMat <- matrix(x[(nObs - nUsed + 1L):nObs], nrow = period, ncol = nSub)

  stats::optimize(guerCv, c(lower, upper), xMat = xMat)$minimum
}


# Modified version of boxCox from MASS package
.bcLogLik <- function(x, lower = -1, upper = 2) {

  n <- length(x)
  logx <- log(x)
  xdot <- exp(mean(logx))

  # The model whose profile log likelihood is maximised: a linear time
  # trend for non-seasonal data, plus seasonal dummies when x carries a
  # frequency > 1. Building the design matrix directly avoids a
  # dependency on forecast::tslm for the seasonal case.
  period <- .seasonalPeriod(x)
  trend <- seq_len(n)

  design <- if (period > 1L) {
    # The seasonal model has period + 1 parameters: intercept, trend and
    # period - 1 dummies. Positive residual degrees of freedom therefore
    # require n > period + 1 - a monthly series of 24 values is well
    # identified (24 - 13 = 11 df). Requiring two full cycles would be a
    # stability preference, not a rank condition, so it is not imposed.
    if (n <= period + 1L)
      stop(gettextf(
        "'x' has frequency %d but only %d observations; method \"loglik\" needs more than %d for the seasonal model",
        period, n, period + 1L), domain = NA)

    cycleIdx <- droplevels(factor(stats::cycle(x)))
    if (nlevels(cycleIdx) < 2L)
      stop("'x' covers only one seasonal position; no seasonal model can be fitted")

    stats::model.matrix(~ trend + cycleIdx)

  } else {
    stats::model.matrix(~ trend)
  }

  # qr() once, outside the objective: the design is fixed and only the
  # transformed response varies with lambda.
  xqr <- qr(design)

  if (xqr$rank < ncol(design))
    stop("the model matrix is rank-deficient; 'x' is too short or too regular")

  negLogLik <- function(lam) {
    # Near lambda == 0 the direct form (x^lam - 1)/lam is numerically
    # unstable, so a Taylor expansion around log(x) is used instead.
    xt <- if (abs(lam) > 0.02)
      (x^lam - 1) / lam
    else
      logx * (1 + (lam * logx) / 2 * (1 + (lam * logx) / 3 * (1 + (lam * logx) / 4)))

    rss <- sum(qr.resid(xqr, xt / xdot^(lam - 1))^2)

    # A perfectly fitted (e.g. exactly linear) series gives rss == 0 and
    # log(rss) == -Inf, which optimize() cannot work with. Signal the
    # degenerate case rather than returning a non-finite objective.
    if (!is.finite(rss) || rss <= 0)
      stop("the profile log likelihood is not finite; 'x' is fitted exactly by the model")

    # optimize() minimises, so return the negated profile log likelihood
    # (-n/2 * log(rss) becomes +n/2 * log(rss)).
    n / 2 * log(rss)
  }

  stats::optimize(negLogLik, c(lower, upper))$minimum
}
