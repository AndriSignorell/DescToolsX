
#' Kurtosis
#'
#' `kurt()` returns the excess kurtosis, therefore the kurtosis calculates
#' as `kurt(x) + 3` if required.
#'
#' If `na.rm` is `TRUE` then missing values in `x` and in `weights` are
#' removed, pairwise, before computation proceeds. \cr
#'
#' The estimator for calculating kurtosis can either be:\cr
#' `1: g_2 = m_4 / m_2^2 - 3 ` \cr
#' `2: G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) ` \cr
#' `3: b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 ` \cr
#'
#' 1 is the typical definition used in Stata and in many older textbooks.  \cr
#' 2 is used in SAS and SPSS; it is unbiased under normality, not in
#' general.  \cr 3 is used in MINITAB and BMDP. \cr
#'
#' Estimators 1 and 3 need \eqn{n \ge 2}, estimator 2 needs \eqn{n \ge 4}.
#' For shorter or constant input the kurtosis is not defined: the result is
#' `NA` (all three elements if an interval was requested) with a warning.
#'
#' `method = "classic"` uses a Wald interval with the standard error of the
#' kurtosis under normality. For estimator 1 this is the exact standard
#' error of \eqn{g_2}
#' \preformatted{SE(g_2) = sqrt(24*n*(n - 2)*(n - 3) / ((n + 1)^2*(n + 3)*(n + 5)))}
#' and since estimators 2 and 3 are linear in \eqn{g_2}, their standard
#' errors follow by multiplying with the respective slope,
#' \eqn{(n-1)(n+1)/((n-2)(n-3))} and \eqn{((n-1)/n)^2}. For estimator 2
#' this gives the familiar formula of Cramer (1997), as reported by SPSS:
#' \preformatted{SES = sqrt(6*n*(n - 1) / ((n - 2)*(n + 1)*(n + 3)))
#' SEK = 2 * SES * sqrt((n^2 - 1) / ((n - 3)*(n + 5)))
#'     = sqrt(24*n*(n - 1)^2 / ((n - 3)*(n - 2)*(n + 3)*(n + 5)))}
#' The standard error is defined from \eqn{n = 4} on; below, the interval
#' is `NA` with a warning. The interval remains a Wald approximation even
#' though the variance is exact under normality. \cr
#' However, Joanes and Gill (1998) advise
#' against this approach, pointing out that the normal assumptions would
#' virtually always be violated.  They suggest using the bootstrap method.
#' That's why the default method for the confidence interval type is set to
#' `"boot"`. If not further specified the boot ci type will be chosen as
#' `"bca"`. If some bootstrap replicates are undefined (a constant
#' resample) or [boot::boot.ci()] cannot compute the interval, the bounds
#' are `NA` with a warning. \cr
#'
#' **Weights** are frequency weights: `sum(weights)` takes the place of
#' \eqn{n} in the bias corrections and in the standard error. For integer
#' weights `kurt(x, weights = w)` equals `kurt(rep(x, w))`. An observation
#' with weight 0 is absent: its `x` may be `NA` or infinite without effect.
#' A missing weight makes the observation missing. The bootstrap resamples
#' the expanded sample and therefore requires integer weights (memory grows
#' with `sum(weights)`, and BCa's jackknife with it). Non-integer weights
#' are admitted for the point estimate and `method = "classic"` as a
#' computational extension of the same formulas; they correspond to no
#' actual sample, so the exact normal-theory variance does not strictly
#' apply to them. Analysis or survey weights are
#' not supported: neither the bias correction nor the row bootstrap would
#' reflect them.
#'
#' This implementation is comparably fast, as the expensive sums are coded
#' in C++. The data are rescaled before centring and again after, so that
#' neither the deviations nor their powers can overflow for any finite
#' input.
#'
#' @name kurt
#'
#' @param x a numeric vector. A matrix or array contributes all its
#' elements, as `as.vector(x)`.
#' @param estimator integer, either 1, 2 or 3 (default) defining the algorithm
#' used for calculation. See Details.
#' @param weights a non-negative numeric vector of frequency weights, the
#' same length as `x`. See Details.
#'
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned. One-sided
#'   intervals require `conf.level > 0.5`.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#' @param method character string specifying the confidence interval method.
#'   `"boot"` (default) uses a nonparametric bootstrap, with BCa
#'   intervals unless another bootstrap type is supplied through `\dots`;
#'   `"classic"` uses a Wald interval based on the standard error under
#'   normality. See Details and [ConfidenceIntervals()].
#'
#' @param na.rm logical, indicating whether `NA` values should be stripped
#' before the computation proceeds. Defaults to `FALSE`.
#' @param \dots bootstrap settings for `method = "boot"`, and only these:
#' `R` (number of replicates), `type` (one of `"bca"`, `"perc"`,
#' `"basic"`, `"norm"`; `"stud"` is not supported, as no suitable
#' variance estimate is implemented here), `parallel` and
#' `ncpus` as in [boot::boot()]. Other arguments are an error.
#'
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{kurtosis estimate}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @references Cramer, D. (1997): *Basic Statistics for Social Research*
#' Routledge.
#'
#' Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and
#' kurtosis. *The Statistician*, 47, 183-189.
#'
#' @examples
#'
#' kurt(bedrock::Pizza$price, na.rm=TRUE)
#'
#' # use sapply to calculate kurtosis for a data.frame
#' sapply(bedrock::Pizza[,c("temperature","price","delivery_min")],
#'        kurt, na.rm=TRUE)
#'
#' # or apply to do that columnwise with a matrix
#' apply(as.matrix(bedrock::Pizza[,c("temperature","price","delivery_min")]),
#'       2, kurt, na.rm=TRUE)
#'
#' # frequency weights are the same as repeating the values
#' kurt(c(1, 2, 3, 10), weights = c(3, 1, 2, 1))
#' kurt(rep(c(1, 2, 3, 10), c(3, 1, 2, 1)))
#'
#' @seealso [skew], [meanX], [sdX], similar code in \pkg{e1071}
#'
#' @section Random number generation:
#' `method = "boot"` - the default - resamples and therefore advances
#' R's global random number generator. Call [base::set.seed()]
#' beforehand for reproducible intervals.
#'
#' @family shape
#' @concept descriptive-statistics
#' @concept robust-statistics
#' @export
kurt <- function(x,
                 conf.level = NA,
                 sides = c("two.sided", "left", "right"),
                 method = c("boot", "classic"),
                 estimator = 3,
                 weights = NULL,
                 na.rm = FALSE,
                 ...) {

  .shape(.i.kurt, x,
         conf.level = conf.level,
         sides      = match.arg(sides),
         method     = match.arg(method),
         estimator  = estimator,
         weights    = weights,
         na.rm      = na.rm,
         ...)
}


# == internal helper functions ================================================


# Kernel for .shape(): x and w are clean (see .shapeData()), n is the
# sample size (sum of the frequency weights). Returns c(est, se) or an
# .shapeUndefined() with the reason.
.i.kurt <- function(x, w, n, estimator) {

  nmin <- if (estimator == 2) 4 else 2

  if (n < nmin)
    return(.shapeUndefined(sprintf(
      "kurtosis estimator %d needs n >= %d, got n = %s",
      estimator, nmin, format(n))))

  # tested in R, not only via a NaN from C++: a mean need not reproduce a
  # constant exactly, and the rescaled moments would then report -2
  if (all(x == x[1L]))
    return(.shapeUndefined("kurtosis is undefined for constant 'x'"))

  g2 <- if (is.null(w)) kurt_cpp(x) else kurt_weighted_cpp(x, w)

  if (!is.finite(g2))
    return(.shapeUndefined("kurtosis could not be computed"))

  # All corrections are written in u = 1/n, so that a large n (e.g. a big
  # sum of frequency weights) cannot overflow the products n^3, n^4.
  u <- 1 / n

  # exact SE of g_2 under normality, 24n(n-2)(n-3)/((n+1)^2(n+3)(n+5)),
  # defined from n = 4 on (at n = 3 the radicand is 0). Estimators 2 and 3
  # are linear in g_2, so their SE is this one times the slope; for
  # estimator 2 that is Cramer's (1997)
  # sqrt(24n(n-1)^2 / ((n-3)(n-2)(n+3)(n+5))).
  se <- if (n < 4) NA_real_
        else sqrt(24 * (1 - 2 * u) * (1 - 3 * u) /
                    (n * (1 + u)^2 * (1 + 3 * u) * (1 + 5 * u)))

  switch(estimator,
    # 1: g_2, older textbooks / Stata
    c(est = g2, se = se),
    # 2: G_2 = ((n+1) g_2 + 6)(n-1) / ((n-2)(n-3)), SAS / SPSS
    c(est = ((1 + u) * g2 + 6 * u) * (1 - u) / ((1 - 2 * u) * (1 - 3 * u)),
      se  = se * (1 - u) * (1 + u) / ((1 - 2 * u) * (1 - 3 * u))),
    # 3: b_2 = (g_2 + 3)((n-1)/n)^2 - 3, MINITAB / BMDP
    c(est = (g2 + 3) * (1 - u)^2 - 3,
      se  = se * (1 - u)^2)
  )
}
