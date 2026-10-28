
#' Skewness
#'
#' `skew` computes the skewness, [kurt] the excess kurtosis of the
#' values in x.
#'
#' If `na.rm` is `TRUE` then missing values in `x` and in `weights` are
#' removed, pairwise, before computation proceeds. \cr
#'
#' The estimator for calculating the skewness can either be:\cr `1: g_1 =
#' m_3 / m_2^(3/2) ` \cr `2: G_1 = g_1 * sqrt(n(n-1)) / (n-2) `\cr
#' `3: b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) ` \cr
#'
#' 1 is the typical definition used in Stata and in many older textbooks.  \cr
#' 2 is used in SAS and SPSS.  \cr 3 is used in MINITAB and BMDP. \cr
#'
#' Estimators 1 and 3 need \eqn{n \ge 2}, estimator 2 needs \eqn{n \ge 3}.
#' For shorter or constant input the skewness is not defined: the result is
#' `NA` (all three elements if an interval was requested) with a warning.
#'
#' Cramer (1997) mentions the asymptotic standard error of `G_1`, that is
#' of `estimator = 2`:
#' \preformatted{ASE.skew = sqrt( 6*n*(n-1)/((n-2)*(n+1)*(n+3)) )} to be
#' used for calculating the confidence intervals. The standard errors of the
#' other two estimators follow from it by the same factors that relate the
#' estimators themselves, so that `est/se` does not depend on the choice
#' of `estimator`. This is implemented here with `method="classic"`. The
#' standard error is defined from \eqn{n = 3} on; below, the interval is
#' `NA` with a warning.
#' \cr However, Joanes and Gill (1998) advise
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
#' weights `skew(x, weights = w)` equals `skew(rep(x, w))`. An observation
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
#' @name skew
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
#'   `"classic"` uses a Wald interval based on the asymptotic standard
#'   error. See Details and [ConfidenceIntervals()].
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
#'   \item{`est`}{skewness estimate}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @references
#' Cramer, D. (1997): *Basic Statistics for Social Research*
#' Routledge.
#'
#' Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and
#' kurtosis. *The Statistician*, 47, 183-189.
#'
#' @examples
#'
#' skew(bedrock::Pizza$price, na.rm=TRUE)
#'
#' # use sapply to calculate skewness for a data.frame
#' sapply(bedrock::Pizza[,c("temperature","price","delivery_min")],
#'        skew, na.rm=TRUE)
#'
#' # the estimate lies inside its own confidence interval
#' set.seed(1)
#' skew(rlnorm(50), conf.level=0.95, method="classic")
#'
#' # frequency weights are the same as repeating the values
#' skew(c(1, 2, 3, 10), weights = c(3, 1, 2, 1))
#' skew(rep(c(1, 2, 3, 10), c(3, 1, 2, 1)))
#'
#' @seealso [kurt], [meanX], [sdX], similar code in \pkg{e1071}
#'
#' @section Random number generation:
#' `method = "boot"` - the default - resamples and therefore advances
#' R's global random number generator. Call [base::set.seed()]
#' beforehand for reproducible intervals.
#'
#' @family shape
#' @concept moments
#' @export
skew <- function(x,
                 conf.level = NA,
                 sides = c("two.sided", "left", "right"),
                 method = c("boot", "classic"),
                 estimator = 3,
                 weights = NULL,
                 na.rm = FALSE,
                 ...) {

  .shape(.i.skew, x,
         conf.level = conf.level,
         sides      = match.arg(sides),
         method     = match.arg(method),
         estimator  = estimator,
         weights    = weights,
         na.rm      = na.rm,
         ...)
}


# == internal helper functions ================================================


# Kernel for .shape(), see .i.kurt()
.i.skew <- function(x, w, n, estimator) {

  nmin <- if (estimator == 2) 3 else 2

  if (n < nmin)
    return(.shapeUndefined(sprintf(
      "skewness estimator %d needs n >= %d, got n = %s",
      estimator, nmin, format(n))))

  if (all(x == x[1L]))
    return(.shapeUndefined("skewness is undefined for constant 'x'"))

  g1 <- if (is.null(w)) skew_cpp(x) else skew_weighted_cpp(x, w)

  if (!is.finite(g1))
    return(.shapeUndefined("skewness could not be computed"))

  # corrections in u = 1/n, see .i.kurt()
  u <- 1 / n

  # asymptotic SE of g_1 under normality, 6(n-2)/((n+1)(n+3)), defined from
  # n = 3 on; the other estimators scale it by the factor that relates them
  # to g_1, so est/se is the same for all three
  se <- if (n < 3) NA_real_
        else sqrt(6 * (1 - 2 * u) / (n * (1 + u) * (1 + 3 * u)))

  f <- switch(estimator,
    1,                            # g_1, older textbooks / Stata
    sqrt(1 - u) / (1 - 2 * u),    # G_1 = g_1 sqrt(n(n-1))/(n-2), SAS / SPSS
    (1 - u)^(3 / 2)               # b_1 = g_1 ((n-1)/n)^(3/2), MINITAB / BMDP
  )

  c(est = f * g1, se = f * se)
}
