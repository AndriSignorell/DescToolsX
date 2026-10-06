
#' (Weighted) Sample Quantiles
#'
#' Compute sample quantiles, with optional weights.
#'
#' Without `weights` the call is handed to [stats::quantile()], so all nine
#' types are available and the results are identical to base R. The one
#' deliberate difference is the treatment of missing values, see below.
#'
#' With `weights` two algorithms exist, and they interpret the weights
#' **differently**:
#'
#' \describe{
#'   \item{`type = 2`}{inverse of the weighted empirical distribution
#'     function, averaging at discontinuities - the weighted counterpart of
#'     [stats::quantile()] type 2. The weights are *relative*: only their
#'     ratios matter, multiplying every weight by a constant leaves the
#'     result unchanged, and equal weights reproduce the unweighted type 2.
#'     This is the Eurostat definition (EU-SILC 131-rev/04). `type = 5` is
#'     accepted as an alias, see the note on DescTools below.}
#'   \item{`type = 7`}{treats the weights as *frequency* weights, i.e. as
#'     replication counts: with integer weights the result equals
#'     `quantile(rep(x, weights), type = 7)`. The effective sample size is
#'     `sum(weights)`, so the result is **not** scale-invariant, and weights
#'     normalized to sum to 1 are degenerate. `type = 7` therefore requires
#'     `sum(weights) >= 2` and raises an error otherwise.}
#' }
#'
#' Relative weights (survey or design weights) call for `type = 2`,
#' replication counts for `type = 7`.
#'
#' **Missing values.** `NA` and `NaN` are treated alike, in `x` and in
#' `weights`; an observation is missing if its value or its weight is.
#' With `na.rm = FALSE`, a missing observation yields `NA` for every
#' requested probability, with and without weights - as [mean()] does
#' ([stats::quantile()] raises an error instead). With `na.rm = TRUE` the
#' missing observations are removed first; if nothing is left, the result
#' is `NA`. Invalid arguments (e.g. `probs` outside \eqn{[0,1]}, negative
#' or infinite weights) are an error even when data are missing. An `NA` in
#' `probs` yields `NA` at that position only.
#'
#' **Difference to DescTools.** `DescTools::Quantile()` labelled the
#' Eurostat algorithm `type = 5`. It is not R's type 5, which interpolates
#' linearly between order statistics: with equal weights the old weighted
#' `type = 5` and the unweighted `type = 5` gave different answers. Here the
#' algorithm carries its correct number 2; `5` still selects it, so results
#' of existing calls do not change.
#'
#' @param x a numeric vector
#' @param weights an optional numeric vector of non-negative, finite sample
#'   weights, of the same length as `x`. Missing weights are handled like
#'   missing values in `x`.
#' @param probs numeric vector of probabilities with values in \eqn{[0,1]};
#'   `NA` is allowed
#' @param na.rm logical; if `TRUE`, observations with a missing value or a
#'   missing weight are removed before the computation
#' @param names logical; if true, the result has a [names()] attribute. Set
#'   to `FALSE` for speedup with many `probs`.
#' @param type an integer selecting the quantile algorithm. Without weights
#'   one of the nine types of [stats::quantile()] (default 7). With weights
#'   2 (alias 5) or 7 (default); see Details.
#' @param digits used only when `names` is true: the precision to use when
#'   formatting the percentages.
#'
#' @return a numeric vector of the same length as `probs`, named when
#'   `names = TRUE`
#'
#' @note The weighted algorithms are based on code by Andreas Alfons and
#'   Matthias Templ (`laeken::weightedQuantile()`), adapted to conform to
#'   package standards.
#'
#' @references
#' Working group on Statistics on Income and Living Conditions (2004).
#' Common cross-sectional EU indicators based on EU-SILC; the gender pay
#' gap. *EU-SILC 131-rev/04*, Eurostat.
#'
#' Hyndman, R. J., Fan, Y. (1996). Sample quantiles in statistical
#' packages. *The American Statistician*, 50(4), 361–365.
#' \doi{10.1080/00031305.1996.10473566}
#'
#' @examples
#' # Pizza$temperature contains missing values: without na.rm the result is
#' # NA for every prob
#' quantileX(Pizza$temperature, weights = rep(1:3, length.out = nrow(Pizza)),
#'           na.rm = TRUE)
#'
#' x <- c(3.7, 3.3, 3.5, 2.8)
#'
#' # type 2 only looks at the ratios of the weights ...
#' quantileX(x, weights = c(5, 5, 4, 1),      type = 2)
#' quantileX(x, weights = c(5, 5, 4, 1) / 15, type = 2)   # identical
#'
#' # ... and equal weights give the unweighted type 2
#' quantileX(x, weights = rep(1, 4), type = 2)
#' quantileX(x, type = 2)
#'
#' # type 7 reads the weights as replication counts
#' quantileX(x, weights = c(5, 5, 4, 1), type = 7)
#' quantileX(rep(x, c(5, 5, 4, 1)), type = 7)              # identical
#'
#' @seealso [medianX()], [iqrX()], [stats::quantile()], [lumen::quantileCI()]
#'
#' @family quantile
#' @concept quantile
#' @concept distribution-summary
#' @export
quantileX <- function(x, probs = seq(0, 1, 0.25), weights = NULL,
                      na.rm = FALSE, names = TRUE, type = 7, digits = 7) {

  # further weighted quantiles in Hmisc and modi, both on CRAN

  # == argument checks =======================================================
  #
  # All of them BEFORE any early NA return: missing data must not mask an
  # invalid call (probs = 1.5, a negative weight, an unknown type).

  if (!is.numeric(probs) && !all(is.na(probs)))
    stop("'probs' must be a numeric vector with values in [0,1]")
  probs <- as.numeric(probs)
  if (isTRUE(any(probs < 0 | probs > 1)))
    stop("'probs' must be a numeric vector with values in [0,1]")

  if (!(length(type) == 1L && type %in% 1:9))
    stop("'type' must be an integer between 1 and 9")

  if (names && length(probs) > 0L)
    stopifnot(is.numeric(digits), digits >= 1)

  if (!is.null(weights)) {

    # c(NA, NA) is LOGICAL: the natural way to write "all missing" must
    # reach the NA handling, not the type check
    if (is.logical(x) && all(is.na(x))) x <- as.numeric(x)
    if (is.logical(weights) && all(is.na(weights)))
      weights <- as.numeric(weights)

    # unweighted, stats::quantile() also takes dates and ordered factors
    if (!is.numeric(x)) stop("'x' must be a numeric vector")
    if (!is.numeric(weights)) stop("'weights' must be a numeric vector")
    if (length(weights) != length(x))
      stop("'weights' must have the same length as 'x'")

    # NA/NaN weights are missing data (handled below), Inf and negative
    # weights are invalid. A negative weight makes cumsum(weights)
    # non-monotonic, and both algorithms read it as an increasing index.
    if (any(is.infinite(weights))) stop("'weights' must be finite")
    if (any(weights < 0, na.rm = TRUE)) stop("'weights' must not be negative")

    # NOTE on the numbering (deviation from DescTools): DescTools::Quantile()
    # and laeken::weightedQuantile() called the Eurostat algorithm below
    # "type 5". It is R's type 2 (inverse ECDF, averaging at jumps), not
    # R's type 5 (linear interpolation, m = 0.5): with equal weights the old
    # weighted "type 5" did NOT agree with quantile(x, type = 5), e.g.
    #   x <- c(2.8, 3.3, 3.5, 3.7), p = 0.3:  3.3 (= type 2) vs 3.15 (type 5)
    # 5 is kept as an alias, silently, so existing calls give the same
    # values.
    if (type == 5) type <- 2
    if (!type %in% c(2, 7))
      stop(gettextf(
        "type = %s is not implemented for weighted quantiles; use 2 or 7",
        type), domain = NA)
  }


  # == missing values =========================================================
  #
  # Suite rule: missing data are not a calling error. na.rm = FALSE -> NA
  # (as mean()), na.rm = TRUE -> drop them first. NaN counts as NA, in x
  # and in the weights; a pair is missing if either part is.
  # stats::quantile() would stop instead, so it must never see NAs.

  # NA result of the documented shape: double, length(probs), named
  naResult <- function() .quantileXNames(rep.int(NA_real_, length(probs)),
                                         probs, names, digits)

  miss <- is.na(x)
  if (!is.null(weights)) miss <- miss | is.na(weights)

  if (any(miss)) {
    if (!isTRUE(na.rm)) return(naResult())
    x <- x[!miss]
    if (!is.null(weights)) weights <- weights[!miss]
  }

  if (is.null(weights))
    return(stats::quantile(x = x, probs = probs, names = names,
                           type = type, digits = digits))


  # == weighted ===============================================================

  # Nothing left after removing the missing pairs: empty data, no warning.
  if (length(x) == 0L)
    return(naResult())

  # Observations left, but none carries weight: an unusable weighting
  # scheme, almost always a mistake by the caller - hence the warning.
  # Undefined is NA, not a fabricated zero.
  keep <- weights > 0
  if (!any(keep)) {
    warning("all weights equal to zero")
    return(naResult())
  }

  # Drop zero-weight observations. They contribute nothing by definition,
  # but leave a repeated value in cumsum(weights), which the exact
  # comparison in type 2 and approx() in type 7 (tie collapsing, with a
  # warning) both trip over.
  x <- x[keep]
  weights <- weights[keep]
  n <- length(x)

  o <- order(x)
  x <- x[o]
  weights <- weights[o]

  if (type == 2) {

    rw <- cumsum(weights) / sum(weights)

    # Tolerance instead of rw == p: normalized weights such as rep(0.1, 10)
    # accumulate rounding error in cumsum(), and whether the averaging case
    # was hit then depended on the scale of the weights - exactly the
    # invariance this type promises.
    tol <- sqrt(.Machine$double.eps)

    qs <- vapply(probs, function(p) {
      if (is.na(p)) return(NA_real_)
      if (p <= 0)   return(x[1L])
      if (p >= 1)   return(x[n])
      select <- which.max(rw >= p - tol)    # first hit; rw[n] == 1
      if (abs(rw[select] - p) < tol && select < n)
        (x[select] + x[select + 1L]) / 2
      else
        x[select]
    }, numeric(1))

  } else {

    # Replication counts: the sum takes the place of the sample size, and
    # cumsum(weights) indexes the order statistics. Not scale invariant.
    #
    # With weights normalized to sum to 1, sumW is 1 and
    #     ord = 1 + (sumW - 1) * probs = 1   for EVERY prob,
    # so every quantile collapsed onto the largest observation. Hence the
    # error instead of a silent, plausible-looking wrong answer.
    sumW <- sum(weights)

    if (sumW < 2)
      stop(gettextf(
        paste("type = 7 reads 'weights' as replication counts, so their sum",
              "(%g) must be at least 2. Rescale them to counts, or use",
              "type = 2, which depends only on their ratios."),
        sumW), domain = NA)

    ord  <- 1 + (sumW - 1) * probs
    low  <- pmax(floor(ord), 1)
    high <- pmin(low + 1, sumW)
    frac <- ord %% 1

    # smallest x whose cumulative frequency is >= low resp. high;
    # NA probs pass through approx() as NA
    k <- length(probs)
    allq <- stats::approx(cumsum(weights), x, xout = c(low, high),
                          method = "constant", f = 1, rule = 2)$y
    qs <- (1 - frac) * allq[seq_len(k)] + frac * allq[k + seq_len(k)]
  }

  .quantileXNames(qs, probs, names, digits)
}


# == internal helper functions ================================================

# names exactly as stats::quantile() would set them, "" for NA probs included
.quantileXNames <- function(qs, probs, names, digits) {

  if (names && length(probs) > 0L) {
    names(qs) <- names(stats::quantile(0, probs = probs, names = TRUE,
                                       type = 1, digits = digits))
  }
  qs
}
