
#' Relative Risk
#'
#' Computes the relative risk for a 2x2 contingency table together with
#' optional confidence intervals.
#'
#' The relative risk compares the event probability in the exposed group with
#' the event probability in the unexposed group.
#'
#' The function expects the exposure groups in the rows and the outcome in the
#' columns:
#'
#' \preformatted{
#'                outcome = 1   outcome = 0
#' exposed = 1        x1           n1 - x1
#' exposed = 0        x2           n2 - x2
#' }
#'
#' The relative risk is defined as:
#'
#' \deqn{
#' RR = \frac{x_1 / n_1}{x_2 / n_2}
#' }
#'
#' Confidence intervals can be calculated using the score method of Koopman
#' (1984), a Wald approximation, or via transformation of the odds ratio.
#'
#' @param x a numeric 2x2 matrix or table of finite, non-negative integer
#'   counts, with positive row totals; alternatively an exposure vector when
#'   `y` is supplied. The first row is the exposed group and the first column
#'   is the event, regardless of their names.
#' @param y an optional outcome vector. If supplied, `table(x, y, ...)` is
#'   computed without reordering its levels. For binary 0/1 data, use
#'   `factor(x, levels = c(1, 0))` and `factor(y, levels = c(1, 0))` to obtain
#'   the orientation shown above. Missing values follow [table()] conventions.
#' @param conf.level a single confidence level strictly between 0 and 1,
#'   or `NA` (the default) to return only the point estimate. One-sided
#'   intervals require a confidence level greater than 0.5.
#' @param sides character string specifying the sidedness of the confidence
#'   interval: `"two.sided"` (default), `"left"`, or `"right"`.
#'   `"left"` retains the lower bound and sets the upper bound to `Inf`;
#'   `"right"` sets the lower bound to 0 and retains the upper bound.
#'   See also [ConfidenceIntervals()].
#' @param method character string specifying the interval method:
#'   `"score"` (default), `"wald"`, or `"use-or"`. See Details.
#' @param delta a single finite, non-negative number (default 0), the cell
#'   correction of the Wald-type methods. For `method = "wald"` it is added to
#'   each of the four cells before computing both the point estimate and the
#'   interval, so each row total increases by `2 * delta`; this also applies
#'   when only the point estimate is requested. For `method = "use-or"` it is
#'   passed to the Wald odds-ratio interval only; the estimate and `p2` stay
#'   uncorrected. `"score"` ignores `delta` after validating it.
#' @param \dots further arguments passed to [table()] when `y` is supplied
#'
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named numeric
#'   vector with elements `est`, `lci`, and `uci` for the estimate and bounds.
#'   Without a Wald correction, no events in either group give `est = NaN`;
#'   events only in the first group give `est = Inf`, and events only in the
#'   second group give `est = 0`. The score interval remains available in these
#'   cases, including `[0, Inf]` when neither group has events.
#'
#' @details
#' The score interval uses the uncorrected Koopman (1984) score statistic.
#' It does not include the `N / (N - 1)` variance correction associated with
#' the Miettinen--Nurminen variant, where `N = n1 + n2`. Bounds are found by
#' inverting the score statistic on the log risk-ratio scale. Closed forms
#' apply when both groups have no events or both groups contain only events.
#' If only the reference group has no events, the rows are reversed and the
#' resulting bounds are inverted. The numerical search expands adaptively;
#' failure to bracket a bound within numerical limits raises an error rather
#' than returning an artificial zero or infinite endpoint.
#'
#' The Wald method uses the log risk ratio and variance
#' `1 / x1 - 1 / n1 + 1 / x2 - 1 / n2`, evaluated after any cell correction.
#' With `delta = 0`, a zero event count in either group makes the log-Wald
#' interval unavailable: a warning is issued and its bounds are `NA`.
#' Use the score method for such data, or explicitly choose a positive
#' correction, for example `delta = 0.5`. Wald intervals are asymptotic and
#' can perform poorly with small counts or probabilities near the boundaries.
#' If both groups contain only events and `delta = 0`, the formal Wald
#' interval is `[1, 1]`; it is returned with a warning because it is degenerate.
#' For a one-sided interval, the corresponding open endpoint is still applied.
#'
#' The `"use-or"` method transforms the Wald bounds returned by
#' [oddsRatio()] using `RR = OR / (1 - p2 + p2 * OR)`, with `p2 = x2 / n2`
#' fixed at its observed value. This is a plug-in approximation: monotonicity
#' of the transformation does not establish nominal coverage when `p2` is
#' estimated. It requires `0 < p2 < 1`, and, like the Wald odds-ratio
#' interval, positive cells unless `delta > 0`; otherwise a warning is issued
#' and the interval bounds are `NA`. An infinite OR bound is mapped to its
#' limit `1 / p2`.
#'
#' For unavailable intervals, one-sided output still uses the natural open
#' endpoint (0 or `Inf`); the estimated endpoint remains `NA`.
#' Rows or columns can be reversed with [bedrock::revX()], or the table
#' transposed with [t()], to obtain the required orientation.
#'
#' @references
#' Koopman, P. A. R. (1984). Confidence intervals for the ratio of two
#' binomial proportions. *Biometrics*, *40*(2), 513--517.
#'
#' Miettinen, O., & Nurminen, M. (1985). Comparative analysis of two rates.
#' *Statistics in Medicine*, *4*(2), 213--226.
#'
#' Rothman, K. J., Greenland, S., & Lash, T. L. (2008).
#' *Modern Epidemiology* (3rd ed.). Lippincott Williams & Wilkins.
#'
#' @examples
#' m <- matrix(
#'   c(78, 50,
#'     1422, 950),
#'   nrow = 2,
#'   dimnames = list(
#'     water = c("cont", "clean"),
#'     diarrhea = c("yes", "no")
#'   )
#' )
#'
#' relRisk(m, conf.level = 0.95)
#'
#'
#' mm <- cbind(c(9, 20), c(41, 29))
#'
#' relRisk(t(mm), conf.level = 0.95)
#'
#' relRisk(
#'   t(mm),
#'   conf.level = 0.95,
#'   method = "wald"
#' )
#'
#' relRisk(
#'   t(mm),
#'   conf.level = 0.95,
#'   method = "use-or"
#' )
#'
#' # unexposed group without non-events: the score interval is still valid
#' relRisk(matrix(c(2, 5, 3, 0), nrow = 2), conf.level = 0.95)
#'
#'
#' # explicitly corrected Wald estimate and interval
#' relRisk(matrix(c(0, 4, 10, 6), nrow = 2),
#'         method = "wald", delta = 0.5, conf.level = 0.95)
#'
#' @family effect.size
#' @concept effect-size
#' @concept binary-outcome
#'
#'
#' @export
relRisk <- function(
    x,
    y = NULL,
    conf.level = NA,
    sides = c("two.sided", "left", "right"),
    method = c("score", "wald", "use-or"),
    delta = 0,
    ...
) {

  x <- .as2x2(x, y, ...)

  # NA is LOGICAL, so the type must not be tested before the NA case is
  # admitted - otherwise the function rejects its own default. The shared
  # helper is the single place that gets this right.
  conf.level <- checkConfLevel(conf.level)

  sides <- match.arg(sides)

  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1

  delta <- .checkDelta(delta)

  method <- match.arg(method)

  if (method == "wald" && delta > 0) {
    x <- x + delta
    if (any(!is.finite(rowSums(x))))
      stop("'delta' produces non-finite row totals.")
  }

  x1 <- x[1L, 1L]
  x2 <- x[2L, 1L]

  n1 <- sum(x[1L, ])
  n2 <- sum(x[2L, ])

  estimate <- (x1 / n1) / (x2 / n2)

  if (is.na(conf.level))
    return(estimate)

  ci <- switch(
    method,

    "score" = .relRiskScore(
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2,
      conf.level = confAdj
    ),

    "wald" = .relRiskWald(
      estimate = estimate,
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2,
      conf.level = confAdj
    ),

    "use-or" = .relRiskUseOr(
      x = x,
      x2 = x2,
      n2 = n2,
      conf.level = confAdj,
      delta = delta
    )
  )

  # Natural parameter space for the open side of a one-sided interval.
  c(
    est = estimate,
    applySides(c(ci[["lci"]], ci[["uci"]]), sides, lo = 0, hi = Inf)
  )

}



# == internal helper functions ================================================

# Invert the score statistic on the log risk-ratio scale, with closed forms
# when both groups have no events or both groups contain only events.
.relRiskScore <- function(x1, x2, n1, n2, conf.level) {

  # Use the lower tail to avoid rounding (1 + conf.level) / 2 to 1.
  z <- abs(qnorm((1 - conf.level) / 2))

  if (x1 == 0 && x2 == 0)
    return(c(lci = 0, uci = Inf))

  if (x1 == n1 && x2 == n2)
    return(c(lci = n1 / (n1 + z^2), uci = (n2 + z^2) / n2))

  # An infinite estimate is handled by reversing rows and taking reciprocals.
  if (x2 == 0) {
    ci <- .relRiskScoreNumeric(x2, x1, n2, n1, z)
    return(c(lci = 1 / ci[["uci"]], uci = Inf))
  }

  .relRiskScoreNumeric(x1, x2, n1, n2, z)

}


# Score statistic for H0: p1 = theta * p2, evaluated at the constrained MLE.
# The constrained MLE of p2 solves
#   (n1 + n2) theta p^2 - (theta (n1 + x2) + n2 + x1) p + (x1 + x2) = 0 .
# Scale the quadratic coefficients to avoid overflow during log-scale searches.
.relRiskScoreStat <- function(theta, x1, x2, n1, n2) {

  if (!is.finite(theta) || theta <= 0)
    stop("The score statistic requires a finite positive risk ratio.")

  scale <- max(n1, n2)
  n1s <- n1 / scale
  n2s <- n2 / scale
  x1s <- x1 / scale
  x2s <- x2 / scale
  if (theta <= 1) {
    a <- (n1s + n2s) * theta
    b <- theta * (n1s + x2s) + n2s + x1s
    cc <- x1s + x2s
  } else {
    a <- n1s + n2s
    b <- n1s + x2s + (n2s + x1s) / theta
    cc <- (x1s + x2s) / theta
  }

  p2 <- 2 * cc / (b + sqrt(max(b^2 - 4 * a * cc, 0)))
  p2 <- min(p2, 1, 1 / theta)
  p1 <- min(theta * p2, 1)

  .relRiskScoreTerm(x1, n1, p1) + .relRiskScoreTerm(x2, n2, p2)

}


# A zero-variance binomial contributes zero only when the data match its
# deterministic outcome. Incompatible boundary observations contribute Inf.
.relRiskScoreTerm <- function(x, n, p) {
  if (p == 0) return(if (x == 0) 0 else Inf)
  if (p == 1) return(if (x == n) 0 else Inf)
  n * ((x / n - p)^2 / p) / (1 - p)
}


.relRiskScoreNumeric <- function(x1, x2, n1, n2, z) {

  target <- z^2
  g <- function(lt) {
    value <- .relRiskScoreStat(exp(lt), x1, x2, n1, n2) - target
    if (is.na(value)) stop("Could not evaluate the relative-risk score statistic.")
    value
  }

  # The score statistic is U-shaped around the estimate, so each bound is
  # bracketed by its own outward search (see .seekLog()).
  limits <- c(log(.Machine$double.xmin), log(.Machine$double.xmax) - 1)

  if (x1 == 0) {
    # Start at the risk ratio corresponding to one event in the first group,
    # then search towards zero for a point inside the confidence set.
    seed <- log(n2) - log(n1) - log(x2)
    seed <- max(limits[1], min(limits[2], seed))
    inside <- if (g(seed) <= 0) seed else .seekLog(g, seed, -1, sign = -1)
    lci <- 0
  } else {
    inside <- log(x1) - log(n1) - log(x2) + log(n2)
    if (inside <= limits[1] || inside >= limits[2] || g(inside) > 0)
      stop("Could not locate an interior point for the relative-risk score interval.")
    lower <- .seekLog(g, inside, -1, sign = 1)
    lci <- exp(uniroot(g, c(lower, inside), tol = 1e-10,
                       check.conv = TRUE)$root)
  }

  upper <- .seekLog(g, inside, 1, sign = 1)
  uci <- exp(uniroot(g, c(inside, upper), tol = 1e-10,
                     check.conv = TRUE)$root)
  c(lci = lci, uci = uci)

}



.relRiskWald <- function(
    estimate,
    x1,
    x2,
    n1,
    n2,
    conf.level
) {

  if (x1 == 0 || x2 == 0) {
    warning("The log-Wald interval requires positive event counts; use method = 'score' or delta > 0.")
    return(c(lci = NA_real_, uci = NA_real_))
  }

  if (x1 == n1 && x2 == n2)
    warning("The log-Wald interval is degenerate because both groups contain only events; use method = 'score'.")

  logEstimate <- log(estimate)

  # Equivalent to 1/x1 - 1/n1 + 1/x2 - 1/n2, without cancellation.
  seLogEstimate <- sqrt(
    ((n1 - x1) / n1) / x1 + ((n2 - x2) / n2) / x2
  )

  z <- abs(qnorm((1 - conf.level) / 2))

  c(
    lci = exp(logEstimate - z * seLogEstimate),
    uci = exp(logEstimate + z * seLogEstimate)
  )

}



.relRiskUseOr <- function(
    x,
    x2,
    n2,
    conf.level,
    delta = 0
) {

  p2 <- x2 / n2
  if (p2 == 0 || p2 == 1) {
    warning("The use-or interval requires 0 < x2 / n2 < 1; use method = 'score'.")
    return(c(lci = NA_real_, uci = NA_real_))
  }

  # The Wald OR interval, with the caller's cell correction: like the Wald
  # RR interval it is unavailable for a zero cell unless delta > 0.
  or <- suppressWarnings(oddsRatio(
    x,
    conf.level = conf.level,
    method = "wald",
    delta = delta
  ))

  # read by name, not by position: a helper that returns a shorter or
  # differently ordered vector must fail loudly here, not silently shift
  # the bounds (cf. .pearsonCI()/.assocsTab()).
  if (!all(c("est", "lci", "uci") %in% names(or)))
    stop("oddsRatio() did not return the expected 'est'/'lci'/'uci' vector.")

  if (anyNA(or[c("lci", "uci")])) {
    warning("The use-or interval requires positive cell counts; ",
            "use method = 'score' or delta > 0.", call. = FALSE)
    return(c(lci = NA_real_, uci = NA_real_))
  }

  # Fixing p2 gives a plug-in interval, not guaranteed nominal RR coverage.
  tr <- function(o) {
    if (is.na(o)) return(NA_real_)
    if (o == 0) return(0)
    # This form also handles o = Inf and avoids overflow in p2 * o.
    1 / (p2 + (1 - p2) / o)
  }

  c(
    lci = unname(tr(or[["lci"]])),
    uci = unname(tr(or[["uci"]]))
  )

}
