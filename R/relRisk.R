
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
#' @param x a numeric 2x2 matrix or table containing non-negative counts
#' @param y an optional vector. If supplied, \code{table(x, y, ...)} is
#'   computed.
#' @param delta small continuity correction added to the event counts in the
#'   \emph{standard error} of the Wald interval. Only used if
#'   \code{method = "wald"}; see the note below.
#'   
#' @param method character string specifying the confidence interval method.
#'   One of \code{"score"}, \code{"wald"}, or \code{"use-or"}.
#'   
#' @param conf.level confidence level for the interval estimate. If
#'   \code{NA} (default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See details in \code{\link{ConfidenceIntervals}}.
#'
#' @param \dots further arguments passed to \code{\link{table}}
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{relative risk estimate}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @details
#' The score interval is based on the method of Koopman (1984) and
#' Miettinen and Nurminen (1985). It is obtained from the closed-form
#' solution of the cubic equation in the constrained maximum likelihood
#' estimate; if the unexposed group has no non-events (\code{x2 == n2}) that
#' cubic has a root on the parameter boundary and its roots can no longer be
#' assigned to the two interval bounds by their order alone. This case is
#' therefore solved directly from the score statistic
#' (\code{\link[stats]{uniroot}}); both routes agree to numerical precision
#' wherever the closed form applies.
#'
#' The Wald interval is asymptotic and may perform poorly for small counts or
#' extreme probabilities. Note that \code{delta} enters the standard error
#' only, not the point estimate the interval is centred on: with a zero cell
#' the point estimate is \code{0} or \code{Inf} and the Wald interval
#' degenerates accordingly. Use \code{method = "score"} for tables with zero
#' cells.
#'
#' If the table orientation differs from the required structure, rows or
#' columns can be reversed using \code{\link[bedrock]{revX}} or transposed
#' with \code{\link{t}}.
#'
#' @references
#' Koopman, P. A. R. (1984). Confidence intervals for the ratio of two
#' binomial proportions. \emph{Biometrics}, \emph{40}(2), 513--517.
#'
#' Miettinen, O., & Nurminen, M. (1985). Comparative analysis of two rates.
#' \emph{Statistics in Medicine}, \emph{4}(2), 213--226.
#'
#' Rothman, K. J., Greenland, S., & Lash, T. L. (2008).
#' \emph{Modern Epidemiology} (3rd ed.). Lippincott Williams & Wilkins.
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
    delta = 0.5,
    ...
) {

  if (!is.null(y))
    x <- table(x, y, ...)

  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")

  if (anyNA(x))
    stop("Argument 'x' must not contain missing values.")

  if (length(dim(x)) != 2L)
    stop("Argument 'x' must be a matrix.")

  if (!all(dim(x) == c(2L, 2L)))
    stop("Argument 'x' must be a 2x2 matrix.")

  if (any(x < 0))
    stop("Argument 'x' must contain non-negative counts.")

  if (any(x %% 1 != 0))
    stop("Argument 'x' must contain integer counts.")

  if (any(rowSums(x) == 0))
    stop("Rows of 'x' must contain positive totals.")

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

  if (length(delta) != 1L || !is.numeric(delta) || is.na(delta) || delta < 0)
    stop("'delta' must be a single non-negative number.")

  method <- match.arg(method)

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
      delta = delta,
      conf.level = confAdj
    ),

    "use-or" = .relRiskUseOr(
      x = x,
      x2 = x2,
      n2 = n2,
      conf.level = confAdj
    )
  )

  # The relative risk is a ratio of probabilities: bounded below by 0,
  # unbounded above. This is the first measure in this round whose open
  # side really is infinite - the same range the caller table records for
  # oddsRatio, gmean, hmean and coefVarCI.
  c(
    est = estimate,
    applySides(c(ci[["lci"]], ci[["uci"]]), sides, lo = 0, hi = Inf)
  )

}



# == internal helper functions ================================================

.relRiskScore <- function(
    x1,
    x2,
    n1,
    n2,
    conf.level
) {

  z <- abs(qnorm((1 - conf.level) / 2))

  if (x1 == 0 && x2 == 0) {
    return(c(lci = 0, uci = Inf))
  }

  # x2 == n2 puts the constrained MLE on the boundary p2 = 1. One root of the
  # cubic then coincides with that boundary, and the "smallest root gives the
  # upper bound, middle root the lower bound" rule no longer holds -- it
  # returned intervals not containing the estimate, intervals with lci > uci,
  # and NaN. Solved from the score statistic instead.
  # x1 == n1 is the mirror image: the constrained MLE then sits on p1 = 1, the
  # middle root no longer delivers the lower bound (the interval came out far
  # too narrow), and at p0 = (x2 + n1)/(n1 + n2) the bound formula degenerates
  # to 0/0. x2 == 0 keeps its own branch below, where the estimate is infinite
  # and the numeric bracketing has nothing to start from.
  if (x2 != 0 && ((x2 == n2 && x1 != n1) || (x1 == n1 && x2 != n2))) {
    return(.relRiskScoreNumeric(x1 = x1, x2 = x2, n1 = n1, n2 = n2, z = z))
  }

  roots <- .relRiskScoreRoots(
    x1 = x1,
    x2 = x2,
    n1 = n1,
    n2 = n2,
    z = z
  )

  p0low <- roots[["p0low"]]
  p0up  <- roots[["p0up"]]

  if (x2 == 0 && x1 != 0) {

    lci <- .relRiskScoreBound(
      p0 = p0low,
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2
    )

    return(c(lci = lci, uci = Inf))

  }

  if (x2 != n2 && x1 == 0) {

    uci <- .relRiskScoreBound(
      p0 = p0up,
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2
    )

    return(c(lci = 0, uci = uci))

  }

  if (x2 == n2 && x1 == n1) {

    return(c(
      lci = n1 / (n1 + z^2),
      uci = (n2 + z^2) / n2
    ))

  }

  lci <- .relRiskScoreBound(
    p0 = p0low,
    x1 = x1,
    x2 = x2,
    n1 = n1,
    n2 = n2
  )

  uci <- .relRiskScoreBound(
    p0 = p0up,
    x1 = x1,
    x2 = x2,
    n1 = n1,
    n2 = n2
  )

  c(lci = lci, uci = uci)

}



.relRiskScoreRoots <- function(
    x1,
    x2,
    n1,
    n2,
    z
) {

  a1 <- n2 * (
    n2 * (n2 + n1) * x1 +
      n1 * (n2 + x1) * z^2
  )

  a2 <- -n2 * (
    n2 * n1 * (x2 + x1) +
      2 * (n2 + n1) * x2 * x1 +
      n1 * (n2 + x2 + 2 * x1) * z^2
  )

  a3 <- (
    2 * n2 * n1 * x2 * (x2 + x1) +
      (n2 + n1) * x2^2 * x1 +
      n2 * n1 * (x2 + x1) * z^2
  )

  a4 <- -n1 * x2^2 * (x2 + x1)

  b1 <- a2 / a1
  b2 <- a3 / a1
  b3 <- a4 / a1

  c1 <- b2 - b1^2 / 3
  c2 <- b3 - b1 * b2 / 3 + 2 * b1^3 / 27

  acosArg <- sqrt(27) * c2 / (2 * c1 * sqrt(-c1))
  acosArg <- min(1, max(-1, acosArg))

  ceta <- acos(acosArg)

  t1 <- -2 * sqrt(-c1 / 3) * cos(pi / 3 - ceta / 3)
  t2 <- -2 * sqrt(-c1 / 3) * cos(pi / 3 + ceta / 3)
  t3 <-  2 * sqrt(-c1 / 3) * cos(ceta / 3)

  p01 <- t1 - b1 / 3
  p02 <- t2 - b1 / 3
  p03 <- t3 - b1 / 3

  p0up <- min(p01, p02, p03)

  p0sum <- p01 + p02 + p03

  p0low <- p0sum - p0up - max(p01, p02, p03)

  list(
    p0low = p0low,
    p0up  = p0up
  )

}



.relRiskScoreBound <- function(
    p0,
    x1,
    x2,
    n1,
    n2
) {

  # x1 == n1 makes the numerator exactly zero, and the denominator vanishes at
  # p0 = (x2 + n1)/(n1 + n2) - which is where the relevant root sits in that
  # case. Without this the expression is 0/0 = NaN.
  num <- (n1 - x1) * (1 - p0)
  ratio <- if (num == 0) 0 else num / (x2 + n1 - (n1 + n2) * p0)

  (1 - ratio) / p0

}



# Score statistic for H0: p1 = theta * p2, evaluated at the constrained MLE.
# The constrained MLE of p2 solves
#   (n1 + n2) theta p^2 - (theta (n1 + x2) + n2 + x1) p + (x1 + x2) = 0 .
# Checked against .relRiskScoreRoots() for all 2x2 tables with
# n1, n2 in {4, 5, 10, 25, 60} and 0 < x1 < n1, 0 < x2 < n2 (9801 tables):
# maximum relative deviation of both bounds 3.3e-08 (root finder tolerance).
.relRiskScoreStat <- function(theta, x1, x2, n1, n2) {

  a <- (n1 + n2) * theta
  b <- -(theta * (n1 + x2) + n2 + x1)
  cc <- x1 + x2

  p2 <- (-b - sqrt(max(b^2 - 4 * a * cc, 0))) / (2 * a)
  p1 <- theta * p2

  s <- 0
  if (p1 > 0 && p1 < 1) s <- s + (x1 - n1 * p1)^2 / (n1 * p1 * (1 - p1))
  if (p2 > 0 && p2 < 1) s <- s + (x2 - n2 * p2)^2 / (n2 * p2 * (1 - p2))

  s

}


.relRiskScoreNumeric <- function(x1, x2, n1, n2, z) {

  target <- z^2
  f <- function(theta) .relRiskScoreStat(theta, x1, x2, n1, n2) - target

  est <- (x1 / n1) / (x2 / n2)

  # lower bound
  if (x1 == 0) {
    lci <- 0
  } else {
    lo <- est
    while (f(lo) < 0 && lo > 1e-12) lo <- lo / 2
    lci <- if (f(lo) > 0) uniroot(f, c(lo, est), tol = .Machine$double.eps^0.5)$root
           else 0
  }

  # upper bound
  hi <- max(est, 1e-8)
  while (f(hi) < 0 && hi < 1e12) hi <- hi * 2
  uci <- if (f(hi) > 0)
           uniroot(f, c(max(est, 1e-12), hi), tol = .Machine$double.eps^0.5)$root
         else Inf

  c(lci = lci, uci = uci)

}



.relRiskWald <- function(
    estimate,
    x1,
    x2,
    n1,
    n2,
    delta,
    conf.level
) {

  x1d <- x1 + delta
  x2d <- x2 + delta

  logEstimate <- log(estimate)

  seLogEstimate <- sqrt(
    1 / x1d - 1 / n1 +
      1 / x2d - 1 / n2
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
    conf.level
) {

  or <- oddsRatio(
    x,
    conf.level = conf.level
  )

  # read by name, not by position: a helper that returns a shorter or
  # differently ordered vector must fail loudly here, not silently shift
  # the bounds (cf. .pearsonCI()/.assocsTab()).
  if (!all(c("est", "lci", "uci") %in% names(or)))
    stop("oddsRatio() did not return the expected 'est'/'lci'/'uci' vector.")

  p2 <- x2 / n2

  # RR = OR / (1 - p2 + p2 * OR) is monotone increasing in OR, so the
  # transformation maps the bounds onto the bounds.
  tr <- function(o) o / ((1 - p2) + p2 * o)

  c(
    lci = unname(tr(or[["lci"]])),
    uci = unname(tr(or[["uci"]]))
  )

}
