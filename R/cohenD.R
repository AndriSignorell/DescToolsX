#' Cohen's and Hedges' Effect Size
#'
#' Computes the Cohen's d and Hedges' g effect size statistics.
#'
#' @details
#' For a single sample, \eqn{d = \bar{x} / s}; for two samples,
#' \eqn{d = (\bar{x} - \bar{y}) / s_{pooled}}. With `correct = TRUE`
#' Hedges' bias correction \eqn{J = 1 - 3/(4\nu - 1)}, with \eqn{\nu} the
#' residual degrees of freedom, is applied to the estimate and, where
#' computed, to the interval.
#'
#' Confidence intervals invert the noncentral \eqn{t} distribution
#' (Steiger & Fouladi): the noncentrality parameter is
#' \eqn{d\sqrt{n}} with \eqn{n - 1} degrees of freedom in the one-sample
#' case, and \eqn{d / \sqrt{1/n_x + 1/n_y}} with \eqn{n_x + n_y - 2}
#' degrees of freedom in the two-sample case.
#'
#' `sides` names the side on which the finite bound lies:
#' `"left"` yields \eqn{[lci, \infty)}, `"right"` yields
#' \eqn{(-\infty, uci]}. 
#'
#' @param x a non-empty numeric vector of data values
#' @param y an optional non-empty numeric vector of data values
#' 
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'
#' @param correct logical; whether to apply the Hedges correction. Defaults to
#' `FALSE`.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' `FALSE`.
#'
#' @name cohenD
#'
#' @return if `conf.level = NA`, a numeric scalar containing the effect
#' size; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of Cohen's \eqn{d} or Hedges' \eqn{g}.}
#'   \item{`lci`}{lower confidence interval bound.}
#'   \item{`uci`}{upper confidence interval bound.}
#' }
#' The magnitude category and pooled standard deviation are stored in the
#' attributes `magnitude` and `sdPooled`, respectively.
#'
#' @note
#' Based on code by William Revelle.
#'
#' @seealso [glassDelta()], [meanX()], [varX()]
#'
#' @references Cohen, J. (1988) *Statistical power analysis for the
#' behavioral sciences (2nd ed.)* Academic Press, New York.
#'
#' Hedges, L. V. & Olkin, I. (1985) *Statistical methods for
#' meta-analysis* Academic Press, Orlando, FL
#'
#' Smithson, M.J. (2003) *Confidence Intervals, Quantitative Applications
#' in the Social Sciences Series*, No. 140. Thousand Oaks, CA: Sage. pp. 39-41
#'
#' @examples
#'
#' x <- Pizza$price[Pizza$driver == "Carter"]
#' y <- Pizza$price[Pizza$driver == "Miller"]
#'
#' cohenD(x, y, conf.level = 0.95, na.rm = TRUE)
#'
#' # Hedges' g
#' cohenD(x, y, conf.level = 0.95, correct = TRUE, na.rm = TRUE)
#'
#' # one-sided: the finite bound lies on the left
#' cohenD(x, y, conf.level = 0.95, sides = "left", na.rm = TRUE)
#'
#' @rdname cohenD
#' @family effect.size
#' @concept effect-size
#' @export
cohenD <- function(x, y = NULL,
                   conf.level = NA, sides = c("two.sided", "left", "right"),
                   correct = FALSE, 
                   na.rm = FALSE) {

  sides <- match.arg(sides)

  if (na.rm) {
    x <- na.omit(x)
    if (!is.null(y)) y <- na.omit(y)
  }

  if (!is.na(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level) != 1L ||
       conf.level <= 0 || conf.level >= 1))
    stop("'conf.level' must be a single number in (0, 1), or NA")

  if (is.null(y)) {

    # ---- one sample -----------------------------------------------------
    n   <- length(x)
    .sd <- sd(x)
    d   <- mean(x) / .sd

    # 'correct' used to be silently ignored on this branch: cohenD(x,
    # correct = TRUE) returned the uncorrected d.
    corr <- if (correct) .hedgesJ(n - 1) else 1

    if (is.na(conf.level)) {
      res <- d * corr

    } else {
      ci <- .cohenDCI(d = d, n = n, alpha = .oneSidedAlpha(conf.level, sides))
      res <- .cohenDAssemble(ci * corr, sides)
    }

  } else {

    # ---- two samples ----------------------------------------------------
    nx <- length(x)
    ny <- length(y)
    DF <- nx + ny - 2

    .sd <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / DF)
    d   <- (mean(x) - mean(y)) / .sd

    # Hedges, L. V. & Olkin, I. (1985). Statistical methods for
    # meta-analysis. Orlando, FL: Academic Press.
    corr <- if (correct) .hedgesJ(DF) else 1

    if (is.na(conf.level)) {
      res <- d * corr

    } else {
      ci <- .cohenDCI(d = d, n1 = nx, n2 = ny,
                      alpha = .oneSidedAlpha(conf.level, sides))
      res <- .cohenDAssemble(ci * corr, sides)
    }
  }

  ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159.
  # the category describes the value actually reported, i.e. after the
  # Hedges correction when one was applied
  est <- if (length(res) == 3L) res[["est"]] else res
  attr(res, "magnitude") <-
    c("negligible", "small", "medium", "large")[
      findInterval(abs(est), c(0.2, 0.5, 0.8)) + 1]
  attr(res, "sdPooled") <- .sd

  return(res)
}


# == internal helper functions ==================================================

# One-sided intervals put the full alpha on the single finite side. The
# doubling below lets every method compute a two-sided interval and pick
# the relevant bound afterwards.
.oneSidedAlpha <- function(conf.level, sides)
  if (sides == "two.sided") 1 - conf.level else 2 * (1 - conf.level)


# Turns the (lower, effect, upper) triple into the package's est/lci/uci
# vector and opens the appropriate side.
#
# The previous version had this inverted AND mis-indexed: sides == "left"
# set ci[1] - the LOWER bound - to +Inf, producing lci = Inf with a finite
# uci. Per design_rules.md 4.1 'sides' names the side carrying the FINITE
# bound, so "left" opens the upper end and "right" the lower one.
.cohenDAssemble <- function(ci, sides) {

  lci <- ci[[1L]]
  uci <- ci[[3L]]

  if (sides == "left")       uci <- Inf
  else if (sides == "right") lci <- -Inf

  c(est = unname(ci[[2L]]), lci = unname(lci), uci = unname(uci))
}


# Hedges' bias correction, expressed in degrees of freedom so that it is
# defined for one and two samples alike. For two samples nu = nx + ny - 2
# and this reproduces the previous .J(nx, ny) exactly; for one sample
# nu = n - 1, which the old two-argument form could not express.
.hedgesJ <- function(df) {
  1 - 3 / (4 * df - 1)
}


# Noncentral-t confidence interval for Cohen's d (Steiger & Fouladi), via
# the shared root finder in .nctCI().
#
# The former implementation carried its own copy of Revelle's uniroot
# machinery and routed the one-sample case through the TWO-sample
# equal-group formulas: t = d*sqrt(n)/2 with df = n/2 - 1, where the
# one-sample quantities are t = d*sqrt(n) with df = n - 1. The one-sample
# interval was therefore the interval of a two-group design of the same
# total size. Two-sample results are unchanged.
.cohenDCI <- function(d, n = NULL, n1 = NULL, n2 = NULL, alpha = 0.05) {

  if (is.null(n1) || is.null(n2)) {
    if (is.null(n))
      stop("either 'n' or both 'n1' and 'n2' must be supplied")
    tval <- d * sqrt(n)
    df   <- n - 1
    toD  <- function(ncp) ncp / sqrt(n)

  } else {
    scale <- sqrt(1 / n1 + 1 / n2)
    tval  <- d / scale
    df    <- n1 + n2 - 2
    toD   <- function(ncp) ncp * scale
  }

  if (df < 1)
    stop("too few observations to compute a confidence interval")

  ncp <- .nctCI(tval, df = df, conf.level = 1 - alpha)

  c(lower = toD(unname(ncp[["lci"]])),
    effect = d,
    upper = toD(unname(ncp[["uci"]])))
}
