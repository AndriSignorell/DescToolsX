
#' Cohen's h for a 2x2 Table
#'
#' Computes Cohen's \eqn{h}, a standardized effect size for the
#' difference between two proportions in a 2x2 contingency table.
#'
#' Cohen's \eqn{h} is defined as:
#'
#' \deqn{
#' h = 2\arcsin(\sqrt{p_1}) - 2\arcsin(\sqrt{p_2})
#' }{h = 2*asin(sqrt(p1)) - 2*asin(sqrt(p2))}
#'
#' where \eqn{p_1} and \eqn{p_2} are the event probabilities
#' in the first and second row, respectively.
#'
#' Optionally, an approximate asymptotic confidence interval
#' is computed.
#'
#' @param x a 2x2 contingency table or matrix, or a categorical vector
#'   when `y` is supplied
#' @param y an optional second variable used together with `x`
#'   to create a contingency table via `table(x, y, ...)`
#'   
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()]. Since \eqn{h} lies in
#'   \eqn{[-\pi, \pi]}, the open side is reported at \eqn{\pm\pi} and the
#'   two-sided interval is clamped to that range. One-sided intervals
#'   require `conf.level > 0.5`.
#'   
#' @param ... additional arguments passed to `table()`
#'
#' @return if `conf.level = NA`, a numeric scalar containing Cohen's
#' \eqn{h}; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of Cohen's \eqn{h}.}
#'   \item{`lci`}{lower confidence interval bound.}
#'   \item{`uci`}{upper confidence interval bound.}
#' }
#'
#' @details
#' Cohen's \eqn{h} is a variance-stabilized standardized effect
#' size for comparing two proportions.
#'
#' Approximate interpretation thresholds suggested by Cohen are:
#'
#' \tabular{ll}{
#' |h| < 0.2 \tab negligible effect \cr
#' |h| >= 0.2 \tab small effect \cr
#' |h| >= 0.5 \tab medium effect \cr
#' |h| >= 0.8 \tab large effect
#' }
#'
#' The confidence interval is based on the asymptotic standard error:
#'
#' \deqn{
#' SE(h) = \sqrt{\frac{1}{n_1} + \frac{1}{n_2}}
#' }{SE(h) = sqrt(1/n1 + 1/n2)}
#'
#' `sides` names the side on which the finite bound lies:
#' `"left"` yields \eqn{[lci, \infty)}, `"right"` yields
#' \eqn{(-\infty, uci]}. 
#'
#' @examples
#' tab <- matrix(
#'   c(26, 26,
#'     6, 7),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#'
#' cohenH(tab)
#' cohenH(tab, conf.level = 0.95)
#'
#' x <- c(rep("A", 52), rep("B", 13))
#' y <- c(rep(c("yes", "no"), c(26, 26)),
#'        rep(c("yes", "no"), c(6, 7)))
#'
#' cohenH(x, y, conf.level = 0.95)
#'
#' @references
#' Cohen J (1988). Statistical Power Analysis for the Behavioral
#' Sciences (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @family effect.size
#' @concept effect-size
#' @concept binary-outcome
#' @export
cohenH <- function(x,
                   y = NULL,
                   conf.level = NA,
                   sides = c("two.sided", "left", "right"),
                   ...) {

  sides <- match.arg(sides)

  if (!is.null(y))
    x <- table(x, y, ...)

  # dim(x) is NULL for a plain vector, and all(NULL == c(2, 2)) is
  # all(logical(0)), i.e. TRUE - so a vector used to sail past this guard
  # and fail two lines later with "incorrect number of dimensions".
  if (length(dim(x)) != 2L || !all(dim(x) == c(2L, 2L)))
    stop("Input must be a 2x2 table; supply 'y' to cross-tabulate two vectors.")

  if (!is.numeric(x) && !is.table(x))
    stop("Input must be numeric.")

  # Counts, so finite and non-negative. Without this, rows (-5, -5) and
  # (5, 5) gave p1 = p2 = 0.5 and the plausible-looking h = 0, and an NA
  # cell reached the n1 == 0 test as "missing value where TRUE/FALSE
  # needed".
  if (anyNA(x) || any(!is.finite(x)))
    stop("The table must not contain missing or infinite counts.")
  if (any(x < 0))
    stop("The table counts must be non-negative.")

  # checked before is.na(conf.level) is read below: NULL or c(0.9, 0.95)
  # broke that if() internally, and NaN silently meant "no interval"
  checkConfLevel(conf.level)

  # a/b/c/d as local names would mask base::c(); spelled out instead
  n1 <- x[1L, 1L] + x[1L, 2L]
  n2 <- x[2L, 1L] + x[2L, 2L]

  if (n1 == 0 || n2 == 0)
    stop("Both rows of the table must contain at least one observation.")

  p1 <- x[1L, 1L] / n1
  p2 <- x[2L, 1L] / n2

  h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))

  if (is.na(conf.level))
    return(h)

  if (sides != "two.sided" && conf.level <= 0.5)
    stop("Argument 'conf.level' must exceed 0.5 for a one-sided interval.")

  se <- sqrt(1 / n1 + 1 / n2)

  confAdj <- if (sides != "two.sided") 2 * conf.level - 1 else conf.level
  z <- qnorm(1 - (1 - confAdj) / 2)

  # h = 2 asin(sqrt(p1)) - 2 asin(sqrt(p2)) with both terms in [0, pi], so
  # h lies in [-pi, pi]: the Wald interval is clamped to that range and the
  # open side of a one-sided interval is reported at its boundary rather
  # than at +/-Inf
  c(est = h, applySides(h + c(-1, 1) * z * se, sides, lo = -pi, hi = pi))
}
