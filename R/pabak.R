
#' Prevalence-Adjusted and Bias-Adjusted Kappa (PABAK)
#'
#' Computes the prevalence-adjusted and bias-adjusted kappa (PABAK) as a
#' measure of inter-rater agreement, together with an asymptotic confidence
#' interval and the auxiliary prevalence and bias indices.
#'
#' @details
#' Cohen's kappa can be paradoxically low when the prevalence of one category
#' differs markedly from 0.5 (prevalence effect) or when the two raters
#' systematically disagree on the marginal frequency of a category (bias
#' effect). PABAK corrects for both distortions by replacing the chance
#' agreement term with the fixed value \eqn{p_e = 0.5}. The statistic
#' simplifies to
#' \deqn{\mathrm{PABAK} = 2 p_o - 1}
#' where \eqn{p_o} is the observed proportion of agreement (diagonal sum of
#' the relative-frequency table).
#'
#' The asymptotic standard error is derived from the delta method applied to
#' the above identity:
#' \deqn{\mathrm{SE} = 2 \sqrt{p_o (1 - p_o) / n}}
#'
#' Two diagnostic indices quantify the prevalence and bias effects that can
#' cause Cohen's kappa to differ from PABAK. They are defined only for
#' \eqn{2 \times 2} tables following Byrt et al. (1993); for larger tables
#' they are `NA`:
#' \describe{
#'   \item{Prevalence index}{
#'     \eqn{|p_{11} - p_{22}|}. A high value indicates that one outcome
#'     dominates, which deflates Cohen's kappa relative to PABAK.}
#'   \item{Bias index}{
#'     \eqn{|p_{1\cdot} - p_{\cdot 1}|}. A high value signals that the two
#'     raters assign the positive label at systematically different rates.}
#' }
#' When both indices are near zero, PABAK and Cohen's kappa will be nearly
#' identical. Both are returned as attributes rather than as elements of the
#' result, since they are separate diagnostics rather than further views of
#' the estimate, and they do not depend on `conf.level`.
#'
#' The Wald confidence interval is truncated to the admissible range
#' \eqn{[-1, 1]}.
#'
#' `sides` names the side on which the finite bound lies:
#' `"left"` yields \eqn{[lci, \infty)} and `"right"`
#' \eqn{(-\infty, uci]}. 
#'
#' Data can be passed either as a square confusion matrix (or data frame) in
#' `x`, or as two vectors `x` and `y`, in which case
#' `table(x, y, \dots)` is computed internally.
#'
#' Missing values are handled as [table()] does - excluded by
#' default. Pass `useNA = "ifany"` via `...` to include them.
#'
#' @param x a square confusion matrix (or data frame), or a categorical vector
#'   when `y` is provided.
#' @param y `NULL` (default) or a categorical vector of the same length
#'   as `x`. When supplied, `table(x, y, \dots)` is computed
#'   internally.
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'
#' @param ... further arguments passed to [table()] for the vector
#'   interface, for example `useNA`.
#'
#' @return
#' a named numeric vector.
#'
#' If `conf.level = NA`, a single element `est`; otherwise the
#' elements:
#'
#' \describe{
#'   \item{`est`}{point estimate of PABAK.}
#'   \item{`lci`}{lower confidence interval bound.}
#'   \item{`uci`}{upper confidence interval bound.}
#' }
#'
#' In both cases the diagnostics are attached as attributes:
#'
#' \describe{
#'   \item{nObs}{number of observations in the table.}
#'   \item{prevalenceIndex}{prevalence index, or `NA` for \eqn{k > 2}.}
#'   \item{biasIndex}{bias index, or `NA` for \eqn{k > 2}.}
#' }
#'
#' @references
#' Byrt, T., Bishop, J., & Carlin, J. B. (1993). Bias, prevalence and kappa.
#'   *Journal of Clinical Epidemiology*, *46*(5), 423-429.
#'   \doi{10.1016/0895-4356(93)90018-V}
#'
#' Hoehler, F. K. (2000). Bias and prevalence effects on kappa viewed in
#'   terms of sensitivity and specificity.
#'   *Journal of Clinical Epidemiology*, *53*(5), 499-503.
#'   \doi{10.1016/S0895-4356(99)00174-2}
#'
#' @examples
#' # 2x2 table: two raters classifying 100 patients as positive / negative
#' m2 <- matrix(c(45, 15, 5, 35), nrow = 2, byrow = TRUE,
#'              dimnames = list(rater1 = c("pos", "neg"),
#'                              rater2 = c("pos", "neg")))
#'
#' pabak(m2)
#' pabak(m2, conf.level = 0.95)
#'
#' # the diagnostics are available either way
#' attr(pabak(m2), "prevalenceIndex")
#' attr(pabak(m2), "biasIndex")
#'
#' # Compare with cohenKappa: the indices show where the gap comes from
#' cohenKappa(m2)
#'
#' # 4x4 table: the indices are NA for k > 2
#' m4 <- matrix(c(400, 40, 20, 10,
#'                 50,300, 60, 20,
#'                 10, 40,120,  5,
#'                  5, 90, 50, 80),
#'              nrow = 4, byrow = TRUE)
#'
#' pabak(m4, conf.level = 0.95)
#'
#' # vector interface
#' x <- bedrock::untable(m2)
#' pabak(x$rater1, x$rater2, conf.level = 0.95)
#'
#' @family assoc.agreement
#' @concept interrater-agreement
#' @concept categorical-agreement
#'
#' @export
pabak <- function(x,
                  y          = NULL,
                  conf.level = NA,
                  sides      = c("two.sided", "left", "right"),
                  ...) {

  # Checked for type and length before is.na(), which would otherwise be
  # passed a zero-length or multi-element value and make the if() below
  # fail with an internal condition-length error rather than a clear
  # message.
  if (!is.numeric(conf.level) && !is.logical(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if (length(conf.level) != 1L)
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  # NaN is numeric and NA-like, but suppressing the interval on a NaN
  # confidence level would hide a caller error rather than express an
  # intent to omit it, so only a true NA does that.
  if (is.nan(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if (!is.na(conf.level)) {

    if (!is.numeric(conf.level) ||
        !is.finite(conf.level) ||
        conf.level <= 0 ||
        conf.level >= 1)
      stop("Argument 'conf.level' must be a single number between 0 and 1.")

  }

  sides <- match.arg(sides)

  x <- normalizeToConfusion(x = x, y = y, ...)

  n <- sum(x)

  if (n == 0)
    stop("Confusion matrix is empty (all cells zero).")

  p <- x / n

  # po: observed agreement (diagonal sum). PABAK fixes pe = 0.5, so
  # PABAK = (po - 0.5) / (1 - 0.5) = 2*po - 1.
  po  <- sum(diag(p))
  est <- 2 * po - 1

  # Computed before the conf.level branch: the indices describe the table,
  # not the interval, so they are attached whether or not a CI was asked
  # for.
  if (ncol(x) == 2L) {

    prevalenceIndex <- abs(p[1L, 1L] - p[2L, 2L])
    biasIndex <- abs(sum(p[1L, ]) - sum(p[, 1L]))

  } else {

    # Byrt et al. (1993) define both for 2x2 tables only, and no
    # generally accepted extension to k > 2 exists.
    prevalenceIndex <- NA_real_
    biasIndex <- NA_real_

  }

  attrs <- list(
    nObs = n,
    prevalenceIndex = unname(prevalenceIndex),
    biasIndex = unname(biasIndex)
  )

  if (is.na(conf.level))
    return(.makeEstimateResult(est = est, attrs = attrs))

  # Var(po) ~ po*(1-po)/n (binomial approximation for the diagonal sum),
  # and Var(PABAK) = 4*Var(po) by the delta method.
  se <- 2 * sqrt(po * (1 - po) / n)

  # qnorm(conf.level) directly, rather than rescaling conf.level and
  # halving alpha again: the two agree for conf.level > 0.5 but the
  # detour yields NaN at 0.5 and below, where the one-sided quantile is
  # perfectly well defined.
  zCrit <- if (sides == "two.sided")
    qnorm(1 - (1 - conf.level) / 2)
  else
    qnorm(conf.level)

  # Truncate to [-1, 1]: the Wald approximation can produce bounds outside
  # the admissible range, most commonly uci > 1 near perfect agreement.
  lci <- max(-1, est - zCrit * se)
  uci <- min( 1, est + zCrit * se)

  # PABAK = 2*po - 1 lies in [-1, 1], and the two-sided bounds are
  # truncated to exactly that range four lines above. The open side
  # therefore belongs at the boundary, not at +/-Inf (design_rules.md 4.1,
  # as decided for cohenKappa) - an uci of Inf claims a value the measure
  # cannot take.
  if (sides == "left")
    uci <- 1

  if (sides == "right")
    lci <- -1

  .makeEstimateResult(
    est = est,
    lci = lci,
    uci = uci,
    attrs = attrs
  )

}
