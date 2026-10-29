
#' Compute Odds Ratios
#'
#' Computes odds ratios, either from a 2x2 contingency table or from a
#' binomial generalized linear model. The table method returns a single odds
#' ratio, the model method one odds ratio per regression coefficient.
#'
#' @details
#' \subsection{Contingency tables}{
#' For a 2x2 table with cells \eqn{n_{ij}} the sample odds ratio is
#'
#' \deqn{OR = \frac{n_{11} n_{22}}{n_{12} n_{21}}}
#'
#' Three methods are available, and each comes with its own point estimate.
#'
#' `"wald"` uses the sample odds ratio and the asymptotic interval on the log
#' scale, with variance \eqn{\sum 1/n_{ij}}. It is fast and adequate for
#' reasonably large counts. With a zero cell the estimate is 0 or `Inf` and
#' the interval is unavailable: a warning is issued and its bounds are `NA`.
#' Use `"exact"` or `"mid-p"` for such data, or explicitly choose a
#' correction, for example `delta = 0.5` (Haldane--Anscombe).
#'
#' `"exact"` uses the conditional maximum likelihood estimate and the
#' conditional (Fisher) interval, both based on the noncentral
#' hypergeometric distribution of \eqn{n_{11}} given the margins. The
#' interval guarantees coverage but is conservative, sometimes markedly so.
#' These are the estimate and interval of [stats::fisher.test()], computed
#' to full precision (see below).
#'
#' `"mid-p"` uses the conditional median-unbiased estimate and the mid-p
#' interval, which counts only half the probability of the observed table.
#' Its coverage is closer to the nominal level than that of the exact
#' interval, without the Wald interval's reliance on large counts.
#'
#' The conditional methods remain informative with a zero cell: if
#' \eqn{n_{11}} lies at the lower end of its support, the estimate and the
#' lower bound are 0; at the upper end, the estimate and the upper bound are
#' `Inf`. The other bound is finite.
#'
#' The conditional estimates and bounds are found by root finding on the
#' log odds-ratio scale with a relative tolerance of about `1e-10`.
#' [stats::fisher.test()] uses an absolute tolerance of about `1e-4` on the
#' odds-ratio scale instead, which is inaccurate for small odds ratios and
#' bounds; results agree with it up to that tolerance.
#' }
#'
#' \subsection{Binomial models}{
#' For a model fitted with `glm(family = binomial)`, each coefficient is
#' exponentiated: \eqn{\exp(\beta_j)} is the factor by which the odds of the
#' response are multiplied when the corresponding predictor increases by one
#' unit, all other predictors held fixed. For a dummy variable this is the
#' odds ratio between the level and its reference level. Quasi-binomial
#' models are not supported; their coefficient table reports t rather than
#' z statistics.
#'
#' The intercept is exponentiated along with the rest, but
#' \eqn{\exp(\beta_0)} is *not* an odds ratio - it is the odds of the
#' response when all predictors are zero. It is reported for completeness
#' and is usually not the quantity of interest.
#'
#' Two interval methods are available. `"wald"` is the symmetric
#' interval on the log-odds scale, back-transformed. `"profile"` inverts
#' the likelihood ratio test through [stats::confint.glm()]; it is
#' asymmetric on the odds scale, generally more reliable in small samples or
#' with sparse cells, and considerably slower because the model is refitted
#' along each coefficient. Profile intervals are two-sided by construction,
#' so `sides` is ignored for them and a warning is issued.
#'
#' Unlike the table method, the model method computes an interval by default
#' (`conf.level = 0.95`): a coefficient table without intervals would
#' be less informative than `summary()` itself.
#' }
#'
#' @param x a 2x2 contingency table of finite, non-negative integer counts
#'   with positive row and column totals, an exposure vector to be
#'   cross-tabulated with `y`, or a binomial [stats::glm()] object
#' @param ... further arguments passed to methods. For the default method
#'   with two vectors, these reach [table()], so `useNA` can
#'   be set here.
#'
#' @return
#' For a contingency table with `conf.level = NA` a numeric scalar,
#' otherwise a named numeric vector with the elements `est`, `lci`
#' and `uci`. The estimate depends on `method`, see Details.
#'
#' For a binomial model an object of class `"OddsRatio"`, a list with:
#' \describe{
#'   \item{`coefficients`}{a data frame with one row per coefficient and
#'     the columns `term`, `est` (the exponentiated coefficient),
#'     `logEst` (the coefficient itself), `stdError` (on the log
#'     scale), `pValue`, `lci` and `uci`}
#'   \item{`source`}{`"glm"`}
#'   \item{`method`, `conf.level`, `sides`}{as supplied - with
#'     `sides` recording what was computed, which for
#'     `method = "profile"` is always `"two.sided"`}
#'   \item{`nObs`}{number of observations used in the fit}
#'   \item{`call`}{the model call}
#' }
#' There is a `print` method; the interval bounds are on the odds scale,
#' the standard error on the log scale.
#'
#' @references
#' Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.).
#' Wiley.
#'
#' Fisher, R. A. (1935). The logic of inductive inference.
#' *Journal of the Royal Statistical Society*,
#' *98*(1), 39--82.
#'
#' Gart, J. J. (1966). Alternative analyses of contingency tables.
#' *Journal of the Royal Statistical Society Series B*,
#' *28*(1), 164--179.
#'
#' Hirji, K. F. (2006). *Exact Analysis of Discrete Data*.
#' Chapman & Hall/CRC.
#'
#' @note
#' No short alias is exported by default to avoid conflicts with
#' \pkg{rlang} and base R naming conventions.  Call
#' [attachAliases()] once per session (or script) to make
#' `or()` available as a convenient shorthand.
#'
#' @seealso [attachAliases], [relRisk], [stats::fisher.test()],
#'   [stats::confint.glm()]
#'
#' @examples
#' # --- 2x2 table -------------------------------------------------
#' tab <- matrix(c(10, 20,
#'                  5, 30), nrow = 2)
#'
#' oddsRatio(tab)
#' oddsRatio(tab, conf.level = 0.95)
#'
#' # each method has its own estimate; for this table the exact interval
#' # is the widest and the Wald interval the narrowest
#' sapply(c("wald", "exact", "mid-p"),
#'        function(m) oddsRatio(tab, conf.level = 0.95, method = m))
#'
#' # one-sided: "left" carries the finite lower bound
#' oddsRatio(tab, conf.level = 0.95, sides = "left")
#'
#' # a zero cell: the conditional methods keep the informative bound
#' z <- matrix(c(10, 0, 5, 7), nrow = 2)
#' oddsRatio(z, conf.level = 0.95, method = "mid-p")
#' oddsRatio(z, conf.level = 0.95, method = "wald", delta = 0.5)
#'
#'
#' # --- binomial model --------------------------------------------
#' fit <- glm(vs ~ am + wt, data = mtcars, family = binomial)
#'
#' oddsRatio(fit)
#'
#' # the exponentiated intercept is the baseline odds, not an odds ratio
#' res <- oddsRatio(fit)
#' res$coefficients
#'
#' # profile likelihood intervals: asymmetric on the odds scale, slower
#' oddsRatio(fit, method = "profile")
#'
#'
#' @family effect.size
#' @concept effect-size
#' @concept binary-outcome
#' @export
oddsRatio <- function(x, ...) {
  UseMethod("oddsRatio")
}



#' @param y optional second variable. If supplied,
#'   `table(x, y, ...)` is computed without reordering its levels.
#' @param conf.level confidence level of the interval. For the table method
#'   `NA` (the default) returns the point estimate only; the model
#'   method computes an interval by default.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()]. An odds ratio
#'   is bounded below by 0 and unbounded above, so the open side is reported
#'   at 0 or `Inf` accordingly. Ignored, with a warning, for
#'   `method = "profile"`.
#' @param method character string specifying the method. For a
#'   contingency table one of `"wald"`, `"exact"` or
#'   `"mid-p"`; for a binomial model one of `"wald"` or
#'   `"profile"`. See Details.
#' @param delta a single finite, non-negative number (default 0). Only for
#'   `method = "wald"`, this value is added to each of the four cells before
#'   computing both the point estimate and the interval, also when only the
#'   point estimate is requested. The other methods ignore `delta` after
#'   validating it.
#'
#' @rdname oddsRatio
#' @method oddsRatio default
#' @export
oddsRatio.default <- function(
    x,
    y = NULL,
    conf.level = NA,
    sides = c("two.sided", "left", "right"),
    method = c("wald", "exact", "mid-p"),
    delta = 0,
    ...
) {

  # All argument checks up front, none behind a branch.
  method <- match.arg(method)
  sides  <- match.arg(sides)

  conf.level <- checkConfLevel(conf.level)

  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  delta <- .checkDelta(delta)

  # A fitted model reaches this method whenever there is no oddsRatio
  # method for its class - an lm, say. Without this it fails on
  # !is.numeric(x) with "Argument 'x' must be numeric", which sends the
  # caller looking at the wrong end of the problem.
  if (inherits(x, c("lm", "glm", "merMod", "gam")))
    stop(gettextf(
      "odds ratios are computed from a 2x2 table or a binomial glm, not from an object of class %s",
      dQuote(class(x)[1L], FALSE)), domain = NA)

  # The odds ratio is symmetric in rows and columns, so both margins must
  # be positive: with a constant outcome it is undefined.
  x <- .as2x2(x, y, ..., cols = TRUE)

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1. All three methods build their
  # two-sided interval by inverting two one-sided constructions at
  # alpha/2, so reading one end off the adjusted interval gives exactly
  # the one-sided bound, and the helpers need not know 'sides'.
  confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1

  res <- switch(
    method,
    "wald"  = .oddsRatioWald(x + delta, conf.level = confAdj),
    "exact" = .oddsRatioConditional(x, conf.level = confAdj, midp = FALSE),
    "mid-p" = .oddsRatioConditional(x, conf.level = confAdj, midp = TRUE)
  )

  if (is.na(conf.level))
    return(res)

  # an odds ratio is bounded below by 0 and unbounded above
  c(est = unname(res[["est"]]),
    applySides(unname(res[c("lci", "uci")]), sides, lo = 0, hi = Inf))

}



#' @rdname oddsRatio
#' @method oddsRatio glm
#' @export
oddsRatio.glm <- function(
    x,
    conf.level = 0.95,
    sides = c("two.sided", "left", "right"),
    method = c("wald", "profile"),
    ...
) {

  method <- match.arg(method)
  sides  <- match.arg(sides)

  conf.level <- checkConfLevel(conf.level)

  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  if (!inherits(x, "glm"))
    stop("Object must inherit from class 'glm'.")

  if (family(x)$family != "binomial")
    stop("Model must use binomial family.")

  coefTable <- summary(x)$coefficients

  beta <- coefTable[, "Estimate"]
  se   <- coefTable[, "Std. Error"]
  pval <- coefTable[, "Pr(>|z|)"]

  est <- exp(beta)

  if (is.na(conf.level)) {

    # the object always carries lci/uci columns, so they are filled with
    # NA rather than dropped - a caller reading $coefficients$lci should
    # not have to test for the column's existence
    lci <- rep(NA_real_, length(beta))
    uci <- rep(NA_real_, length(beta))

  } else if (method == "wald") {

    confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1
    # lower tail: (1 + confAdj) / 2 can round to 1 for levels close to 1
    z <- abs(qnorm((1 - confAdj) / 2))

    lci <- exp(beta - z * se)
    uci <- exp(beta + z * se)

    # per coefficient, so that a model with a single term does not fall
    # through to a length-2 vector
    bounds <- vapply(seq_along(lci),
                     function(i) applySides(c(lci[i], uci[i]), sides,
                                             lo = 0, hi = Inf),
                     numeric(2))
    lci <- bounds[1L, ]
    uci <- bounds[2L, ]

  } else {

    if (sides != "two.sided") {
      warning("profile likelihood intervals are always two-sided; ",
              "'sides' is ignored", call. = FALSE)
      # recorded as what was actually computed, so that print() does not
      # announce a one-sided interval it did not produce
      sides <- "two.sided"
    }

    # confint.glm() announces "Waiting for profiling to be done..." as a
    # message - progress chatter from a call the user never made
    # directly. Only messages are muffled; warnings about the profile
    # (e.g. non-convergence) still reach the caller.
    ci <- exp(suppressMessages(confint(x, level = conf.level)))
    ci <- ci[rownames(coefTable), , drop = FALSE]

    lci <- ci[, 1L]
    uci <- ci[, 2L]

  }

  coefficients <- data.frame(
    term = rownames(coefTable),
    est = est,
    logEst = beta,
    stdError = se,
    pValue = pval,
    lci = lci,
    uci = uci,
    row.names = NULL
  )

  res <- list(
    coefficients = coefficients,
    source = "glm",
    method = method,
    conf.level = conf.level,
    sides = sides,
    nObs = nobs(x),
    call = x$call
  )

  class(res) <- "OddsRatio"

  res

}


#' @rdname oddsRatio
#' @param digits number of digits used for printing
#' @export
print.OddsRatio <- function(x, digits = 3, ...) {

  cat("\nCall:\n")
  print(x$call)

  if (is.na(x$conf.level)) {
    cat("\nOdds Ratios (no confidence interval):\n\n")
  } else {
    cat("\nOdds Ratios (", 100 * x$conf.level, "% ", x$sides,
        " CI, method = ", x$method, "):\n\n", sep = "")
  }

  tab <- x$coefficients

  tabPrint <- data.frame(
    est    = pharos::fm(tab$est, digits = digits),
    lci    = pharos::fm(tab$lci, digits = digits),
    uci    = pharos::fm(tab$uci, digits = digits),
    pValue = pharos::fm(tab$pValue, digits = digits),
    row.names = tab$term
  )

  print(tabPrint, right = TRUE)
  cat("\n")

  invisible(x)

}



# == internal helper functions ==============================================

# The helpers return a TWO-SIDED interval at the level they are given.
# Opening the relevant side is the caller's job, in one place, via
# applySides() - see oddsRatio.default().

.oddsRatioWald <- function(x, conf.level) {

  # The caller has already added any explicit cell correction. Zero cells
  # are not corrected silently: the estimate is then 0 or Inf (rows and
  # columns are positive, so never 0/0) and no log-Wald interval exists.
  logEst <- log(x[1L, 1L]) + log(x[2L, 2L]) - log(x[1L, 2L]) - log(x[2L, 1L])
  est <- exp(logEst)

  if (is.na(conf.level))
    return(est)

  if (any(x == 0)) {
    warning("The log-Wald interval requires positive cell counts; ",
            "use method = 'exact', method = 'mid-p' or delta > 0.",
            call. = FALSE)
    return(c(est = est, lci = NA_real_, uci = NA_real_))
  }

  se <- sqrt(sum(1 / x))
  z  <- abs(qnorm((1 - conf.level) / 2))

  c(
    est = est,
    lci = exp(logEst - z * se),
    uci = exp(logEst + z * se)
  )

}



# Conditional estimate and interval for the odds ratio: exact (Fisher) or
# mid-p. Given the margins, n11 follows the noncentral hypergeometric
# distribution with noncentrality psi = OR,
#   P(X = s) proportional to choose(m, s) choose(n, k - s) psi^s,
# on the support s = max(0, k - n), ..., min(k, m).
#
#   exact: est = conditional MLE (E[X] = n11),
#          lci solves P(X >= n11) = alpha/2, uci solves P(X <= n11) = alpha/2
#   mid-p: est = median-unbiased (P(X < n11) = P(X > n11)),
#          the tails count P(X = n11) only half
#
# Every root is found on the log-OR scale, so the precision is relative.
# The tails are summed directly (never as 1 - complement), which keeps the
# small tail probabilities at the bounds accurate.
.oddsRatioConditional <- function(x, conf.level, midp) {

  n11 <- x[1L, 1L]
  m <- x[1L, 1L] + x[2L, 1L]
  n <- x[1L, 2L] + x[2L, 2L]
  k <- x[1L, 1L] + x[1L, 2L]

  support <- max(0, k - n):min(k, m)
  logBase <- dhyper(support, m, n, k, log = TRUE)
  below <- support < n11
  above <- support > n11
  at    <- support == n11
  w     <- if (midp) 0.5 else 1

  # probabilities at log odds ratio 'lor', normalised on the log scale
  dens <- function(lor) {
    d <- logBase + lor * support
    d <- exp(d - max(d))
    d / sum(d)
  }

  # (mid-)p tail probabilities P(X <= n11) and P(X >= n11); the first
  # decreases, the second increases with the odds ratio
  lowerTail <- function(lor) { d <- dens(lor); sum(d[below]) + w * sum(d[at]) }
  upperTail <- function(lor) { d <- dens(lor); sum(d[above]) + w * sum(d[at]) }

  atMin <- n11 == support[1L]
  atMax <- n11 == support[length(support)]

  est <- if (atMin) {
    0
  } else if (atMax) {
    Inf
  } else if (midp) {
    exp(.rootLog(function(l) { d <- dens(l); sum(d[below]) - sum(d[above]) },
                 increasing = FALSE))
  } else {
    exp(.rootLog(function(l) sum(support * dens(l)) - n11, increasing = TRUE))
  }

  if (is.na(conf.level))
    return(est)

  alpha2 <- (1 - conf.level) / 2

  lci <- if (atMin) 0 else
    exp(.rootLog(function(l) upperTail(l) - alpha2, increasing = TRUE))

  uci <- if (atMax) Inf else
    exp(.rootLog(function(l) lowerTail(l) - alpha2, increasing = FALSE))

  c(est = est, lci = lci, uci = uci)

}
