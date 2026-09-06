
#' Kappa for m Raters
#'
#' Computes agreement among \eqn{m \ge 2} raters on categorical items, using
#' Fleiss' kappa (the default), Conger's exact kappa, or Light's kappa.
#'
#' All three coefficients contrast the mean observed agreement with the
#' agreement expected by chance, and differ in how that chance agreement is
#' derived.
#'
#' \describe{
#'   \item{`"fleiss"`}{Fleiss (1971) bases chance agreement on the
#'     category proportions pooled across all raters. It generalises Scott's
#'     pi (Scott, 1955), and for \eqn{m = 2} it equals Scott's pi - not
#'     Cohen's kappa.}
#'   \item{`"conger"`}{Conger (1980) derives chance agreement from each
#'     rater's own marginal distribution, and reduces exactly to Cohen's
#'     kappa when \eqn{m = 2}.}
#'   \item{`"light"`}{Light (1971) is the unweighted mean of all
#'     \eqn{\binom{m}{2}} pairwise Cohen kappas.}
#' }
#'
#' Confidence intervals are of Wald type. For `"fleiss"` and
#' `"conger"` the standard error is the analytic one given by the
#' respective author. For `"light"` no closed-form variance exists;
#' since the estimate is the mean of the pairwise kappas, its standard error
#' is estimated as their standard deviation divided by
#' \eqn{\sqrt{\binom{m}{2}}}. This treats the pairwise kappas as independent,
#' which they are not - they share raters - so the interval is approximate
#' and tends to be too narrow. A bootstrap interval is preferable when the
#' assumption is doubtful.
#'
#' Kappa lies in \eqn{[-1, 1]}, so the interval is restricted to that range
#' and the open side of a one-sided interval is reported at the boundary
#' rather than at \eqn{\pm\infty}. See [ConfidenceIntervals()].
#'
#' @param x a \eqn{n \times m}{n x m} matrix or data frame, \eqn{n} subjects
#' in rows and \eqn{m} raters in columns
#' @param estimator a character string specifying the coefficient to compute.
#' One of `"fleiss"` (default), `"conger"`, or `"light"`.
#' These are three different coefficients for the same quantity, not three
#' interval methods - hence `estimator` and not `method`.
#' 
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'
#' @param use a character string giving the treatment of missing values. One
#' of `"complete.obs"` (default), which drops any subject rated
#' incompletely; `"everything"`, which returns `NA` if any value is
#' missing; or `"pairwise.complete.obs"`, which uses all subjects rated
#' by both members of each rater pair. The last is available for
#' `estimator = "light"` only, since Fleiss' and Conger's coefficients
#' require a complete row per subject.
#'
#' @return a named numeric vector. If `conf.level = NA`, only
#' `est` is returned; otherwise the vector has elements:
#' \describe{
#'   \item{`est`}{point estimate of the selected kappa coefficient}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @note Based on code by Matthias Gamer previously published as `kappaM.fleiss()` in
#' the \pkg{irr} package, rewritten to conform to package standards.
#'
#' @references
#' Conger, A. J. (1980). Integration and generalisation of Kappas for
#'   multiple raters. *Psychological Bulletin*, *88*(2), 322-328.
#'
#' Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters.
#'   *Psychological Bulletin*, *76*(5), 378-382.
#'
#' Fleiss, J. L., Levin, B., & Paik, M. C. (2003). *Statistical Methods
#'   for Rates and Proportions* (3rd ed.). New York: John Wiley & Sons.
#'
#' Light, R. J. (1971). Measures of response agreement for qualitative data:
#'   Some generalizations and alternatives. *Psychological Bulletin*,
#'   *76*(5), 365-377.
#'
#' Scott, W. A. (1955). Reliability of content analysis: The case of nominal
#'   scale coding. *Public Opinion Quarterly*, *19*(3), 321-325.
#'
#' @examples
#' statement <- data.frame(
#'   A = c(2,3,1,3,1,2,1,2,3,3,3,3,3,2,1,3,3,2,2,1,
#'         2,1,3,3,2,2,1,2,1,1,2,3,3,3,3,3,1,2,1,1),
#'   B = c(2,2,2,1,1,2,1,2,3,3,2,3,1,3,1,1,3,2,1,2,
#'         2,1,3,2,2,2,3,2,1,1,2,2,3,3,3,3,2,2,2,3),
#'   C = c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,2,2,2,2,3,
#'         2,2,3,3,2,2,3,2,2,2,2,3,3,3,3,3,3,2,2,2),
#'   D = c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,3,2,2,2,2,
#'         3,1,3,2,2,2,1,2,2,1,2,3,3,3,3,3,3,2,2,1),
#'   E = c(2,2,2,3,3,2,3,1,3,3,2,3,3,3,3,3,2,2,2,3,
#'         2,3,3,2,2,2,3,2,1,3,2,3,3,1,3,3,3,2,2,1)
#' )
#'
#' kappaM(statement)
#'
#' # Conger's exact kappa
#' kappaM(statement, estimator = "conger")
#'
#' # Light's kappa, the mean of the pairwise Cohen kappas
#' kappaM(statement, estimator = "light")
#'
#' # Fleiss' kappa with a confidence interval
#' kappaM(statement, conf.level = 0.95)
#'
#' @family assoc.agreement
#' @concept interrater-agreement
#' @concept categorical-agreement
#'
#' @export
kappaM <- function(x,
                   conf.level = NA,
                   sides = c("two.sided", "left", "right"),
                   estimator = c("fleiss", "conger", "light"),
                   use = c("complete.obs", "everything",
                           "pairwise.complete.obs")) {

  if(!is.matrix(x) && !is.data.frame(x))
    stop("Argument 'x' must be a matrix or a data frame.")

  estimator <- match.arg(estimator)
  sides     <- match.arg(sides)
  use       <- match.arg(use)

  # Fleiss' and Conger's coefficients count, for every subject, how often
  # each category was chosen across the full set of raters. A subject
  # rated by only some of them contributes a different denominator, which
  # is the variable-rater extension of Fleiss' kappa - a different
  # estimator rather than a different missing-value rule.
  if(use == "pairwise.complete.obs" && estimator != "light")
    stop(gettextf(
      "use = \"pairwise.complete.obs\" is available for estimator = \"light\" only, not for \"%s\".",
      estimator), domain = NA)

  # Four hand-written blocks stood here, all with the same message. The
  # order they got right - length and type before is.na(), NaN excluded
  # explicitly - is exactly what the shared helper encodes.
  conf.level <- checkConfLevel(conf.level)

  if(sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  # factor() is needed below to force a common level set across raters;
  # a matrix has no columns to convert, so it becomes a data frame first.
  if(is.matrix(x))
    x <- as.data.frame(x)

  if(use == "everything" && anyNA(x)) {
    if(is.na(conf.level))
      return(.makeEstimateResult(est = NA_real_))
    return(.makeEstimateResult(
      est = NA_real_,
      lci = NA_real_,
      uci = NA_real_
    ))
  }

  if(use == "complete.obs")
    x <- na.omit(x)

  ns <- nrow(x)
  nr <- ncol(x)

  if(nr < 2L)
    stop("Argument 'x' must have at least 2 raters (columns).")

  if(ns < 2L)
    stop("Argument 'x' must have at least 2 subjects (rows) with usable ratings.")

  # A common level set across all raters, so that a category no single
  # rater used still occupies its own column in the count table.
  lev <- levels(factor(unlist(x)))
  levi <- seq_along(lev)

  if(length(lev) < 2L)
    stop("Argument 'x' must contain at least 2 distinct rating categories.")

  xx <- do.call(cbind, lapply(x, factor, levels = lev))

  # ttab[i, k]: how many raters assigned category k to subject i.
  ttab <- apply(
    bedrock::abind(
      lapply(as.data.frame(xx),
             function(z) dummy(z, method = "full", levels = levi)),
      along = 3),
    c(1, 2), sum)

  agreeP <- sum((rowSums(ttab^2) - nr) / (nr * (nr - 1)) / ns)

  res <- switch(
    estimator,

    "fleiss" = .kappaFleiss(ttab, ns, nr, agreeP),
    "conger" = .kappaConger(ttab, xx, ns, nr, levi, agreeP),
    "light"  = .kappaLight(x, nr, use)

  )

  if(is.na(conf.level))
    return(.makeEstimateResult(est = res$est))

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1. Written this way rather than
  # as a separate qnorm(conf.level) branch, so that every function in the
  # suite adjusts the level in the same place and the same way; the number
  # is identical.
  confAdj <- if(sides == "two.sided") conf.level else 2 * conf.level - 1
  zCrit   <- qnorm(1 - (1 - confAdj) / 2)

  # A zero or non-finite standard error leaves the Wald interval
  # undefined. It used to collapse onto the estimate, which reads like a
  # computed interval of width zero and rules out every other value - a
  # claim no finite sample supports. NA with a reason, as in spearmanCor
  # and cramerV; the estimate is still returned.
  if(!is.finite(res$se) || res$se == 0) {

    warning("the standard error is zero or undefined; no interval computed",
            call. = FALSE)

    lci <- NA_real_
    uci <- NA_real_

  } else {

    lci <- res$est - zCrit * res$se
    uci <- res$est + zCrit * res$se

  }

  # Kappa lies in [-1, 1]. The Wald interval can leave that range, and the
  # open side of a one-sided interval belongs at the boundary rather than
  # at an infinity kappa cannot reach - applySides() does both.
  ci <- applySides(c(lci, uci), sides, lo = -1, hi = 1)

  .makeEstimateResult(
    est = res$est,
    lci = ci[["lci"]],
    uci = ci[["uci"]]
  )

}


# == internal helper functions ================================================

.kappaFleiss <- function(ttab, ns, nr, agreeP) {

  chanceP <- sum(colSums(ttab)^2) / (ns * nr)^2
  est <- (agreeP - chanceP) / (1 - chanceP)

  pj <- colSums(ttab) / (ns * nr)
  qj <- 1 - pj

  # Fleiss (1971), as given in Fleiss, Levin & Paik (2003):
  #   var = 2 / (n m (m-1)) * [ (sum pq)^2 - sum pq(q-p) ] / (sum pq)^2
  spq <- sum(pj * qj)

  varkappa <- (2 / (spq^2 * (ns * nr * (nr - 1)))) *
    (spq^2 - sum(pj * qj * (qj - pj)))

  list(est = est, se = sqrt(max(varkappa, 0)))

}


.kappaConger <- function(ttab, xx, ns, nr, levi, agreeP) {

  # rtab[k, r]: the proportion of subjects to which rater r assigned
  # category k, i.e. each rater's own marginal distribution.
  rtab <- apply(
    bedrock::abind(
      lapply(as.data.frame(t(xx)),
             function(z) dummy(z, method = "full", levels = levi)),
      along = 3),
    c(1, 2), sum)

  rtab <- rtab / ns

  chanceP <- sum(colSums(ttab)^2) / (ns * nr)^2 -
    sum(apply(rtab, 2, var) * (nr - 1) / nr) / (nr - 1)

  est <- (agreeP - chanceP) / (1 - chanceP)

  # Observed agreement per subject.
  Po_i <- apply(xx, 1, function(row) {
    counts <- table(row)
    sum(counts * (counts - 1)) / (nr * (nr - 1))
  })

  # Rater-specific marginal distributions, categories in rows.
  pjr <- do.call(cbind, lapply(seq_len(nr), function(r) {
    as.numeric(table(xx[, r])) / ns
  }))
  rownames(pjr) <- as.character(levi)

  chanceP_i <- .congerChancePerSubject(xx, pjr)

  num <- mean(((1 - chanceP) * Po_i - 2 * (1 - agreeP) * chanceP_i)^2) -
    (agreeP * chanceP - 2 * chanceP + agreeP)^2

  varkappa <- num / ((1 - chanceP)^4 * ns)

  list(est = est, se = sqrt(max(varkappa, 0)))

}


# Expected agreement per subject, given each rater's own marginals.
#
# M: n x m matrix of ratings, sharing one level set.
# pjr: k x m matrix of marginal proportions, categories in rows and
#      raters in columns.
#
# For every ordered pair of distinct raters (r, r2) this takes the
# probability that rater r would assign the category rater r2 actually
# chose for that subject, and averages over the m(m-1) ordered pairs.
.congerChancePerSubject <- function(M, pjr) {

  nSubj <- nrow(M)
  nRater <- ncol(M)

  # Map every observed rating onto its row number in pjr.
  lv <- rownames(pjr)
  idx <- apply(M, 2, function(col) match(as.character(col), lv))

  vapply(seq_len(nSubj), function(i) {

    # pjr[idx[i, ], ] picks, for each rater r2 (rows of the result), the
    # marginal every rater r (columns) attaches to r2's chosen category.
    # The diagonal is the r == r2 case and is excluded.
    mat <- pjr[idx[i, ], , drop = FALSE]

    (sum(mat) - sum(diag(mat))) / (nRater * (nRater - 1))

  }, numeric(1))

}


.kappaLight <- function(x, nr, use) {

  # Light's kappa is the mean of all pairwise Cohen kappas. Computing
  # them explicitly - rather than via pairApply() - lets the pairwise
  # missing-value rule apply per pair.
  pairs <- combn(nr, 2)

  kappas <- vapply(seq_len(ncol(pairs)), function(p) {

    i <- pairs[1, p]
    j <- pairs[2, p]

    xi <- x[[i]]
    xj <- x[[j]]

    if(use == "pairwise.complete.obs") {

      keep <- !is.na(xi) & !is.na(xj)
      xi <- xi[keep]
      xj <- xj[keep]

    }

    if(length(xi) < 2L)
      return(NA_real_)

    cohenKappa(xi, xj)

  }, numeric(1))

  est <- mean(kappas, na.rm = TRUE)

  # No closed-form variance exists for Light's kappa. The estimate is a
  # mean over the pairwise kappas, so its standard error is taken from
  # their dispersion. They share raters and are therefore not
  # independent, which makes this approximate and generally optimistic.
  #
  # The published formula this replaces multiplied choose(m, 2) terms of
  # order n^2 each, overflowing to a non-finite value - and hence to a
  # NaN standard error - for even moderate n and m.
  nPairs <- sum(!is.na(kappas))

  se <- if(nPairs < 2L)
    NA_real_
  else
    sd(kappas, na.rm = TRUE) / sqrt(nPairs)

  list(est = est, se = se)

}
