
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
#'   \item{\code{"fleiss"}}{Fleiss (1971) bases chance agreement on the
#'     category proportions pooled across all raters. It generalises Scott's
#'     pi (Scott, 1955), and for \eqn{m = 2} it equals Scott's pi - not
#'     Cohen's kappa.}
#'   \item{\code{"conger"}}{Conger (1980) derives chance agreement from each
#'     rater's own marginal distribution, and reduces exactly to Cohen's
#'     kappa when \eqn{m = 2}.}
#'   \item{\code{"light"}}{Light (1971) is the unweighted mean of all
#'     \eqn{\binom{m}{2}} pairwise Cohen kappas.}
#' }
#'
#' Confidence intervals are of Wald type. For \code{"fleiss"} and
#' \code{"conger"} the standard error is the analytic one given by the
#' respective author. For \code{"light"} no closed-form variance exists;
#' since the estimate is the mean of the pairwise kappas, its standard error
#' is estimated as their standard deviation divided by
#' \eqn{\sqrt{\binom{m}{2}}}. This treats the pairwise kappas as independent,
#' which they are not - they share raters - so the interval is approximate
#' and tends to be too narrow. A bootstrap interval is preferable when the
#' assumption is doubtful.
#'
#' \code{sides} names the side on which the finite bound lies:
#' \code{"left"} yields \eqn{[lci, \infty)} and \code{"right"}
#' \eqn{(-\infty, uci]}. Note that this is the reverse of the convention in
#' \pkg{DescTools}, where \code{sides} follows the alternative hypothesis of
#' \code{\link[stats]{t.test}}.
#'
#' @param x a \eqn{n \times m}{n x m} matrix or data frame, \eqn{n} subjects
#' in rows and \eqn{m} raters in columns
#' @param method a character string specifying the coefficient to compute.
#' One of \code{"fleiss"} (default), \code{"conger"}, or \code{"light"}.
#' @param conf.level a single confidence level for the returned confidence
#' interval. Set to \code{NA} (default) to suppress confidence interval
#' calculation.
#' @param sides a character string specifying a two-sided or one-sided
#' confidence interval
#' @param use a character string giving the treatment of missing values. One
#' of \code{"complete.obs"} (default), which drops any subject rated
#' incompletely; \code{"everything"}, which returns \code{NA} if any value is
#' missing; or \code{"pairwise.complete.obs"}, which uses all subjects rated
#' by both members of each rater pair. The last is available for
#' \code{method = "light"} only, since Fleiss' and Conger's coefficients
#' require a complete row per subject.
#'
#' @return a named numeric vector. If \code{conf.level = NA}, only
#' \code{est} is returned; otherwise the vector has elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the selected kappa coefficient}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @note Based on code by Matthias Gamer previously published as \code{kappaM.fleiss()} in
#' the \pkg{irr} package, rewritten to conform to package standards.
#'
#' @references
#' Conger, A. J. (1980). Integration and generalisation of Kappas for
#'   multiple raters. \emph{Psychological Bulletin}, \emph{88}(2), 322-328.
#'
#' Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters.
#'   \emph{Psychological Bulletin}, \emph{76}(5), 378-382.
#'
#' Fleiss, J. L., Levin, B., & Paik, M. C. (2003). \emph{Statistical Methods
#'   for Rates and Proportions} (3rd ed.). New York: John Wiley & Sons.
#'
#' Light, R. J. (1971). Measures of response agreement for qualitative data:
#'   Some generalizations and alternatives. \emph{Psychological Bulletin},
#'   \emph{76}(5), 365-377.
#'
#' Scott, W. A. (1955). Reliability of content analysis: The case of nominal
#'   scale coding. \emph{Public Opinion Quarterly}, \emph{19}(3), 321-325.
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
#' kappaM(statement, method = "conger")
#'
#' # Light's kappa, the mean of the pairwise Cohen kappas
#' kappaM(statement, method = "light")
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
                   method = c("fleiss", "conger", "light"),
                   conf.level = NA,
                   sides = c("two.sided", "left", "right"),
                   use = c("complete.obs", "everything",
                           "pairwise.complete.obs")) {

  if(!is.matrix(x) && !is.data.frame(x))
    stop("Argument 'x' must be a matrix or a data frame.")

  method <- match.arg(method)
  sides <- match.arg(sides)
  use <- match.arg(use)

  # Fleiss' and Conger's coefficients count, for every subject, how often
  # each category was chosen across the full set of raters. A subject
  # rated by only some of them contributes a different denominator, which
  # is the variable-rater extension of Fleiss' kappa - a different
  # estimator rather than a different missing-value rule.
  if(use == "pairwise.complete.obs" && method != "light")
    stop(gettextf(
      "use = \"pairwise.complete.obs\" is available for method = \"light\" only, not for \"%s\".",
      method), domain = NA)

  # Checked for type and length before is.na(), which would otherwise be
  # passed a zero-length or multi-element value and make the if() below
  # fail with an internal condition-length error rather than a clear
  # message.
  if(!is.numeric(conf.level) && !is.logical(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if(length(conf.level) != 1L)
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  # NaN is numeric and NA-like, but suppressing the interval on a NaN
  # confidence level would hide a caller error rather than express an
  # intent to omit it, so only a true NA does that.
  if(is.nan(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if(!is.na(conf.level)) {

    if(!is.numeric(conf.level) ||
       !is.finite(conf.level) ||
       conf.level <= 0 ||
       conf.level >= 1) {

      stop("Argument 'conf.level' must be a single number between 0 and 1.")

    }

  }

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
    method,

    "fleiss" = .kappaFleiss(ttab, ns, nr, agreeP),
    "conger" = .kappaConger(ttab, xx, ns, nr, levi, agreeP),
    "light"  = .kappaLight(x, nr, use)

  )

  if(is.na(conf.level))
    return(.makeEstimateResult(est = res$est))

  alpha <- 1 - conf.level

  zCrit <- if(sides == "two.sided")
    qnorm(1 - alpha / 2)
  else
    qnorm(conf.level)

  # A zero or non-finite standard error leaves the Wald interval
  # undefined; it collapses onto the estimate rather than returning
  # limits that only look informative.
  if(!is.finite(res$se) || res$se == 0) {

    lci <- res$est
    uci <- res$est

  } else {

    lci <- res$est - zCrit * res$se
    uci <- res$est + zCrit * res$se

  }

  if(sides == "left")
    uci <- Inf

  if(sides == "right")
    lci <- -Inf

  .makeEstimateResult(
    est = res$est,
    lci = lci,
    uci = uci
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
