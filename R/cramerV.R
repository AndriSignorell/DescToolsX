
#' Cramer's V
#'
#' Measures the strength of association between two categorical variables. These
#' can be provided as two data vectors \code{x} and \code{y}, or as
#' a contingency table (see \link{Association}).
#'
#' Cramer's V ranges from 0 to 1, with 0 indicating statistical independence.
#'
#' @inheritParams Association
#'
#' @param correct logical; whether to apply the bias correction of Bergsma
#' (2013); defaults to \code{FALSE}
#'
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#'
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See details in \code{\link{ConfidenceIntervals}}.
#'
#' @param method character string specifying the confidence interval method:
#' \code{"ncchisq"} (default, using the noncentral chi-squared distribution),
#' \code{"ncchisqadj"}, \code{"fisher"} (using fisher z transformation),
#' or \code{"fisheradj"} (using the Fisher z transformation and bias correction)
#'
#' @param ... further arguments, passed on to
#'   \code{\link{normalizeToConfusion}} and \code{\link{table}} for building
#'   the table - \code{useNA} is the usual one.
#'
#' @return if \code{conf.level = NA}, a numeric scalar containing Cramer's V;
#' otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Cramer's V.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' @details
#' \code{correct = TRUE} applies Bergsma's bias correction to the point
#' estimate \emph{and} to the interval: both are put through the same
#' transformation of the chi-squared statistic, so estimate and bounds
#' always live on the same scale. Formerly the two noncentral chi-squared
#' methods returned an uncorrected interval around a corrected estimate.
#'
#' All four methods are analytical; there is no bootstrap here, and none is
#' needed - unlike [contCoef], where no generally accepted analytical
#' interval exists.
#'
#' For a one-sided interval the bound is computed at the adjusted level
#' \eqn{2\gamma - 1} and the open side is closed at the boundary of the
#' attainable range, 0 or 1, rather than at an infinity V can never reach.
#' \code{sides = "left"} names the side carrying the \emph{finite} bound and
#' is the analogue of \code{alternative = "greater"} in a test.
#'
#' The two Fisher methods rest on \eqn{\mathrm{Var}(\mathrm{atanh}(V))
#' \approx 1/(n-3)}. Where that approximation has nothing to say - three
#' or fewer observations, or a perfect association, where the
#' transformation is infinite - the bounds are \code{NA} and a warning
#' names the reason. The estimate itself is still returned.
#'
#' @note Based on code by  Michael Smithson (confidence intervals),
#' adapted to conform to package standards.
#'
#' @seealso [base::table], [pharos::plotCor],
#' [bedrock::pairApply], [Association]
#'
#' @references
#' Cramer, H. (1946) \emph{Mathematical Methods of Statistics}. Princeton
#' University Press
#'
#' Agresti, Alan (1996) \emph{Introduction to categorical data analysis}. NY:
#' John Wiley and Sons
#'
#' Bergsma, W. (2013) A bias-correction for Cramer's V and Tschuprow's T
#' \emph{Journal of the Korean Statistical Society} 42(3) DOI:
#' 10.1016/j.jkss.2012.10.002
#'
#' @examples
#'
#' tab <- table(Pizza$driver, Pizza$wine_delivered)
#' cramerV(tab)
#'
#' # just x and y
#' cramerV(Pizza$driver, Pizza$wine_delivered)
#'
#' # data.frame
#' bedrock::pairApply(Pizza[, c("driver", "operator", "area")], cramerV,
#'                    symmetric = TRUE)
#'
#' # useNA is passed on to table()
#' bedrock::pairApply(Pizza[, c("driver", "operator", "area")], cramerV,
#'                    useNA = "ifany", symmetric = TRUE)
#'
#' d.frm <- Pizza[, c("driver", "operator", "area")]
#' bedrock::pairApply(d.frm[complete.cases(d.frm), ], cramerV, symmetric = TRUE)
#'
#' # one-sided: "left" carries the finite lower bound, the upper one opens
#' # to the maximum V can attain
#' cramerV(tab, conf.level = 0.95, sides = "left")
#'
#'
#' # Bootstrap confidence intervals for Cramer's V
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, p. 1821
#'
#' tab <- as.table(rbind(
#'   c(26, 26, 23, 18,  9),
#'   c( 6,  7,  9, 14, 23)))
#' d.frm <- bedrock::untable(tab)
#'
#' set.seed(1)
#' n <- 1000
#' idx <- matrix(sample(nrow(d.frm), size = nrow(d.frm) * n, replace = TRUE),
#'               ncol = n, byrow = FALSE)
#' v <- apply(idx, 2, function(x) cramerV(d.frm[x, 1], d.frm[x, 2]))
#' quantile(v, probs = c(0.025, 0.975))
#'
#' # compare this to the analytical ones
#' cramerV(tab, conf.level = 0.95)
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @concept chi-square-based
#' @export
cramerV <- function(x, y = NULL, 
                    correct = FALSE,
                    conf.level = NA,
                    sides = c("two.sided", "left", "right"),
                    method = c("ncchisq", "ncchisqadj",
                               "fisher", "fisheradj"),
                     ...){

  # match.arg() used to sit inside switch(), so a misspelled method was
  # accepted without complaint whenever conf.level was NA
  method <- match.arg(method)
  sides  <- match.arg(sides)

  if (!is.logical(correct) || length(correct) != 1L || is.na(correct))
    stop("'correct' must be a single non-missing logical value")

  # Length and type BEFORE is.na(): NA is logical, and `&&` errors on a
  # condition of length != 1 since R 4.3 - conf.level = c(0.9, 0.95) or
  # NULL used to abort with a message about the condition rather than
  # about the argument, and NaN slipped through into the point estimate.
  conf.level <- .checkConfLevel(conf.level)
  
  # ... carries table() arguments such as useNA, which the documented
  # examples rely on; it was accepted and then dropped on the floor
  tab <- normalizeToConfusion(x, y, mode = "association", ...)

  # CIs and power for the noncentral chi-sq noncentrality parameter (ncp):
  # The function lochi computes the lower CI limit and hichi computes the upper limit.
  # Both functions take 3 arguments: observed chi-sq, df, and confidence level.

  # author:   Michael Smithson
  # http://psychology3.anu.edu.au/people/smithson/details/CIstuff/Splusnonc.pdf

  # see also: MBESS::conf.limits.nc.chisq, Ken Kelly

  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  chisq.hat <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  df <- prod(dim(tab) - 1)
  n <- sum(tab)

  v <- .cramerVFromChisq(chisq.hat, tab, n, correct)

  if (is.na(conf.level))
    return(v)

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1. Below 0.5 that level is not
  # positive and none of the four methods has anything to compute - the
  # noncentral search would be asked for a negative coverage, and the
  # Fisher half-width would come out negative and silently reverse the
  # bounds. tukeyBiweight refuses the same case; contCoef currently does
  # not, which is one of the open items for ConfidenceIntervals.
  if (sides != "two.sided" && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1

  switch(method,
         ncchisq = {
           ci <- .chisqNcpCI(chisq.hat, df, confAdj)
           ci <- .cramerVFromChisq(ci, tab, n, correct)
         },

         ncchisqadj = {
           ci <- .chisqNcpCI(chisq.hat, df, confAdj) + df
           ci <- .cramerVFromChisq(ci, tab, n, correct)
         },

         fisher = {
           halfWidth <- .fisherHalfWidth(n, confAdj, v, "fisher")
           ci <- tanh(atanh(v) + c(-halfWidth, halfWidth))
         },

         fisheradj = {
           halfWidth <- .fisherHalfWidth(n, confAdj, v, "fisheradj")
           # bias correction
           adj <- 0.5 * v / (n - 1)
           ci <- tanh(atanh(v) + c(-halfWidth, halfWidth) + adj)
         })

  # V lives in [0, 1], so the open side of a one-sided interval is closed
  # at the range boundary rather than at an infinity V can never reach.
  # .applySides() also clamps the two-sided interval, which the four
  # methods can overshoot.
  #    "Cram\u00E9r's association coefficient"
  c("est" = v, .applySides(ci, sides, lo = 0, hi = 1))
}



# == internal helper functions ==============================================


# Half-width of the Fisher-z interval, or NA where the transformation has
# nothing to say.
#
# Var(atanh(V)) is approximated by 1/(n - 3): infinite at n = 3, negative
# below. The old code returned (0, 1) at n = 3 and NaN plus a bare "NaNs
# produced" from sqrt() at n = 2 - two neighbouring sample sizes, two
# different answers, neither of them naming the reason.
#
# v == 1 is the same class and worse: atanh(1) is infinite, tanh(Inf - h)
# is exactly 1, so the interval collapsed to (1, 1) and ruled out every
# value below 1 - a claim no finite sample supports. Reachable with any
# perfectly associated table, e.g. matrix(c(5, 0, 0, 5), 2).
#' @noRd
.fisherHalfWidth <- function(n, confAdj, v, method) {

  if (n <= 3) {
    warning(gettextf(
      "method %s needs more than 3 observations; no interval computed",
      dQuote(method, FALSE)), call. = FALSE, domain = NA)
    return(NA_real_)
  }

  if (!is.finite(v) || v >= 1) {
    warning(gettextf(
      "method %s cannot bound a perfect association; no interval computed",
      dQuote(method, FALSE)), call. = FALSE, domain = NA)
    return(NA_real_)
  }

  qnorm(1 - (1 - confAdj) / 2) / sqrt(n - 3)
}


# The single mapping from a chi-squared quantity to Cramer's V, used for
# the point estimate and for both interval bounds so that the two cannot
# drift apart. Vectorised over 'chisq'.
#
# Bergsma, W. (2013) A bias-correction for Cramer's V and Tschuprow's T,
# Journal of the Korean Statistical Society 42(3).
#' @noRd
.cramerVFromChisq <- function(chisq, tab, n, correct = FALSE) {

  chisq <- as.numeric(chisq)

  if (!correct)
    return(sqrt(chisq / (n * (min(dim(tab)) - 1))))

  df <- prod(dim(tab) - 1)
  phi2 <- chisq / n
  dimTilde <- vapply(dim(tab), function(i) i - (i - 1)^2 / (n - 1), numeric(1L))

  sqrt(pmax(0, phi2 - df / (n - 1)) / min(dimTilde - 1))
}
