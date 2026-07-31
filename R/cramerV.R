
#' Cramer's V
#'
#' Measures the strength of association between two categorical variables. These
#' can be provided as two data vectors \code{x} and \code{y}, or as
#' a contingency table (see \link{Association}).
#'
#' Cramer's V ranges from 0 to 1, with 0 indicating statistical independence.
#'
#' @inheritParams Association
#' @param method character string specifying the confidence interval method:
#' \code{"ncchisq"} (default, using the noncentral chi-squared distribution),
#' \code{"ncchisqadj"}, \code{"fisher"} (using fisher z transformation),
#' or \code{"fisheradj"} (using the Fisher z transformation and bias correction)
#' @param correct logical; whether to apply the bias correction of Bergsma
#' (2013); defaults to \code{FALSE}
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
cramerV <- function(x, y = NULL, conf.level = NA,
                    method = c("ncchisq", "ncchisqadj",
                               "fisher", "fisheradj"),
                    correct = FALSE, ...){

  # match.arg() used to sit inside switch(), so a misspelled method was
  # accepted without complaint whenever conf.level was NA
  method <- match.arg(method)

  if (!is.logical(correct) || length(correct) != 1L || is.na(correct))
    stop("'correct' must be a single non-missing logical value")

  if (!is.na(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level) != 1L ||
       conf.level <= 0 || conf.level >= 1))
    stop("'conf.level' must be a single number in (0, 1), or NA")

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

  switch(method,
         ncchisq = {
           ci <- .chisqNcpCI(chisq.hat, df, conf.level)
           ci <- .cramerVFromChisq(ci, tab, n, correct)
         },

         ncchisqadj = {
           ci <- .chisqNcpCI(chisq.hat, df, conf.level) + df
           ci <- .cramerVFromChisq(ci, tab, n, correct)
         },

         fisher = {
           halfWidth <- 1 / sqrt(n - 3) * qnorm(1 - (1 - conf.level) / 2)
           ci <- tanh(atanh(v) + c(-halfWidth, halfWidth))
         },

         fisheradj = {
           halfWidth <- 1 / sqrt(n - 3) * qnorm(1 - (1 - conf.level) / 2)
           # bias correction
           adj <- 0.5 * v / (n - 1)
           ci <- tanh(atanh(v) + c(-halfWidth, halfWidth) + adj)
         })

  #    "Cram\u00E9r's association coefficient"
  c("est" = v, lci = max(0, ci[1]), uci = min(1, ci[2]))
}


# == internal helper functions ==============================================

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
