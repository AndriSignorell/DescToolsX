
#' Goodman Kruskal's Gamma 
#' 
#' Calculate Goodman Kruskal's Gamma statistic, a measure of association for
#' ordinal factors in a two-way table.\cr The function has interfaces for a
#' contingency table (matrix) and for single vectors (which will then be
#' tabulated).
#' 
#' The estimator of \eqn{\gamma}{gamma} is based only on the number of
#' concordant and discordant pairs of observations. It ignores tied pairs (that
#' is, pairs of observations that have equal values of X or equal values of Y).
#' Gamma is appropriate only when both variables lie on an ordinal scale. \cr
#' It has the range \verb{[-1, 1]}. If the two variables are independent, then the
#' estimator of gamma tends to be close to zero. For \eqn{2 \times 2}{2 x 2}
#' tables, gamma is equivalent to Yule's Q (\code{\link{yuleQ}}). \cr Gamma is
#' estimated by \deqn{ G = \frac{P-Q}{P+Q}}{G = (P-Q) / (P+Q) } where P equals
#' twice the number of concordances and Q twice the number of discordances. 
#' 
#' @inheritParams Association
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to control the handling of \code{NAs} by
#' setting the \code{useNA} argument. This refers only to the vector interface,
#' the dots are ignored if \code{x} is a contingency table. 
#' 
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso There's another implementation of gamma in \pkg{vcdExtra}
#' \code{\link[vcdExtra]{GKgamma}} \code{\link{Association}}
#' 
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons, pp. 57-59.
#' 
#' Brown, M.B., Benedetti, J.K.(1977) Sampling Behavior of Tests for
#' Correlation in Two-Way Contingency Tables, \emph{Journal of the American
#' Statistical Association}, 72, 309-315.
#' 
#' Goodman, L. A., & Kruskal, W. H. (1954) Measures of association for cross
#' classifications. \emph{Journal of the American Statistical Association}, 49,
#' 732-764.
#' 
#' Goodman, L. A., & Kruskal, W. H. (1963) Measures of association for cross
#' classifications III: Approximate sampling theory. \emph{Journal of the
#' American Statistical Association}, 58, 310-364.
#' 
#' 
#' @family topic.association-measures
#' @concept association
#' @concept ordinal-data
#' 
#' @examples
#' 
#' 
#' # example in:
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. S. 1821 (149)
#' 
#' tab <- as.table(rbind(
#'   c(26,26,23,18, 9),
#'   c( 6, 7, 9,14,23))
#'   )
#' 
#' gkGamma(tab, conf.level=0.95)
#' 
#' 

#' @export
gkGamma <- function(x, y = NULL, conf.level = NA, ...) {
  
  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)
  
  # tab is a matrix of counts
  # Based on code of Michael Friendly and Laura Thompson
  # Confidence interval calculation and output from Greg Rodd
  
  x <- conDisPairsTab(tab)
  
  psi <- 2 * (x$D * x$pi.c - x$C * x$pi.d)/(x$C + x$D)^2
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- sum(tab * psi^2) - sum(tab * psi)^2
  
  gamma <- (x$C - x$D)/(x$C + x$D)
  
  if(is.na(conf.level)){
    result <- gamma
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma
    result <- c(gamma = gamma,  lwr.ci=max(ci[1], -1), upr.ci=min(ci[2], 1))
  }
  
  return(result)
  
}
