
#' Stuart \eqn{\tau_{c}}{Tau-c} 
#' 
#' Calculate Stuart's \eqn{\tau_{c}}{tau-c} statistic, a measure of association
#' for ordinal factors in a two-way table.\cr The function has interfaces for a
#' table (matrix) and for single vectors. 
#' 
#' Stuart's \eqn{\tau_{c}}{tau-c} makes an adjustment for table size in
#' addition to a correction for ties. \eqn{\tau_{c}}{Tau-c} is appropriate only
#' when both variables lie on an ordinal scale. \cr It is estimated by \cr
#' \deqn{ \tau_{c} = \frac{2 m \cdot(P-Q)}{n^2 \cdot (m-1)}}{tau_c = 2m * (P-Q)
#' / (n^2 (m-1))} where P equals the number of concordances and Q the number of
#' discordances, n is the total amount of observations and m = min(R, C). The
#' range of \eqn{\tau_{c}}{tau-c} is \verb{[-1, 1]}. \cr See
#' \url{http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf},
#' pp. 1739 for the estimation of the asymptotic variance.
#' 
#' The use of Stuart's Tau-c versus Kendall's Tau-b is recommended when the two
#' ordinal variables under consideration have different numbers of values, e.g.
#' good, medium, bad versus high, low.
#'
#' @inheritParams Association 
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to set useNA. This refers only to the
#' vector interface. 
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{Association}}
#' 
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons, pp. 57--59.
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
#' @keywords multivar nonparametric
#' @examples
#' 
#' # example in:
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. S. 1821
#' 
#' tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))
#' 
#' stuartTauC(tab, conf.level=0.95)
#' 

#' @export
stuartTauC <- function(x, y = NULL, conf.level = NA, ...) {
  
  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)
  
  # Reference:
  # http://v8doc.sas.com/sashtml/stat/chap28/sect18.htm
  x <- conDisPairsTab(tab)
  
  m <- min(dim(tab))
  n <- sum(tab)
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- 4 * m^2 / ((m-1)^2 * n^4) * (sum(tab * (x$pi.c - x$pi.d)^2) - 4 * (x$C -x$D)^2/n)
  # debug: print(sqrt(sigma2))
  
  # Tau-c = (C - D)*[2m/(n2(m-1))]
  tauc <- (x$C - x$D) * 2 * min(dim(tab)) / (sum(tab)^2*(min(dim(tab))-1))
  
  if(is.na(conf.level)){
    result <- tauc
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    CI <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + tauc
    result <- c(tauc = tauc,  lwr.ci=max(CI[1], -1), upr.ci=min(CI[2], 1))
  }
  
  return(result)
  
}



