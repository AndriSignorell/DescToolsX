
#' Kendall's \eqn{\tau_{a}}{Tau-a} 
#' 
#' Calculate Kendall's tau-a statistic, a measure of association for ordinal
#' factors in a two-way table.\cr The function has interfaces for a table
#' (matrix) and for single vectors. 
#' 
#' Kendall's tau coefficient (sometimes called "Kendall rank correlation
#' coefficient"), is a statistic used to measure the association between two
#' measured quantities. It is a measure of rank correlation: the similarity of
#' the orderings of the data when ranked by each of the quantities. \cr
#' Kendall's tau-a is computed as \deqn{ \tau_a(C|R) = \frac{P-Q}{\frac{1}{2}
#' \cdot n \cdot (n-1)}}{tau_a = 2 * (P-Q) / (n * (n-1))} where P equals twice
#' the number of concordances and Q twice the number of discordances. Its range
#' is \verb{[-1, 1]}.\cr (Note that Kendall tau-a does not take into consideration any
#' ties, which makes it unpractical. Consider using \code{\link{kendallTauB}}
#' (Tau-b) when ties are present.) 
#' 
#' @inheritParams Association
#' @param direction direction of the calculation. Can be \code{"row"} (default)
#' or \code{"column"}, where \code{"row"} calculates Kendall's tau-a (R|C)
#' ("column dependent"). 
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
#' & Sons, pp. 57-59.
#' 
#' Hollander, M, Wolfe, D. A., Chicken, E. (2014) \emph{Nonparametric
#' Statistical Methods}, Third edition, Wiley,
#' 
#' Liebetrau, A. M. (1983) \emph{Measures of Association}, Sage University
#' Papers Series on Quantitative Applications in the Social Sciences, 07-004.
#' Newbury Park, CA: Sage, pp. 49-56
#' @keywords multivar nonparametric
#' @examples
#' 
#' 
#' # example in:
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. S. 1821
#' 
#' tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))
#' 
#' # Kendall's tau-a C|R
#' kendallTauA(tab, direction="column", conf.level=0.95)
#' # Kendall's tau-a R|C
#' kendallTauA(tab, direction="row", conf.level=0.95)
#' 
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. 1814 (143)
#' tab <- as.table(cbind(c(11,2),c(4,6)))
#' 
#' kendallTauA(tab, direction="row", conf.level=0.95)
#' kendallTauA(tab, direction="column", conf.level=0.95)
#' 
#' # Liebetrau, pp. 52
#' x <- c(1,2,2,3,3,3,4,5)
#' y <- c(1,3,2,1,5,3,4,5)
#' 
#' kendallTauA(x, y, conf.level=0.95)
#' 


#' @export 
kendallTauA <- function(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...){
  
  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)
  
  x <- conDisPairsTab(tab)
  
  n <- sum(tab)
  n0 <- n*(n-1)/2
  
  taua <- (x$C - x$D) / n0
  
  # Hollander, Wolfe pp. 415/416
  # think we should not consider ties here, so take only the !=0 part
  Ci <- as.vector((x$pi.c - x$pi.d) * (tab!=0))
  Ci <- Ci[Ci!=0]
  C_ <- sum(Ci)/n
  sigma2 <- 2/(n*(n-1)) * ((2*(n-2))/(n*(n-1)^2) * sum((Ci - C_)^2) + 1 - taua^2)
  
  if (is.na(conf.level)) {
    result <- taua
  }
  else {
    
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + taua
    result <- c(tau_a = taua, lwr.ci = max(ci[1], -1), upr.ci = min(ci[2], 1))
  }
  
  return(result)
  
}

