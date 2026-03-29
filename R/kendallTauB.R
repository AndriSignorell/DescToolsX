
#' Kendall's \eqn{\tau_{b}}{Tau-b} 
#' 
#' Calculate Kendall's tau-b. The estimator could also be calculated with
#' \code{cor(..., method="kendall")}. The calculation of confidence intervals
#' however would not be found there. 
#' 
#' 
#' @param x a numeric vector, matrix or data.frame. 
#' @param y NULL (default) or a vector with compatible dimensions to \code{x}.
#' If y is provided, \code{table(x, y, \dots)} is calculated. 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence interval will be calculated. 
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to set useNA. This refers only to the
#' vector interface.
#' 
#' @inheritParams ConfidenceIntervals
#' 
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{Association}} 
#' 
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons, pp. 57-59.
#' 
#' Kendall, M. (1955) \emph{Rank Correlation Methods}, Second Edition, London:
#' Charles Griffin and Co.
#' 
#' Brown, M.B.andBenedetti, J.K.(1977) Sampling Behavior of Tests for
#' Correlation in Two-Way Contingency Tables, \emph{Journal of the American
#' Statistical Association}, 72, 309-315.
#' 
#' @family topic.correlation
#' @concept association
#' @concept ordinal-data
#' @concept rank-methods
#' @concept nonparametric
#' 
#' @examples
#' 
#' # example in:
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. S. 1821
#' 
#' tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))
#' 
#' kendallTauB(tab, conf.level=0.95)
#' 

#' @export
kendallTauB <- function(x, y = NULL, 
                        conf.level = NA, 
                        sides = c("two.sided", "left", "right"),
                        ...){
  
  # Ref: http://www.fs.fed.us/psw/publications/lewis/LewisHMP.pdf
  # pp 2-9
  #
  
  if(!is.null(y)) {
    z <- conDisPairsXY_ind_cpp(x, y)
    
  } else {
    tab <- as.table(x)
    z <- conDisPairsTab(tab)
  }
  
  n <- sum(tab)
  n0 <- n*(n-1)/2
  ti <- rowSums(tab)
  uj <- colSums(tab)
  n1 <- sum(ti * (ti-1) / 2)
  n2 <- sum(uj * (uj-1) / 2)
  
  taub <- (z$C - z$D) / sqrt((n0-n1)*(n0-n2))
  
  pi <- tab / sum(tab)
  
  pdiff <- (z$pi.c - z$pi.d) / sum(tab)
  Pdiff <- 2 * (z$C - z$D) / sum(tab)^2
  
  rowsum <- rowSums(pi) 
  colsum <- colSums(pi)  
  
  rowmat <- matrix(rep(rowsum, dim(tab)[2]), ncol = dim(tab)[2])
  colmat <- matrix(rep(colsum, dim(tab)[1]), nrow = dim(tab)[1], byrow = TRUE)
  
  delta1 <- sqrt(1 - sum(rowsum^2))
  delta2 <- sqrt(1 - sum(colsum^2))
  
  # Compute asymptotic standard errors taub
  tauphi <- (2 * pdiff + Pdiff * colmat) * delta2 * delta1 + (Pdiff * rowmat * delta2)/delta1
  sigma2 <- ((sum(pi * tauphi^2) - sum(pi * tauphi)^2)/(delta1 * delta2)^4) / n
  
  # for very small pi/tauph it's possible that sigma2 gets negative so we cut small negative values here
  # example:  kendallTauB(table(iris$Species, iris$Species))
  if(sigma2 < .Machine$double.eps * 10) sigma2 <- 0
  
  if (is.na(conf.level)) {
    result <- taub
  }
  else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + taub
    result <- c(tau_b = taub, lci = max(ci[1], -1), uci = min(ci[2], 1))
  }
  
  return(result)

}

