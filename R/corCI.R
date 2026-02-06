
#' Confidence Intervals for Pearson Correlation
#' 
#' Find the confidence intervals for a specified correlation based on 
#' Fisher's z-transformation.
#' 
#' The sampling distribution of Pearson's r is not normal. Fisher
#' developed a transformation now called "Fisher's z-transformation"
#' used for the calculation of normal distributed confidence intervals.
#' 
#' @aliases corCI

#' @param rho the Pearson's correlation coefficient
#' @param n sample size used for calculating the confidence intervals
#' @param alternative is a character string, one of \code{"greater"},
#' \code{"less"}, or \code{"two.sided"}, or the initial letter of each,
#' indicating the specification of the alternative hypothesis.
#' \code{"greater"} corresponds to positive association, \code{"less"} to
#' negative association.
#' @param conf.level confidence level for the returned confidence interval,
#' restricted to lie between zero and one.
#' @return rho, lower and upper confidence intervals (corCI) \cr
#' @author William Revelle <revelle@@northwestern.edu>, \cr slight
#' modifications Andri Signorell <andri@@signorell.net> based on R-Core code
#' @seealso \code{\link{fisherZ}}, \code{\link{fisherZInv}}
#' @keywords multivariate models
#' @examples
#' 
#' cors <- seq(-.9, .9, .1)
#' 
#' zs <- fisherZ(cors)
#' rs <- fisherZInv(zs)
#' round(zs, 2)
#' n <- 30
#' r <- seq(0, .9, .1)
#' rc <- t(sapply(r, corCI, n=n))
#' t <- r * sqrt(n-2) / sqrt(1-r^2)
#' p <- (1 - pt(t, n-2)) / 2
#' 
#' r.rc <- data.frame(r=r, z=fisherZ(r), lower=rc[,2], upper=rc[,3], t=t, p=p)
#' 
#' round(r.rc,2)
#' 


#' @export
corCI <- function(rho, n, conf.level = 0.95, alternative = c("two.sided","less","greater")) {
  
  
  if (n < 3L)
    stop("not enough finite observations")
  
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level)
                               || conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  
  alternative <- match.arg(alternative)
  
  # correct rho == 1 with rho == almost 1 in order to return ci = c(1, 1)
  # which is a sensible value for the confidence interval
  if(identical(rho, 1)) 
    ci <- c(1, 1)
  
  else {
    z <- fisherZ(rho)
    sigma <- 1/sqrt(n - 3)
    
    ci <- switch(alternative,
                 less = c(-Inf, z + sigma * qnorm(conf.level)),
                 greater = c(z - sigma * qnorm(conf.level), Inf),
                 two.sided = z + c(-1, 1) * sigma * qnorm((1 + conf.level)/2))
    
    ci <- fisherZInv(ci)
    
  }
  
  return(c(cor = rho, lwr.ci = ci[1], upr.ci = ci[2]))
  
}


