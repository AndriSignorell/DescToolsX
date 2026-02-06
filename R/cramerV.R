
#' Cramer's V
#' 
#' Measures the strength of association between two categorical variables. These 
#' can either be provided by two data vectors \code{x}, \code{y} or by 
#' a contingency table (see \link{Association}).
#' 
#' Cramer's V range goes from 0 to 1. A Cramer's V in the range of \verb{[0, 0.3]} 
#' is considered as weak, \verb{[0.3,0.7]} as medium and > 0.7 as strong. 
#' The minimum value for all is 0 under statistical independence.
#' 
#' @aliases cramerV
#' @inheritParams Association
#' @param method string defining the method to calculate confidence intervals
#' , one out of \code{"ncchisq"} (default, using noncentral chisquare),
#' \code{"ncchisqadj"}, \code{"fisher"} (using fisher z transformation),
#' \code{"fisheradj"} (using fisher z transformation and bias correction), \code{"boot"} 
#' bootstrap intervals.
#' @param correct logical. Should a bias correction (see Bergsma, 2013) be
#' applied or not. Default is \code{FALSE}.
#' 
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval
#' @author Andri Signorell <andri@@signorell.net>, \cr Michael Smithson
#' <michael.smithson@@anu.edu.au> (confidence intervals for Cramer V)
#' @seealso \code{\link{table}}, \code{\link{plotCorr}},
#' \code{\link{pairApply}}, \code{\link{Association}}
#' %\url{http://faculty.chass.ncsu.edu/garson/PA765/assocnominal.htm} this is
#' outdated...
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
#' @keywords multivariate
#' @examples
#' 
#' tab <- table(d.pizza$driver, d.pizza$wine_delivered)
#' cramerV(tab)
#' 
#' # just x and y
#' cramerV(d.pizza$driver, d.pizza$wine_delivered)
#' 
#' # data.frame
#' pairApply(d.pizza[,c("driver","operator","area")], cramerV, symmetric = TRUE)
#' 
#' 
#' # useNA is passed to table
#' pairApply(d.pizza[,c("driver","operator","area")], cramerV,
#'           useNA="ifany", symmetric = TRUE)
#' 
#' d.frm <- d.pizza[,c("driver","operator","area")]
#' pairApply(d.frm[complete.cases(d.frm),], cramerV, symmetric = TRUE)
#' 
#' 
#' # Bootstrap confidence intervals for Cramer's V
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, p. 1821
#' 
#' tab <- as.table(rbind(
#'   c(26,26,23,18, 9),
#'   c( 6, 7, 9,14,23)))
#' d.frm <- untable(tab)
#' 
#' n <- 1000
#' idx <- matrix(sample(nrow(d.frm), size=nrow(d.frm) * n, replace=TRUE), ncol=n, byrow=FALSE)
#' v <- apply(idx, 2, function(x) cramerV(d.frm[x,1], d.frm[x,2]))
#' quantile(v, probs=c(0.025,0.975))
#' 
#' # compare this to the analytical ones
#' cramerV(tab, conf.level=0.95)
#' 
#' 


#' @export
cramerV <- function(x, y = NULL, conf.level = NA,
                    method = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"), 
                    correct=FALSE, ...){

  tab <- .normalizeToConfusion(x, y, mode = "association")

  # CIs and power for the noncentral chi-sq noncentrality parameter (ncp):
  # The function lochi computes the lower CI limit and hichi computes the upper limit.
  # Both functions take 3 arguments: observed chi-sq, df, and confidence level.
  
  # author:   Michael Smithson
  # http://psychology3.anu.edu.au/people/smithson/details/CIstuff/Splusnonc.pdf
  
  # see also: MBESS::conf.limits.nc.chisq, Ken Kelly
  

  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  chisq.hat <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  df <- prod(dim(tab)-1)
  n <- sum(tab)
  
  if(correct){
    
    # Bergsma, W, A bias-correction for Cramer's V and Tschuprow's T
    # September 2013Journal of the Korean Statistical Society 42(3)
    # DOI: 10.1016/j.jkss.2012.10.002
    phi.hat <- chisq.hat / n
    v <- as.numeric(sqrt(max(0, phi.hat - df/(n-1)) / 
                           (min(sapply(dim(tab), function(i) i - 1 / (n-1) * (i-1)^2) - 1))))
    
  } else {
    v <- as.numeric(sqrt(chisq.hat/(n * (min(dim(tab)) - 1))))
  }
  
  
  if (is.na(conf.level)) {
    res <- v
    
  } else {
    
    switch(match.arg(method),
           ncchisq={
             ci <- .ncp_ci_chisq(chisq.hat, df, conf.level)
             # corrected by michael smithson, 17.5.2014:
             #    ci <- unname(sqrt( (ci + df) / (sum(x) * (min(dim(x)) - 1)) ))
             ci <- unname(sqrt( (ci) / (n * (min(dim(tab)) - 1)) ))
           },
           
           ncchisqadj={
             ci <- .ncp_ci_chisq(chisq.hat, df, conf.level) + df
             # corrected by michael smithson, 17.5.2014:
             #    ci <- unname(sqrt( (ci + df) / (sum(x) * (min(dim(x)) - 1)) ))
             ci <- unname(sqrt( (ci) / (n * (min(dim(tab)) - 1)) ))
           },
           
           fisher={
             se <- 1 / sqrt(n-3) * qnorm(1-(1-conf.level)/2)
             ci <- tanh(atanh(v) + c(-se, se))
           },
           
           fisheradj={
             se <- 1 / sqrt(n-3) * qnorm(1-(1-conf.level)/2)
             # bias correction
             adj <- 0.5 * v / (n-1)
             ci <- tanh(atanh(v) + c(-se, se) + adj)
             
           })
    
    #    "Cram\u00E9r's association coefficient"
    res <- c("est"=v, lci=max(0, ci[1]), uci=min(1, ci[2]))
    
  }
  
  return(res)
}

