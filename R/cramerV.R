
#' Cramer's V
#' 
#' Measures the strength of association between two categorical variables. These
#' can be provided as two data vectors \code{x} and \code{y}, or as
#' a contingency table (see \link{Association}).
#' 
#' Cramer's V ranges from 0 to 1, with 0 indicating statistical independence.
#' 
#' @aliases cramerV
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
# 
#' @examples
#' 
#' tab <- table(Pizza$driver, Pizza$wine_delivered)
#' cramerV(tab)
#' 
#' # just x and y
#' cramerV(Pizza$driver, Pizza$wine_delivered)
#' 
#' # data.frame
#' bedrock::pairApply(Pizza[,c("driver","operator","area")], cramerV, symmetric = TRUE)
#' 
#' 
#' # useNA is passed to table
#' bedrock::pairApply(Pizza[,c("driver","operator","area")], cramerV,
#'           useNA="ifany", symmetric = TRUE)
#' 
#' d.frm <- Pizza[,c("driver","operator","area")]
#' bedrock::pairApply(d.frm[complete.cases(d.frm),], cramerV, symmetric = TRUE)
#' 
#' 
#' # Bootstrap confidence intervals for Cramer's V
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, p. 1821
#' 
#' tab <- as.table(rbind(
#'   c(26,26,23,18, 9),
#'   c( 6, 7, 9,14,23)))
#' d.frm <- bedrock::untable(tab)
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
#' @family assoc.nominal  
#' @concept association-measure  
#' @concept nominal  
#' @concept chi-square-based
#'
#'
#' @export
cramerV <- function(x, y = NULL, conf.level = NA,
                    method = c("ncchisq", "ncchisqadj", 
                               "fisher", "fisheradj"), 
                    correct=FALSE, ...){

  tab <- normalizeToConfusion(x, y, mode = "association")

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
             ci <- .chisqNcpCI(chisq.hat, df, conf.level)
             # corrected by michael smithson, 17.5.2014:
             #    ci <- unname(sqrt( (ci + df) / (sum(x) * (min(dim(x)) - 1)) ))
             ci <- unname(sqrt( (ci) / (n * (min(dim(tab)) - 1)) ))
           },
           
           ncchisqadj={
             ci <- .chisqNcpCI(chisq.hat, df, conf.level) + df
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
