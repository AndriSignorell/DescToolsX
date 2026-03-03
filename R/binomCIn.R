
#' Sample Size for a Desired Width of a Binomial Confidence Interval
#'
#' \code{binomCIn()} computes the required sample size to obtain a binomial
#' confidence interval of a specified width, as calculated by \code{binomCI()}.
#' The function uses \code{\link{uniroot}()} to numerically solve for the
#' corresponding sample size.
#'
#' \strong{Required Samplesize: } 
#' The required sample size for a given confidence interval width depends
#' on the assumed population proportion. Since this proportion is often
#' unknown at the planning stage of a study, a conservative approach is to
#' use the worst-case scenario of \eqn{p = 0.5}, which maximizes the variance
#' and therefore yields the largest required sample size. If a more 
#' accurate estimate of the population proportion is available,
#' it can be used to obtain a smaller required sample size for the same
#' level of precision.
#'  
#' @param p probability for success, defaults to \code{0.5} as worst case. 
#' @param width the width of the confidence interval 
#' @param interval a vector containing the end-points of the interval to be
#' searched for the root. The defaults are set to \code{c(1, 100000)}. 
#' 
#' @return \code{binomCI()} returns a single numeric value 
#' 
#' @examples
#' 
#' binomCIn(p=0.1, width=0.05, method="pratt")
#' 

#' @rdname binomCI
#' @export
binomCIn <- function(p=0.5, width, interval=c(1, 1e5), 
                     conf.level=0.95, sides="two.sided", method="wilson") {
  
  uniroot(f = function(n) diff(binomCI(x=p*n, n=n, conf.level=conf.level, 
                                       sides=sides, method=method)[-1]) - width, 
          interval = interval)$root
  
}


