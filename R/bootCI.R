
#' Simple Bootstrap Confidence Intervals 
#' 
#' Convenience wrapper for calculating bootstrap confidence intervals for
#' univariate and bivariate statistics. 
#' 
#' 
#' @param x a (non-empty) numeric vector of data values.
#' @param y NULL (default) or a vector with compatible dimensions to \code{x},
#' when a bivariate statistic is used.
#' @param FUN the function to be used
#' @param bci.method A vector of character strings representing the type of
#' intervals required. The value should be any subset of the values
#' \code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"},
#' as it is passed on as \code{method} to \code{\link[boot]{boot.ci}}.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param ... further arguments are passed to the function \code{FUN}.
#' @param R The number of bootstrap replicates. Usually this will be a single
#' positive integer. For importance resampling, some resamples may use one set
#' of weights and others use a different set of weights. In this case \code{R}
#' would be a vector of integers where each component gives the number of
#' resamples from each of the rows of weights.
#' 
#' @return a named numeric vector with 3 elements: \item{est}{the specific
#' estimate, as calculated by \code{FUN}} \item{lwr.ci}{lower bound of the
#' confidence interval} \item{upr.ci}{upper bound of the confidence interval}
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{meanCI}}, \code{\link{medianCI}}
#' 
#' @keywords univar nonparametric
#' @examples
#' 
#' set.seed(1984)
#' bootCI(d.pizza$temperature, FUN=mean, na.rm=TRUE, bci.method="basic")
#' bootCI(d.pizza$temperature, FUN=mean, trim=0.1, na.rm=TRUE, bci.method="basic")
#' 
#' bootCI(d.pizza$temperature, FUN=skewX, na.rm=TRUE, bci.method="basic")
#' 
#' bootCI(d.pizza$operator, d.pizza$area, FUN=cramerV)
#' 
#' spearman <- function(x,y) cor(x, y, method="spearman", use="p")
#' bootCI(d.pizza$temperature, d.pizza$delivery_min, FUN=spearman)
#' 
#' 
#' 

#' @export
bootCI <- function(x, y=NULL, FUN, ..., bci.method = c("norm", "basic", "stud", "perc", "bca"),
                   conf.level = 0.95, sides = c("two.sided", "left", "right"), R = 999) {
  
  dots <- as.list(substitute(list( ... ))) [-1]
  bci.method <- match.arg(bci.method)
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  if(is.null(y)) {
    if(is.matrix(x) || is.data.frame(x)){
      boot.fun <- boot::boot(x, function(x, d) 
        do.call(FUN, append(list(x[d, , drop=FALSE]), dots)), R = R)
    } else {
      boot.fun <- boot::boot(x, function(x, d) 
        do.call(FUN, append(list(x[d]), dots)), R = R)
    }
  } else
    boot.fun <- boot::boot(x, function(x, d) do.call(FUN, append(list(x[d], y[d]), dots)), R = R)
  
  ci <- boot::boot.ci(boot.fun, conf=conf.level, type=bci.method)[[4]]
  
  res <- c(est = boot.fun$t0, 
           lci = ci[ncol(ci)-1],
           uci = ci[ncol(ci)])
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  names(res)[1] <- deparse(substitute(FUN))
  return(res)
  
}



