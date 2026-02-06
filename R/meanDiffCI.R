
#' Confidence Interval For Difference of Means
#' 
#' Calculates the confidence interval for the difference of two means either
#' the classical way or with the bootstrap approach.
#' 
#' This function collects code from two sources. The classical confidence
#' interval is calculated by means of \code{\link{t.test}}. The bootstrap
#' intervals are strongly based on the example in \code{\link[boot]{boot}}.
#' 
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method a vector of character strings representing the type of
#' intervals required. The value should be any subset of the values
#' \code{"classic"}, \code{"boot"}. Bootstrap type can be provided by the dots.
#' See \code{\link[boot]{boot.ci}}.
#' @param paired a logical indicating whether you want confidence intervals for
#' a paired design. Defaults to \code{FALSE}.
#' @param var.equal a logical variable indicating whether to treat the two
#' variances as being equal. Default is \code{FALSE}. If \code{TRUE} then the
#' pooled variance is used to estimate the variance otherwise the Welch (or
#' Satterthwaite) approximation to the degrees of freedom is used. Passed on to
#' \code{\link{t.test}()}.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}.
#' @param \dots further arguments, can be used to provide further arguments to
#' the boot function.
#' @return a numeric vector with 3 elements: \item{meandiff}{the difference:
#' mean(x) - mean(y)} \item{lci}{lower bound of the confidence interval}
#' \item{uci}{upper bound of the confidence interval}
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{meanCI}}, \code{\link{varCI}}, \code{\link{medianCI}},
#' \code{\link[boot]{boot.ci}}
#' @keywords univar
#' @examples
#' 
#' x <- d.pizza$price[d.pizza$driver=="Carter"]
#' y <- d.pizza$price[d.pizza$driver=="Miller"]
#' 
#' meanDiffCI(x, y, na.rm=TRUE)
#' meanDiffCI(x, y, conf.level=0.99, na.rm=TRUE)
#' 
#' # the different types of bootstrap confints
#' meanDiffCI(x, y, method="boot", type="norm", na.rm=TRUE)
#' meanDiffCI(x, y, method="boot", type="basic", na.rm=TRUE)
#' # meanDiffCI(x, y, method="boot", type="stud", na.rm=TRUE)
#' meanDiffCI(x, y, method="boot", type="perc", na.rm=TRUE)
#' meanDiffCI(x, y, method="boot", type="bca", na.rm=TRUE)
#' 
#' # for long form variables
#' with(d.pizza, with(split(price, driver), 
#'   meanDiffCI(Carter, Miller) )
#' )
#' 


#' @export
meanDiffCI <- function (x, y,
                        conf.level = 0.95, sides = c("two.sided","left","right"), 
                        method = c("classic", "boot"),
                        paired = FALSE, var.equal = FALSE, na.rm = FALSE, ...) {
  
  if (na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  method <- match.arg(method, c("classic", "boot"))
  if(method == "classic"){
    a <- t.test(x, y, conf.level = conf.level, paired = paired, var.equal = var.equal)
    if(paired)
      res <- c(meandiff = mean(x - y), lci = a$conf.int[1], uci = a$conf.int[2])
    else
      res <- c(meandiff = mean(x) - mean(y), lci = a$conf.int[1], uci = a$conf.int[2])
    
  } else {
    
    # boot arguments in dots ...
    btype <- inDots(..., arg="type", default="bca")
    R <- inDots(..., arg="R", default=999)
    parallel <- inDots(..., arg="parallel", default="no")
    ncpus <- inDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
    
    diff.means <- function(d, f){
      n <- nrow(d)
      gp1 <- 1:table(as.numeric(d[,2]))[1]
      m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
      m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
      m1 - m2
    }
    
    m <- cbind(c(x,y), c(rep(1,length(x)), rep(2,length(y))))
    
    if(paired)
      boot.fun <- boot::boot(x-y, function(d, i) mean(d[i]), stype="i", 
                             R=R, parallel=parallel, ncpus=ncpus)
    else
      boot.fun <- boot::boot(m, diff.means, stype="f", strata = m[,2], 
                             R=R, parallel=parallel, ncpus=ncpus)
    
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(meandiff=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(meandiff=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
  }
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return(res)
  
}

