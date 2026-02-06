
#' Confidence Intervals for the Variance
#' 
#' Calculates confidence intervals for the variance. Available approachs are
#' the classical one using the ChiSquare distribution, a more robust version
#' proposed by Bonett and the bootstrap options available in the package
#' \code{boot}.
#' 
#' The confidence interval for the variance is very sensitive to non-normality
#' in the data. Bonett (2006) has proposed an interval that is nearly exact
#' when the data is normally distributed and provides good performance for
#' moderately non-normal data. See the references for the details.
#' 
#' @param x a (non-empty) numeric vector of data values.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}.  You can specify just the initial letter. \code{"left"}
#' would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method vector of character strings representing the type of intervals
#' required.  The value should be any subset of the values \code{"classic"},
#' \code{"bonett"}, \code{"norm"}, \code{"boot"}. Bootstrap type can be set by
#' the ... arguments.  See \code{\link[boot]{boot.ci}}.
#' @param na.rm logical. Should missing values be removed? Defaults to FALSE.
#' @param \dots further arguments, can be used to provide further arguments to
#' the boot function.
#' @return a numeric vector with 3 elements: \item{est}{variance}
#' \item{lci}{lower bound of the confidence interval} \item{uci}{upper bound of
#' the confidence interval}
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{meanCI}}, \code{\link{medianCI}},
#' \code{\link[DescTools]{VarTest}}, \code{\link[DescTools]{Var}} 
#' 
#' @references Bonett (2006) Approximate Confidence Interval for Standard
#' Deviation of Nonnormal Distributions, \emph{Computational Statistics and
#' Data Analysis}, Vol. 50, pp. 775 - 782.\cr
#' https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/sdconfli.htm
#' (might be outdated)
#' @keywords univar
#' @examples
#' 
#' varCI(d.pizza$price, na.rm=TRUE)
#' varCI(d.pizza$price, conf.level=0.99, na.rm=TRUE)
#' 
#' x <- c(14.816, 14.863, 14.814, 14.998, 14.965, 14.824, 14.884, 14.838, 14.916,
#'        15.021, 14.874, 14.856, 14.860, 14.772, 14.980, 14.919)
#' varCI(x, conf.level=0.9)
#' 
#' # and for the standard deviation
#' sqrt(varCI(x, conf.level=0.9))
#' 
#' 
#' # from Bonett's paper
#' # expected results:
#' # ------------------------------------
#' #  conf.lvl       sd      lci      uci
#' # ------------------------------------
#' #      90.0   0.5168   0.3592   0.9359
#' #      95.0   0.5168   0.3263   1.0841
#' #      99.0   0.5168   0.2607   1.5109
#' 
#' p <- c(15.83, 16.01, 16.24, 16.42, 15.33, 15.44, 16.88, 16.31)
#' sqrt(varCI(p, method="bonett", conf.level=0.9))
#' sqrt(varCI(p, method="bonett"))
#' sqrt(varCI(p, method="bonett", conf.level=0.99))
#' 
#' # some bootstrap intervals
#' varCI(x, method="boot", type="norm")
#' varCI(x, method="boot", type="perc")
#' varCI(x, method="boot", type="bca")
#' 


#' @export
varCI <- function (x, 
                   conf.level = 0.95, sides = c("two.sided","left","right"), 
                   method = c("classic", "bonett", "boot"), 
                   na.rm = FALSE, ...) {
  
  if (na.rm) x <- na.omit(x)
  method <- match.arg(method, c("classic","bonett", "boot"))
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  if(method == "classic"){
    df <- length(x) - 1
    v <- var(x)
    res <- c (var = v, 
              lci = df * v/qchisq((1 - conf.level)/2, 
                                                  df, lower.tail = FALSE), 
              uci = df * v/qchisq((1 - conf.level)/2, df) )
    
  } else if(method=="bonett") {
    
    z <- qnorm(1-(1-conf.level)/2)
    n <- length(x)
    cc <- n/(n-z)
    v <- var(x)
    mtr <- mean(x, trim = 1/(2*(n-4)^0.5))
    m <- mean(x)
    gam4 <- n * sum((x-mtr)^4) / (sum((x-m)^2))^2
    se <- cc * sqrt((gam4 - (n-3)/n)/(n-1))
    lci <- exp(log(cc * v) - z*se)
    uci <- exp(log(cc * v) + z*se)
    
    res <- c(var=v, lci=lci, uci=uci)
    
  } else {
    
    # boot arguments in dots ...
    btype <- inDots(..., arg="type", default="bca")
    R <- inDots(..., arg="R", default=999)
    parallel <- inDots(..., arg="parallel", default="no")
    ncpus <- inDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
    
    boot.fun <- boot::boot(x, function(x, d) var(x[d], na.rm=na.rm), 
                           R=R, parallel=parallel, ncpus=ncpus)
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(var=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(var=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
  }
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- 0
  
  return(res)
  
}

