

#' Sample Size for a Given Width of a Confidence Interval for a Mean 
#' 
#' Returns the required sample size to obtain a given width of a confidence
#' interval for the sample mean. The function uses \code{\link{uniroot}()} to
#' find a numeric solution. The t distribution is used. 
#' 
#' The required sample sizes for a specific width of confidence interval for
#' the mean depends recursively on the sample size, as the samplesize defines
#' the degrees of freedom in the t-distribution. Although in most practical
#' cases it will be sufficient to use the normal distribution, we might be
#' interested in exact results. 
#' 
#' @param ci the left and right bound of the interval, which is presumed to be
#' symmetric. 
#' @param sd the standard deviation of the sample. 
#' @param interval the interval for the sample size to be searched into,
#' (default is c(2, 100000)). 
#' @param conf.level confidence level, defaults to \code{0.95}.
#' @param norm logical, determining if the t- or normaldistribution should be
#' used. 
#' @param tol the desired accuracy (convergence tolerance). 
#' @return a numeric value 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{binomCIn}()} 
#' @examples
#' 
#' meanCIn(ci=c(25, 27), sd=5) 
#' 

#' @export
meanCIn <- function(ci, sd, interval=c(2, 1e5), conf.level=0.95, norm=FALSE, 
                    tol = .Machine$double.eps^0.5) {
  
  width <- diff(ci)/2
  alpha <- (1-conf.level)/2
  
  if(width > sd){
    warning("Width of confidence intervall > 2*sd, samplesize n=1 is ok for that case.")
    return(1)
    
  } else {
    if(norm)
      uniroot(f = function(n) sd/sqrt(n) * qnorm(p = 1-alpha) - width, 
              interval = interval, tol = tol)$root
    else
      uniroot(f = function(n) (qt(1-alpha, df=n-1) * sd / sqrt(n)) - width, 
              interval = interval, tol = tol)$root
  }
}


