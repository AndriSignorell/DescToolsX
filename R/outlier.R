
#' Outlier
#' 
#' Return outliers following Tukey's boxplot and Hampel's median/mad
#' definition.
#' 
#' Outlier detection is a tricky problem and should be handled with care. We
#' implement Tukey's boxplot rule as a rough idea of spotting extreme values.
#' 
#' Hampel considers values outside of median +/- 3 * (median absolute
#' deviation) to be outliers.
#' 
#' @param x a (non-empty) numeric vector of data values.
#' @param method the method to be used. So far Tukey's boxplot and Hampel's
#' rule are implemented.
#' @param value logical. If \code{FALSE}, a vector containing the (integer)
#' indices of the outliers is returned, and if \code{TRUE} (default), a vector
#' containing the matching elements themselves is returned.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}.
#' @return the values of x lying outside the whiskers in a boxplot \cr or the
#' indices of them
#' @author Andri Signorell <andri@@signorell.net>, performance improvement by
#' Luis Gustavo Schuck
#' @seealso \code{\link{boxplot}}
#' @references Hampel F. R. (1974) The influence curve and its role in robust
#' estimation, \emph{Journal of the American Statistical Association}, 69,
#' 382-393
#' @keywords univar
#' @examples
#' 
#' outlier(d.pizza$temperature, na.rm=TRUE)
#' 
#' # it's the same as the result from boxplot
#' sort(d.pizza$temperature[outlier(d.pizza$temperature, value=FALSE, na.rm=TRUE)])
#' b <- boxplot(d.pizza$temperature, plot=FALSE)
#' sort(b$out)
#' 
#' # nice to find the corresponding rows
#' d.pizza[outlier(d.pizza$temperature, value=FALSE, na.rm=TRUE), ]
#' 
#' # compare to Hampel's rule
#' outlier(d.pizza$temperature, method="hampel", na.rm=TRUE)
#' 
#' 
#' # outliers for the each driver
#' tapply(d.pizza$temperature, d.pizza$driver, outlier, na.rm=TRUE)
#' 
#' # the same as:
#' boxplot(temperature ~ driver, d.pizza)$out
#' 
#' 

#' @export
outlier <- function(x, method=c("boxplot", "hampel"), value=TRUE, na.rm=FALSE){
  
  switch(match.arg(arg = method, choices = c("boxplot", "hampel")),
         
         boxplot =  {
           qq <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm, names = FALSE)
           iqr <- diff(qq)
           id <- x < (qq[1] - 1.5 * iqr) | x > (qq[2] + 1.5 * iqr)
         },
         
         hampel = {
           med_x <- median(x, na.rm=na.rm)
           
           # hampel considers values outside of median ± 3*(median absolute deviation) 
           # to be outliers
           id <- x %][% (med_x + c(-3, 3) * mad(x, na.rm=na.rm, center = med_x))
         }
  )
  
  if(value)
    res <- x[id]
  else
    res <- which(id)
  
  res <- res[!is.na(res)]
  
  return(res)
  
}




