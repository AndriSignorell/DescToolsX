

#' Mean Absolute Deviation From a Center Point 
#' 
#' Calculates the mean absolute deviation from a center point, typically the
#' sample mean or the median. 
#' 
#' 
#' The \code{meanAD} function calculates the mean absolute deviation from the mean
#' value (or from another supplied center point) of x, after having removed
#' \code{NA} values (if requested): \deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left
#' | x_{i}-c \right | \; \; \; \textup{where} \; c=mean(x) \; \textup{or} \;
#' c=med(x)}{1/n sum(|x_i - c|) where c=mean(x) or c=med(x)} \cr The function
#' supports the use of weights. The default function for the center value
#' \code{\link{meanX}()} has a weights arguments, too. If a user defined
#' function is used it must be assured that it has a weights argument. 
#' 
#' @param x a vector containing the observations.
#' 
#' @param weights a numerical vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}.
#' 
#' @param center a single numerical value or the name of a function applied to \code{x} to be used
#' as center. Can as well be a self defined function.  Default is
#' \code{\link{meanX}()}.
#' 
#' @param na.rm a logical value indicating whether or not missing values should
#' be removed. Defaults to \code{FALSE}.
#' 
#' @return Numeric value.
#' @author Andri Signorell <andri@@signorell.net> following an idea of Danielle
#' Navarro (\code{aad} in the \pkg{lsr} package)
#' 
#' @seealso \code{\link{mad}}
#' 
#' @keywords univar
#' @examples
#' 
#' x <- runif(100)
#' meanAD(x)
#' 
#' speed <- c(58, 88, 40, 60, 72, 66, 80, 48, NA)
#' meanAD(speed)
#' meanAD(speed, na.rm=TRUE)
#' 
#' 
#' # using the median as centerpoint
#' x <- c(2,3,5,3,1,15,23)
#' 
#' meanAD(x, center=mean)
#' meanAD(x, center=median)
#' 
#' # define a fixed center
#' meanAD(x, center=4)
#' 
#' # use of weights
#' meanAD(x=0:6, weights=c(21,46,54,40,24,10,5))
#' 



#' @export
meanAD <- function (x, weights=NULL, center = meanX, na.rm = FALSE) {
  
  if (na.rm) 
    x <- na.omit(x)
  
  if (is.function(center)) {
    FUN <- center
    center <- "FUN"
    if(is.null(weights))
      center <- gettextf("%s(x)", center)
    else
      center <- gettextf("%s(x, weights=weights)", center)
    center <- eval(parse(text = center))
  }
  
  if(!is.null(weights)) {
    z <- .NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
    res <- sum(abs(z$x - center) * z$weights) / z$wsum
    
  } else {
    # Calculates the mean absolute deviation from the sample mean.
    res <- mean(abs(x - center))
  }
  
  return(res)
  
}  


