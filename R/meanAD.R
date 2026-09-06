

#' Mean Absolute Deviation From a Center Point 
#' 
#' Calculates the mean absolute deviation from a center point, typically the
#' sample mean or the median. 
#' 
#' 
#' The `meanAD` function calculates the mean absolute deviation from the mean
#' value (or from another supplied center point) of x, after having removed
#' `NA` values (if requested): \deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left
#' | x_{i}-c \right | \; \; \; \textup{where} \; c=mean(x) \; \textup{or} \;
#' c=med(x)}{1/n sum(|x_i - c|) where c=mean(x) or c=med(x)} \cr The function
#' supports the use of weights. The default function for the center value
#' [meanX()] has a weights arguments, too. If a user defined
#' function is used it must be assured that it has a weights argument. 
#' 
#' @param x a vector containing the observations
#' 
#' @param weights a numerical vector of weights the same length as `x`
#' giving the weights to use for elements of `x`
#' 
#' @param center a numeric center or a function applied to `x`.
#' User-defined functions must accept `weights` when weights are supplied.
#' Defaults to [meanX()].
#' 
#' @param na.rm logical; whether to remove missing values. Defaults to
#' `FALSE`.
#' 
#' @return a numeric scalar containing the mean absolute deviation
#' 
#' @note Rewritten following an idea of Danielle Navarro (`aad` in the \pkg{lsr} package).
#' 
#' 
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
#' @seealso [mad()]
#' 
#' @family dispersion
#' @concept dispersion
#' @export
meanAD <- function (x, weights=NULL, center = meanX, na.rm = FALSE) {
  
  # weights have to be filtered ALONGSIDE x. The former na.omit(x) left
  # weights at their original length, so from here on observation i of x
  # was paired with the weight of a different observation - and the
  # center below was computed from that mismatched pair as well.
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
    if (!is.null(weights)) weights <- weights[ok]
  }
  
  # Call the function; do not build a call as a string and parse it.
  # eval(parse(text = "FUN(x, weights=weights)")) does nothing that
  # FUN(x, weights = weights) does not, and it hides the call from every
  # tool that reads the source.
  if (is.function(center)) {
    center <- if (is.null(weights))
      center(x)
    else
      center(x, weights = weights)
  }
  
  if(!is.null(weights)) {
    # na.rm = FALSE: the removal has already happened above, and doing it
    # again here would filter x a second time against weights that are
    # already aligned
    z <- .normWeights(x, weights, na.rm=FALSE)
    res <- sum(abs(z$x - center) * z$weights) / z$wsum
    
  } else {
    # Calculates the mean absolute deviation from the sample mean.
    res <- mean(abs(x - center))
  }
  
  return(res)
  
}  

