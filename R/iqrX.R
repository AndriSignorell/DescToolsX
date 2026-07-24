
#' The (weighted) Interquartile Range 
#' 
#' computes interquartile range of the x values. Weights are supported. 
#' 
#' This implementation is based on \code{\link{quantileX}()} function, which
#' allows to define weights. 
#' 
#' @param x numeric vector
#' @param weights optional numeric vector giving the sample weights
#' @param na.rm logical; whether to remove missing values
#' @param type an integer selecting one of the many quantile algorithms, see
#' \code{\link{quantileX}()}
#'
#' @return numeric scalar containing the interquartile range
#' 
#' 
#' @examples
#' 
#' x <- c(3.7,3.3,3.5,2.8)
#' w <- c(5, 5, 4, 1)/15
#' 
#' iqrX(x=x, weights=w)
#' 
#' @seealso [medianX], [quantileX], [IQR], [quantile] 
#' 
#' @family dispersion  
#' @concept dispersion
#'
#'
#' @export
iqrX <- function (x, weights = NULL, na.rm = FALSE, type = 7) {
  
  if(is.null(weights))
    IQR(x=x, na.rm=na.rm, type=type)
  
  else 
    diff(quantileX(x, weights=weights, probs=c(0.25, 0.75), na.rm=na.rm, type=type))
  
}
