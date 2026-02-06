
#' The (weighted) Interquartile Range 
#' 
#' computes interquartile range of the x values. Weights are supported. 
#' 
#' This implementation is based on \code{\link{quantileX}()} function, which
#' allows to define weights. 
#' 
#' @param x a numeric vector. 
#' @param weights an optional numeric vector giving the sample weights. 
#' @param na.rm logical. Should missing values be removed? 
#' @param type an integer selecting one of the many quantile algorithms, see
#' \code{\link{quantileX}()}.
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{medianX}()}, \code{\link{quantileX}()},
#' \code{\link{IQR}()}, \code{\link{quantile}()} 
#' @keywords IO robust distribution univar
#' @examples
#' 
#' x <- c(3.7,3.3,3.5,2.8)
#' w <- c(5, 5, 4, 1)/15
#' 
#' iqrX(x=x, weights=w)
#' 


#' @export
iqrX <- function (x, weights = NULL, na.rm = FALSE, type = 7) {
  
  if(is.null(weights))
    IQR(x=x, na.rm=na.rm, type=type)
  
  else 
    diff(quantileX(x, weights=weights, probs=c(0.25, 0.75), na.rm=na.rm, type=type))
  
}

