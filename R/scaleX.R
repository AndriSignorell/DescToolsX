

#' Robust Scaling With Median and Mad 
#' 
#' \code{scaleX} is a wrapper function for robust standardization, using
#' \code{\link{median}} and \code{\link{mad}} instead of \code{\link{mean}} and
#' \code{\link{sd}}. 
#' 
#' @param x a numeric matrix(like object).
#' 
#' @param center a logical value defining whether x should be centered by the
#' median.  Centering is done by subtracting the column medians (omitting \code{NA}s)
#' of x from their corresponding columns.  If center is \code{FALSE},
#' no centering is done. 
#' @param scale a logical value defining whether x should be scaled by the mad.
#' Scaling is done by dividing the (centered) columns of x by their mad.  If
#' scale is \code{FALSE}, no scaling is done.
#' 
#' @return the centered, scaled matrix.  The numeric centering and scalings
#' used (if any) are returned as attributes "scaled:center" and "scaled:scale"
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{scale}, \code{sweep}
#' @keywords univar robust
#' @examples
#' 
#' x <- d.pizza$temperature
#' plot(x=seq_along(x), y=scaleX(x), xlim=c(0,100))
#' points(x=seq_along(x), y=scale(x), col="red" )
#' 


#' @export
scaleX <- function(x, center = TRUE, scale = TRUE){
  
  x <- as.matrix(x)
  
  if(center) {
    x <- scale(x, center = apply(x, 2, median, na.rm=TRUE), 
               scale = FALSE)
  }
  if(scale) {
    x <- scale(x, center = FALSE, 
               scale = apply(x, 2, mad, na.rm=TRUE))
  }
  return(x)
}



