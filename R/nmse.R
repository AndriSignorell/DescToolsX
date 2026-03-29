
#' Normalized Mean Squared Error
#'
#' Computes the normalized mean squared error (NMSE) between predictions
#' and reference values.
#'
#' @param x Numeric vector of predicted values.
#' @param ref Numeric vector of reference (true) values.
#' @param train.y Numeric vector used as baseline to compute normalization.
#'
#' @return A numeric value representing the normalized mean squared error.
#'
#' @details
#' The normalized mean squared error is defined as:
#' \deqn{
#' \frac{\sum (ref - x)^2}{\sum (ref - mean(train.y))^2}
#' }
#'
#' The denominator represents the squared deviation from the mean of the
#' training response, providing a baseline for comparison.
#'
#' If the denominator is zero, \code{NA} is returned.
#'
#' @family error metrics
#' @concept normalized error
#' @concept regression metrics 
#' 
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#' train.y <- c(2, 3, 4, 3)
#'
#' nmse(x, ref, train.y)
#'
#' @seealso \code{\link{mean}}, \code{\link{sum}}
#'

#' @export
nmse <- function(x, ref, train.y){
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  sse <- sum((ref - x)^2)
  
  den <- sum((ref - mean(train.y))^2)
  if(den == 0)
    return(NA_real_)
  
  sse / den
}