
#' Normalized Mean Absolute Error
#'
#' Computes the normalized mean absolute error (NMAE) between predictions
#' and reference values.
#'
#' @param x Numeric vector of predicted values.
#' @param ref Numeric vector of reference (true) values.
#' @param trainY Numeric vector used as baseline to compute normalization.
#'
#' @return A numeric value representing the normalized mean absolute error.
#'
#' @details
#' The normalized mean absolute error is defined as:
#' \deqn{
#' \frac{\sum |ref - x|}{\sum |ref - mean(trainY)|}
#' }
#'
#' The denominator represents the absolute deviation from the mean of the
#' training response, providing a baseline for comparison.
#'
#' If the denominator is zero, \code{NA} is returned.
#'
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#' trainY <- c(2, 3, 4, 3)
#'
#' nmae(x, ref, trainY)
#'
#' @seealso \code{\link{mean}}, \code{\link{abs}}
#'


#' @family error.measures
#' @concept prediction-accuracy
#' @concept descriptive-statistics
#'
#'
#' @export
nmae <- function(x, ref, trainY){
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  sae <- sum(abs(ref - x))
  
  den <- sum(abs(ref - mean(trainY)))
  if(den == 0)
    return(NA_real_)
  
  sae / den
}
