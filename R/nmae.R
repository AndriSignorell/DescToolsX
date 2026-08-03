
#' Normalized Mean Absolute Error
#'
#' Computes the normalized mean absolute error (NMAE) between predictions
#' and reference values.
#'
#' @param x numeric vector of predicted values
#' @param ref numeric vector of reference (true) values
#' @param trainY numeric vector used as the normalization baseline
#' @param na.rm logical; whether to remove incomplete cases before the
#'   computation. Defaults to \code{FALSE}, in which case a missing value
#'   anywhere makes the result \code{NA}.
#'
#' @return a numeric scalar containing the normalized mean absolute error
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
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept prediction-error
#' @export
nmae <- function(x, ref, trainY, na.rm = FALSE){
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  # na.rm was missing here although mae(), mape() and mse() - the same
  # @family - all offer it. Without it a single missing value turned the
  # whole result into NA with nothing to say about it.
  if(na.rm){
    ok <- !is.na(x) & !is.na(ref)
    x <- x[ok]
    ref <- ref[ok]
    trainY <- trainY[!is.na(trainY)]
  }
  
  if(length(x) == 0L || length(trainY) == 0L)
    return(NA_real_)
  
  sae <- sum(abs(ref - x))
  
  den <- sum(abs(ref - mean(trainY)))
  if(den == 0)
    return(NA_real_)
  
  sae / den
}
