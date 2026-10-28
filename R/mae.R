
#' Mean Absolute Error
#'
#' Computes the mean absolute error (MAE) between predicted
#' and reference values.
#'
#' @param x a numeric vector of predicted values.
#'   Methods for fitted models are provided by the package \pkg{alloy}.
#' @param ... additional arguments passed to methods
#'
#' @return a numeric scalar containing the MAE
#'
#' @details
#' The mean absolute error is defined as:
#' \deqn{
#' \frac{1}{n} \sum |ref - x|
#' }
#'
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#'
#' mae(x, ref)
#'
#' @seealso [mean()], [abs()]
#'
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept prediction-error
#' @export
mae <- function(x, ...) {
  UseMethod("mae")
}


#' @rdname mae
#' @param ref numeric vector of reference (true) values
#' @param na.rm logical; whether to remove missing values
#' @export
mae.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  mean(abs(ref - x), na.rm = na.rm, ...)
}
