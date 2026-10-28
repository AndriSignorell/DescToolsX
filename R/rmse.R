
#' Root Mean Squared Error
#'
#' Computes the root mean squared error (RMSE) between predicted
#' and reference values.
#'
#' @param x a numeric vector of predicted values.
#'   Methods for fitted models are provided by the package \pkg{alloy}.
#' @param ... additional arguments passed to methods
#'
#' @return a numeric scalar containing the RMSE
#'
#' @details
#' The RMSE is defined as:
#' \deqn{
#' \sqrt{\frac{1}{n} \sum (ref - x)^2}
#' }
#'
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#'
#' rmse(x, ref)
#'
#' @seealso [mse()], [mae()]
#'
#'
#' @family model.metrics  
#' @concept prediction-error
#'
#'
#' @export
rmse <- function(x, ...) {
  UseMethod("rmse")
}


#' @rdname rmse
#' @param ref numeric vector of reference (true) values
#' @param na.rm logical; whether to remove missing values
#' @export
rmse.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  sqrt(mse(x, ref, na.rm = na.rm, ...))
}
