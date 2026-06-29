
#' Mean Absolute Error
#'
#' Computes the mean absolute error (MAE) between predicted
#' and reference values.
#'
#' @param x An object. Methods are available for numeric vectors and
#'   model objects (e.g. \code{lm}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric value representing the MAE.
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
#' # with linear model
#' fit <- lm(mpg ~ hp, data = mtcars)
#' mae(fit)
#'
#' @seealso \code{\link{mean}}, \code{\link{abs}}
#'




#' @family model.metrics  
#' @concept model-evaluation  
#' @concept prediction-error
#'
#'
#' @export
mae <- function(x, ...) {
  UseMethod("mae")
}


#' @rdname mae
#' @export
mae.lm <- function(x, ...) {
  mae(
    predict(x, type = "response"),
    model.response(model.frame(x)),
    ...
  )
}


#' @rdname mae
#' @param ref Numeric vector of reference (true) values.
#' @param na.rm Logical; should missing values be removed?
#' @export
mae.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  mean(abs(ref - x), na.rm = na.rm, ...)
}

