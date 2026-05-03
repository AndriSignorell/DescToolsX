
#' Mean Squared Error
#'
#' Computes the mean squared error (MSE) between predicted
#' and reference values.
#'
#' @param x An object. Methods are available for numeric vectors and
#'   model objects (e.g. \code{lm}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric value representing the MSE.
#'
#' @details
#' The mean squared error is defined as:
#' \deqn{
#' \frac{1}{n} \sum (ref - x)^2
#' }
#'
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#'
#' mse(x, ref)
#'
#' # with linear model
#' fit <- lm(mpg ~ hp, data = mtcars)
#' mse(fit)
#'
#' @seealso \code{\link{mean}}
#'

#' @family error.measures
#' @concept prediction-accuracy
#' @concept descriptive-statistics
#' @concept regression
#'
#'
#' @export
mse <- function(x, ...) {
  UseMethod("mse")
}


#' @rdname mse
#' @export
mse.lm <- function(x, ...) {
  mse(
    predict(x, type = "response"),
    model.response(model.frame(x)),
    ...
  )
}


#' @rdname mse
#' @param ref Numeric vector of reference (true) values.
#' @param na.rm Logical; should missing values be removed?
#' @export
mse.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  mean((ref - x)^2, na.rm = na.rm, ...)
}
