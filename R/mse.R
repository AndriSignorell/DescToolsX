
#' Mean Squared Error
#'
#' Computes the mean squared error (MSE) between predicted
#' and reference values.
#'
#' @param x an object. Methods are available for numeric vectors and
#'   model objects (e.g. \code{lm}).
#' @param ... additional arguments passed to methods
#'
#' @return a numeric scalar containing the MSE
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
#' @family model.metrics
#' @concept model-evaluation
#' @concept prediction-error
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
#' @param ref numeric vector of reference (true) values
#' @param na.rm logical; whether to remove missing values
#' @export
mse.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  # no `...` into mean(): its only other argument is `trim`, and a
  # trimmed mean of squared errors is not an MSE. The dots stay in the
  # signature for S3 consistency and are ignored here, as in mae().
  mean((ref - x)^2, na.rm = na.rm)
}
