
#' Root Mean Squared Error
#'
#' Computes the root mean squared error (RMSE) between predicted
#' and reference values.
#'
#' @param x An object. Methods are available for numeric vectors and
#'   model objects (e.g. \code{lm}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric value representing the RMSE.
#'
#' @details
#' The RMSE is defined as:
#' \deqn{
#' \sqrt{\frac{1}{n} \sum (ref - x)^2}
#' }
#'
#' @family error metrics
#' @concept absolute error
#' @concept regression metrics 
#' 
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#'
#' rmse(x, ref)
#'
#' # with linear model
#' fit <- lm(mpg ~ hp, data = mtcars)
#' rmse(fit)
#'
#' @seealso \code{\link{mean}}, \code{\link{sqrt}}
#'

#' @export
rmse <- function(x, ...) {
  UseMethod("rmse")
}


#' @rdname rmse
#' @export
rmse.lm <- function(x, ...) {
  rmse(
    predict(x, type = "response"),
    model.response(model.frame(x)),
    ...
  )
}


#' @rdname rmse
#' @param ref Numeric vector of reference (true) values.
#' @param na.rm Logical; should missing values be removed?
#' @export
rmse.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  sqrt(mse(x, ref, na.rm = na.rm, ...))
}