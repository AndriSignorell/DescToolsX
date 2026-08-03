
#' Root Mean Squared Error
#'
#' Computes the root mean squared error (RMSE) between predicted
#' and reference values.
#'
#' @param x an object. Methods are available for numeric vectors and
#'   model objects (e.g. \code{lm}).
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
#' # with linear model
#' fit <- lm(mpg ~ hp, data = mtcars)
#' rmse(fit)
#'
#' @seealso \code{\link{mse}}, \code{\link{mae}}
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
#' @export
rmse.lm <- function(x, ...) {
  
  ref <- model.response(model.frame(x))
  
  # glm objects inherit from lm, so this method is also reached for, say, a
  # logistic fit, where the response is a factor or a two-column matrix.
  if(!is.numeric(ref) || !is.null(dim(ref)))
    stop("the model response must be a numeric vector to compute rmse()")
  
  rmse(
    predict(x, type = "response"),
    ref,
    ...
  )
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
