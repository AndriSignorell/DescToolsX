
#' Mean Absolute Percentage Error
#'
#' Computes the mean absolute percentage error (MAPE) between predicted
#' and reference values.
#'
#' @param x An object. Methods are available for numeric vectors and
#'   model objects (e.g. \code{lm}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric value representing the MAPE.
#'
#' @details
#' The MAPE is defined as:
#' \deqn{
#' \frac{1}{n} \sum \left| \frac{ref - x}{ref} \right|
#' }
#'
#' Note that values where \code{ref = 0} lead to division by zero and
#' result in \code{NA}.
#'
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#'
#' mape(x, ref)
#'
#' # with linear model
#' fit <- lm(mpg ~ hp, data = mtcars)
#' mape(fit)
#'



#' @family model.metrics  
#' @concept model-evaluation  
#' @concept prediction-error
#'
#'
#' @export
mape <- function(x, ...) {
  UseMethod("mape")
}


#' @rdname mape
#' @export
mape.lm <- function(x, ...) {
  mape(
    predict(x, type = "response"),
    model.response(model.frame(x)),
    ...
  )
}


#' @rdname mape
#' @param ref Numeric vector of reference (true) values.
#' @param na.rm Logical; should missing values be removed?
#' @export
mape.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  res <- abs((ref - x) / ref)
  
  # handle division by zero
  res[ref == 0] <- NA_real_
  
  mean(res, na.rm = na.rm, ...)
}


