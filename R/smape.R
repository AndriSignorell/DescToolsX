
#' Symmetric Mean Absolute Percentage Error
#'
#' Computes the symmetric mean absolute percentage error (SMAPE) between
#' predicted and reference values.
#'
#' @param x an object. Methods are available for numeric vectors and
#'   model objects (e.g. \code{lm}).
#' @param ... additional arguments passed to methods
#'
#' @return a numeric scalar containing the SMAPE
#'
#' @details
#' The SMAPE is defined as:
#' \deqn{
#' \frac{1}{n} \sum \frac{2 |ref - x|}{|x| + |ref|}
#' }
#'
#' Values are bounded between 0 and 2. Division by zero is handled by
#' returning \code{NA} for those terms.
#'
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#'
#' smape(x, ref)
#'
#' # with linear model
#' fit <- lm(mpg ~ hp, data = mtcars)
#' smape(fit)
#'
#'
#' @family model.metrics  
#' @concept model-evaluation  
#' @concept prediction-error
#'
#'
#' @export
smape <- function(x, ...) {
  UseMethod("smape")
}


#' @rdname smape
#' @export
smape.lm <- function(x, ...) {
  smape(
    predict(x, type = "response"),
    model.response(model.frame(x)),
    ...
  )
}


#' @rdname smape
#' @param ref numeric vector of reference (true) values
#' @param na.rm logical; whether to remove missing values
#' @export
smape.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  denom <- abs(x) + abs(ref)
  num <- 2 * abs(ref - x)
  
  # avoid division by zero
  res <- num / denom
  res[denom == 0] <- NA_real_
  
  mean(res, na.rm = na.rm, ...)
}


# Chen and Yang (2004), in an unpublished working paper, defined the sMAPE as
# \[\text{sMAPE} = \text{mean}(2|y_t - \hat{y}_t|/(|y_t| + |\hat{y}_t|)).\]
# They still called it a measure of "percentage error" even though they dropped the multiplier 100.
# At least they got the range correct, stating that this measure has a maximum value of two when
# either y_t or \hat{y}_t is zero, but is undefined when both are zero.
# The range of this version of sMAPE is (0,2). Perhaps this is the definition that Makridakis and
# Armstrong intended all along, although neither has ever managed to include it correctly
# in one of their papers or books.
# source: http://robjhyndman.com/hyndsight/smape/

