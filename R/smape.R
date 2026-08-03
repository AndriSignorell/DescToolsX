
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
#' Values are bounded between 0 and 2. Note that this is a ratio, not a
#' percentage: the factor 100 of the original definition is not applied, which
#' is the form for which the bound of 2 holds.
#'
#' A term is undefined when \code{x} and \code{ref} are both zero, and is set
#' to \code{NA}. With the default \code{na.rm = FALSE} a single such pair
#' therefore makes the whole result \code{NA}; with \code{na.rm = TRUE} those
#' terms are dropped along with genuinely missing ones, so the mean is taken
#' over fewer than \code{length(x)} terms.
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
  
  ref <- model.response(model.frame(x))
  
  # glm objects inherit from lm, so this method is also reached for, say, a
  # logistic fit, where the response is a factor or a two-column matrix and
  # the arithmetic below would fail with an unrelated message.
  if(!is.numeric(ref) || !is.null(dim(ref)))
    stop("the model response must be a numeric vector to compute smape()")
  
  smape(
    predict(x, type = "response"),
    ref,
    ...
  )
}


#' @rdname smape
#' @param ref numeric vector of reference (true) values
#' @param na.rm logical; whether to remove missing and undefined terms
#' @export
smape.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(!is.numeric(x) || !is.numeric(ref))
    stop("'x' and 'ref' must be numeric")
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be a single non-missing logical value")
  
  denom <- abs(x) + abs(ref)
  num <- 2 * abs(ref - x)
  
  # avoid division by zero
  res <- num / denom
  
  # denom is zero only when both values are, in which case num is zero too and
  # the term is 0/0. A logical index containing NA (from a missing x or ref) is
  # allowed here because the replacement has length 1; those positions are
  # skipped and are already NA.
  res[!is.na(denom) & denom == 0] <- NA_real_
  
  # '...' is deliberately not forwarded to mean(): it would accept trim= and,
  # worse, swallow a mistyped argument name without complaint.
  mean(res, na.rm = na.rm)
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
