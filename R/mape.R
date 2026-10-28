
#' Mean Absolute Percentage Error
#'
#' Computes the mean absolute percentage error (MAPE) between predicted
#' and reference values.
#'
#' @param x a numeric vector of predicted values.
#'   Methods for fitted models are provided by the package \pkg{alloy}.
#' @param ... additional arguments passed to methods
#'
#' @return a numeric scalar containing the MAPE, as a **fraction**,
#' not a percentage: a mean absolute relative error of six percent is
#' returned as `0.06`. Multiply by 100 for the percentage form. The
#' name is conventional; the definition below is the one implemented.
#'
#' @details
#' The MAPE is defined as:
#' \deqn{
#' \frac{1}{n} \sum \left| \frac{ref - x}{ref} \right|
#' }
#'
#' Note that values where `ref = 0` lead to division by zero and
#' result in `NA`.
#'
#' @examples
#' x <- c(2.5, 3.0, 2.8)
#' ref <- c(3.0, 2.5, 3.0)
#'
#' mape(x, ref)
#'
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept prediction-error
#' @export
mape <- function(x, ...) {
  UseMethod("mape")
}


#' @rdname mape
#' @param ref numeric vector of reference (true) values
#' @param na.rm logical; whether to remove missing values
#' @export
mape.default <- function(x, ref, na.rm = FALSE, ...) {
  
  if(length(x) != length(ref))
    stop("'x' and 'ref' must have same length")
  
  res <- abs((ref - x) / ref)
  
  # handle division by zero. !is.na() guards the index: a missing ref
  # makes `ref == 0` NA, and a logical index containing NA is only
  # tolerated because the replacement has length 1.
  res[!is.na(ref) & ref == 0] <- NA_real_
  
  mean(res, na.rm = na.rm, ...)
}

