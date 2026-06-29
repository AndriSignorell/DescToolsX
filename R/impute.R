
#' Impute Missing Values in a Vector
#'
#' Replaces missing values (`NA`) in a vector by either
#' a summary statistic computed from the data (e.g. mean, median)
#' or a user-supplied scalar value.
#'
#' If `FUN` is a function, it is applied to `x` to compute
#' a single imputation value. If the function accepts an argument
#' named `na.rm`, it will be passed automatically.
#'
#' Alternatively, `FUN` can be a single scalar value, which will
#' directly replace all missing values.
#'
#' @param x An atomic vector.
#' @param FUN A function used to compute the imputation value,
#'   or a single scalar value. Default is \code{median}.
#' @param na.rm Logical. Should missing values be removed before
#'   computing the summary statistic? Default is \code{TRUE}.
#' @param ... Additional arguments passed to \code{FUN}.
#'
#' @return A vector of the same length and type as \code{x},
#'   with missing values replaced.
#'
#' @details
#' If \code{FUN} is a function, it must return a single value.
#' An error is thrown if the returned value is not scalar.
#'
#' If \code{FUN} does not accept an argument named \code{na.rm},
#' the function is called again without it.
#'
#' @seealso For direct value replacement see [bedrock::naReplace()].
#' 
#' @examples
#' x <- c(2, 3, NA, 5, 9)
#'
#' # Default: median(x, na.rm=TRUE)
#' impute(x)
#'
#' # Using mean(x, na.rm=TRUE)
#' impute(x, mean)
#'
#' # Using trimmed mean
#' impute(x, function(x) meanX(x, trim=0.3, na.rm = TRUE))
#'
#' # Constant replacement
#' impute(x, 99)
#' 



#' @family impute  
#' @concept imputation  
#' @concept missing-value
#'
#'
#' @export
impute <- function(x, FUN = median, na.rm = TRUE, ...) {
  
  if (is.function(FUN)) {
    
    value <- tryCatch(
      FUN(x, na.rm = na.rm, ...),
      error = function(e) FUN(x, ...)
    )
    
  } else if (length(FUN) == 1) {
    
    value <- FUN
    
  } else {
    stop("FUN must be a function or a scalar value.")
  }
  
  if (length(value) != 1)
    stop("Imputation value must be scalar.")
  
  replace(x, is.na(x), value)
}
