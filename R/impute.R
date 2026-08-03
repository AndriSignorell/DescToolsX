
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
#' @param x atomic vector
#' @param FUN function used to compute the imputation value, or a scalar value;
#' default is \code{median}
#' @param na.rm logical; whether to remove missing values before computing the
#' summary statistic
#' @param ... additional arguments passed to \code{FUN}
#'
#' @return vector of the same length as \code{x}, with missing values
#' replaced. The type follows R's usual coercion rules: imputing a double
#' (the median of an integer vector, say) into an integer vector returns a
#' double.
#'
#' @details
#' If \code{FUN} is a function, it must return a single value.
#' An error is thrown if the returned value is not scalar.
#'
#' Whether \code{FUN} accepts \code{na.rm} is decided from its formal
#' arguments; a function taking \code{\dots} is assumed to pass it on.
#' Functions without either are called without it.
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
#' # an integer vector stays integer only if the value is one too
#' impute(c(2L, 3L, NA, 5L), FUN = 4L)
#'
#' @family impute
#' @concept imputation
#' @concept missing-value
#' @export
impute <- function(x, FUN = median, na.rm = TRUE, ...) {
  
  if (is.function(FUN)) {

    # Ask whether FUN takes na.rm instead of calling it and catching the
    # failure. tryCatch(error = ) swallowed EVERY error, so a function
    # that failed for an unrelated reason was quietly re-run without
    # na.rm - either producing a different (wrong) value or a second,
    # more confusing error from the retry. A function with ... is assumed
    # to forward it, which is what median/mean and their kin do.
    fmls <- names(formals(args(FUN)))
    takesNaRm <- !is.null(fmls) && ("na.rm" %in% fmls || "..." %in% fmls)

    value <- if (takesNaRm) FUN(x, na.rm = na.rm, ...) else FUN(x, ...)

  } else if (length(FUN) == 1) {
    
    value <- FUN
    
  } else {
    stop("FUN must be a function or a scalar value.")
  }
  
  if (length(value) != 1)
    stop("Imputation value must be scalar.")
  
  replace(x, is.na(x), value)
}
