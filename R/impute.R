
#' Impute Missing Values in a Vector
#'
#' Replaces missing values in a vector using a specified summary function.
#'
#' @param x A vector in which missing values (\code{NA}) should be imputed.
#' @param FUN A function or character string specifying the summary
#'   function used to compute the replacement value. The function must
#'   accept \code{x} as its first argument. By default, the median
#'   (with \code{na.rm = TRUE}) is used.
#'
#' @details
#' If \code{FUN} is a function, it is applied to \code{x} to compute
#' a single replacement value. All missing values in \code{x} are then
#' replaced with this value.
#'
#' If \code{FUN} is a character string, it must represent a valid
#' R expression involving \code{x}, e.g. \code{"mean(x, na.rm = TRUE)"}.
#'
#' @return A vector of the same type and length as \code{x},
#'   with missing values replaced.
#'   
#' @family data_processing
#' @concept data processing
#' @concept imputation
#'
#' @examples
#' x <- c(1, 2, NA, 4)
#'
#' # Default (median)
#' impute(x)
#'
#' # Using mean
#' impute(x, function(x) mean(x, na.rm = TRUE))
#'
#' # Using a character expression
#' impute(x, "mean(x, na.rm = TRUE)")
#' 



#' @export
impute <- function(x, FUN = function(x) median(x, na.rm=TRUE)) {
  
  if(is.function(FUN)) {
    fct <- FUN
    FUN <- "fct"
    FUN <- gettextf("%s(x)", FUN)
  }
  
  return(eval(parse(text = gettextf("replace(x, is.na(x), %s)", FUN))))
}
