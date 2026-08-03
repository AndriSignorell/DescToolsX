
#' Standard Error of Mean
#' 
#' Calculates the standard error of mean. 
#' 
#' \code{meanSE()} calculates the standard error of the mean defined as:
#' \deqn{\frac{\sigma}{\sqrt{n}}} \eqn{\sigma} being standard deviation of
#' \code{x} and n the length of \code{x}.
#' 
#' @param x a non-empty numeric vector of data values
#' @param sd the standard deviation of \code{x}. If supplied, it is interpreted
#' as the population standard deviation. If \code{NULL} (default), the sample
#' standard deviation \code{sd(x)} is used.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}. 
#' 
#' @return the standard error as a numeric scalar
#' 
#' @examples
#' 
#' meanSE(Pizza$price, na.rm=TRUE)
#' 
#' # evaluate data.frame
#' sapply(Pizza[, 1:4], meanSE, na.rm=TRUE)
#' 
#' 
#' @seealso [lumen::meanCI]
#' 
#' @family dispersion
#' @concept dispersion
#' @export
meanSE <- function(x, sd = NULL, na.rm = FALSE) {
  # standard error of mean
  if(na.rm) x <- na.omit(x)

  # stats::sd(), spelled out: the argument of the same name shadows the
  # function for every reader, and only R's habit of skipping non-function
  # bindings in call position keeps `sd(x)` working at all
  s <- if(is.null(sd)) stats::sd(x) else sd

  s/sqrt(length(x))
}

