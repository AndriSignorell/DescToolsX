
#' Standard Error of Mean
#' 
#' Calculates the standard error of mean. 
#' 
#' MeanSE calculates the standard error of the mean defined as:
#' \deqn{\frac{\sigma}{\sqrt{n}}} \eqn{\sigma} being standard deviation of
#' \code{x} and n the length of \code{x}.
#' 
#' @param x a (non-empty) numeric vector of data values. 
#' @param sd the standard deviation of \code{x}. If provided it's interpreted
#' as sd of the population. If left to \code{NULL} (default) the sample
#' \code{sd(x)} will be used.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}. 
#' @return the standard error as numeric value.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link[lumen]{meanCI}} 
#' @keywords univar
#' @examples
#' 
#' meanSE(d.pizza$price, na.rm=TRUE)
#' 
#' # evaluate data.frame
#' sapply(d.pizza[,1:4], meanSE, na.rm=TRUE)
#' 


#' @family dispersion
#' @concept descriptive-statistics
#' @concept confidence-intervals
#'
#'
#' @export
meanSE <- function(x, sd = NULL, na.rm = FALSE) {
  # standard error of mean
  if(na.rm) x <- na.omit(x)
  s <- if(is.null(sd)) sd(x) else sd
  s/sqrt(length(x))
}