
#' Expected Frequencies 
#' 
#' Calculate the expected frequencies of an n-way table assuming independence.
#' 
#' 
#' @param x a table
#' @param freq whether absolute or relative frequencies are computed. Must be
#' \code{"abs"} or \code{"rel"}; partial matching is supported.
#' @details
#' Expected values for a 2-dimensional table can be easily calculated with
#' res <- \code{\link{chisq.test}(x)} and accessed by \code{res$expected}.
#' This approach fails for higher dimensional tables, for which there is no support.
#' \code{expFreq()} also returns expected frequencies for higher dimensional
#' tables.
#' 
#' @return a table with either the absolute or the relative expected
#' frequencies
#' @note Based on code by David Meyer previously published as 
#' \code{independence_table} in \pkg{vcd}, adapted to conform to package standards.
#' 
#' @seealso \code{\link{chisq.test}} 
#' 
#' @examples
#' 
#' expFreq(Titanic)
#' 
#' expFreq(UCBAdmissions, freq="r")
#' 
#' 
#' @family frequency  
#' @concept frequency-table  
#' @concept chi-square-based
#'
#'
#' @export
expFreq <- function(x, freq = c("abs", "rel")) {
  
  # returns the expected frequencies of a table assuming independence
  
  # this is a copy of independence_table {vcd}
  # by David Meyer David.Meyer@R-project.org
  
  if (!is.array(x))
    stop("Need array of absolute frequencies!")
  
  frequency <- match.arg(freq)
  
  n <- sum(x)
  x <- x/n
  d <- dim(x)
  margins <- lapply(1:length(d), function(i) apply(x, i, sum))
  
  tab <- array(apply(expand.grid(margins), 1, prod), 
               d, dimnames = dimnames(x))
  
  if (frequency == "rel")
    tab
  else 
    tab * n
  
}
