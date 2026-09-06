
#' Expected Frequencies 
#' 
#' Calculate the expected frequencies of an n-way table assuming independence.
#' 
#' 
#' @param x a table
#' @param freq whether absolute or relative frequencies are computed. Must be
#' `"abs"` or `"rel"`; partial matching is supported.
#' @details
#' Expected values for a 2-dimensional table can be easily calculated with
#' res <- `[chisq.test](x)` and accessed by `res$expected`.
#' This approach fails for higher dimensional tables, for which there is no support.
#' `expFreq()` also returns expected frequencies for higher dimensional
#' tables.
#' 
#' @return a table with either the absolute or the relative expected
#' frequencies, of the same dimension and dimnames as `x`
#' @note Based on code by David Meyer previously published as 
#' `independence_table` in \pkg{vcd}, adapted to conform to package standards.
#' 
#' @seealso [chisq.test()] 
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
#' @export
expFreq <- function(x, freq = c("abs", "rel")) {
  
  # returns the expected frequencies of a table assuming independence
  
  # this is a copy of independence_table {vcd}
  # by David Meyer David.Meyer@R-project.org
  
  if (!is.array(x))
    stop("Need array of absolute frequencies!")
  
  # 'frequency' would mask stats::frequency; the argument itself is
  # already matched here
  freq <- match.arg(freq)

  n <- sum(x)
  x <- x/n
  d <- dim(x)
  margins <- lapply(seq_along(d), function(i) apply(x, i, sum))

  # expand.grid() varies the first factor fastest and array() fills
  # column-major, so the two orderings agree
  tab <- array(apply(expand.grid(margins), 1, prod),
               d, dimnames = dimnames(x))

  if (freq == "abs")
    tab <- tab * n

  # the input is a table, so the expected frequencies should be one too -
  # array() dropped the class and with it the table print method
  if (inherits(x, "table"))
    tab <- as.table(tab)

  tab

}
