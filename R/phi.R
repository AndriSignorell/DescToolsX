
#' Phi Coefficient
#'
#' Computes the Phi coefficient as a measure of association between two
#' categorical variables. For 2x2 contingency tables, Phi is equivalent
#' to the Pearson correlation coefficient.
#'
#' If \code{y} is provided, a contingency table is created using
#' \code{table(x, y, ...)}. Otherwise, \code{x} is assumed to already be
#' a contingency table.
#'
#' Note: Yates' continuity correction is **not applied** when computing
#' the chi-squared statistic.
#'
#' @param x A vector of categorical data or a contingency table
#'   (matrix or \code{table}).
#' @param y Optional second categorical vector. If provided, a contingency
#'   table is constructed from \code{x} and \code{y}.
#' @param ... Additional arguments passed to \code{\link[base]{table}}.
#'
#' @return A numeric value representing the Phi coefficient.
#'
#' @details
#' The Phi coefficient is defined as:
#' \deqn{
#' \phi = \sqrt{ \frac{\chi^2}{n} }
#' }
#' where \eqn{\chi^2} is the chi-squared test statistic and \eqn{n} is
#' the total sample size.
#'
#' For contingency tables larger than 2x2, Phi is not bounded by 1 and
#' may exceed 1. In such cases, \code{\link{cramerV}} is usually preferred.
#'
#' @seealso \code{\link{chisq.test}}, \code{\link{cramerV}}
#'
#' @examples
#' # Example with vectors
#' x <- c("A", "A", "B", "B")
#' y <- c("yes", "no", "yes", "no")
#' phi(x, y)
#'
#' # Example with contingency table
#' tab <- matrix(c(10, 20, 30, 40), nrow = 2)
#' phi(tab)
#'



#' @family assoc.nominal  
#' @concept association-measure  
#' @concept nominal  
#' @concept chi-square-based
#'
#'
#' @export
phi  <- function (x, y = NULL, ...) {
  if(!is.null(y)) x <- table(x, y, ...)
  # when computing phi, note that Yates' correction to chi-square must not be used.
  as.numeric( sqrt( suppressWarnings(chisq.test(x, correct=FALSE)$statistic) / sum(x) ) )
  
  # should we implement: ??
  # following http://technology.msb.edu/old/training/statistics/sas/books/stat/chap26/sect19.htm#idxfrq0371
  # (Liebetrau 1983)
  # this makes phi -1 < phi < 1 for 2x2 tables  (same for CramerV)
  # (prod(diag(x)) - prod(diag(Rev(x, 2)))) / sqrt(prod(colSums(x), rowSums(x)))
  
}

