#' Tschuprow's T
#'
#' Computes Tschuprow's T, a measure of association between two categorical
#' variables based on the chi-squared statistic.
#'
#' If `y` is provided, a contingency table is created using
#' `table(x, y, ...)`. Otherwise, `x` is assumed to already be
#' a two-dimensional contingency table.
#'
#' @param x a vector of categorical data (then `y` must be given) or a
#'   two-dimensional contingency table (matrix or `table`)
#' @param y optional second categorical vector. If provided, a contingency
#'   table is constructed from `x` and `y`.
#' @param correct logical; if `TRUE`, applies a bias correction
#'   according to Bergsma (2013).
#' @param ... additional arguments passed to [base::table()]. This
#'   refers only to the vector interface.
#'
#' @return a numeric scalar containing Tschuprow's T
#'
#' @details
#' Tschuprow's T is defined as:
#' \deqn{
#' T = \sqrt{ \frac{\chi^2}{n \sqrt{(r - 1)(c - 1)}} }
#' }
#' where \eqn{\chi^2} is the chi-squared statistic, \eqn{n} is the total
#' sample size, and \eqn{r} and \eqn{c} are the number of rows and columns
#' of the contingency table.
#'
#' If `correct = TRUE`, a bias-corrected version is computed based on
#' Bergsma (2013), which adjusts the estimate especially for small samples.
#' It replaces \eqn{\phi^2 = \chi^2/n} by
#' \eqn{\tilde\phi^2 = \max(0, \phi^2 - (r-1)(c-1)/(n-1))} and the dimensions
#' by \eqn{\tilde r = r - (r-1)^2/(n-1)} and
#' \eqn{\tilde c = c - (c-1)^2/(n-1)}.
#'
#' For a 2x2 table T coincides with Cramer's V and with the absolute value of
#' the phi coefficient; the sign of the association is not reported.
#'
#' @references
#' Tschuprow, A. A. (1939). *Principles of the Mathematical Theory of
#' Correlation*. W. Hodge & Co.
#'
#' Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T.
#' *Journal of the Korean Statistical Society*, 42(3), 323--328.
#' https://doi.org/10.1016/j.jkss.2012.10.002
#'
#' @seealso [stats::chisq.test()], [cramerV()]
#'
#' @examples
#' # Example with vectors
#' x <- c("A", "A", "B", "B")
#' y <- c("yes", "no", "yes", "no")
#' tschuprowT(x, y)
#'
#' # Example with contingency table
#' tab <- matrix(c(10, 20, 30, 40), nrow = 2)
#' tschuprowT(tab)               # 0.08908708
#'
#' # Bias-corrected version: the correction exceeds the estimate here,
#' # so the corrected value is 0
#' tschuprowT(tab, correct = TRUE)
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @concept chi-square-based
#'
#' @export
tschuprowT <- function(x, y = NULL, correct = FALSE, ...){

  if(!is.null(y))
    x <- table(x, y, ...)

  # Tschuprow, A. A. (1939) Principles of the Mathematical Theory of
  # Correlation; translated by M. Kantorowitsch. W. Hodge & Co.
  # http://en.wikipedia.org/wiki/Tschuprow's_T
  # Hartung S. 451

  # A one-dimensional argument would be silently passed on to chisq.test(),
  # which then performs a goodness-of-fit test and returns a number that has
  # nothing to do with an association measure.
  if(length(dim(x)) != 2L)
    stop("'x' must be a two-dimensional contingency table, ",
         "or 'y' must be supplied.")

  if(!is.numeric(x) || anyNA(x) || any(x < 0))
    stop("'x' must contain non-negative counts without missing values.")

  if(!isTRUE(correct) && !isFALSE(correct))
    stop("Argument 'correct' must be TRUE or FALSE.")

  n <- sum(x)
  if(n < 2)
    stop("The table must contain at least 2 observations.")

  d <- dim(x)
  df <- prod(d - 1L)
  if(df == 0)             # single row or column: no association defined
    return(NA_real_)

  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  chisq.hat <- suppressWarnings(chisq.test(x, correct = FALSE)$statistic)

  if(correct) {
    # Bergsma, W, A bias-correction for Cramer's V and Tschuprow's T
    # September 2013 Journal of the Korean Statistical Society 42(3)
    # DOI: 10.1016/j.jkss.2012.10.002
    # see also cramerV()

    phi.hat <- chisq.hat / n

    # bias corrected dimensions, r~ = r - (r-1)^2/(n-1)
    dcorr <- d - (d - 1L)^2 / (n - 1)

    denom <- prod(dcorr - 1)
    if(denom <= 0)
      # can happen for very small n, where the correction removes more
      # than the whole table
      return(NA_real_)

    res <- sqrt(max(0, phi.hat - df/(n - 1)) / sqrt(denom))

  } else {
    res <- sqrt(chisq.hat / (n * sqrt(df)))
  }

  as.numeric(res)

}
