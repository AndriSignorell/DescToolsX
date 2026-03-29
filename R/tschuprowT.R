
#' Tschuprow's T
#'
#' Computes Tschuprow's T, a measure of association between two categorical
#' variables based on the chi-squared statistic.
#'
#' If \code{y} is provided, a contingency table is created using
#' \code{table(x, y, ...)}. Otherwise, \code{x} is assumed to already be
#' a contingency table.
#'
#' @param x A vector of categorical data or a contingency table
#'   (matrix or \code{table}).
#' @param y Optional second categorical vector. If provided, a contingency
#'   table is constructed from \code{x} and \code{y}.
#' @param correct Logical; if \code{TRUE}, applies a bias correction
#'   according to Bergsma (2013).
#' @param ... Additional arguments passed to \code{\link[base]{table}}.
#'
#' @return A numeric value representing Tschuprow's T.
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
#' If \code{correct = TRUE}, a bias-corrected version is computed based on
#' Bergsma (2013), which adjusts the estimate especially for small samples.
#'
#'
#' @references
#' Tschuprow, A. A. (1939). \emph{Principles of the Mathematical Theory of
#' Correlation}. W. Hodge & Co.
#'
#' Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T.
#' \emph{Journal of the Korean Statistical Society}, 42(3), 323–328.
#' https://doi.org/10.1016/j.jkss.2012.10.002
#'
#' @seealso \code{\link{chisq.test}} 
#' 
#' @family topic.associationMeasures
#' @concept association
#' @concept contingency-tables
#' @concept categorical-data
#' 
#' @examples
#' # Example with vectors
#' x <- c("A", "A", "B", "B")
#' y <- c("yes", "no", "yes", "no")
#' tschuprowT(x, y)
#'
#' # Example with contingency table
#' tab <- matrix(c(10, 20, 30, 40), nrow = 2)
#' tschuprowT(tab)
#'
#' # Bias-corrected version
#' tschuprowT(tab, correct = TRUE)
#'



#' @export
tschuprowT <- function(x, y = NULL, correct = FALSE, ...){
  
  if(!is.null(y)) x <- table(x, y, ...)
  
  # Tschuprow, A. A. (1939) Principles of the Mathematical Theory of Correlation; translated by M. Kantorowitsch. W. Hodge & Co.
  # http://en.wikipedia.org/wiki/Tschuprow's_T
  # Hartung S. 451
  
  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  chisq.hat <- suppressWarnings(chisq.test(x, correct = FALSE)$statistic)
  n <- sum(x)
  df <- prod(dim(x)-1)
  
  if(correct) {
    # Bergsma, W, A bias-correction for Cramer's V and Tschuprow's T
    # September 2013 Journal of the Korean Statistical Society 42(3)
    # DOI: 10.1016/j.jkss.2012.10.002
    # see also CramerV
    
    phi.hat <- chisq.hat / n
    as.numeric(sqrt(max(0, phi.hat - df/(n-1)) / 
                      (sqrt(prod(sapply(dim(x), function(i) i - 1 / (n-1) * (i-1)^2) - 1)))))
    
  } else {
    as.numeric( sqrt(chisq.hat/(n * sqrt(df))))
  }
  
}


