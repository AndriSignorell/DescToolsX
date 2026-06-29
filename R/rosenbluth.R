
#' Rosenbluth Index
#'
#' Computes the Rosenbluth index as a measure of concentration.
#'
#' The Rosenbluth index is based on the ranked shares and is
#' inversely related to market concentration. Smaller values
#' indicate stronger concentration.
#'
#' @param x Numeric vector of non-negative values (e.g. market shares
#'   or frequencies).
#' @param n Optional frequency weights. Each element of \code{x}
#'   is replicated \code{n} times.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value of the Rosenbluth index.
#'
#' @details
#' The measure is computed from the ordered values of \code{x}
#' and reflects the distribution of shares across units.
#' If negative values or missing values (when \code{na.rm = FALSE})
#' are present, \code{NA} is returned.
#'
#' @references
#' Rosenbluth, G. (1955). Measures of concentration.
#'



#' @family inequality  
#' @concept inequality  
#' @concept concentration-index
#'
#'
#' @export
rosenbluth <- function(x, n = rep(1, length(x)), na.rm = FALSE) {
  
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
  n <- length(x)
  x <- sort(x)
  HT <- (n:1)*x
  HT <- 2*sum(HT/sum(x))
  HT <- 1/(HT-1)
  HT
}


