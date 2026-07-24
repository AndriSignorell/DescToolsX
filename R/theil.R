
#' Theil Index
#'
#' Computes the Theil inequality index (Theil T).
#'
#' The Theil index is an entropy-based measure of inequality.
#' It belongs to the class of Generalized Entropy measures
#' with parameter \eqn{\alpha = 1}.
#'
#' @param x numeric vector of non-negative values, such as incomes
#' @param n optional frequency weights. Each element of \code{x}
#'   is replicated \code{n} times.
#' @param na.rm logical. If \code{TRUE}, missing values are removed.
#'
#' @return a numeric scalar containing the Theil index. The value is 0 under
#'   perfect equality and increases with inequality.
#'
#' @details
#' The Theil T index is defined as
#'
#' \deqn{
#' T = \frac{1}{n} \sum \frac{x_i}{\bar{x}} \log\left(\frac{x_i}{\bar{x}}\right)
#' }
#'
#' where \eqn{\bar{x}} is the mean of \code{x}.
#'
#' The index is decomposable into within- and between-group
#' components, which makes it particularly useful in applied
#' inequality analysis.
#'
#' If negative values or missing values (when \code{na.rm = FALSE})
#' are present, \code{NA} is returned.
#'
#' @references
#' Theil, H. (1967). Economics and Information Theory.
#'
#' @family inequality
#' @concept inequality
#' @concept concentration-index
#'
#'
#' @export
theil <- function(x, n = rep(1, length(x)), na.rm = FALSE) {
  
  x <- rep(x, n)  # consistent handling with other measures
  if (na.rm) x <- na.omit(x)
  
  if (any(is.na(x)) || any(x < 0))
    return(NA_real_)
  
  mu <- mean(x)
  if (mu == 0)
    return(0)
  
  r <- x / mu
  T <- mean(r * log(r))
  
  T
}
