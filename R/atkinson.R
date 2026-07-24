#' Atkinson Index
#'
#' Computes the Atkinson inequality index.
#'
#' The Atkinson index measures income inequality based on a
#' social welfare function and includes an inequality aversion
#' parameter \eqn{\varepsilon >= 0}. Larger values of
#' \code{epsilon} imply greater sensitivity to lower incomes.
#'
#' @param x numeric vector of non-negative values, such as incomes
#' @param n optional frequency weights. Each element of \code{x}
#'   is replicated \code{n} times.
#' @param epsilon inequality aversion parameter \eqn{\varepsilon}.
#'   Must be non-negative. The case \code{epsilon = 1} is handled
#'   separately.
#' @param na.rm logical. If \code{TRUE}, missing values are removed.
#'
#' @return numeric value of the Atkinson index in the interval \verb{[0, 1]}
#'
#' @details
#' The index is defined as
#'
#' \deqn{
#' A(\varepsilon) = 1 - \frac{\left( \frac{1}{n} \sum x_i^{1-\varepsilon} \right)^{1/(1-\varepsilon)}}{\bar{x}}
#' }
#'
#' for \eqn{\varepsilon \neq 1}. For \eqn{\varepsilon = 1},
#'
#' \deqn{
#' A(1) = 1 - \frac{\exp\left( \frac{1}{n} \sum \log x_i \right)}{\bar{x}}
#' }
#'
#' If negative values or missing values (when \code{na.rm = FALSE})
#' are present, \code{NA} is returned.
#'
#' @references
#' Atkinson, A. B. (1970). On the measurement of inequality.
#' Journal of Economic Theory.
#'



#' @family inequality  
#' @concept inequality  
#' @concept concentration-index
#'
#'
#' @export
atkinson <- function(x, n = rep(1, length(x)), epsilon = 0.5, na.rm = FALSE) {
  
  x <- rep(x, n)  # consistent handling with other measures
  if (na.rm) x <- na.omit(x)
  
  if (any(is.na(x)) || any(x < 0) || epsilon < 0)
    return(NA_real_)
  
  mu <- mean(x)
  if (mu == 0)
    return(0)
  
  if (epsilon == 1) {
    A <- 1 - exp(mean(log(x))) / mu
  } else {
    A <- 1 - (mean(x^(1 - epsilon)))^(1 / (1 - epsilon)) / mu
  }
  
  A
}

