
#' Atkinson Index
#'
#' Computes the Atkinson inequality index.
#'
#' @param x numeric vector of non-negative values, such as incomes
#' @param weights optional frequency weights; either a single non-negative whole
#'   number or a vector having the same length as `x`
#' @param epsilon single non-negative numeric value specifying the inequality
#'   aversion parameter
#' @param na.rm logical; whether missing values in `x` are removed
#' @param tol single non-negative numeric value specifying the tolerance for
#'   treating `epsilon` as equal to one
#'
#' @return a numeric value in the interval \verb{[0, 1]}, or
#'   `NA_real_` if the index is undefined
#'
#' @details
#' With frequency weights \eqn{n_i}, the weighted arithmetic mean is
#'
#' \deqn{
#' \bar{x}_n =
#' \frac{\sum_i n_i x_i}{\sum_i n_i}.
#' }{
#' mean_n(x) = sum(n * x) / sum(n)
#' }
#'
#' For \eqn{\varepsilon \ne 1}, the Atkinson index is
#'
#' \deqn{
#' A(\varepsilon) =
#' 1 -
#' \frac{
#' \left(
#' \frac{\sum_i n_i x_i^{1-\varepsilon}}
#'      {\sum_i n_i}
#' \right)^{1/(1-\varepsilon)}
#' }{\bar{x}_n}.
#' }{
#' A(e) =
#' 1 - (sum(n * x^(1-e)) / sum(n))^(1/(1-e)) / mean_n(x)
#' }
#'
#' For \eqn{\varepsilon = 1},
#'
#' \deqn{
#' A(1) =
#' 1 -
#' \frac{
#' \exp\left(
#' \frac{\sum_i n_i \log(x_i)}
#'      {\sum_i n_i}
#' \right)
#' }{\bar{x}_n}.
#' }{
#' A(1) =
#' 1 - exp(sum(n * log(x)) / sum(n)) / mean_n(x)
#' }
#'
#' The calculation uses normalized frequency weights and logarithmic power
#' means. It therefore does not construct the potentially very large vector
#' that would result from `rep(x, weights)`.
#'
#' Observations with zero frequency are ignored. If all frequencies are zero
#' or no observations remain after removing missing values, `NA_real_`
#' is returned.
#'
#' If all values are zero, the index is defined as zero. If at least one value
#' is zero and `epsilon >= 1`, the equally distributed equivalent value
#' is zero and the index is one.
#'
#' Negative values, non-finite values, and missing values when
#' `na.rm = FALSE` produce `NA_real_`. A negative
#' `epsilon` also produces `NA_real_`.
#'
#' @references
#' Atkinson, A. B. (1970). On the measurement of inequality.
#' *Journal of Economic Theory*, 2(3), 244--263.
#'
#' @examples
#' x <- c(541, 1463, 2445, 3438, 4437,
#'        5401, 6392, 8304, 11904, 22261)
#'
#' atkinson(x)
#' atkinson(x, epsilon = 1)
#' atkinson(x, epsilon = 2)
#'
#' # frequency weights
#' atkinson(c(10, 20, 30), weights = c(3, 1, 1))
#'
#' # zero incomes
#' atkinson(c(0, 10, 20), epsilon = 1)
#'
#' @family inequality
#' @concept inequality
#' @concept concentration-index
#' @export
atkinson <- function(x, weights = NULL, epsilon = 0.5,
                     tol = 1e-8, na.rm = FALSE) {
  
  if (!is.numeric(x))
    stop("'x' must be a numeric vector")
  
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be TRUE or FALSE")
  
  if (!is.numeric(epsilon) || length(epsilon) != 1L ||
      !is.finite(epsilon))
    stop("'epsilon' must be a single finite number")
  
  if (!is.numeric(tol) || length(tol) != 1L ||
      !is.finite(tol) || tol < 0)
    stop("'tol' must be a single non-negative finite number")
  
  if (epsilon < 0)
    return(NA_real_)
  
  if (is.null(weights))
    weights <- rep(1, length(x))

  if (!is.numeric(weights) || anyNA(weights) || any(!is.finite(weights)) ||
      any(weights < 0) || any(weights != floor(weights)))
    stop("'weights' must contain non-negative finite whole numbers")
  
  if (length(weights) == 1L) {
    weights <- rep(weights, length(x))
  } else if (length(weights) != length(x)) {
    stop("'weights' must have length one or the same length as 'x'")
  }
  
  if (length(x) == 0L)
    return(NA_real_)
  
  # Values with zero frequency have no influence, including invalid values.
  keep <- weights > 0
  x <- x[keep]
  weights <- weights[keep]
  
  if (length(x) == 0L)
    return(NA_real_)
  
  if (na.rm) {
    keep <- !is.na(x)
    x <- x[keep]
    weights <- weights[keep]
    
  } else if (anyNA(x)) {
    return(NA_real_)
  }
  
  if (length(x) == 0L)
    return(NA_real_)
  
  if (any(!is.finite(x)) || any(x < 0))
    return(NA_real_)
  
  # All observations are equal and the arithmetic mean is zero.
  if (all(x == 0))
    return(0)
  
  # A(0) is identically zero.
  if (epsilon == 0)
    return(0)
  
  # The index is scale invariant. Scaling avoids overflow in the mean.
  x <- x / max(x)
  
  # Normalize through the maximum so that the sum cannot overflow.
  w <- weights / max(weights)
  w <- w / sum(w)
  
  arithmeticMean <- sum(w * x)
  
  if (epsilon == 1 || abs(epsilon - 1) <= tol) {
    
    if (any(x == 0))
      return(1)
    
    logEquivalent <- sum(w * log(x))
    
  } else {
    
    power <- 1 - epsilon
    
    if (power < 0 && any(x == 0))
      return(1)
    
    logX <- log(x)
    
    # Factoring out an extreme log(x) keeps the exponential arguments
    # non-positive and prevents overflow for large epsilon.
    anchor <- if (power > 0) max(logX) else min(logX)
    
    logEquivalent <-
      anchor +
      log(sum(w * exp(power * (logX - anchor)))) / power
  }
  
  logRatio <- logEquivalent - log(arithmeticMean)
  res <- -expm1(logRatio)
  
  if (!is.finite(res))
    return(NA_real_)
  
  # Remove small floating-point excursions outside the theoretical range.
  res <- min(1, max(0, res))
  
  return(res)
}


