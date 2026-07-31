#' Atkinson Index
#'
#' Computes the Atkinson inequality index.
#'
#' The Atkinson index measures income inequality based on a
#' social welfare function and includes an inequality aversion
#' parameter \eqn{\varepsilon \ge 0}. Larger values of
#' \code{epsilon} imply greater sensitivity to lower incomes.
#'
#' @param x numeric vector of non-negative values, such as incomes
#' @param n optional frequency weights, a vector of non-negative whole
#'   numbers. Each element of \code{x} is replicated \code{n} times.
#' @param epsilon inequality aversion parameter \eqn{\varepsilon}.
#'   The case \code{epsilon = 1} is handled separately. A negative value
#'   yields \code{NA}.
#' @param na.rm logical. If \code{TRUE}, missing values are removed.
#' @param tol numeric tolerance for detecting the special case
#'   \eqn{\varepsilon \approx 1}
#'
#' @return numeric value of the Atkinson index in the interval \verb{[0, 1]}
#'
#' @details
#' The index is defined as
#'
#' \deqn{
#' A(\varepsilon) = 1 - \frac{\left( \frac{1}{n} \sum x_i^{1-\varepsilon} \right)^{1/(1-\varepsilon)}}{\bar{x}}
#' }{A(e) = 1 - (mean(x^(1-e)))^(1/(1-e)) / mean(x)}
#'
#' for \eqn{\varepsilon \neq 1}. For \eqn{\varepsilon = 1},
#'
#' \deqn{
#' A(1) = 1 - \frac{\exp\left( \frac{1}{n} \sum \log x_i \right)}{\bar{x}}
#' }{A(1) = 1 - exp(mean(log(x))) / mean(x)}
#'
#' The limiting case is used whenever \code{|epsilon - 1| < tol}. Without
#' that tolerance the general branch raises a quantity very close to 1 to
#' the power \eqn{1/(1-\varepsilon)}, which loses all precision as
#' \code{epsilon} approaches 1.
#'
#' If negative values or missing values (when \code{na.rm = FALSE})
#' are present, \code{NA} is returned.
#'
#' @references
#' Atkinson, A. B. (1970). On the measurement of inequality.
#' Journal of Economic Theory.
#'
#' @examples
#' x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
#'
#' atkinson(x)
#' atkinson(x, epsilon = 1)
#' atkinson(x, epsilon = 2)
#'
#' # frequency weights
#' atkinson(c(10, 20, 30), n = c(3, 1, 1))
#'
#' @family inequality
#' @concept inequality
#' @concept concentration-index
#' @export
atkinson <- function(x, n = rep(1, length(x)), epsilon = 0.5, na.rm = FALSE,
                     tol = 1e-8) {

  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon))
    stop("'epsilon' must be a single number")

  # Returns NA rather than stopping: that is the documented contract and
  # what test-atkinson.R asserts. I had changed this to stop() on the
  # grounds that a negative aversion parameter is a misspecified call,
  # not a degenerate sample - that argument still stands, but it is an
  # API decision, not a bug fix, and it is not mine to make silently.
  if (epsilon < 0)
    return(NA_real_)

  if (!is.numeric(n) || any(n < 0, na.rm = TRUE) ||
      any(n %% 1 != 0, na.rm = TRUE) || anyNA(n))
    stop("'n' must be a vector of non-negative whole numbers")

  x <- rep(x, n)  # consistent handling with other measures
  if (na.rm) x <- as.numeric(na.omit(x))

  if (length(x) == 0L)
    return(NA_real_)

  if (any(is.na(x)) || any(x < 0))
    return(NA_real_)

  mu <- mean(x)
  if (mu == 0)
    return(0)

  if (abs(epsilon - 1) < tol) {
    1 - exp(mean(log(x))) / mu
  } else {
    1 - (mean(x^(1 - epsilon)))^(1 / (1 - epsilon)) / mu
  }
}
