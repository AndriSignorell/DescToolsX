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
#'   is replicated \code{n} times. Must be a vector of non-negative
#'   integers of the same length as \code{x}.
#' @param na.rm logical. If \code{TRUE}, missing values are removed.
#'
#' @return a numeric scalar containing the Theil index. The value is 0 under
#'   perfect equality and increases with inequality, up to a maximum of
#'   \eqn{\log(n)}, attained when a single unit holds the entire total.
#'
#' @details
#' The Theil T index is defined as
#'
#' \deqn{
#' T = \frac{1}{n} \sum \frac{x_i}{\bar{x}} \log\left(\frac{x_i}{\bar{x}}\right)
#' }
#'
#' where \eqn{\bar{x}} is the mean of \code{x} and \eqn{n} the number of
#' (replicated) observations.
#'
#' Zero values are admissible: following the usual convention
#' \eqn{x \log x \to 0} as \eqn{x \to 0}, they contribute 0 to the sum.
#'
#' The index is decomposable into within- and between-group
#' components, which makes it particularly useful in applied
#' inequality analysis.
#'
#' If negative values or missing values (when \code{na.rm = FALSE})
#' are present, \code{NA} is returned. The same holds if no observation
#' remains after removing missing values.
#'
#' @references
#' Theil, H. (1967). Economics and Information Theory.
#'
#' @examples
#' theil(c(10, 10, 10, 10))          # perfect equality: 0
#' theil(c(0, 0, 0, 40))             # everything in one hand: log(4)
#' theil(c(1, 2, 3, 4, 5))
#'
#' # frequency weights replicate the observations
#' theil(1:3, n = c(1, 2, 3))
#' theil(rep(1:3, times = c(1, 2, 3)))
#'
#' @family inequality
#' @concept inequality
#' @concept concentration-index
#'
#' @export
theil <- function(x, n = rep(1, length(x)), na.rm = FALSE) {

  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")

  # -- frequency weights ------------------------------------------------
  # rep() would silently truncate non-integer values, so check explicitly
  if (length(n) != length(x))
    stop("Argument 'n' must have the same length as 'x'.")
  if (anyNA(n) || any(n < 0) || any(n != trunc(n)))
    stop("Argument 'n' must contain non-negative integer frequencies.")

  x <- rep(x, n)

  if (na.rm)
    x <- x[!is.na(x)]

  if (length(x) == 0L)
    return(NA_real_)

  if (anyNA(x) || any(x < 0))
    return(NA_real_)

  mu <- mean(x)
  if (mu == 0)                # all values are 0, no inequality
    return(0)

  r <- x / mu

  # convention 0 * log(0) = 0; without this a single zero value would
  # turn the whole index into NaN
  rlogr <- ifelse(r == 0, 0, r * log(r))

  mean(rlogr)

}
