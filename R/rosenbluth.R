
#' Rosenbluth Index
#'
#' Computes the Rosenbluth index as a measure of concentration.
#'
#' The Rosenbluth index is based on the ranked shares and is
#' directly related to market concentration. Larger values
#' indicate stronger concentration.
#'
#' @param x numeric vector of non-negative values, such as market shares or
#' frequencies
#' @param n optional frequency weights. Each element of \code{x}
#'   is replicated \code{n} times.
#' @param na.rm logical. If \code{TRUE}, missing values are removed.
#'
#' @return a numeric scalar containing the Rosenbluth index
#'
#' @details
#' With the shares \eqn{p_i = x_i / \sum x} sorted in decreasing order and
#' \eqn{i} their rank, the index is
#' \deqn{HT = 1 / (2 \sum i p_i - 1).}
#' It ranges from \eqn{1/k} for \eqn{k} units of equal size to 1 for a single
#' unit holding everything, so it is read on the same scale as the Herfindahl
#' index rather than as an inequality measure.
#'
#' If negative values or missing values (when \code{na.rm = FALSE})
#' are present, \code{NA} is returned. The index is undefined when all values
#' are zero, and \code{NA} is returned in that case as well.
#'
#' @references
#' Rosenbluth, G. (1955). Measures of concentration. In: \emph{Business
#' Concentration and Price Policy}. Princeton University Press, 57-99.
#'
#' Hall, M., Tideman, N. (1967). Measures of concentration.
#' \emph{Journal of the American Statistical Association}, 62, 162-168.
#'
#' @examples
#' # four units of equal size: the index takes its minimum 1/4
#' rosenbluth(c(1, 1, 1, 1))
#'
#' # one unit holding everything: the maximum 1
#' rosenbluth(c(1, 0, 0, 0))
#'
#' # a dominant unit next to three small ones
#' rosenbluth(c(10, 1, 1, 1))
#'
#' # frequency weights replicate the values
#' rosenbluth(c(10, 1), n = c(1, 3))
#'
#' @seealso \code{\link{gini}}, \code{\link{lc}}
#'
#' @family inequality  
#' @concept concentration-index
#'
#'
#' @export
rosenbluth <- function(x, n = rep(1, length(x)), na.rm = FALSE) {
  
  if(!is.numeric(x))
    stop("'x' must be numeric")
  
  if(!is.numeric(n) || anyNA(n) || any(n < 0))
    stop("'n' must be a numeric vector of non-negative frequencies")
  
  # rep() truncates a fractional times= argument without a word, so 2.9 would
  # silently become 2 replications.
  if(any(n != floor(n)))
    stop("'n' must contain whole numbers")
  
  if(length(n) != 1L && length(n) != length(x))
    stop("'n' must have length 1 or length(x)")
  
  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be a single non-missing logical value")
  
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
  # Without any mass there are no shares to rank; the ratio below would be
  # 0/0 and returned NaN, which is not one of the documented outcomes.
  if (length(x) == 0L || sum(x) == 0)
    return(NA_real_)
  
  # 'n' is the frequency argument up to here; from here on the number of units.
  nUnits <- length(x)
  x <- sort(x)
  HT <- (nUnits:1)*x
  HT <- 2*sum(HT/sum(x))
  HT <- 1/(HT-1)
  unname(HT)
}
