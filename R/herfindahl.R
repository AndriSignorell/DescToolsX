
#' Herfindahl Index
#'
#' Computes the Herfindahl (or Herfindahl-Hirschman) index as a measure
#' of concentration or inequality.
#'
#' The index is defined as the power mean of order \eqn{m+1} of the
#' relative shares. For `parameter = 1`, the classical
#' Herfindahl-Hirschman Index (HHI) is obtained.
#'
#' @param x numeric vector of non-negative values, such as market shares,
#' incomes, or frequencies
#' @param n optional frequency weights. Each element of `x`
#'   is replicated `n` times.
#' @param parameter parameter \eqn{m} controlling sensitivity to concentration;
#' must be strictly positive, default is `1`. `m = 0` is rejected:
#' it degenerates to a constant 1 for every input.
#' @param na.rm logical; whether to remove missing values
#'
#' @return numeric scalar containing the Herfindahl index
#'
#' @details
#' Larger values indicate higher concentration.
#' If negative values or missing values (when `na.rm = FALSE`)
#' are present, `NA` is returned.
#'
#' @note Based on code by Achim Zeileis, rewritten to conform to package standards.
#' 
#' @references Cowell, F. A. (2000) Measurement of Inequality, in Atkinson, A.
#' B., Bourguignon, F.  *Handbook of Income Distribution*. (Eds) Amsterdam
#' 
#' Cowell, F. A. (1995) *Measuring Inequality*. Prentice Hall/Harvester
#' Wheatshef
#' 
#' Hall, M., Tidemann, N. (1967) *Measures of Concentration*, JASA 62,
#' 162-168.
#'
#' Hirschman, A. O. (1964). The paternity of an index.
#' 
#' @examples
#' # generate vector (of sales)
#' x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
#' 
#' # compute Herfindahl coefficient with parameter 1
#' herfindahl(x)
#' 
#' # Some more examples
#' herfindahl(c(261,29,33,15,39,28,95,5,6,28,69,8,105,38,15))
#' herfindahl(c(783,121,112,70,201,153,425,19,37,126,325,51,442,193,41))
#'  
#' 
#' @seealso [gini()], [atkinson()]
#' 
#' @family diversity.concentration
#' @concept inequality
#' @concept concentration-index
#' @export
herfindahl <- function(x, n = rep(1, length(x)), parameter = 1, na.rm = FALSE) {

  # same validation as atkinson(), which shares the family
  if (!is.numeric(n) || anyNA(n) || any(n < 0, na.rm = TRUE) ||
      any(n %% 1 != 0, na.rm = TRUE))
    stop("'n' must be a vector of non-negative whole numbers")

  m <- if (is.null(parameter)) 1 else parameter

  if (!is.numeric(m) || length(m) != 1L || is.na(m) || m <= 0)
    stop("'parameter' must be a single positive number")

  x <- rep(x, n)    # same handling as Lc and Gini

  if(na.rm) x <- as.numeric(na.omit(x))
  if (length(x) == 0L) return(NA_real_)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  # all shares zero leaves 0/0
  if (sum(x) == 0) return(NA_real_)

  Herf <- x/sum(x)
  Herf <- Herf^(m+1)
  Herf <- sum(Herf)^(1/m)
  Herf
}
