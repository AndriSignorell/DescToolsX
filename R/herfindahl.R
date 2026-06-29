
#' Herfindahl Index
#'
#' Computes the Herfindahl (or Herfindahl-Hirschman) index as a measure
#' of concentration or inequality.
#'
#' The index is defined as the power mean of order \eqn{m+1} of the
#' relative shares. For \code{parameter = 1}, the classical
#' Herfindahl-Hirschman Index (HHI) is obtained.
#'
#' @param x Numeric vector of non-negative values (e.g. market shares,
#'   incomes, or frequencies).
#' @param n Optional frequency weights. Each element of \code{x}
#'   is replicated \code{n} times.
#' @param parameter Parameter \eqn{m} controlling the sensitivity
#'   to concentration. Default is \code{1}.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed.
#'
#' @return Numeric value of the Herfindahl index.
#'
#' @details
#' Larger values indicate higher concentration.
#' If negative values or missing values (when \code{na.rm = FALSE})
#' are present, \code{NA} is returned.
#'
#' @author originally based on code of Achim Zeileis <achim.zeileis@@r-project.org>, 
#' rewritten by Andri Signorell <andri@@signorell.net>
#' 
#' @seealso See \code{\link{gini}}, \code{\link{atkinson}} and
#' 
#' @references Cowell, F. A. (2000) Measurement of Inequality, in Atkinson, A.
#' B., Bourguignon, F.  \emph{Handbook of Income Distribution}. (Eds) Amsterdam
#' 
#' Cowell, F. A. (1995) \emph{Measuring Inequality}. Prentice Hall/Harvester
#' Wheatshef
#' 
#' Hall, M., Tidemann, N. (1967) \emph{Measures of Concentration}, JASA 62,
#' 162-168.
#'
#' Hirschman, A. O. (1964). The paternity of an index.
#' 
#' @examples
#' 
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
#' 
#' 


#' @family inequality  
#' @concept inequality  
#' @concept concentration-index
#'
#'
#' @export
herfindahl <- function(x, n = rep(1, length(x)), parameter=1, na.rm = FALSE) {
  
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
  if(is.null(parameter))
    m <- 1
  else
    m <- parameter
  Herf <- x/sum(x)
  Herf <- Herf^(m+1)
  Herf <- sum(Herf)^(1/m)
  Herf
}
