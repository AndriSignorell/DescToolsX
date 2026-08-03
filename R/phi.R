
#' Phi Coefficient
#'
#' Computes the Phi coefficient as a measure of association between two
#' categorical variables.
#'
#' If \code{y} is provided, a contingency table is created using
#' \code{table(x, y, ...)}. Otherwise, \code{x} is assumed to already be
#' a contingency table.
#'
#' Note: Yates' continuity correction is **not applied** when computing
#' the chi-squared statistic.
#'
#' @param x a vector of categorical data or a contingency table
#'   (matrix or \code{table})
#' @param y optional second categorical vector. If provided, a contingency
#'   table is constructed from \code{x} and \code{y}.
#' @param ... additional arguments passed to \code{\link[base]{table}}
#'
#' @return a numeric scalar containing the Phi coefficient
#'
#' @details
#' The Phi coefficient is defined as:
#' \deqn{
#' \phi = \sqrt{ \frac{\chi^2}{n} }
#' }
#' where \eqn{\chi^2} is the chi-squared test statistic and \eqn{n} is
#' the total sample size.
#'
#' This definition is \strong{unsigned}. For a 2x2 table the signed
#' coefficient \eqn{(n_{11} n_{22} - n_{12} n_{21}) /
#' \sqrt{n_{1\cdot} n_{2\cdot} n_{\cdot 1} n_{\cdot 2}}} equals the Pearson
#' correlation of the two 0/1 indicators and lies in \eqn{[-1, 1]}; the value
#' returned here is its absolute value, so the direction of the association
#' is not reported. See \code{\link{pearsonCor}} if the sign is needed.
#'
#' For contingency tables larger than 2x2, Phi is not bounded by 1 and
#' may exceed 1. In such cases, \code{\link{cramerV}} is usually preferred.
#'
#'
#' @examples
#' # Example with vectors
#' x <- c("A", "A", "B", "B")
#' y <- c("yes", "no", "yes", "no")
#' phi(x, y)
#'
#' # Example with contingency table. Note that the signed coefficient is
#' # -0.0891 here: phi() reports the magnitude only.
#' tab <- matrix(c(10, 20, 30, 40), nrow = 2)
#' phi(tab)
#'
#'
#' @seealso \code{\link{chisq.test}}
#'
#' @family assoc.nominal  
#' @concept association-measure  
#' @concept nominal  
#' @concept chi-square-based
#'
#'
#' @export
phi  <- function (x, y = NULL, ...) {

  if(!is.null(y)) x <- table(x, y, ...)

  if (length(dim(x)) != 2L)
    stop("'x' must be a two-dimensional contingency table.")

  if (anyNA(x) || any(x < 0))
    stop("'x' must contain non-negative counts without missing values.")

  if (sum(x) == 0)
    stop("'x' must contain at least one observation.")

  # when computing phi, note that Yates' correction to chi-square must not be used.
  as.numeric( sqrt( suppressWarnings(chisq.test(x, correct=FALSE)$statistic) / sum(x) ) )

  # TODO (open, see REVIEW.md): should the 2x2 case return the SIGNED
  # coefficient, as Liebetrau (1983) and SAS PROC FREQ do?
  # (prod(diag(x)) - prod(diag(revX(x, 2)))) / sqrt(prod(colSums(x), rowSums(x)))
  # That would make phi in (-1, 1) for 2x2 tables and match the documented
  # equivalence with the Pearson correlation -- but it changes results for
  # every existing caller, so it is left as a decision.

}
