
#' Goodman Kruskal's Tau
#' 
#' Calculate Goodman-Kruskal's tau, a measure of association for nominal
#' variables in a two-way table. The function accepts either a contingency
#' table or two vectors.
#' 
#' Goodman-Kruskal tau measures association for cross tabulations of nominal
#' level variables. Goodman-Kruskal tau is based on random category assignment.
#' It measures the percentage improvement in predictability of the dependent
#' variable (column or row variable) given the value of other variables (row or
#' column variables). Goodman-Kruskal tau is the same as Goodman-Kruskal lambda
#' except the calculations of the tau statistic are based on assignment
#' probabilities specified by marginal or conditional proportions.
#' Misclassification probabilities are based on random category assignment with
#' probabilities specified by marginal or conditional proportion.
#' 
#' Goodman Kruskal tau reduces to \eqn{\phi^2} (see: \code{\link{phi}}) in the
#' 2x2-table case.\cr
#' 
#' The measure lies in \eqn{[0, 1]} by construction. Both ends are reached by
#' cancellation, so an estimate within a few machine epsilons of a bound is
#' reported as that bound. Tau is undefined when the dependent variable has
#' fewer than two non-empty categories (the denominator is then zero), which is
#' signalled with an error.
#' 
#' The confidence interval uses the asymptotic standard error of Liebetrau
#' (1983). That variance vanishes at both ends of the range: under exact
#' independence (\eqn{\tau = 0}, where the limiting distribution is a weighted
#' sum of chi-square variables rather than normal) and under perfect
#' prediction (\eqn{\tau = 1}). Where the estimated standard error is zero the
#' interval would collapse to a single point and thus exclude every other
#' value, which no sample supports; the bounds are returned as \code{NA} with a
#' warning instead. Close to either end the normal approximation is poor and
#' the interval is too narrow.
#' 
#' @name gkTau
#' @param x a vector (typically a factor, character, or numeric vector)
#'        containing categorical data, or a contingency table.
#' @param y \code{NULL} (default) or a vector with compatible dimensions to
#' \code{x}. If supplied, \code{table(x, y, \dots)} is calculated.
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See \code{\link{ConfidenceIntervals}}.
#' @param direction direction of the calculation. Must be \code{"row"}
#' (default) or \code{"column"}. \code{"row"} gives tau (R|C), i.e. the row
#' variable is the dependent one and is predicted from the column variable;
#' \code{"column"} gives tau (C|R).
#'
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to set useNA. This refers only to the
#' vector interface; supplying them without \code{y} is an error.
#' 
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Goodman-Kruskal's tau}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @note Based on code by Antti Arppe, adapted to conform to package standards.
#' 
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons, pp. 57-59.
#' 
#' Goodman, L. A., & Kruskal, W. H. (1954) Measures of association for cross
#' classifications. \emph{Journal of the American Statistical Association}, 49,
#' 732-764.
#' 
#' Goodman, L. A., & Kruskal, W. H. (1963) Measures of association for cross
#' classifications III: Approximate sampling theory. \emph{Journal of the
#' American Statistical Association}, 58, 310-364.
#' 
#' Liebetrau, A. M. (1983) \emph{Measures of Association}, Sage University
#' Papers Series on Quantitative Applications in the Social Sciences, 07-004.
#' Newbury Park, CA: Sage, pp. 24--30
#' 
#' @seealso \code{\link{lambda}}, \code{\link{cramerV}},
#' \code{\link{Association}}
#' 
#' @examples
#' # example in:
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. S. 1821
#' 
#' tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))
#' 
#' # Goodman Kruskal's tau C|R
#' gkTau(tab, direction="column", conf.level=0.95)
#' # Goodman Kruskal's tau R|C
#' gkTau(tab, direction="row", conf.level=0.95)
#' 
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. 1814 (143)
#' tab <- as.table(cbind(c(11,2),c(4,6)))
#' 
#' gkTau(tab, direction="row", conf.level=0.95)
#' gkTau(tab, direction="column", conf.level=0.95)
#' # reduce both to:
#' phi(tab)^2
#' 
#' 
#' # example 1 in Liebetrau (1983)
#' 
#' tt <- matrix(c(549,93,233,119,225,455,402,  
#'                212,124,78,42,41,12,132,
#'                54,54,33,13,46,7,153), ncol=3,
#'              dimnames=list(rownames=c("Gov", "Mil", "Edu", "Eco", "Intel", "Rel", "For"), 
#'                            colnames=c("One", "Two", "Multi")))
#' 
#' gkTau(tt, direction = "row", conf.level = 0.95)
#' gkTau(tt, direction = "column", conf.level = 0.95)
#' 
#' 
#' # SPSS
#' ttt <- matrix(c(225,53,206,3,1,12), nrow=3,
#'               dimnames=list(rownames=c("right","center", "left"), 
#'                             colnames=c("us","ussr")))
#' 
#' round(gkTau(ttt, direction = "row", conf.level = 0.95), digits = 3)
#' round(gkTau(ttt, direction = "column"), digits = 3)
#' 
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @export
gkTau <- function(x, y = NULL, 
                  conf.level = NA,
                  sides = c("two.sided", "left", "right"), 
                  direction = c("row", "column"),
                  ...){

  # matched up front, not inside switch(): a misspelled 'direction' used
  # to be caught only where the branch was taken, and 'sides' did not
  # exist at all
  direction <- match.arg(direction)
  sides     <- match.arg(sides)

  conf.level <- checkConfLevel(conf.level)

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1. At or below 0.5 that level is
  # not positive and the normal quantile turns negative, which hands back
  # the two bounds in reverse order.
  if(sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  confAdj <- if(sides == "two.sided") conf.level else 2 * conf.level - 1

  if(!is.null(y)){
    x <- table(x, y, ...)

  } else {
    # the dots reach table() only on the vector interface - silently
    # dropping them would hide both a typo and a useNA that never took
    # effect
    if(...length() > 0L)
      stop("'...' is passed on to table() and requires 'y' to be supplied")

    # is.table() is TRUE for any number of dimensions, and as.matrix()
    # would fold a 3d array into a single column instead of complaining
    if(length(dim(x)) != 2L)
      stop("'x' must be a two-dimensional table or matrix when 'y' is NULL")
  }

  x <- as.matrix(x)

  if(!is.numeric(x))
    stop("'x' must be numeric, i.e. a table of counts")
  if(anyNA(x))
    stop("'x' must not contain NA")
  if(any(!is.finite(x)))
    stop("'x' must contain only finite counts")
  if(any(x < 0))
    stop("'x' must not contain negative counts")

  # Perform all subsequent calculations in double precision.
  storage.mode(x) <- "double"

  # Use tau C|R as the canonical orientation. Tau R|C is the same
  # calculation after transposing the table.
  if(direction == "row") x <- t(x)

  # Unused factor levels can produce empty margins and hence divisions by
  # zero. They contain no information and can safely be removed.
  x <- x[rowSums(x) > 0, colSums(x) > 0, drop = FALSE]

  # the denominator below is 1 - sum(p_.j^2), which is zero as soon as the
  # dependent variable has a single category left - tau is then 0/0
  if(ncol(x) < 2L)
    stop(gettextf(
      "tau is not defined: the %s variable has fewer than two non-empty categories",
      direction), domain = NA)

  n <- sum(x)
  sum.col <- colSums(x)
  p.row <- x / rowSums(x)               # recycles by row: p.row[i, j] = n_ij / n_i.
  sum.p2.row <- rowSums(p.row^2)

  # naming follows the prediction rule, not the error: err.marginal is the
  # error of assignment from the column margin alone, err.rowwise the error
  # of assignment from the conditional row distributions
  err.rowwise  <- n^2 - n * sum(x * p.row)
  err.marginal <- n^2 - sum(sum.col^2)

  est <- 1 - err.rowwise / err.marginal

  # tau is a proportional reduction in error and cannot leave [0, 1], but
  # both ends are reached by cancellation: an exactly independent table such
  # as outer(c(850, 783), c(198, 71)) comes out as +-2 * eps, not as 0. Snap
  # to the bound, so that the reported estimate and the degeneracy check
  # below both see the value the table actually carries.
  tol <- 100 * .Machine$double.eps
  if(est <= tol)
    est <- 0
  else if(est >= 1 - tol)
    est <- 1

  v <- err.rowwise / n^2
  d <- err.marginal / n^2
  f <- d * (v + 1) - 2 * v

  # Liebetrau (1983), eq. 3.11. Vectorised: the double loop evaluated the
  # same expression cell by cell, which is O(r*c) interpreted calls on a
  # table that R can handle in one pass.
  # The column margin has to be recycled explicitly (byrow = TRUE), the row
  # quantities p.row and sum.p2.row align with R's own recycling.
  colP <- matrix(sum.col / n, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  psi <- -2 * v * colP + d * (2 * p.row - sum.p2.row) - f
  sigma2 <- sum(x * psi^2) / (n^2 * d^4)

  if(is.na(conf.level)){
    res <- est

  } else if(est <= 0 || est >= 1 || !is.finite(sigma2) || sigma2 <= 0){
    # The variance vanishes at both ends of the range. It is tested through
    # the estimate, which has a known scale of [0, 1]: sigma2 itself comes
    # out as rounding noise there (1e-36, not 0), and a threshold on it
    # would have to be pulled out of thin air.
    warning("the asymptotic standard error of tau is zero or undefined here, ",
            "the confidence bounds are reported as NA")
    res <- c(est = est, lci = NA_real_, uci = NA_real_)

  } else {
    pr2 <- 1 - (1 - confAdj)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + est
    # tau is a proportional-reduction-in-error measure and lives in
    # [0, 1] - applySides() clamps to that range and closes the open
    # side of a one-sided interval there instead of at an infinity tau
    # cannot reach
    res <- c(est = est, applySides(ci, sides, lo = 0, hi = 1))
  }

  return(res)
}
