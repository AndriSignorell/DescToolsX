
#' Kendall's \eqn{\tau_a} (Tau-a)
#'
#' @description
#' Computes Kendall's \eqn{\tau_a}, a rank-based measure of association for ordinal variables.
#' The function provides interfaces for both contingency tables and paired vectors.
#'
#' @details
#' Kendall's \eqn{\tau_a} is defined as
#'
#' \deqn{
#' \tau_a = \frac{C - D}{\frac{1}{2} n (n - 1)}
#' }
#'
#' where \eqn{C} and \eqn{D} denote the number of concordant and discordant pairs.
#'
#' \eqn{\tau_a} takes values in \eqn{[-1, 1]}. Values close to \eqn{1} indicate
#' strong positive association, values close to \eqn{-1} strong negative association.
#'
#' In contrast to \code{\link{kendallTauB}}, \eqn{\tau_a} does not adjust for ties.
#' Therefore, it is generally not recommended when ties are present in the data.
#'
#' Kendall's \eqn{\tau_a} is symmetric:
#' \code{kendallTauA(x, y)} = \code{kendallTauA(y, x)}.
#'
#' @param x a numeric vector or a contingency table (matrix or table)
#' @param y optional numeric vector. If supplied, must have the same length as \code{x}.
#' @param conf.level confidence level for confidence intervals. If \code{NA},
#'   no confidence interval is returned.
#' @param \dots further arguments passed to \code{\link{ordAssocs}}
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Kendall's tau-a}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @references
#' Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley and Sons, pp. 57--59.
#'
#' Hollander, M., Wolfe, D. A. and Chicken, E. (2014)
#' \emph{Nonparametric Statistical Methods}, 3rd edition, Wiley.
#'
#' Liebetrau, A. M. (1983)
#' \emph{Measures of Association}, Sage.
#'
#' @examples
#'
#' # Table example
#' tab <- as.table(rbind(
#'   c(26,26,23,18,9),
#'   c(6,7,9,14,23)
#' ))
#'
#' kendallTauA(tab, conf.level=0.95)
#'
#' # Vector example
#' x <- c(1,2,2,3,3,3,4,5)
#' y <- c(1,3,2,1,5,3,4,5)
#'
#' kendallTauA(x, y, conf.level=0.95)
#'
#'
#' @family assoc.ordinal
#' @concept association-measure
#' @concept ordinal
#' @concept rank-correlation
#' @export
kendallTauA <- function(x, y = NULL,
                        conf.level = NA,
                        ...){
  
  # ... was documented as reaching table() and then never forwarded at
  # all - the same defect as in gkGamma(). ordAssocs() does not call
  # table() on the vector path either, so the promise was doubly wrong;
  # the arguments now go where they can actually be used.
  res <- ordAssocs(
    x = x,
    y = y,
    which = "tauA",
    conf.level = conf.level,
    ...
  )
  
  if(is.na(conf.level))
    unname(res[[1]])
  else
    res[[1]]
  
}
