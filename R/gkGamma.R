
#' Goodman–Kruskal's Gamma
#'
#' @description
#' Computes Goodman–Kruskal's Gamma, a measure of association for ordinal variables.
#' The function provides interfaces for both contingency tables and paired vectors.
#'
#' @details
#' Goodman–Kruskal's Gamma is based solely on the number of concordant and
#' discordant pairs and ignores ties. It is defined as
#'
#' \deqn{
#' \gamma = \frac{C - D}{C + D}
#' }
#'
#' where \eqn{C} and \eqn{D} denote the number of concordant and discordant pairs.
#'
#' Gamma takes values in \eqn{[-1, 1]}. Values close to \eqn{1} indicate strong
#' positive association, values close to \eqn{-1} strong negative association.
#'
#' In contrast to Kendall's \eqn{\tau_b} and Somers' D, Gamma ignores ties,
#' which can lead to overestimation of association when ties are present.
#'
#' For \eqn{2 \times 2} tables, Gamma is equivalent to Yule's Q
#' (\code{\link{yuleQ}}).
#'
#' Gamma is symmetric:
#' \code{gkGamma(x, y)} = \code{gkGamma(y, x)}.
#'
#' @param x A numeric vector or a contingency table (matrix or table).
#' @param y Optional numeric vector. If supplied, must have the same length as \code{x}.
#' @param conf.level Confidence level for confidence intervals. If \code{NA},
#'   no confidence interval is returned.
#' @param \dots Further arguments passed to \code{\link{table}} in the vector interface.
#'
#' @return
#' If \code{conf.level = NA}, a single numeric value is returned.
#' Otherwise a named numeric vector with elements:
#' \itemize{
#'   \item \code{est}: estimate
#'   \item \code{lci}: lower confidence interval
#'   \item \code{uci}: upper confidence interval
#' }
#'
#' @seealso
#' \code{\link{yuleQ}} for \eqn{2 \times 2} tables,
#'
#' @references
#' Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons, pp. 57–59.
#'
#' Brown, M. B., & Benedetti, J. K. (1977).
#' Sampling behavior of tests for correlation in two-way contingency tables.
#' \emph{Journal of the American Statistical Association}, 72, 309–315.
#'
#' Goodman, L. A., & Kruskal, W. H. (1954).
#' Measures of association for cross classifications.
#' \emph{Journal of the American Statistical Association}, 49, 732–764.
#'
#' Goodman, L. A., & Kruskal, W. H. (1963).
#' Measures of association for cross classifications III.
#' \emph{Journal of the American Statistical Association}, 58, 310–364.
#'
#' @examples
#'
#' # Example from SAS documentation (PROC FREQ)
#' # https://support.sas.com/documentation/
#'
#' tab <- as.table(rbind(
#'   c(26,26,23,18, 9),
#'   c( 6, 7, 9,14,23)
#' ))
#'
#' gkGamma(tab, conf.level=0.95)
#'
#' # Vector interface
#' gkGamma(mtcars$wt, mtcars$mpg)
#'




#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal
#'
#'
#' @export
gkGamma <- function(x, y = NULL,
                    conf.level = NA, ...){
  
  res <- assocsXY(
    x = x,
    y = y,
    which = "gamma",
    conf.level = conf.level
  )
  
  if(is.na(conf.level))
    unname(res[[1]])
  else
    res[[1]]
  
}


