
#' Stuart's \eqn{\tau_c} (Tau-c)
#'
#' @description
#' Computes Stuart's \eqn{\tau_c}, a measure of association for ordinal variables.
#' The function provides interfaces for both contingency tables and paired vectors.
#'
#' @details
#' Stuart's \eqn{\tau_c} is a modification of Kendall's tau that adjusts for
#' both ties and table size. It is defined as
#'
#' \deqn{
#' \tau_c = \frac{2 m (C - D)}{n^2 (m - 1)}
#' }
#'
#' where \eqn{C} and \eqn{D} denote the number of concordant and discordant pairs,
#' \eqn{n} is the total number of observations, and
#' \eqn{m = \min(R, C)} is the smaller dimension of the contingency table.
#'
#' \eqn{\tau_c} takes values in \eqn{[-1, 1]}. Values close to \eqn{1} indicate
#' strong positive association, values close to \eqn{-1} strong negative association.
#'
#' In contrast to Kendall's \eqn{\tau_b}, Stuart's \eqn{\tau_c} includes an adjustment
#' for table size and is often preferred when the two variables have different
#' numbers of categories.
#'
#' Stuart's \eqn{\tau_c} is symmetric:
#' \code{stuartTauC(x, y)} = \code{stuartTauC(y, x)}.
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
#' \code{\link{kendallTauB}} for Kendall's \eqn{\tau_b},
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
#'   c(26,26,23,18,9),
#'   c(6,7,9,14,23)
#' ))
#'
#' stuartTauC(tab, conf.level=0.95)
#'
#' # Vector interface
#' stuartTauC(mtcars$wt, mtcars$mpg)
#'



#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal
#'
#'
#' @export
stuartTauC <- function(x, y = NULL,
                       conf.level = NA,
                       ...){
  
  res <- assocsXY(
    x = x,
    y = y,
    which = "tau-c",
    conf.level = conf.level
  )
  
  if(is.na(conf.level))
    unname(res[[1]])
  else
    res[[1]]

}


