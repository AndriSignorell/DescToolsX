
#' Kendall's \eqn{\tau_b} (Tau-b)
#'
#' @description
#' Computes Kendall's \eqn{\tau_b}, a measure of association for ordinal variables.
#' The function provides interfaces for both contingency tables and paired vectors.
#'
#' @details
#' Kendall's \eqn{\tau_b} is a symmetric rank-based measure of association that
#' adjusts for ties in both variables. It is defined as
#'
#' \deqn{
#' \tau_b = \frac{C - D}{\sqrt{(C + D + T_X)(C + D + T_Y)}}
#' }
#'
#' where \eqn{C} and \eqn{D} denote the number of concordant and discordant pairs,
#' and \eqn{T_X}, \eqn{T_Y} the number of pairs tied on \eqn{X} and \eqn{Y},
#' respectively.
#'
#' \eqn{\tau_b} takes values in \eqn{[-1, 1]}. Values close to \eqn{1} indicate
#' strong positive association, values close to \eqn{-1} strong negative association.
#'
#' In contrast to Somers' D, Kendall's \eqn{\tau_b} is symmetric:
#' \code{kendallTauB(x, y)} = \code{kendallTauB(y, x)}.
#'
#' The estimator is equivalent to \code{cor(x, y, method="kendall")} for vectors,
#' but additionally provides confidence intervals.
#'
#' @param x a numeric vector or a contingency table (matrix or table)
#' @param y optional numeric vector. If supplied, must have the same length as \code{x}.
#' @param conf.level confidence level for confidence intervals. If \code{NA},
#'   no confidence interval is returned.
#' @param \dots further arguments passed to \code{\link{table}} in the vector interface
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Kendall's tau-b}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @seealso
#' \code{\link{cor}} for the standard Kendall correlation without confidence intervals.
#'
#' @references
#' Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons, pp. 57–59.
#'
#' Kendall, M. (1955) \emph{Rank Correlation Methods}, Second Edition.
#' London: Charles Griffin and Co.
#'
#' Brown, M. B., & Benedetti, J. K. (1977).
#' Sampling behavior of tests for correlation in two-way contingency tables.
#' \emph{Journal of the American Statistical Association}, 72, 309–315.
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
#' kendallTauB(tab, conf.level=0.95)
#'
#' # Vector interface
#' kendallTauB(mtcars$wt, mtcars$mpg)
#'
#'
#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal  
#' @concept rank-correlation
#'
#'
#' @export
kendallTauB <- function(x, y = NULL,
                        conf.level = NA,
                        ...){
  
  res <- ordAssocs(
    x = x,
    y = y,
    which = "tauB",
    conf.level = conf.level
  )
  
  # unwrap single element list
  if(is.na(conf.level))
    unname(res[[1]])
  else
    res[[1]]
  
}
